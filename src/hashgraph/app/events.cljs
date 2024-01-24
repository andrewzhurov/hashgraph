(ns hashgraph.app.events
  (:require
   [cljs.math :refer [floor ceil]]
   [hashgraph.main :as hg]
   [hashgraph.members :as hg-members]
   [hashgraph.utils :refer [update!] :refer-macros [l defn*]]
   [taoensso.timbre :refer-macros [spy]]
   [taoensso.tufte :as tufte :refer [defnp p profiled profile]]))

(def evt-view-r 10)
(def evt-view-s (* evt-view-r 2))
(def evt-padding evt-view-s)

(def wit-view-r (* 2 evt-view-r))

(def vote-view-r (-> wit-view-r
                     (- (-> wit-view-r
                            (- evt-view-r)
                            (/ 2)))))
(def vote-view-circumferance (* 2 js/Math.PI vote-view-r))
#_(def vote-circumferance
  (memoize
   (fn [event]
     (let [member (-> event hg/event->member)]
       (-> vote-view-circumferance
           (/ hg/total-stake)
           (* (:member/stake member)))))))
(def vote-view-stroke-width (/ vote-view-r 2))

(def idx-view-position-x
  (fn [idx]
    (-> idx
        (* (+ evt-padding evt-view-s))
        (+ evt-view-r evt-padding))))

(def evt-view-position-x
  (fn [evt]
    (-> evt :event/creator hg-members/member-name->person :member/idx idx-view-position-x)))

(def t->view-position-y
  (memoize
   (fn [t]
     (-> t
         (* (+ evt-view-s evt-padding))
         (+ evt-view-r evt-padding)))))

(def evt-view-position-y
  (memoize ;; over time will become slower to lookup than to calc
   (fn [evt]
     (-> (:event/creation-time evt)
         t->view-position-y))))

(def evt-view-position
  (memoize
   (fn [evt]
     (let [x (evt-view-position-x evt)
           y (evt-view-position-y evt)]
       [x y]))))

(def load-area-height 200)

(defn ->viz-height [event]
  (+ (evt-view-position-y event)
     (* 2 load-area-height)))


(def c->hg-init (hash-map "Alice" {:event/creator      "Alice"
                                   :event/creation-time 1}))
(defonce *c->hg (atom c->hg-init))

(defn pick-random-n-items [n items]
  (if (or (zero? n) (empty? items))
    #{}
    (let [item (rand-nth (vec items))]
      (conj (pick-random-n-items (dec n) (disj (set items) item))
            item))))

#_
(def event->pending-invitor-event
  (memoize
   (fn [event]
     (let [pending-invitor-event
           (-> event
               (hg/parents)
               (->> (some event->pending-invitor-event))) ;; bug. perhaps issue on stake-map change
           received?
           (fn [evt pending-evt]
             (some (fn [re] (= (:received-event/event re) pending-evt)) (take-while some? (iterate :received-event/prev-received-event (hg/last-received-event evt)))))]
       (or (and pending-invitor-event
                (not (received? event pending-invitor-event))
                pending-invitor-event)
           (and (let [member-invites-in-this-round (hg/event->most-weigted-member event)]
                  (= member-invites-in-this-round (:event/creator event)))
                event))))))

#_
(def invitor-event->invitee
  (memoize
   (fn [invitor-event]
     (let [invitee-idx      (-> invitor-event
                                hg/event->present-members
                                (->> (map :member/idx)
                                     (apply max))
                                (inc))
           to-invite-member (nth hg-members/people invitee-idx nil)]
       (:member/name to-invite-member)))))

#_
(def event->invitee
  (memoize
   (fn [event]
     (let [invitor-event (-> event event->pending-invitor-event)]
       (and (= invitor-event event)
            (invitor-event->invitee invitor-event))))))

(defn make-share-stake-tx [from to percent]
  {:tx/fn-id :share-stake
   :tx/args  {:share-stake/from    from
              :share-stake/to      to
              :share-stake/percent percent}})

#_
(defn maybe-assoc-invitee [evt]
  (let [invitee (event->invitee evt)]
    (cond-> evt
      invitee (assoc :event/tx (make-share-stake-tx (:event/creator evt) invitee 50)))))

(def names-without-name
  (fn [names name]
    (vec (disj (set names) name))))

(defn with-once-per-round-random-share-stake-tx [evt]
  (cond-> evt
    (rand-nth [false false false true]) #_(some-> evt hg/self-parent (->> hg/witness? evt))
    (assoc :event/tx (let [from    (:event/creator evt)
                           to      (rand-nth (names-without-name hg-members/names from))
                           percent (rand-nth (if (= (:event/creator evt) "Alice")
                                               [33 66] ;; Alice's used as main concluding member, so let's not drop her from the system by sharing all her stake out
                                               [33 66 100]))]
                       (make-share-stake-tx from to percent)))))

(defn* issue [creator->hg enough?]
  (let [alice-tip            (hg/creator-hg-map->?alice-tip creator->hg)
        cr                   (-> alice-tip hg/->concluded-round)
        stake-map            (-> cr hg/concluded-round->stake-map)
        current-member-names (set (keys stake-map))
        sender               (rand-nth (vec current-member-names))
        ?sender-hg           (get creator->hg sender)

        new-creator>hg
        (if (nil? ?sender-hg)
          (let [new-sender-hg {:event/creator sender
                               :event/creation-time
                               (if-let [highest-hg-creation-time (some-> creator->hg
                                                                         vals
                                                                         (->> (map :event/creation-time)
                                                                              (sort))
                                                                         last)]
                                 (inc highest-hg-creation-time)
                                 0)}]
            (assoc creator->hg sender new-sender-hg))

          (let [sender-hg                    ?sender-hg
                receivers                    (-> current-member-names
                                                 (disj sender)
                                                 (->> (remove (fn [creator]
                                                                (when-let [creator-hg (creator->hg creator)]
                                                                  (hg/ancestor? creator-hg sender-hg)))))) ;; gets increasingly more costly
                reachable-receivers          (filter (fn [receiver]
                                                       (let [reaching? #(rand-nth [false false false false false false false true])]
                                                         receiver
                                                         ;; TODO hardly reachable
                                                         #_(and (or (not (contains? hardly-reachable-members receiver))
                                                                    (reaching?))
                                                                (or (not (contains? hardly-reachable-members sender))
                                                                    (reaching?))
                                                                receiver)))
                                                     receivers)
                selected-reachable-receivers (pick-random-n-items (max 1 (ceil (/ (count receivers) 3))) reachable-receivers)]
            (->> selected-reachable-receivers
                 (reduce (fn [creator->hg receiver]
                           (update creator->hg receiver
                                   (fn [?receiver-hg]
                                     (let [new-receiver-hg
                                           (cond-> {:event/creator      receiver
                                                    :event/other-parent sender-hg
                                                    :event/creation-time (inc (max (:event/creation-time sender-hg)
                                                                                   (:event/creation-time ?receiver-hg)))}
                                             ?receiver-hg (assoc :event/self-parent ?receiver-hg)
                                             :always      with-once-per-round-random-share-stake-tx)]
                                       new-receiver-hg))))
                         creator->hg))))]
    (if (enough? new-creator>hg)
      new-creator>hg
      (recur new-creator>hg enough?))))

(defn issue! [enough?]
  (swap! *c->hg (fn [c->hg] (issue c->hg enough?))))
