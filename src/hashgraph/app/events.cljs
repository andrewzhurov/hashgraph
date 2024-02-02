(ns hashgraph.app.events
  (:require
   [cljs.math :refer [floor ceil]]
   [clojure.set :as set]
   [rum.core :as rum]
   [hashgraph.main :as hg]
   [hashgraph.members :as hg-members]
   [hashgraph.utils :refer [log!] :refer-macros [l defn*] :as utils]
   [taoensso.timbre :refer-macros [spy]]
   [taoensso.tufte :as tufte :refer [defnp p profiled profile]]))

(def evt-view-r 10)
(def evt-view-s (* evt-view-r 2))
(def sp-padding  (* evt-view-s 1.5))
(def hgs-padding  (* sp-padding 2))
(def evt-view-offset (+ evt-view-s sp-padding))
(def members-height 70)
(def window-height js/window.innerHeight)
(def window-size window-height)


(def load-area-height (+ sp-padding evt-view-s))
(def below-viz-buffer (* 20 evt-view-offset))
(def playback-size (-> window-size
                       (- load-area-height members-height)))
(def viz-offset playback-size)

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
        (* (+ hgs-padding evt-view-s))
        (+ evt-view-r hgs-padding))))

(def evt-view-position-x
  (fn [evt]
    (-> evt :event/creator hg-members/member-name->person :member/idx idx-view-position-x)))

(def t->view-position-y
  (memoize
   (fn [t]
     (-> t
         (+ viz-offset))
     #_
     (-> t
         (* (+ evt-view-s sp-padding))
         (+ evt-view-r sp-padding)
         (+ viz-offset)))))

(def evt-view-position-y
  (memoize ;; over time will become slower to lookup than to calc
   (fn [evt]
     (-> evt :event/creation-time t->view-position-y))))

(def evt-view-position
  (memoize
   (fn [evt]
     (let [x (evt-view-position-x evt)
           y (evt-view-position-y evt)]
       [x y]))))

(defn ->viz-height [event]
  (-> (or (some-> event
                  evt-view-position-y
                  (+ members-height)
                  (+ load-area-height))
          js/window.innerHeight)
      (+ below-viz-buffer)))

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

(defn make-share-stake-tx [from to ratio]
  (hash-map :tx/fn-id :share-stake
            :tx/args  [{:share-stake/from  from
                        :share-stake/to    to
                        :share-stake/ratio ratio}]))

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
    (zero? (rand-int 4)) #_(some-> evt hg/self-parent (->> hg/witness? evt))
    (assoc :event/tx (let [from    (:event/creator evt)
                           to      (rand-nth (names-without-name hg-members/names from))
                           percent (rand-nth (if (= (:event/creator evt) hg/main-creator)
                                               [[1 3] [2 3]] ;; initiator's used as main concluding member, so let's not drop her from the system by sharing all her stake out
                                               [[1 3] [2 3] [3 3]]))]
                       (make-share-stake-tx from to percent)))))

(defn* ^:memoizing events>->c->hg [[event & rest-events>]]
  (if (nil? event)
    (hash-map)
    (let [prev-c->hgs (events>->c->hg rest-events>)]
      (assoc prev-c->hgs (:event/creator event) event))))

(defn c->hg->events> [c->hg]
  (let [tips        (vals c->hg)
        _ (log! :tips tips)
        events> (-> tips
                    ;; this recursion may blowup
                    (->> (map hg/ancestors)
                         (log! :ancestors)
                         (apply set/union))
                    (into tips)
                    (->> (sort-by :event/creation-time >)))]
    events>))

(defn ->last-creation-time [events]
  (:event/creation-time (last events)))
(defn ->new-creation-time [events]
  (+ (->last-creation-time events) (rand-nth [20 30 40])))

;; Due to scroll being the playback position,
;; in order to be able to play to a particular event,
;; we need to make sure that events have distinct creation times (as int, because scroll position is an int).
;; To achieve that we can track taken creation times, and ensure that newly issued creation time is distinct.
(defonce *taken-creation-times (atom #{}))
(defn ->next-creation-time [prev-creation-time]
  (let [next-creation-time-candidate
        (-> prev-creation-time
            (+ evt-view-offset)
            ;; add small random offset, to ensure creation-time is distinct
            ;; would be enough to give [0; members-count]
            (+ (rand-int (* (count hg-members/names) 2))) ;; * 2 to give more leeway
            )]
    (if (not (contains? @*taken-creation-times next-creation-time-candidate))
      (do (swap! *taken-creation-times conj next-creation-time-candidate)
          next-creation-time-candidate)
      ;; try again, with a different random offset
      (->next-creation-time prev-creation-time))))

(defn* issue* [playback-events< left< ->enough? new-events<]
  (let [events<              (concat playback-events< left< new-events<)
        events>              (reverse events<)
        creator->hg          (-> events> events>->c->hg)
        main-tip             (-> events> hg/events>->main-tip)
        cr                   (-> main-tip hg/->concluded-round)
        stake-map            (-> cr hg/concluded-round->stake-map)
        current-member-names (set (keys stake-map))
        sender               (rand-nth (vec current-member-names))
        ?sender-hg           (get creator->hg sender)

        new-events<
        (if (nil? ?sender-hg)
          (let [new-sender-hg (hash-map :event/creator sender
                                        :event/creation-time
                                        (if-let [highest-hg-creation-time
                                                 (some-> creator->hg
                                                         vals
                                                         (->> (map :event/creation-time)
                                                              (sort))
                                                         last)]
                                          (->next-creation-time highest-hg-creation-time)
                                          0))]
            (conj new-events< new-sender-hg))

          (let [sender-hg                    ?sender-hg
                receivers                    (-> current-member-names
                                                 (disj sender)
                                                 (->> (remove (fn [creator]
                                                                (when-let [creator-hg (creator->hg creator)]
                                                                  (hg/ancestor? creator-hg sender-hg)))))) ;; gets increasingly more costly
                reachable-receivers          (filter (fn [receiver]
                                                       (let [reaching? #(zero? (rand-int 8))]
                                                         (and (or (not (contains? hg-members/hardly-reachable-member-names receiver))
                                                                  (reaching?))
                                                              (or (not (contains? hg-members/hardly-reachable-member-names sender))
                                                                  (reaching?))
                                                              receiver)))
                                                     receivers)
                selected-reachable-receivers (utils/random-nths (max 1 (ceil (/ (count receivers) 3))) reachable-receivers)]
            (->> selected-reachable-receivers
                 (reduce (fn [new-events-acc receiver]
                           (conj new-events-acc
                                 (let [?receiver-hg (creator->hg receiver)
                                       new-receiver-hg
                                       (cond-> (hash-map :event/creator      receiver
                                                         :event/other-parent sender-hg
                                                         :event/creation-time
                                                         (-> (max (:event/creation-time sender-hg)
                                                                  (:event/creation-time ?receiver-hg))
                                                             (->next-creation-time)))
                                         ?receiver-hg (assoc :event/self-parent ?receiver-hg)
                                         :always      with-once-per-round-random-share-stake-tx)]
                                   new-receiver-hg)))
                         new-events<))))]
    (if (->enough? new-events<)
      (sort-by :event/creation-time (concat left< new-events<))
      (recur playback-events< left< ->enough? new-events<))))

(defn issue [playback-events< left< ->enough?] (issue* playback-events< left< ->enough? []))
