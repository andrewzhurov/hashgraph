(ns hashgraph.app.events
  (:require
   [cljs.math :refer [floor ceil]]
   [clojure.set :as set]
   [rum.core :as rum]
   [hashgraph.main :as hg]
   [hashgraph.app.view :as hga-view]
   [hashgraph.members :as hg-members]
   [hashgraph.utils.core :refer [log!] :refer-macros [l defn*] :as utils]
   [taoensso.timbre :refer-macros [spy]]
   [taoensso.tufte :as tufte :refer [defnp p profiled profile]]))
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

(def share-stake-tx-delay    60)
(def share-stake-tx-n-events 30)
(defn with-occasional-share-stake-tx [evt events<]
  (let [events-count (- (count events<) share-stake-tx-delay)
        first-tx?    (zero? events-count)]
    (cond-> evt
      (and (not (neg? events-count))
           (zero? (mod events-count share-stake-tx-n-events)))
      (assoc :event/tx (let [from    (hg/creator evt)
                             to      (if first-tx?
                                       (rand-nth (vec (set/difference (set hg-members/names) (set hg-members/initial-member-names))))
                                       (rand-nth (names-without-name hg-members/names from)))
                             percent (rand-nth (if (= (hg/creator evt) hg/main-creator)
                                                 [[1 3] [2 3]] ;; initiator's used as main concluding member, so let's not drop her from the system by sharing all her stake out
                                                 [[1 3] [2 3] [3 3]]))]
                         (make-share-stake-tx from to percent))))))

(def inc-counter-tx-delay    25)
(def inc-counter-tx-n-events 24)
(def *first-inc-counter-tx-issued? (volatile! false))
(defn with-occasional-inc-counter-tx [evt events<]
  (let [events-count (- (count events<) inc-counter-tx-delay)]
    (cond-> evt
      (and (not (neg? events-count))
           (if-not true #_ @*first-inc-counter-tx-issued? ;; nah, make the joke random, gets old otherwise
             (when (= (hg/creator evt) "Elon")
               (vreset! *first-inc-counter-tx-issued? true))
             (zero? (mod events-count inc-counter-tx-n-events))))
      (assoc :event/tx {:tx/fn-id :inc-counter}))))

(defn* ^:memoizing events>->c->hg [[event & rest-events>]]
  (if (nil? event)
    (hash-map)
    (let [prev-c->hgs (events>->c->hg rest-events>)]
      (assoc prev-c->hgs (:event/creator event) event))))

(defn c->hg->events> [c->hg]
  (let [tips        (vals c->hg)
        events> (-> tips
                    ;; this recursion may blowup
                    (->> (map hg/ancestors)
                         (apply set/union))
                    (into tips)
                    (->> (sort-by :event/creation-time >)))]
    events>))

;; Due to scroll being the playback position,
;; in order to be able to play to a particular event,
;; we need to make sure that events have distinct creation times (as int, because scroll position is an int).
;; To achieve that we can track taken creation times, and ensure that newly issued creation time is distinct.
(defonce *taken-creation-times (atom #{}))
(defonce slowdown-period-ms 3000)
(defonce *in-slowdown? (atom true))
(defn ->next-creation-time [prev-creation-time]
  (let [next-creation-time-candidate
        (-> prev-creation-time
            (+ hga-view/evt-offset)
            (+ (-> slowdown-period-ms
                   (- (/ prev-creation-time 2))
                   (max 0)
                   (/ 15)))
            ;; add small random offset, to ensure creation-time is distinct
            ;; would be enough to give [0; members-count]
            (+ (rand-int (* (count hg-members/names) 2))) ;; * 2 to give more leeway
            ceil)]
    (if (not (contains? @*taken-creation-times next-creation-time-candidate))
      (do (when (and @*in-slowdown?
                     (> next-creation-time-candidate slowdown-period-ms))
            (reset! *in-slowdown? false))
          (swap! *taken-creation-times conj next-creation-time-candidate)
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
                                       all< (concat playback-events< left< new-events-acc)
                                       new-receiver-hg
                                       (cond-> (hash-map :event/creator      receiver
                                                         :event/other-parent sender-hg
                                                         :event/creation-time
                                                         (-> (if @*in-slowdown?
                                                               (:event/creation-time (first (sort-by :event/creation-time > (concat left< new-events-acc))))
                                                               (max (:event/creation-time sender-hg)
                                                                    (:event/creation-time ?receiver-hg)))
                                                             (->next-creation-time)))
                                         ?receiver-hg (assoc :event/self-parent ?receiver-hg)
                                         :always      (with-occasional-share-stake-tx all<)
                                         :always      (with-occasional-inc-counter-tx all<))]
                                   new-receiver-hg)))
                         new-events<))))
        new-events-sorted< (sort-by :event/creation-time new-events<)]
    (if (->enough? new-events<)
      new-events-sorted<
      (recur playback-events< left< ->enough? new-events-sorted<))))

(defn issue [playback-events< left< ->enough?] (issue* playback-events< left< ->enough? []))

;; perhaps craft an infinite lazy sequence of events (with iterate ?)
