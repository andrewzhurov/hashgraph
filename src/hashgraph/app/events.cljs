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

(def names-without-name
  (fn [names name]
    (vec (disj (set names) name))))

(defn make-share-stake-tx [from to ratio]
  (hash-map :tx/fn-id :share-stake
            :tx/args  [{:share-stake/from  from
                        :share-stake/to    to
                        :share-stake/ratio ratio}]))

(def share-stake-tx-delay    60)
(def share-stake-tx-n-events 30)
(defn with-occasional-share-stake-tx [evt events>]
  (let [events-count (- (count events>) share-stake-tx-delay)
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
(defn with-occasional-inc-counter-tx [evt events>]
  (let [events-count (- (count events>) inc-counter-tx-delay)]
    (cond-> evt
      (and (not (neg? events-count))
           (if-not true #_ @*first-inc-counter-tx-issued? ;; nah, make the joke random, gets old otherwise
             (when (= (hg/creator evt) "Elon")
               (vreset! *first-inc-counter-tx-issued? true))
             (zero? (mod events-count inc-counter-tx-n-events))))
      (assoc :event/tx {:tx/fn-id :inc-counter}))))

;; TODO Try with :only-last
(defn* ^:memoizing events>->c->hg [[event & rest-events>]]
  (if (nil? event)
    (hash-map)
    (let [prev-c->hgs (events>->c->hg rest-events>)]
      (assoc prev-c->hgs (:event/creator event) event))))

#_
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
(defonce slowdown-period-ms (if hga-view/view-mode-horizontal? 2200 1000)) ;; better be relative to the size of playback area
(defonce *in-slowdown? (atom true))
(defn ->next-creation-time [prev-creation-time & {:keys [dry-run?]
                                                  :or {dry-run? false}}]
  (let [next-creation-time-candidate
        (-> prev-creation-time
            (+ hga-view/evt-offset)
            (+ (-> slowdown-period-ms
                   (- (/ prev-creation-time 2))
                   (max 0)
                   (/ 15)))
            ;; add small random offset, to ensure creation-time is distinct
            ;; would be enough to give [0; members-count]
            (cond->
                (not dry-run?) (+ (rand-int (* (count hg-members/names) 2)))) ;; * 2 to give more leeway
            ceil)]
    (if dry-run?
      next-creation-time-candidate
      (if (not (contains? @*taken-creation-times next-creation-time-candidate))
        (do (when (and @*in-slowdown?
                       (> next-creation-time-candidate slowdown-period-ms))
              (reset! *in-slowdown? false))
            (swap! *taken-creation-times conj next-creation-time-candidate)
            next-creation-time-candidate)
        ;; try again, with a different random offset
        (->next-creation-time prev-creation-time)))))

(defn ->chat [{:chatter/keys [before> prev-t t after>] :as chatter} sender]
  (let [all>          (concat after> before>)
        creator->hg   (-> all> events>->c->hg)
        main-tip      (-> all> hg/events>->main-tip)
        cr            (-> main-tip hg/->concluded-round)
        stake-map     (-> cr hg/concluded-round->stake-map)
        stake-holders (set (keys stake-map))
        ?sender-hg    (get creator->hg sender)

        new-after>*
        (if (nil? ?sender-hg)
          (let [new-sender-hg (hash-map :event/creator       sender
                                        :event/creation-time (->next-creation-time t))]
            (conj after> new-sender-hg))

          (let [sender-hg ?sender-hg
                receivers (-> stake-holders
                              (disj sender)
                              (->> (remove (fn [creator]
                                             (when-let [creator-hg (creator->hg creator)]
                                               (hg/ancestor? creator-hg sender-hg)))))) ;; gets increasingly more costly

                ;; TODO add connectivity
                selected-receivers (utils/random-nths (max 1 (ceil (/ (count receivers) 3))) receivers)]

            (->> selected-receivers
                 (reduce (fn [after>-acc receiver]
                           (conj after>-acc
                                 (let [?receiver-hg (creator->hg receiver)
                                       all>         (concat after>-acc before>)
                                       new-receiver-hg
                                       (cond-> (hash-map :event/creator      receiver
                                                         :event/other-parent sender-hg
                                                         :event/creation-time
                                                         (-> (if @*in-slowdown?
                                                               (:event/creation-time (first all>))
                                                               (max (:event/creation-time sender-hg)
                                                                    (:event/creation-time ?receiver-hg)
                                                                    t))
                                                             (->next-creation-time)))
                                         ?receiver-hg (assoc :event/self-parent ?receiver-hg)
                                         :always      (with-occasional-share-stake-tx all>)
                                         :always      (with-occasional-inc-counter-tx all>))]
                                   new-receiver-hg)))
                         after>))))
        new-after> (sort-by hg/creation-time > new-after>*)]
    (assoc chatter :chatter/after> new-after>)))

(defn ->chatter [{:chatter/keys [before> t after>] :as chatter}]
  (let [all>          (concat after> before>)
        main-tip      (-> all> hg/events>->main-tip)
        cr            (-> main-tip hg/->concluded-round)
        stake-map     (-> cr hg/concluded-round->stake-map)
        stake-holders (set (keys stake-map))

        new-chatter*              (reduce ->chat chatter stake-holders)
        new-after>                (:chatter/after> new-chatter*)
        new-t                     (->next-creation-time t :dry-run? true)
        [new-after> just-before>] (->> new-after> (split-with #(> (hg/creation-time %) new-t)))]
    (assoc new-chatter*
           :chatter/t            new-t
           :chatter/before>      (concat just-before> before>)
           :chatter/just-before> just-before>
           :chatter/after>       new-after>)))

(defonce ^:dynamic initial-events< '())

(defn ->events<
  ([] (reduce (fn [acc initial-evt] (cons initial-evt acc))
              (->events< #:chatter{:before> (reverse initial-events<)
                                   :t       (or (some->> initial-events< last hg/creation-time inc)
                                                0)
                                   :after>  '()})
              (reverse initial-events<)))
  ([prev-chatter]
   (lazy-seq (let [chatter      (->chatter prev-chatter)
                   just-before> (:chatter/just-before> chatter)]
               (reduce (fn [acc evt] (cons evt acc)) (->events< chatter) just-before>)))))

(defonce ^:dynamic events< '())

