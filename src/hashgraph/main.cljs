(ns hashgraph.main
  "Implementation of the Hashgraph algorithm. Likely misbehaving, but enough to get intuition of how it works via viz.
   This implementation sacrificed simplicity in favor of some performance.

   If you wish to grok on it, consider looking at the official algorithm.
   It's in functional form, simple and elegant (yet may be difficult to grasp).
   Can be found in the Appendix A (at the bottom) of the paper: https://www.swirlds.com/downloads/SWIRLDS-TR-2016-01.pdf"

  (:refer-clojure
   :exclude [parents])
  (:require [cljs.math :refer [floor ceil]]
            [garden.color :as gc]
            [hashgraph.members :as hg-members]
            [hashgraph.utils.core :refer-macros [defn* defnm defnml l cl] :refer [*mem* *from-mem*]]
            [hashgraph.utils2 :refer-macros [td time3]]
            [taoensso.timbre :refer-macros [spy] :refer [log info]]
            [taoensso.tufte :as tufte :refer [defnp fnp p pspy profiled profile]]
            [clojure.set :refer [union] :as set]))

;; no point in memoize, it'll be more costly than looking up from x
(def creator :event/creator)
(def self-parent :event/self-parent)
(def other-parent :event/other-parent)
(def creation-time :event/creation-time)

(defn parents
  "Set of events that are parents of x."
  [x]
  (cond-> #{}
    (other-parent x) (conj (other-parent x))
    (self-parent x)  (conj (self-parent x))))

(defn self-parent?  [x y] (identical? (self-parent x) y))
(defn other-parent? [x y] (identical? (other-parent x) y))

(defn parent? [x y]
  (or (self-parent? x y)
      (other-parent? x y)))

(defn* ^:memoizing ancestors
  [x]
  ;; does not consider x an ancestor of itself, which is more intuitive, but diverges from the spec
  ;; however, overall behavior of algorithm is the same, since this logic's moved into see?
  (cond-> #{}
    (self-parent x) (-> (conj (self-parent x))
                        (set/union (ancestors (self-parent x))))
    (other-parent x) (-> (conj (other-parent x))
                         (set/union (ancestors (other-parent x))))))

(defn* ancestor? ;; damm costly, don't use
  [x y]
  (contains? (ancestors x) y)
  #_(or (parent? x y)
      (some-> (self-parent x) (ancestor? y))
      (some-> (other-parent x) (ancestor? y))
      #_(some #(ancestor? % y) (parents x))))

#_
(defn* self-ancestor?
  [x y]
  (and (some? (self-parent x))
       (or (self-parent? x y)
           (self-ancestor? (self-parent x) y))))

#_
(def many-creators?
  (memoize
   (fn [events]
     (-> events
         (->> (map creator))
         distinct
         count
         (> hg-members/many)))))

#_
(def events
  "Returns a set of events known to x."
  (memoize
   (fn [x]
     (cond-> #{}
       (self-parent x)  (-> (conj (self-parent x))
                            (into (events (self-parent x))))
       (other-parent x) (-> (conj (other-parent x))
                            (into (events (other-parent x))))))))


#_
(def children
  "Set of events that are children of y."
  (memoize
   (fn [evts y]
     (->> evts (filter (fn [z] (or (parent? z y)
                                   (self-parent? z y))))))))

#_
(def self-children
  "Set of events that are self-children of y, as known to x."
  (memoize
   (fn [x y]
     (-> x
         events
         (->> (filter (fn [z] (self-parent? z x))))))))

#_
(def tips "Latest events by members"
  (memoize (fn [evts]
             (->> evts (into #{} (remove (fn [x] (some (fn [y] (self-parent? y x)) evts))))))))
#_
(def bottoms "Earliest events by members"
  (memoize (fn [evts]
             (->> evts (into #{} (remove (fn [x] (some (fn [y] (self-parent? x y)) evts))))))))

(def index
  "Index position of event x, as max of index position of self-parent and parent"
  (memoize
   (fn [x]
     (if (empty? (parents x))
       0
       (inc (apply max (map index (parents x))))))))

#_
(def self-ancestors
  (memoize
   (fn [x]
     (if (nil? (self-parent x))
       '()
       (conj (self-ancestors (self-parent x)) (self-parent x))))))

#_
(def self-descendants
  "Returns a set of self-descendants of y, as known by x."
  (memoize
   (fn [x y]
     (-> x
         events
         (->> (filter (fn [z] (self-ancestor? z y))))))))

#_
(def in-fork?
  "Whether y is in a fork, as known to x."
  (memoize
   (fn [x y]
     (or (some-> y
                 self-parent
                 (->> (self-children x))
                 count
                 (> 1))
         (some->> y self-parent (in-fork? x))))))

#_
(defn* ^:memoizing see
  [x]
  (apply set/union
         #{x}
         (map #(see %) (parents x))))

(defn* see?
  [x y]
  ;; TODO add not in fork
  (or (identical? x y)
      (ancestor? x y)))

(declare ->round-number)
(declare witness?)
(defn ->see-r-paths
  ([event cr] (->see-r-paths event cr (->round-number event cr)))
  ([event cr r]
   (let [evt-r (->round-number event cr)]
     (if (< evt-r r)
       (list)
       (let [prev (concat (some-> event self-parent (->see-r-paths cr r))
                          (some-> event other-parent (->see-r-paths cr r)))]
         (-> prev
             (->> (map (fn [path] (conj path event))))
             (cond->
                 (and (= evt-r r) (witness? event cr))
               (conj [event]))))))))

(declare concluded-round->stake-map)
(declare many-stake)
(defn ->strongly-see-r-paths [event cr r]
  (let [stake-map (concluded-round->stake-map cr)]
    (->> (->see-r-paths event cr r)
         (filter (fn [path] (-> (->> path
                                     (group-by creator)
                                     keys
                                     (map stake-map)
                                     (reduce +))
                                (> many-stake))))
         (map-indexed (fn [idx s-path] (with-meta s-path {:strongly-seen-path-idx idx}))))))

#_
(def ->seen-by
  "Events that seen y, as known to x."
  (memoize
   (fn [x y]
     #_(l {:id [:seen-by [x y]]} {:x x :y y})
     #_(l {:id [:seen-by [x y]] :key :seen-by})
     (cond-> #{}
       (see? x y)                         (conj x)
       (some-> (self-parent x) (see? y))  (into (->seen-by (self-parent x) y))
       (some-> (other-parent x) (see? y)) (into (->seen-by (other-parent x) y))))))

(declare many-stake)
(declare concluded-round->stake-map)
#_
(def see-many-see? ;; strongly-see
  "Whether x see many stake from events that see y."
  (memoize
   (fn [x y ?cr]
     (let [seen-by (->seen-by x y)
           seen-stake (reduce + 0 (map (comp (?concluded-round->stake-map ?cr) :event/creator) seen-by))] ;; TODO make creators distinct
       (> seen-stake many-stake)))))


#_#_
(def merge-rws (fn [& rws] (apply merge-with #(merge-with into %1 %2) rws)))
(def round->witnesses->seen-by-creators
  "Round and its witnesses seen by creators, based on a concluded round."
  (memoize
   (fn [x ?cr] ;; maybe pass tip, but be wary of recursion loop
     (if (empty? (parents x))
       {1 {x #{(creator x)}}}

       (let [merged-r-w-sb (-> (parents x)
                               (->> (map (fn [px] (round->witnesses->seen-by-creators px ?cr)))
                                    (apply merge-rws)))
             max-r      (-> merged-r-w-sb keys (->> (apply max)))
             max-r-w-sb (get merged-r-w-sb max-r)
             ;; add seen-by creator
             new-max-r-w-sb (->> max-r-w-sb (into {} (map (fn [[w sb]] [w (conj sb (creator x))]))))

             latest-applicable-stake-map (?concluded-round->stake-map ?cr)
             strongly-seen-many?
             (-> new-max-r-w-sb
                 (->> (filter (fn [[_w sb]]
                                (-> sb
                                    (->> (map latest-applicable-stake-map sb)
                                         (reduce + 0))
                                    (> many-stake))))
                      (map (fn [[w _sb]] (latest-applicable-stake-map (:event/creator w))))
                      (reduce + 0))
                 (> many-stake))]
         (cond
           ;; became witness in new round?
           strongly-seen-many?
           {(inc max-r) {x #{(creator x)}}}

           ;; became witness in this round?
           (not (some (fn [[w _s]] (identical? (creator w) (creator x))) new-max-r-w-sb))
           {max-r (assoc new-max-r-w-sb x #{(creator x)})}

           ;; collect-my-seeing
           :else
           {max-r new-max-r-w-sb}))))))





#_(let [?prev-cr    (:concluded-round/prev-concluded-round cr)
        ?prev-round (->round x ?prev-cr nil)]
    (when (some-> ?prev-round :round/final?)
      ?prev-round))

#_
(cljs.pprint/pprint
 (macroexpand
  '(defnml my-fn
     "docstring"
     ([a] (reduce + (range 1000000)) *mem*))))
#_
(do
  (let [m-tr (transient (hash-map))
        n    10000]
    (doall (map #(assoc! m-tr % %) (range 0 n)))
    (time (doall (map #(get m-tr %) (range 0 n))))
    1)

  (let [m-tr (hash-map)
        n    10000]
    (doall (map #(assoc m-tr % %) (range 0 n)))
    (time (doall (map #(get m-tr %) (range 0 n))))
    1))

;; Which events see which events are accumulating, detected forks affect future seeing.
;; Which events strongly see which events, based on previous cr, are also accumulating.
;; Which events strongly see many which events, giving next round number, based on previous cr, are accumulating.
;; Votes are also accumulating.
;; Many votes conclusion is accumulating.
;; Round votes conclusion is accumulating.
;;
;; In the end, we have event->derived-state.
;; Pro: all relevant info is kept on the tip, efficient and easy to reason.
;; Cons: to lookup info for y, for viz, we need to lookup x that gave that info.
;;
;; Problem: Lookup of round y event, as of some descendant x.
;;   Requirements:
;;     1. efficient.
;;     2. does not sacrifice performance of core algorithm in favore of performance of viz.
;;
;;   Solution: tip+in-view-ys->x, so x event is the one that concluded everything for top events in view.
;;     Pro: this way we won't memoize derived-info for subsequent way-out-of-view tip events.
;;     Cons: set of in-view-events is huge.
;;     Implementation:
;;       1. have index tip+y->x, where x will become fixed at some point, as no more sebsequent events can add anything new for y.
;;          This is similar to how ?cr's round finality gets adopted, and other finality, for vote, votes, concluded round.
;;       2. Similarly to concluded round, each event holds only novel info in their derived-state, which includes self-parent's derived-state.
;;          I.e., we have a chain of derived-info.
;;          Pro: lookup of prev derived-info is near-instant.
;;          And an index x->derived-info, core index is to keep only for (self-parent tip), viz index is to keep for all x.
;;       3. Then, we would iterate down derived-state, dropping non-contributing ones, until the one giving info for some in-view top events.
;;          I.e., tip+in-view-top-ys->x. Viz index.
;;                And, underneath, tip+y->x, taking max x.
;;       4. Then, for an in-view parents of in-view-top-y, we can get its x, by (tip+y->x max-x y)
;;       Pro: seems to keep individual indexes more compact.
;;       Pro: view indexes are separate from main indexes.
;;       Cons: complex logic.
;;
;;   cr is what affects strongly-see, and subsequent round, vote, votes, round conclusion of y,
;;   Solution: cr+y->round


(declare ->round-info)

(defn* ^{:memoizing {:bind {:->in-mem? ->in-mem?
                            :->from-mem ->from-mem}}}
  ->round ;; see-many-see-many-see ;; see-many-strongly-see
  "Round number of y, as known to a previous round concluded x.
   It's either 1 if y has no parents,
   or a max round of events strongly seen by many (as known to x) +1."
  [x cr]

  ;; To efficiently compute round x we need to cater for two cases:
  ;; 1. When new cr arrives => rounds that been final need not be re-computed.
  ;; 2. When new event arrives => it's round will be atop max parent's cr. Since it's round will be atop max parent's round, a continuation of it's compute.
  ;;
  ;; To cater for 1, we can memoize round and it's finality as of some cr, and adopt final round in successive crs. Previous round can be looked up from mem.
  ;; To cater for 2, we can recur down the parents to find the latest final round, and run from it's cr.
  ;;
  ;; Joined algorithm will look like:
  ;; 1. When there's mem for the event, lookup the highest cr that event been run on from mem, if it's final - adopt immediately, else - run from the cr atop it.
  ;; 2. Learn max parents' cr, carry on from it.

  ;; 1. new cr - lookup whether final
  ;; TODO lookup if no mem exist first
  (let [[prev-round-mem prev-cr]
        #_(when-let [x-mem (get mem x)]
          (->> ?cr
               :concluded-round/prev-concluded-round ;; this fn would not be executed on a memoized ?cr
               (iterate :concluded-round/prev-concluded-round)
               (take-while some?)
               (some (fn [cr]
                       (when-let [round-mem (get x-mem cr)] ;; doesn't lookup nil cr
                         [round-mem cr])))))
        (->> cr
             :concluded-round/prev-concluded-round ;; this fn would not be executed on a memoized cr
             (iterate :concluded-round/prev-concluded-round)
             (take-while some?)
             ;; perhaps take n crs to check, e.g., 5, not all, to limit otheriwse increasing cost as crs become many
             (some (fn [cr]
                     (when (->in-mem? [x cr])
                       [(->from-mem [x cr]) cr]))))]
    ;; Once round r is settled, the _future_ rounds will reshuffle,
    ;; and the calculations for round r + 1 famous witnesses will be done using the new stake record.
    ;; source: https://hyp.is/QxYPUqyAEe6hUtsMYuakKQ/www.swirlds.com/downloads/SWIRLDS-TR-2016-01.pdf

    ;; So, once there is a cr that gave round number less or equal to cr+1 - it's final.
    ;; We need _less_ for cases when stake changed and more events received cr's round number.
    (or (when (:round/final? prev-round-mem)
          prev-round-mem)

        ;; 2. new event (or not cr not final)
        ;; or max parent's cr gives final round or the one after
        ;; but it's not guaranteed that parents have final rounds
        ;; and we'd like to memo for all crs anyways, so run from max parents' cr
        (let [p-rounds  (map (fn [p] (->round p cr)) (parents x))
              p-?crs    (->> p-rounds (map :round/cr))
              p-max-?cr (->> p-?crs
                             (sort-by :concluded-round/r)
                             last)
              crs-to-try
              (->> cr
                   (iterate :concluded-round/prev-concluded-round)
                   (take-while some?)
                   (take-while (fn [cr] (and (not (identical? cr p-max-?cr))
                                             #_(not (identical? cr prev-cr)))))
                   reverse)]
          (or (->> crs-to-try
                   (some (fn [cr] (let [cr-round (->round-info x cr)]
                                    (when (:round/final? cr-round) cr-round)))))
              (->round-info x cr))))))

(defn* ^:memoizing ->round-info
  [x cr]
  (let [stake-map (concluded-round->stake-map cr)
        xc        (creator x)
        xc-stake  (get stake-map xc)
        ?spx       (self-parent x)
        ?opx       (other-parent x)]
    (if (and (nil? ?spx)
             (nil? ?opx)) ;; will add (little) cost to all non-bottom events
      (hash-map :round/event    x
                :round/number   1
                :round/final?   true
                ;; many see many see (true) or witness creator -> many see (true) or seen by creators set

                :round/wc->sbcs {xc (with-meta #{xc} xc-stake)}
                :round/cr       cr)

      (let [?spx-round (some-> ?spx (->round cr))
            ?opx-round (some-> ?opx (->round cr))
            max-p-r    (max (some-> ?spx-round :round/number)
                            (some-> ?opx-round :round/number))

            both-eligible? (= (:round/number ?spx-round)
                              (:round/number ?opx-round))
            max-p-round    (max-key :round/number ?opx-round ?spx-round)
            acc            (if both-eligible?
                             (-> ?spx-round :round/wc->sbcs)
                             (if (identical? max-p-round ?opx-round)
                               {xc (with-meta #{xc} xc-stake)} ;; we're continuing opx-round, become a witness
                               {}))
            to-reduce      (if both-eligible?
                             (-> ?opx-round :round/wc->sbcs)
                             (:round/wc->sbcs max-p-round))

            wc->sbcs
            ;; Not pretty, but pretty efficient
            (reduce (fn [wc->sbcs-acc [wc op-sbcs]]
                      (let [sbcs-acc (get wc->sbcs-acc wc)]
                        (if (true? sbcs-acc)
                          wc->sbcs-acc

                          (if (true? op-sbcs)
                            (let [new-wcs-stake (+ (meta wc->sbcs-acc) (get stake-map wc))]
                              (if (> new-wcs-stake many-stake)
                                (reduced true)
                                (-> wc->sbcs-acc
                                    (assoc wc true)
                                    (with-meta new-wcs-stake))))

                            (if (= sbcs-acc op-sbcs)
                              wc->sbcs-acc

                              ;; carry on compute from previous biggest sbcs
                              (let [sbcs-acc       (or sbcs-acc (with-meta #{xc} xc-stake))
                                    sbcs-acc-count (count sbcs-acc)
                                    op-sbcs-count  (count op-sbcs)
                                    biggest        (if (> sbcs-acc-count op-sbcs-count)
                                                     sbcs-acc
                                                     op-sbcs)
                                    smallest       (if (> sbcs-acc-count op-sbcs-count)
                                                     op-sbcs
                                                     sbcs-acc)
                                    new-sbcs-acc
                                    (reduce (fn [biggest-sbcs-acc sbc]
                                              (if (biggest-sbcs-acc sbc)
                                                biggest-sbcs-acc
                                                (let [new-biggest-sbcs-acc-stake (+ (meta biggest-sbcs-acc) (get stake-map sbc))]
                                                  (if (> new-biggest-sbcs-acc-stake many-stake)
                                                    (reduced true)
                                                    (-> biggest-sbcs-acc
                                                        (conj sbc)
                                                        (with-meta new-biggest-sbcs-acc-stake))))))
                                            biggest
                                            smallest)]
                                (if (true? new-sbcs-acc)
                                  (let [new-wcs-stake (+ (meta wc->sbcs-acc) (get stake-map wc))]
                                    (if (> new-wcs-stake many-stake)
                                      (reduced true)
                                      (-> wc->sbcs-acc
                                          (assoc wc true)
                                          (with-meta new-wcs-stake))))
                                  (assoc wc->sbcs-acc wc new-sbcs-acc))))))))
                    acc
                    to-reduce)
            round-next?  (true? wc->sbcs)
            r            (if round-next? (inc max-p-r) max-p-r)
            round-final? (<= r (or (inc (:concluded-round/r cr)) 1))]

        (hash-map
         :round/event    x
         :round/number   r
         :round/next?    round-next?
         :round/final?   round-final?
         :round/wc->sbcs (if round-next?
                           {xc (with-meta #{xc} xc-stake)}
                           wc->sbcs)
         :round/cr       cr)))))

(defn ->round-number
  [x cr]
  (:round/number (->round x cr)))


(defn witness?
  "Whether y is a witness, as known to x."
  [x cr]
  (or (nil? (self-parent x))
      (< (->round-number (self-parent x) cr)
         (->round-number x cr))))

(defn rounds-diff
  "Rounds diff between y and z, as known to x."
  [x y cr]
  (- (->round-number x cr)
     (->round-number y cr)))

(def d 1) ;; round in which voting starts
(def c 10) ;; each c's round is a coin flip round
(defn voting-round?
  [x y cr]
  (> (rounds-diff x y cr) d))

(defn voting-coin-flip-round?
  [x y cr]
  (and (voting-round? x y cr)
       (-> (rounds-diff x y cr) (mod c) (= 0))))

(defn signature [evt] (:event/signature evt))
(defn middle-bit [_sig] 1)

(defn self-witness
  "Self-witness of y, as known to concluded witness."
  [x cr]
  (cond (nil? (self-parent x))        nil
        (witness? (self-parent x) cr) (self-parent x)
        :else                         (self-witness (self-parent x) cr)))

(defn witness-or-self-witness
  [x cr]
  (if (witness? x cr)
    x
    (self-witness x cr)))

#_#_
(def ^:dynamic *rws* nil)
(defn* ^:memoizing ->round-witnesses* [x r ?cr]
  (let [x-r (->round-number x ?cr)]
    (when (and (= x-r r)
               (witness? x ?cr))
      (conj! *rws* x))
    (when (>= x-r r)
      (some-> (self-parent x) (->round-witnesses* r ?cr))
      (some-> (other-parent x) (->round-witnesses* r ?cr)))))

#_
(defn* ^:memoizing ->round-witnesses [x r ?cr]
  (binding [*rws* (transient [])]
    (->round-witnesses* x r ?cr)
    (distinct (persistent! *rws*))))

(defn* ^:memoizing round-witnesses
  "Round r witnesses, known to y, as known to x."
  [x r cr]
  (cond-> #{}
    (and (= (->round-number x cr) r) ;; can be optimized to not run if round > r
         (witness? x cr))
    (conj x)

    (>= (->round-number x cr) r)
    (cond->
        (self-parent  x) (set/union (round-witnesses (self-parent  x) r cr))
        (other-parent x) (set/union (round-witnesses (other-parent x) r cr)))))

(defn* ^:memoizing ->round-witness->seen-by-creators
  "Round r witnesses to seen by creator map, as known to x, based on a ?cr."
  [x r cr]
  (cond-> {}
    (and (= (->round-number x cr) r) ;; can be optimized to not run if round > r
         (witness? x cr))
    (assoc x #{(:event/creator x)})

    (>= (->round-number x cr) r)
    (cond->>
        (self-parent  x) (merge-with set/union (->round-witness->seen-by-creators (self-parent  x) r cr))
        (other-parent x) (merge-with set/union (->round-witness->seen-by-creators (other-parent x) r cr))
        :add-self        (into {} (map (fn [[rw sb-cs]] [rw (conj sb-cs (:event/creator x))]))))))

(declare many-stake)
(declare votes-stake-true)

(defn* ^:memoizing
  ->event->learned-member->learned-event
  "Events learned by creator learned by event, as known to x.
   {event {learned-by-member learned-by-event}}"
  [x]
  (let [m            (:event/creator x)
        sp-e->lm->le (or (some-> (self-parent x) ->event->learned-member->learned-event)
                         {})
        op-e->lm->le (or (some-> (other-parent x) ->event->learned-member->learned-event)
                         {})
        e->lm->le    (merge-with merge
                                 sp-e->lm->le
                                 op-e->lm->le)]
    (-> e->lm->le
        (->> (into {} (map (fn [[e lm->le]]
                             [e (cond-> lm->le
                                  (not (get lm->le m)) (assoc m x))]))))
        (assoc x {m x}))))

#_
(defn* ^:memoizing
  ->learned-events
  "Events that learned of y, as known to x."
  [x y]
  (-> (->event->learned-member->learned-event x)
      (get y)
      vals))


(defn make-vote
  [cr from-event to-event vote-type vote-value & [vote-rest]]
  (let [stake-map (concluded-round->stake-map cr)]
    (merge
     (hash-map :vote/voter   from-event
               :vote/votee   to-event
               :vote/type    vote-type
               :vote/value   vote-value
               :vote/stake   (get stake-map (:event/creator from-event))
               :vote/atop-cr cr)
     vote-rest)))

(defn vote-see? [x y cr] (= (rounds-diff x y cr) 1)) ;; TODO parameterize with d
(defn vote-see  [x y cr] (make-vote cr x y :see (boolean (see? x y))))

(declare self-witness)
(declare ->vote)
(defn vote-copy? [x y cr] (when-let [sw (self-witness x cr)]
                            (when (voting-round? sw y cr)
                              (:vote/final? (->vote sw y cr)))))
(defn vote-copy [x y cr]
  (let [vote-copied (->vote (self-witness x cr) y cr)]
    (make-vote cr x y :copy (:vote/value vote-copied) {:vote/final?      true
                                                       :vote/vote-copied vote-copied})))

(declare votes-stake-fract-true)
(declare many-stake)
(defn vote-coin-flip? [x y cr]
  (and (voting-coin-flip-round? x y cr)
       (>= (votes-stake-fract-true x y cr) (-> many-stake (/ 3)))
       (<= (votes-stake-fract-true x y cr) (-> many-stake (/ 3) (* 2)))))
(defn vote-coin-flip [x y cr] (make-vote cr x y :coin-flip (= 1 (middle-bit (signature x)))))

(declare votes-stake-false)
(defn vote-for-majority [x y cr]
  (let [final? (or (> (votes-stake-true x y cr) many-stake)
                   (> (votes-stake-false x y cr) many-stake))]
    (make-vote cr x y :for-majority (>= (votes-stake-fract-true x y cr) (/ 1 2)) (when final? {:vote/final? true}))))

(defn ->vote
  "Vote of y about fame of z, as known to x."
  [x y cr]
  (cond (vote-see? x y cr)       (vote-see x y cr)
        (vote-copy? x y cr)      (vote-copy x y cr)
        (vote-coin-flip? x y cr) (vote-coin-flip x y cr)
        :else                    (vote-for-majority x y cr)))

(defn* ^:memoizing ->votes
  "Votes on fame of x from witnesses seen by many in the round before x, based on concluded-round."
  [x y cr]
  (let [rw->sbcs  (->round-witness->seen-by-creators x (dec (->round-number x cr)) cr)
        stake-map (concluded-round->stake-map cr)]
    ;; TODO switch to transduce
    ;; TODO peek eligible rws of x from (->round x)
    (->> rw->sbcs
         (filter (fn [[_rw sbcs]] (-> sbcs
                                      (->> (map stake-map)
                                           (reduce + 0))
                                      (> many-stake))))
         (map (fn [[rw _sbcs]] (->vote rw y cr))))))

;; TODO maybe switch to transduce
(defn votes-stake-true  [x y ?cr] (->> (->votes x y ?cr) (filter :vote/value) (map :vote/stake) (reduce + 0)))
(defn votes-stake-false [x y ?cr] (->> (->votes x y ?cr) (remove :vote/value) (map :vote/stake) (reduce + 0)))

(defn votes-stake-fract-true
  [x y ?cr]
  (let [stake-true  (votes-stake-true x y ?cr)
        stake-false (votes-stake-false x y ?cr)]
    (/ stake-true
       (max 1 (+ stake-true stake-false)))))

(declare many-stake)
(declare ->concluded-round)

;; cr       -> rw  -> sbc
;; cr -> re -> etr -> le
;; r        -> w   -> sbc
;; they're about the same - events that learned about an event

(declare ?received-event->stake-map)
(declare concluded-round->event-to-receive->learned-events)
(declare concluded-round->received-event)
(declare share-stake-log->stake-map)
(declare initial-share-stake-log)

;; TODO try with :only-last? ;; won't fly on rewind
(defn* ^:memoizing ->concluded-round
  "Whether the round has fame of all it's witnesses concluded,
   and the previous round, if there is one, been concluded, as known to x."
  [x]
  (let [prev-cr (or (some-> (self-parent x) (->concluded-round))
                    {:concluded-round/r 0
                     :concluded-round/stake-map (share-stake-log->stake-map initial-share-stake-log)})]
    (loop [cr prev-cr]
      (if (not (witness? x cr))
        cr

        (let [next-cr-r  (inc (:concluded-round/r cr))
              next-crw-r (-> next-cr-r (+ d) inc) ;; perhaps not needed
              wx-r       (->round-number x cr)]

          (if (< wx-r next-crw-r)
            cr

            ;; x is a witness able to try to conclude
            (let [ws    (round-witnesses x next-cr-r cr)
                  ;; Derranging from the spec by making concluding witness vote,
                  ;; instead of collecting votes from r-1 witnesses.
                  ;; The result's the same, code seems to be simpler.
                  votes (->> ws (map (fn [w] (->vote x w cr))))
                  ]

              (if-not (->> votes (every? :vote/final?))
                cr

                (let [ufws    (->> votes
                                   (filter :vote/value)
                                   (map :vote/votee))
                      next-cr (hash-map :concluded-round/r                    next-cr-r
                                        :concluded-round/witness-concluded    x
                                        :concluded-round/ws                   ws
                                        :concluded-round/votes                votes
                                        :concluded-round/ufws                 ufws
                                        :concluded-round/prev-concluded-round cr)

                      etr->les (concluded-round->event-to-receive->learned-events next-cr)
                      es-nr*   (transient [])
                      es-r*    (transient [])
                      _        (let [ufws-creators (into #{} (map :event/creator ufws))]
                                 (doseq [[etr les] etr->les]
                                   (if (set/subset? ufws-creators (set (map :event/creator les)))
                                     (conj! es-r* etr)
                                     (conj! es-nr* etr))))
                      es-nr    (set (persistent! es-nr*))
                      es-r     (set (persistent! es-r*))
                      next-cr  (-> next-cr
                                   (assoc :concluded-round/etr->les etr->les)
                                   (assoc :concluded-round/es-nr es-nr)
                                   (assoc :concluded-round/es-r  es-r))

                      last-received-event (concluded-round->received-event next-cr)
                      next-cr             (-> next-cr
                                              ;; Here we kinda build cr->deriving-fn-name->derived-value indexes by hand.
                                              ;; Because it increases performance, compared to deriving-fn-name->cr->derived-value.
                                              ;; However, it couples code. Now we need to pass this index to deriving-fns, so it can lookup from it efficiently.
                                              ;; Alternative is to pass context event x.
                                              ;; Pro of rolling derived-info, is that it's all we need to progress onwards. Current derived-info is all we need to compute the next one.
                                              ;; Although it's not strictly true, there may be novel pretty old events, to settle on their round we'd need derived info as of that time...
                                              ;; Con in that for viz we need info about all previous events, not only the latest.
                                              ;; So we're back to that we need info about y in context of x.
                                              ;;
                                              ;; Perhaps instead of building this index by hand, we can make derived out of cr indexes smarter about where they look up from.
                                              ;; We can derive an index cr->deriving-fn-name->derived-value, and lookup from there.
                                              ;; We can pass x as context, however, x can conclude multiple rounds, so deriving-fns would need to be parameterized with r to resolve x+r to the corresponding cr.
                                              (assoc :concluded-round/stake-map (?received-event->stake-map last-received-event))
                                              (cond->
                                                  last-received-event (assoc :concluded-round/last-received-event last-received-event)))]
                  ;; try to conclude next round
                  (recur next-cr))))))))))


(defn* ^:memoizing ->event-to-receive->learned-event ;; will slowdown over time due to mem lookup
  [x ->to-continue? ->to-receive?]
  (if (->to-continue? x)
    (let [etr->le* (reduce
                    (fn [sp-etr->le [op-etr _op-le]]
                      (if (get sp-etr->le op-etr)
                        sp-etr->le
                        (assoc! sp-etr->le op-etr x)))
                    (transient (or (some-> (self-parent x) (->event-to-receive->learned-event ->to-continue? ->to-receive?))
                                   (hash-map)))
                    (some-> (other-parent x) (->event-to-receive->learned-event ->to-continue? ->to-receive?)))]
      (cond-> etr->le*
        (->to-receive? x) (assoc! x x)
        :always persistent!))

    (hash-map)))

(defn* concluded-round->event-to-receive->learned-events
  [cr]
  (let [cr-r           (:concluded-round/r cr)
        ?prev-cr       (:concluded-round/prev-concluded-round cr)

        ->to-receive?
        (fn [event]
          (let [event-r (->round-number event ?prev-cr)]
            (when (< event-r cr-r)
              (let [?receiving-cr
                    (first (into []
                                 (comp (take-while some?)
                                       (drop-while (fn [prev-cr] (>= event-r (:concluded-round/r prev-cr))))
                                       (drop-while (fn [prev-cr] (let [prev-es-r  (:concluded-round/es-r prev-cr)
                                                                       prev-es-nr (:concluded-round/es-nr prev-cr)]
                                                                   (and (not (contains? prev-es-r event))
                                                                        (not (contains? prev-es-nr event)))))))
                                 (iterate :concluded-round/prev-concluded-round ?prev-cr)))
                    received? (some-> ?receiving-cr :concluded-round/es-r (contains? event))]
                (not received?)))))

        ->to-continue?
        (fn [event]
          (let [event-r (->round-number event ?prev-cr)]
            (or (= event-r (:concluded-round/r cr))
                (->to-receive? event))))]

    (->> (:concluded-round/ws cr)
         (map (fn [w] (->event-to-receive->learned-event w ->to-continue? ->to-receive?)))
         (reduce (fn [acc etr->le]
                   (reduce (fn [acc2 [etr le]]
                             (update acc2 etr (fn [les] (conj (or les []) le))))
                           acc
                           etr->le))
                 {}))))

(def median
  (memoize
   (fn [numbers]
     (let [sorted-numbers (sort numbers)
           len            (count sorted-numbers)]
       (if (even? len)
         (/ (+ (nth sorted-numbers (quot len 2))
               (nth sorted-numbers (dec (quot len 2))))
            2)
         (nth sorted-numbers (quot len 2)))))))

#_
(def received-time
  "Received time of y, as known to x."
  (memoize
   (fn [x y]
     (let [learned-events (vals (learned-member-learned-evt x y))]
       (median (map :event/creation-time learned-events))))))

#_
(def order-events
  (memoize
   (fn [x evts]
     (->> evts
          (sort (fn [e1 e2]
                  (let [rt1 (received-time x e1)
                        rt2 (received-time x e2)]
                    (cond (not= rt1 rt2)
                          (if (> rt1 rt2)
                            1 -1)

                          :else
                          (if (> (:event/creation-time e1)
                                 (:event/creation-time e2))
                            1 -1)))))
          vec))))

(defn* concluded-round->received-event
  [{:concluded-round/keys [prev-concluded-round r ufws etr->les es-r] :as concluded-round}]
  (if (= r 1)
    nil
    (let [received-round-size   (count es-r)
          ?prev-received-event  (some-> prev-concluded-round :concluded-round/last-received-event)]

      (->> es-r
           (map (fn [e-r]
                  ;; TODO perhaps do median calculation on numbers, calcing median members on view side, when required
                  (let [learned-by (get etr->les e-r)
                        middle-learned-events
                        (let [len (count learned-by)]
                          (-> learned-by
                              (->> (sort-by :event/creation-time))
                              (cond->
                                  (even? len)
                                ((juxt #(nth % (quot len 2)) #(nth % (dec (quot len 2))))) ;; crashes when none events received ??, e.g., when ufws are none, or no event intersects them

                                (odd? len)
                                ((juxt #(nth % (quot len 2)))))
                              (set)))

                        received-time (-> middle-learned-events
                                          (->> (map :event/creation-time)
                                               (reduce +))
                                          (/ (count middle-learned-events)))]

                    (hash-map :received-event/learned-by            learned-by
                              :received-event/middle-learned-events middle-learned-events
                              :received-event/received-time         received-time
                              :received-event/event                 e-r
                              :received-event/r                     (:concluded-round/r concluded-round)))))

           (sort-by :received-event/received-time)

           ;; no value in building a chain of received events
           ;; TODO just store ordered received events on concluded-round
           (reduce (fn [?prev-received-event re-part]
                     (let [r-idx (if (some-> ?prev-received-event :received-event/r (= r))
                                   (-> ?prev-received-event :received-event/r-idx inc)
                                   0)

                           ;; TODO perhaps move color calculation into view. Also will allow to change palettes dynamically.
                           ;; TODO move static stuff outside
                           palette1 [[38, 70, 83]
                                     ;; [42, 157, 143]
                                     [233, 196, 106]
                                     ;; [244, 162, 97]
                                     [231, 111, 81]]

                           [rgb1 rgb2 rgb3] palette1

                           [from-rgb to-rgb]
                           (case (mod r 3)
                             1 [rgb3 rgb1]
                             2 [rgb1 rgb2]
                             0 [rgb2 rgb3])

                           [r g b]
                           (map (fn [from-c to-c]
                                  (let [upper-c       (max from-c to-c)
                                        lower-c       (min from-c to-c)
                                        delta-c       (- upper-c lower-c)
                                        steps         received-round-size
                                        steps-delta-c (/ delta-c steps)
                                        step-delta-c  (* steps-delta-c r-idx)
                                        c             (if (= upper-c from-c)
                                                        (- from-c step-delta-c)
                                                        (+ from-c step-delta-c))]
                                    (ceil c)))
                                from-rgb
                                to-rgb)
                           color (gc/rgb r g b)]
                       (cond-> (assoc re-part
                                      :received-event/idx   (or (some-> ?prev-received-event :received-event/idx inc)
                                                                0)
                                      :received-event/r-idx r-idx
                                      :received-event/color color)
                         ?prev-received-event (assoc :received-event/prev-received-event ?prev-received-event))))
                   ?prev-received-event)))))

(def concluded-round->stake-map :concluded-round/stake-map)

#_
(def ->?last-received-event
  (memoize
   (fn [wx]
     #_(assert (witness? wx))
     (when-let [last-concluded-round (->?last-concluded-round wx)]
       (-> last-concluded-round
           :concluded-round/last-received-event)))))


;; --------------- Stake ----------------
(def total-stake 99)
(def many-stake (-> total-stake (* 2) (/ 3)))
(def initial-share-stake-log
  (->> hg-members/initial-member-names
       (map (fn [member-name]
              {:share-stake/to     member-name
               :share-stake/amount (/ total-stake (count hg-members/initial-member-names))}))))

(defn* ^:memoizing share-stake-log->stake-map
  [share-stake-log]
  (if (empty? share-stake-log)
    (hash-map)
    (let [[{:share-stake/keys [from to amount] :as share-stake} & ss-rest] share-stake-log
          prev-stake-map                                                   (-> ss-rest share-stake-log->stake-map)
          new-stake-map                                                    (cond-> prev-stake-map
                                                                             from      (update from - amount)
                                                                             to        (update to   + amount)
                                                                             :sanitize (->> (into {} (remove (comp zero? val))))
                                                                             :sort     (->> (sort-by first)
                                                                                            (apply concat)
                                                                                            (apply array-map)))]
      (if (->> new-stake-map (some (fn [[_ stake]] (> stake (/ total-stake 3)))))
        (do #_(js/console.log "share stake tx would give > 1/3 stake to a single member, skipping it" prev-stake-map share-stake new-stake-map)
            prev-stake-map)
        new-stake-map))))

(def derived-db->stake-map
  (memoize
   (fn [derived-db]
     (-> derived-db
         :share-stake-log
         share-stake-log->stake-map))))

;; -------------- Derived DB --------------------
(def init-db
  {:share-stake-log initial-share-stake-log})

(def fns
  {:share-stake
   (fn [db {:share-stake/keys [from to ratio]}]
     (update db :share-stake-log (fn [share-stake-log]
                                   (let [stake-map (share-stake-log->stake-map share-stake-log)
                                         amount    (-> stake-map
                                                       (get from)
                                                       (* (first ratio))
                                                       (/ (second ratio))
                                                       (floor))]
                                     (conj share-stake-log
                                           {:share-stake/from   from
                                            :share-stake/to     to
                                            :share-stake/amount amount})))))
   :inc-counter
   (fn [db _] (update db :counter inc))})

;; when round has too many events, recursion depth of browser is not enough to calc it all
;; TODO switch to calc for concluded round, from earliest to latest
(defn* ^{:memoizing true #_{:only-last? true}} ?received-event->derived-db
  [?received-event]
  (if (nil? ?received-event)
    init-db
    (let [prev-derived-db (?received-event->derived-db (:received-event/prev-received-event ?received-event))]
      (if-let [{:tx/keys [fn-id args]} (:event/tx (:received-event/event ?received-event))]
        (let [f (get fns fn-id)]
          (apply f  prev-derived-db args))
        prev-derived-db))))

#_
(def event->derived-db
  (memoize
   (fn [event]
     (?received-event->derived-db (last-received-event event)))))

(defn ?received-event->stake-map
  [received-event]
  (-> received-event
      ?received-event->derived-db
      :share-stake-log
      share-stake-log->stake-map))

#_
(defn* event->stake-map
  [x]
  (-> x
      ->?concluded-round
      ?concluded-round->stake-map))

#_
(def witness-concluded->stake-map
  (memoize
   (fn [witness-concluded]
     (-> witness-concluded
         ->?last-received-event
         ?received-event->stake-map))))

(defn event->person
  [event]
  (let [member-name (creator event)]
    (hg-members/member-name->person member-name)))

#_
(def event->stake
  (memoize
   (fn [w]
     (let [member-name (:event/creator w)
           stake-map (-> w event->stake-map)]
       (get stake-map member-name)))))

#_
(def event->most-weigted-member
  (memoize
   (fn [event]
     (-> event
         event->stake-map
         (->> (sort-by second))
         last
         first))))

#_
(def event->present-members
  (memoize
   (fn [event]
     (some-> event
             witness-or-self-witness
             event->stake-map
             keys
             (->> (map hg-members/member-name->person))))))

#_
(defn* creator-hg-map->main-top-witness
  [creator-hg-map]
  (when-let [main-hg (get creator-hg-map "Alice")]
    (let [?cr (->?concluded-round main-hg)]
      (witness-or-self-witness main-hg ?cr))))

(def main-creator "Charlie") ;; creator whose tip used to conclude in viz (he's not special for the main algorithm ^)
(defn events>->main-tip [events>] ;; desc
  (->> events> (some (fn [evt] (when (= (:event/creator evt) main-creator) evt)))))
