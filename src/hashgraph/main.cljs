(ns hashgraph.main
  "Implementation of the Hashgraph algorithm. Likely misbehaving, but enough to get intuition of how it works via viz.
   This implementation sacrificed simplicity in favor of some performance.

   If you wish to grok on it, consider looking at the official algorithm.
   It's in functional form, simple and elegant (yet may be difficult to grasp).
   Can be found in the Appendix A (at the bottom) of the paper: https://www.swirlds.com/downloads/SWIRLDS-TR-2016-01.pdf"

  (:refer-clojure
   :exclude [parents ancestors])
  (:require [hashgraph.members :as hg-members]
            [hashgraph.schemas :as hgs]
            [hashgraph.utils.core :refer-macros [defn* defnm defnml l cl letl2 letlt when-let*] :refer [*mem* *from-mem* xor median not-neg hash= safe-max map-vals filter-map-vals conjv distinct-by]]
            [hashgraph.utils2 :refer-macros [td time3]]

            [clojure.set :refer [union] :as set]
            [clojure.test :refer [deftest testing is are run-test]]
            [cljs.math :refer [floor ceil]]
            [garden.color :as gc]
            [taoensso.timbre :refer-macros [spy] :refer [log info]]
            [taoensso.tufte :as tufte :refer [defnp fnp p pspy profiled profile]]
            [malli.core :as m]))

;; no point in memoize, it'll be more costly than looking up from x
(def creator :event/creator)
(def creation-time :event/creation-time)
(def self-parent :event/self-parent)
(def other-parent :event/other-parent)
(def tx :event/tx)
(def topic :event/topic)

(hgs/register!
 :hash int?
 :public-key string?
 :public-key-hash :hash
 :signature string?
 :idx     [:and :int
           [:fn {:error/message "idx must not be negative"}
            not-neg]]
 :aid->ke [:map-of :aid :ke]
 :stake pos?
 :topic map?

 :text-message
 [:map
  [:text-message/content string?]]

 :share-stake-tx
 [:map
  [:tx/fn-id [:= :share-stake]]
  [:tx/fn-args [:vector {:min 1}
                [:map
                 [:share-stake/from string?]
                 [:share-stake/to   string?]
                 [:share-stake/ratio [:tuple int? int?]]]]]]

 :txes ;; just one tx actually used atm and schemed here V
 [:and [:cat {:gen/fmap vec}
        keyword? [:* any?]]
  vector?]
 #_[:or
    ShareStakeTx])

(defn share-stake-valid? [{:event/keys [creator tx]}]
  (or (nil? tx)
      (not= :share-stake (:tx/fn-id tx))
      (let [share-stakes (-> tx :tx/fn-args)]
        (and (->> share-stakes
                  (every? (fn [{:share-stake/keys [from to]}] (and (= from creator)
                                                                   (not= to creator)))))
             (->> share-stakes
                  (map :share-stake/to)
                  (distinct?))))))

(defn root-event? [{:event/keys [self-parent other-parent]}]
  (and (nil? self-parent)
       (nil? other-parent)))

(defn* ^:memoizing first-self-parent [event]
  (or (some-> (self-parent event) first-self-parent)
      event))


(defn* ^:memoizing evt->root-evt [evt]
  (if (root-event? evt)
    evt
    (or (some-> evt self-parent evt->root-evt)
        (some-> evt other-parent evt->root-evt))))

(defn* ^:memoizing event->index
  "Index position of event x, as its position in self-parent chain."
  [x]
  (or (some-> (self-parent x) event->index inc)
      0))

(defn max-event [evt1 evt2]
  (or (xor evt1 evt2)
      (if (>= (creation-time evt1) (creation-time evt2)) ;; brittle
        evt1 evt2)))

(defn max-sp [evt1 evt2]
  (or (xor evt1 evt2)
      (if (>= (event->index evt1) (event->index evt2))
        evt1 evt2)))

(defn min-sp [evt1 evt2]
  (or (xor evt1 evt2)
      (if (< (event->index evt1) (event->index evt2))
        evt1 evt2)))

;; only one root event is present, it has :event/topic
;; a0
;; | \
;; |  b0
;; | /|
;; a1 b1  _
;; |    \   \
;; |     c0  c0s
;; |   /   /
;; |  /   /
;; | /   /
;; a2 b2
;; | /
;; a3
(def a "a")
(def b "b")
(def c "c")
(declare total-stake)
(def abc-topic {:stake-map {a (/ total-stake 3)
                            b (/ total-stake 3)
                            c (/ total-stake 3)}})

(def a0 {creator       a
         creation-time 0
         topic         abc-topic})
(def b0 {creator       b
         creation-time 1
         other-parent  a0})
(def a1 {creator       a
         creation-time 2
         self-parent   a0
         other-parent  b0})
(def b1 {creator       b
         creation-time 2
         self-parent   b0})
(def c0 {creator       c
         creation-time 3
         other-parent  b1
         tx            [:text-message {:text-message/content "Attack!"}]})
(def c0s {creator       c
          creation-time 3
          other-parent  b1
          tx            [:text-message {:text-message/content "Retreat!"}]})
(def a2 {creator       a
         creation-time 4
         self-parent   a1
         other-parent  c0})
(def b2 {creator       b
         creation-time 4
         self-parent   b1
         other-parent  c0s})
(def a3 {creator       a
         creation-time 5
         self-parent   a2
         other-parent  b2})

(def example-event->depth
  {a0 0
   b0 1
   b1 2
   a1 2
   c0 3
   a2 4})

(defn* ^:memoizing event->depth [event]
  (or (some-> (safe-max (some-> (self-parent event) event->depth)
                        (some-> (other-parent event) event->depth))
              inc)
      0))

(deftest event->depth-test
  (doseq [[event depth] example-event->depth]
    (is (= depth (event->depth event)))))

(defn* ^:memoizing nth-sp
  [event n]
  (let [idx (event->index event)]
    (cond (= idx n)
          event

          (or (< idx n)
              (nil? (self-parent event)))
          nil

          :else
          (nth-sp (self-parent event) n))))

(defn sp-ancestor? [x y]
  (-> x (nth-sp (event->index y)) (hash= y)))



(def example-event->creator->tips
  {a0 {a #{a0}}
   b0 {a #{a0} b #{b0}}
   b1 {a #{a0} b #{b1}}
   a1 {a #{a1} b #{b0}}
   c0 {a #{a0} b #{b1} c #{c0}}
   a2 {a #{a2} b #{b1} c #{c0}}
   b2 {a #{a0} b #{b2} c #{c0s}}
   a3 {a #{a3} b #{b2} c #{c0 c0s}} ;; note, both tips of c is present
   })

(defn* ^:memoizing event->creator->tips [event]
  (merge-with (fn [tips1 tips2]
                (reduce (fn [tips-acc tip]
                          (if-let [tips-acc-sp-descendands (not-empty (set (->> tips-acc (filter #(sp-ancestor? tip %)))))]
                            (-> tips-acc
                                (set/difference tips-acc-sp-descendands)
                                (conj tip))
                            (if (->> tips-acc (some #(sp-ancestor? % tip))) ;; tip has ancestor
                              tips-acc
                              (-> tips-acc (conj tip)))))
                        tips1
                        tips2))
              (some-> (self-parent event) event->creator->tips)
              (some-> (other-parent event) event->creator->tips)
              {(creator event) #{event}}))

(deftest event->creator->tips-test
  (doseq [[event creator->tips] example-event->creator->tips]
    (is (= creator->tips (event->creator->tips event)))))

#_(run-test event->creator->tips-test)


(defn ancestor? [x y]
  (-> x
      event->creator->tips
      (get (creator y))
      (->> (some (fn [y-tip] (sp-ancestor? y-tip y))))
      some?))

(deftest ancestor?-test
  (are [x y] (= true (ancestor? x y))
    a0  a0

    a1  a0
    a1  b0

    b0  b0
    b0  a0

    b1  b0
    b1  a0

    c0  a0
    c0  b0
    c0  b1

    a2  a0
    a2  a1
    a2  b0
    a2  b1
    a2  c0

    c0s a0
    c0s b0
    c0s b1

    b2  a0
    b2  b0
    b2  c0s

    a3  a0
    a3  a1
    a3  a2
    a3  b0
    a3  b1
    a3  b2
    a3  c0
    a3  c0s
    a3  a3)
  (are [x y] (= false (ancestor? x y))
    a0 a1
    a0 b0
    a0 b1
    a0 c0
    a0 c0s
    a0 a2
    a0 b2
    a0 a3

    a1 b1
    a1 c0
    a1 c0s
    a1 a2
    a1 b2
    a1 a3))

#_(run-test ancestor?-test)


(def example-event->creator->unique-tip
  {a0 {a a0}
   b0 {a a0 b b0}
   b1 {a a0 b b1}
   a1 {a a1 b b0}
   c0 {a a0 b b1 c c0}
   a2 {a a2 b b1 c c0}
   b2 {a a0 b b2 c c0s}
   a3 {a a3 b b2} ;; note, no c, as a sees c0 and c0s, thus no unique tip of c
   })


;; here we can check that whether sp's and op's creator's tips is an ancestor and descendant of each other, if not - they're in fork
;; => remove them, and add meta that creator is sybil, so his events are not seen downstream as well, leaving only unique tips / tips from honest creators
;; alternatively, return fork base, this way it can be used in cr to receive events before fork happened
(defn* ^:memoizing event->creator->unique-tip [event]
  (let [creator->tips (-> event event->creator->tips)]
    (->> creator->tips
         (filter-map-vals (fn [tips] (when (= 1 (count tips))
                                       (first tips)))))))

#_(defn* ^:memoizing event->creator->unique-tip [event]
  (let [?sp-c->ut (some-> (self-parent event) event->creator->unique-tip)
        ?op-c->ut (some-> (other-parent event) event->creator->unique-tip)
        p-c->ut   (or (and (nil? ?sp-c->ut) (nil? ?op-c->ut)
                           (with-meta (hash-map) #{}))
                      (xor ?sp-c->ut ?op-c->ut)
                      (reduce (fn [c->ut-acc [c ut]]
                                (if-let [ut-acc (get c->ut-acc c)]
                                  (let [[min-tip max-tip] (sort-by event->index [ut ut-acc])]
                                    (if (sp-ancestor? max-tip min-tip)
                                      (assoc c->ut-acc c max-tip)
                                      (-> c->ut-acc
                                          (dissoc c)
                                          (vary-meta conj c))))
                                  (assoc c->ut-acc c ut)))
                              (-> (apply dissoc ?sp-c->ut (meta ?op-c->ut))
                                  (vary-meta into (meta ?op-c->ut)))
                              (apply dissoc ?op-c->ut (meta ?sp-c->ut))))]
    (cond-> p-c->ut
      (not (contains? (meta p-c->ut) (creator event)))
      (assoc (creator event) event))))

(deftest event->creator->unique-tip-test
  (doseq [[event creator->tip] example-event->creator->unique-tip]
    (is (= creator->tip (event->creator->unique-tip event)))))

#_(run-test event->creator->unique-tip-test)


(defn some-parents? [{:event/keys [self-parent other-parent]}]
  (or (some? self-parent)
      (some? other-parent)))

(defn sp-same-creator? [{:event/keys [creator self-parent]}]
  (or (nil? self-parent)
      (= creator (:event/creator self-parent))))

(defn op-different-creator? [{:event/keys [creator other-parent]}]
  (or (nil? other-parent)
      (not= creator (:event/creator other-parent))))

(defn sp-growing-creation-time? [{:event/keys [creation-time self-parent]}]
  (or (nil? self-parent)
      (> creation-time (:event/creation-time self-parent))))

;; malli.clj-kondo does not know how to treat [:and], so it's separate
;; and even without it it returns :any for Event fn type
(hgs/register!
 ::event
 [:multi {:dispatch (comp some? :event/topic)}
  [true  ::init-event]
  [false ::plain-event]]
 :event ::event

 ::init-event
 [:and
  [:map {:closed true}
   [creator       :public-key]
   [creation-time int?]
   [topic         :topic]]
  [:fn {:error/message "event has a malformed share-stake tx"}
   share-stake-valid?]]

 ::plain-event
 [:and
  [:map {:closed true}
   [creator       :public-key]
   [creation-time int?]
   [tx            {:optional true} :txes] ;; enum registered txes? but they'll be dynamically added eventually (reg schemas for them in run-time?)
   [self-parent   {:optional true} [:ref ::event]]
   [other-parent  {:optional true} [:ref ::event]]]
  [:fn {:error/message "at least one self-parent or other-parent is expected on plain events"}
   some-parents?]
  ;; TODO check that sp's creator (pub key) corresponds to the same aid
  ;; [:fn {:error/message "event's self-parent has a different creator"}
  ;;  sp-same-creator?]
  ;; [:fn {:error/message "event's other-parent has the same creator"}
  ;;  op-different-creator?]
  [:fn {:error/message "event's creation time is not greater than its self-parent's"}
   sp-growing-creation-time?]])


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

(defn* see?
  [x y]
  #_(-> x event->creator->tip (get (creator y)) event->index (>= (event->index y))) ;; is not BFT, will falsely acknowledge duplicate
  (some-> (event->creator->unique-tip x) (get (creator y)) (sp-ancestor? y)) ;; BFT, more costly for non-bft scenarious
  )


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
(declare cr->stake-map)

(defn r+cr->final-round? [r cr]
  (<= r (inc (:concluded-round/r cr))))

(defn* ^{:memoizing {:bind {:->in-mem?  ->in-mem?
                            :->from-mem ->from-mem}}}
  ->round ;; see-many-see-many-see ;; see-many-strongly-see
  "Round number of y, as known to a previous round concluded x.
   It's either 1 if y has no parents,
   or a max round of events strongly seen by many (as known to x) +1."
  [x cr]
  #_
  (when (and (self-parent x)
             (other-parent x))
    (js* "debugger;"))
  ;; To efficiently compute round x we need to cater for two cases:
  ;; 1. When new cr arrives => rounds that been final need not be re-computed.
  ;; 2. When new event arrives => it's round will be atop max parent's cr. Since it's round will be atop max parent's round, a continuation of it's compute.
  ;;
  ;; To cater for 1, we can memoize round and it's finality as of some cr, and adopt final round in successive crs. Previous round can be looked up from mem.
  ;; To cater for 2, we can recur down the parents to find the latest final round, and run from it's cr.
  ;;
  ;; Joined algorithm will look like:
  ;; 1. When there's mem for the event, lookup the highest cr that event been ran on from mem, if it's final - adopt immediately, else - run from the cr atop it.
  ;; 2. Learn max parents' cr, carry on from it.

  ;; 1. new cr - lookup whether final
  ;; TODO lookup if no mem exist first
  (let [?prev-round-mem
        (loop [prev-cr (:concluded-round/prev-concluded-round cr)]
          (if (->in-mem? x prev-cr)
            (->from-mem x prev-cr)
            (when-let [prev-prev-cr (:concluded-round/prev-concluded-round prev-cr)]
              (recur prev-prev-cr))))]

    ;; Once round r is settled, stake-map _may_ change, rounds of further events,
    ;; and, in turn, witnesses, famous witness, crs.
    ;; source: https://hyp.is/QxYPUqyAEe6hUtsMYuakKQ/www.swirlds.com/downloads/SWIRLDS-TR-2016-01.pdf

    ;; So, once there is a cr that gave round number less or equal to cr+1 - it's final.
    ;; We need _less_ for cases when stake changed and more events received cr's round number.
    (or (and (:round/final? ?prev-round-mem)
             ?prev-round-mem)

        ;; adop non-final round if crs atop did not have changed stake-map
        (let [?same-stake-crs-atop-mem<    (when-let [prev-round-mem ?prev-round-mem]
                                             (->> cr
                                                  (iterate :concluded-round/prev-concluded-round)
                                                  (take-while some?)
                                                  (take-while #(> (:concluded-round/r %) (:concluded-round/r (:round/cr prev-round-mem))))
                                                  reverse
                                                  (take-while #(= (-> % cr->stake-map)
                                                                  (-> prev-round-mem :round/cr cr->stake-map)))
                                                  not-empty))
              ?last-same-stake-cr-atop-mem (last ?same-stake-crs-atop-mem<)]
          (when-let [prev-round-mem ?prev-round-mem]
            (when-let [same-stake-crs-atop-mem< ?same-stake-crs-atop-mem<]
              (when-let [same-stake-final-cr (->> same-stake-crs-atop-mem<
                                                  (some (fn [same-stake-cr]
                                                          (when (r+cr->final-round? (-> prev-round-mem :round/number) same-stake-cr)
                                                            same-stake-cr))))]
                (-> prev-round-mem
                    (assoc :round/cr same-stake-final-cr)
                    (assoc :round/final? true)))

              (when (hash= ?last-same-stake-cr-atop-mem cr)
                (-> prev-round-mem
                    (assoc :round/cr cr)))))

          ;; 2. new event (or round is not final and stake-map changed)
          ;; either max parent's cr gives final round or the one after
          ;; but it's not guaranteed that parents have final rounds
          ;; and we'd like to memo for all crs anyways, so run from max parents' cr
          (let [p-rounds      (->> x parents (map (fn [p] (->round p cr))))
                p-crs         (->> p-rounds (map :round/cr))
                ?p-max-cr     (->> p-crs
                                   (sort-by :concluded-round/r >)
                                   first)
                ?try-after-cr (max-key :concluded-round/r
                                       (some-> ?last-same-stake-cr-atop-mem :round/cr)
                                       (some-> ?p-max-cr :concluded-round/prev-concluded-round))

                crs-to-try
                (->> cr
                     (iterate :concluded-round/prev-concluded-round)
                     (take-while some?)
                     (take-while (fn [prev-cr] (not (hash= prev-cr ?try-after-cr))))
                     ;; TODO optimize for not-running same-stake-map crs & short-curcuit on final same-stake-map cr

                     ;; no point in running same stake-map crs in hope they give final round
                     ;; taking last same-stake crs here, as were we take first ones - chances some subsequent would give final round and we'd skip it
                     ;; we could instead take first same-stake cr that can give final round
                     ;; this way we both 1) preserve in :round/cr the first cr that gives finality 2) will not miss those crs
                     ;; do we need 1. though? Let's leave it as is for now due to simplicity of impl
                     ;; (distinct-by cr->stake-map) ;; Note: :round/cr on final rounds may not be the the first same-stake cr that gives finality
                     reverse)

                round (loop [[cr-to-try & crs-to-try-rest] crs-to-try]
                        (let [cr-round (->round-info x cr-to-try)]
                          (cond (:round/final? cr-round) ;; short-curcuit on final round
                                (-> cr-round
                                    (assoc :round/cr cr)) ;; bump final round to latest cr, so future round calculations are faster

                                (empty? crs-to-try-rest)
                                cr-round

                                :else
                                (recur crs-to-try-rest))))]
            round)))))

(declare many-stake)
#_
(defn bump-stake [el c stake-map]
  (let [new-stake (+ (meta el) (get stake-map c))]
    (or (> new-stake many-stake)
        (with-meta el new-stake))))

(defn add-sbc [sbcs sbc stake-map]
  (let [new-stake (+ (meta sbcs) (get stake-map sbc))]
    (or (> new-stake many-stake)
        (-> sbcs
            (conj sbc)
            (with-meta new-stake)))))

;; stronglySee(x, y) = see(x, y) ∧ (∃S ⊆ E, manyCreators(S) ∧(z ∈ S =⇒ (see(x, z) ∧ see(z, y))))

;; round(x) = max({selfParentRound(x)} ∪ {r + 1 | ∃S ⊆ E, manyCreators(S) ∧ (∀y ∈ S, round(y) = r ∧ stronglySee(x, y))})
;; seems safe to assume that:
;; 1. y is a witness, since no self-descendant of y will be seen more
;; 2. y is unique, since x cannot see non-unique y

;; Then, naively round bump can be calculated as:
;; derive creator->unique-tip
;; derive creator->unique-r-tip, (filter those unique-tips whose r = max-parent-r)
;; derive creator->unique-r-witness, (take their last witnesses)
;; for each unique-r-witness, derive whether its strongly seen, derive whether many strongly seen
;; Cons: this is inefficient, O(n2), as every unique-r-tip will be checked whether it can see every unique-r-witness
;;       also, plenty of re-compute
;;       better have an incremental alg that short-curcuits when seen enough
;; Also, dynamic-stake map complicates calculation of unique-r-tip.



;; (= #{1 3 2} #{2 4 0}) => true (due to camparison by hashes that collide easily on simple structs)

;; not BFT
(declare concluded-round->stake-map)
(defn* ^:memoizing ->round-info
  [x cr]
  #_(when (some? (other-parent x)) (js* "debugger;"))
  (let [stake-map    (concluded-round->stake-map cr)
        xc           (creator x)
        self-sbcs    (add-sbc #{} xc stake-map)
        self-w->sbcs (cond-> (hash-map x self-sbcs)
                       (true? self-sbcs) (vary-meta + (get stake-map xc)))
        ?spx         (self-parent x)
        ?opx         (other-parent x)]
    (if (and (nil? ?spx)
             (nil? ?opx)) ;; will add (little) cost to all non-bottom events
      (hash-map :round/event    x
                :round/number   1
                :round/next?    true
                :round/final?   true
                ;; many see many see (true) or witness creator -> many see (true) or seen by creators set
                :round/w->sbcs  self-w->sbcs
                :round/cr       cr)

      (let [?spx-round     (some-> ?spx (->round cr)) ;; can be passed in as args, also perhaps no need to memoize
            ?opx-round     (some-> ?opx (->round cr))
            both-eligible? (= (:round/number ?spx-round)
                              (:round/number ?opx-round))

            max-p-round (max-key :round/number ?opx-round ?spx-round)
            max-p-r     (-> max-p-round :round/number)

            [acc to-reduce] (cond
                              both-eligible?
                              [(-> ?spx-round :round/w->sbcs)
                               (-> ?opx-round :round/w->sbcs)]

                              (identical? max-p-round ?spx-round)
                              [(-> ?spx-round :round/w->sbcs)
                               nil]

                              (identical? max-p-round ?opx-round)
                              [(-> ?opx-round :round/w->sbcs
                                   (->> (map-vals (fn [_op-sbcs] self-sbcs)))
                                   (assoc x self-sbcs)
                                   (cond->
                                       (true? self-sbcs) (vary-meta + (get stake-map xc))))
                               (-> ?opx-round :round/w->sbcs)])

            w->sbcs
            ;; Not pretty, but pretty efficient
            (reduce (fn [w->sbcs-acc [w sbcs]]
                      (let [?sbcs-acc (get w->sbcs-acc w)]
                        (cond (true? ?sbcs-acc)
                              w->sbcs-acc

                              (true? sbcs)
                              (-> w->sbcs-acc
                                  (assoc w true)
                                  (vary-meta + (get stake-map (creator w))))

                              (nil? ?sbcs-acc)
                              (let [new-sbcs (cond-> sbcs
                                               (not (sbcs xc)) (add-sbc xc stake-map))]
                                (cond-> (assoc w->sbcs-acc w new-sbcs)
                                  (true? new-sbcs) (vary-meta + (get stake-map (creator w)))))

                              (and (= (count ?sbcs-acc) (count sbcs))
                                   (every? ?sbcs-acc sbcs))
                              ;; (= ?sbcs-acc sbcs)
                              w->sbcs-acc

                              :else
                              ;; carry on compute from previous biggest sbcs
                              (let [sbcs-acc       ?sbcs-acc
                                    sbcs-acc-count (count sbcs-acc)
                                    sbcs-count     (count sbcs)
                                    biggest        (if (> sbcs-acc-count sbcs-count)
                                                     sbcs-acc
                                                     sbcs)
                                    smallest       (if (> sbcs-acc-count sbcs-count)
                                                     sbcs
                                                     sbcs-acc)
                                    new-sbcs-acc
                                    (reduce (fn [biggest-sbcs-acc sbc]
                                              (if (biggest-sbcs-acc sbc)
                                                biggest-sbcs-acc
                                                (let [new-biggest-sbcs-acc (add-sbc biggest-sbcs-acc sbc stake-map)]
                                                  (if (true? new-biggest-sbcs-acc)
                                                    (reduced true)
                                                    new-biggest-sbcs-acc))))
                                            biggest
                                            smallest)]
                                (cond-> (assoc w->sbcs-acc w new-sbcs-acc)
                                  (true? new-sbcs-acc) (vary-meta + (get stake-map (creator w))))))))
                    acc
                    to-reduce)
            round-next?     (-> w->sbcs meta (> many-stake))
            r               (cond-> max-p-r
                              round-next? inc)
            round-final?    (r+cr->final-round? r cr)
            ;; prev round ws, not BFT, as equivocation is not taken into account
            strongly-see-ws (if round-next?
                              (->> w->sbcs
                                   (filter (comp true? second))
                                   (map first)
                                   set)
                              (some-> max-p-round :round/strongly-see-ws))]

        (hash-map
         :round/event   x
         :round/number  r
         :round/next?   round-next?
         :round/final?  round-final?
         :round/w->sbcs (if round-next?
                          self-w->sbcs
                          w->sbcs)
         :round/strongly-see-ws strongly-see-ws
         :round/cr      cr)))))

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

(def default-delay 1) ;; num of round to wait until voting starts, at least 1 (?)
(def coin-flip-r 10) ;; each c's round is a coin flip round
(defn cr->delay [cr]
  (if (= 1 (count (-> cr :concluded-round/stake-map)))
    0 ;; conclude right away for when there's just 1 member
    default-delay))

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


(defn* ^:memoizing round-witnesses
  "Round r witnesses, known to x, based on cr."
  [x r cr]
  (cond-> #{}
    (and (= (->round-number x cr) r) ;; can be optimized to not run if round > r
         (witness? x cr))
    (conj x)

    (>= (->round-number x cr) r)
    (cond->
        (self-parent  x) (set/union (round-witnesses (self-parent  x) r cr))
        (other-parent x) (set/union (round-witnesses (other-parent x) r cr)))))

#_
(defn* ^:memoizing ->round-witness->seen-by-creators
  "Round r witnesses to seen by creator map, as known to x, based on cr."
  [x r cr]
  (cond-> (hash-map)
    (and (= (->round-number x cr) r) ;; can be optimized to not run if round > r
         (witness? x cr))
    (assoc x #{(:event/creator x)})

    (>= (->round-number x cr) r)
    (cond->>
        (self-parent  x) (merge-with set/union (->round-witness->seen-by-creators (self-parent  x) r cr))
        (other-parent x) (merge-with set/union (->round-witness->seen-by-creators (other-parent x) r cr))
        :add-self        (into (hash-map) (map (fn [[rw sb-cs]] [rw (conj sb-cs (:event/creator x))]))))))

(declare many-stake)
(declare votes-stake-true)
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

(defn vote-see? [x y cr] (= (rounds-diff x y cr)
                            (cr->delay cr)))
(defn vote-see  [x y cr] (make-vote cr x y :see (boolean (see? x y)) (when (= 0 (cr->delay cr)) {:vote/final? true})))

(declare ->vote)
(defn vote-copy? [x y cr] (when-let [sw (self-witness x cr)]
                            (when (> (rounds-diff x y cr)
                                     (cr->delay cr))
                              (:vote/final? (->vote sw y cr)))))
(defn vote-copy [x y cr]
  (let [vote-copied (->vote (self-witness x cr) y cr)]
    (make-vote cr x y :copy (:vote/value vote-copied) {:vote/final?      true
                                                       :vote/vote-copied vote-copied})))

(declare votes-stake-fract-true)
(declare many-stake)
(defn vote-coin-flip? [x y cr]
  (and (> (rounds-diff x y cr)
          (cr->delay cr))
       (-> (rounds-diff x y cr) (mod coin-flip-r) (= 0))
       (>= (votes-stake-fract-true x y cr) (-> many-stake (/ 3)))
       (<= (votes-stake-fract-true x y cr) (-> many-stake (/ 3) (* 2)))))
(defn vote-coin-flip [x y cr] (make-vote cr x y :coin-flip (= 1 (middle-bit (signature x)))))

(declare votes-stake-false)
(defn vote-for-majority [x y cr]
  (let [final? (or (> (votes-stake-true x y cr) many-stake)
                   (> (votes-stake-false x y cr) many-stake))]
    (make-vote cr x y :for-majority (>= (votes-stake-fract-true x y cr) (/ 1 2)) (when final? {:vote/final? true}))))

(defn ->vote
  "Vote of x about fame of y"
  [x y cr]
  (cond (vote-see? x y cr)       (vote-see x y cr)
        (vote-copy? x y cr)      (vote-copy x y cr)
        (vote-coin-flip? x y cr) (vote-coin-flip x y cr)
        :else                    (vote-for-majority x y cr)))

#_
(defn* ^:memoizing ->votes
  "Votes on fame of y from witnesses seen by many in the round before x, based on concluded-round."
  [x y cr]
  ;; TOOD can lookup from round-info :round/strongly-seen-ws
  (let [rw->sbcs  (->round-witness->seen-by-creators x (dec (->round-number x cr)) cr)
        stake-map (concluded-round->stake-map cr)]
    ;; old TODO switch to transduce
    ;; old TODO peek eligible rws of x from (->round x)
    (->> rw->sbcs
         (filter (fn [[_rw sbcs]] (-> sbcs
                                      (->> (map stake-map)
                                           (reduce + 0))
                                      (> many-stake))))
         (map (fn [[rw _sbcs]] (->vote rw y cr))))))

(defn* ^:memoizing ->votes
  "Votes on fame of y from witnesses seen by many in the round before x, based on concluded-round."
  [x y cr]
  (when-not (witness? x cr) (js* "debugger;"))
  (let [{:round/keys [strongly-see-ws]} (->round x cr)]
    (->> strongly-see-ws
         (map (fn [ssw] (->vote ssw y cr))))))

;; TODO maybe switch to transduce
(defn votes-stake-true  [x y cr] (->> (->votes x y cr) (filter :vote/value) (map :vote/stake) (reduce + 0)))
(defn votes-stake-false [x y cr] (->> (->votes x y cr) (remove :vote/value) (map :vote/stake) (reduce + 0)))

(defn votes-stake-fract-true
  [x y cr]
  (let [stake-true  (votes-stake-true x y cr)
        stake-false (votes-stake-false x y cr)]
    (/ stake-true
       (max 1 (+ stake-true stake-false)))))


(def idx? nat-int?)
(def Idx idx?)

(def cr-r :concluded-round/r)
(def cr-stake-map :concluded-round/stake-map)
(def cr-db :concluded-round/db)
(def cr-prev-cr :concluded-round/prev-concluded-round)
(hgs/register!
 ::concluded-round
 [:map
  [cr-r :idx]
  [cr-db :topic]
  [cr-stake-map map?]
  [cr-prev-cr {:optional true} [:ref ::concluded-round]]]
 :concluded-round ::concluded-round
 :cr :concluded-round)

(defn* ^:memoizing event->topic [event]
  (or (some-> event self-parent event->topic)
      (some-> event other-parent event->topic)
      (topic event)))


;; dang, do I check that learned event is seen by everybody?
;; good time to ditch :creation-time in favor of depth?
#_
(defn* ^:memoizing ->event-to-receive->learned-event ;; will slowdown over time due to mem lookup
  [x ->receivable? ->to-receive?]
  (if-not (->receivable? x)
    (hash-map)
    (let [etr->le* (reduce
                    (fn [sp-etr->le [op-etr _op-le]]
                      (if (get sp-etr->le op-etr)
                        sp-etr->le
                        (assoc! sp-etr->le op-etr x)))
                    (transient (or (some-> (self-parent x) (->event-to-receive->learned-event ->receivable? ->to-receive?))
                                   (hash-map)))
                    (some-> (other-parent x) (->event-to-receive->learned-event ->receivable? ->to-receive?)))]
      (cond-> etr->le*
        (->to-receive? x) (assoc! x x)
        :always           persistent!))))

;; needs refactoring, does redundant work
#_
(defn* concluded-round->event-to-receive->learned-events
  [cr]
  (let [cr-r     (:concluded-round/r cr)
        ?prev-cr (:concluded-round/prev-concluded-round cr)

        ->receivable?
        (fn [event]
          (let [event-r (->round-number event ?prev-cr)]
            (<= event-r cr-r))) ;; do not try to receive events of next rounds, cr-r witness may get included (as will be the case for right away conclusion on 1 member topic)

        ->to-receive?
        (fn [event]
          (let [?status (->> ?prev-cr
                             (iterate :concluded-round/prev-concluded-round)
                             (take-while some?)
                             (some (fn [prev-cr] (let [prev-es-r  (:concluded-round/es-r prev-cr)
                                                       prev-es-nr (:concluded-round/es-nr prev-cr)]
                                                   (cond (->> prev-es-r (some (fn [e-r] (hash= e-r event))))    :r
                                                         (->> prev-es-nr (some (fn [e-nr] (hash= e-nr event)))) :nr
                                                         :else                                                  nil)))))]
            (not= :r ?status)))]

    (->> (:concluded-round/ufws cr)
         (map (fn [ufw] (->event-to-receive->learned-event ufw ->receivable? ->to-receive?)))
         (reduce (fn [acc etr->le]
                   (reduce (fn [acc2 [etr le]]
                             (update acc2 etr conjv le))
                           acc
                           etr->le))
                 (hash-map)))))


;; cr       -> rw  -> sbc
;; cr -> re -> etr -> le
;; r        -> w   -> sbc
;; they're about the same - events that learned about an event
(declare concluded-round->?received-event)
(declare many-stake)
(declare prev-db+received-event->db)
(declare db->stake-map)
(declare ->?received-event)
(m/=> ->concluded-round [:=> [:cat ::event] ::concluded-round])
;; TODO try with :only-last? ;; won't fly on rewind
(defn* ^:memoizing ->concluded-round
  "Whether the round has fame of all it's witnesses concluded,
   and the previous round, if there is one, been concluded, as known to x."
  [x]
  (let [prev-cr (or (some-> (self-parent x) (->concluded-round))
                    (some-> (other-parent x) (->concluded-round))
                    (let [topic     (-> x event->topic)
                          stake-map (-> topic :stake-map)]
                      ;; is a hash-map, since normal crs are hash-maps, and I made equiv within same type work by hashes
                      ;; (were it array-may, they'd fall back to normal equiv)
                      (hash-map :concluded-round/r                            0
                                :concluded-round/witness-concluded            x
                                :concluded-round/db                           topic
                                :concluded-round/stake-map                    stake-map
                                :concluded-round/creator->received-unique-tip {})))]
    (loop [cr prev-cr]
      (if (not (witness? x cr))
        cr

        (let [next-cr-r (inc (:concluded-round/r cr))
              delay     (cr->delay cr)
              next-wc-r (if (= 0 delay)
                          next-cr-r
                          (-> next-cr-r (+ delay) inc)) ;; perhaps not needed
              wx-r      (->round-number x cr)]

          (if (< wx-r next-wc-r)
            cr

            ;; x is a witness able to try to conclude
            (let [wcx   x ;; witness concluding (x)
                  ws    (round-witnesses x next-cr-r cr) ;; #{wcx} if 1 member
                  ;; Derranging from the spec by making concluding witness vote,
                  ;; instead of collecting votes from r-1 witnesses.
                  ;; The result's the same, code seems to be simpler.
                  ;; It works since concluding witness (cwx) will copy vote (if its been concluded/decided) or vote for majority,
                  ;; collecting votes from x's r-1 witnesses (as will be the case when it's for cwx right after voting round)
                  votes (->> ws (map (fn [w] (->vote wcx w cr))))]

              (if-not (->> votes (every? :vote/final?))
                cr

                (let [ufws                              (->> votes
                                                             (filter :vote/value)
                                                             (map :vote/votee))
                      creator->unique-tip-seq           (->> ufws (map event->creator->unique-tip))
                      received-creators                 (apply set/intersection (map (comp set keys) creator->unique-tip-seq))
                      creator->received-unique-tip      (->> creator->unique-tip-seq
                                                             (map #(select-keys % received-creators))
                                                             (apply merge-with min-sp))
                      prev-creator->received-unique-tip (:concluded-round/creator->received-unique-tip cr)
                      es-r                              (->> creator->received-unique-tip
                                                             (mapcat (fn [[c received-unique-tip]]
                                                                       (->> received-unique-tip
                                                                            (iterate self-parent)
                                                                            (take-while some?)
                                                                            (take-while (fn [sp-evt] (not (hash= sp-evt (prev-creator->received-unique-tip c)))))))))

                      ;; forks do not affect that we did receive some prior non-forked event of that creator
                      next-creator->received-unique-tip (merge-with max-sp
                                                                    prev-creator->received-unique-tip
                                                                    creator->received-unique-tip)

                      ;; etr->ufw-les  (concluded-round->event-to-receive->learned-events next-cr)
                      ;; ufws-creators (->> ufws (into #{} (map :event/creator)))
                      ;; es-nr*        (transient [])
                      ;; es-r*         (transient [])
                      ;; _             (doseq [[etr ufw-les] etr->ufw-les]
                      ;;                 (if (hash= ufws-creators (set (map :event/creator ufw-les)))
                      ;;                   (conj! es-r* etr)
                      ;;                   (conj! es-nr* etr)))
                      ;; es-nr         (set (persistent! es-nr*))
                      ;; es-r          (set (persistent! es-r*))
                      ;; next-cr       (-> next-cr
                      ;;                   (assoc :concluded-round/etr->ufw-les etr->ufw-les)
                      ;;                   (assoc :concluded-round/es-nr es-nr)
                      ;;                   (assoc :concluded-round/es-r  es-r))

                      ?last-received-event (->?received-event cr next-cr-r es-r)
                      db                   (cr-db cr)
                      next-db              (if (nil? ?last-received-event)
                                             db
                                             (prev-db+received-event->db db ?last-received-event))
                      next-stake-map       (-> next-db :stake-map)
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
                      next-cr              (cond-> (hash-map :concluded-round/r                            next-cr-r
                                                             :concluded-round/witness-concluded            wcx
                                                             :concluded-round/ws                           ws
                                                             :concluded-round/votes                        votes
                                                             :concluded-round/ufws                         ufws
                                                             :concluded-round/es-r                         es-r
                                                             :concluded-round/creator->received-unique-tip next-creator->received-unique-tip
                                                             :concluded-round/prev-concluded-round         cr
                                                             :concluded-round/db                           next-db
                                                             :concluded-round/stake-map                    next-stake-map)
                                             ?last-received-event (assoc :concluded-round/last-received-event ?last-received-event))]
                  ;; try to conclude next round
                  (recur next-cr))))))))))

(defn* ->?received-event
  [prev-cr cr-r cr-es-r]
  (let [received-round-size  (count cr-es-r)
        ?prev-received-event (some-> prev-cr :concluded-round/last-received-event)]

    (->> cr-es-r
         (sort (fn [evt1 evt2] (let [evt1-depth (event->depth evt1)
                                     evt2-depth (event->depth evt2)]
                                 (if (not= evt1-depth evt2-depth)
                                   (< evt1-depth evt2-depth)
                                   (let [evt1-idx (event->index evt1)
                                         evt2-idx (event->index evt2)]
                                     (if (not= evt1-idx evt2-idx)
                                       (< evt1-idx evt2-idx)
                                       (< (hash evt1) (hash evt2))))))))
         ;; no value in building a chain of received events
         ;; TODO just store ordered received events on concluded-round
         (reduce (fn [?prev-received-event evt]
                   (let [r-idx (if (some-> ?prev-received-event :received-event/r (= cr-r))
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
                         (case (mod cr-r 3)
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
                     (cond-> (let [idx (or (some-> ?prev-received-event :received-event/idx inc)
                                           0)]
                               {:received-event/event         evt
                                :received-event/received-time idx
                                :received-event/idx           idx
                                :received-event/r             cr-r
                                :received-event/r-idx         r-idx
                                :received-event/color         color})
                       ?prev-received-event (assoc :received-event/prev-received-event ?prev-received-event))))
                 ?prev-received-event))))

#_
(defn* concluded-round->?received-event
  [{:concluded-round/keys [prev-concluded-round r es-r etr->ufw-les] :as concluded-round}]
  (let [received-round-size  (count es-r)
        ?prev-received-event (some-> prev-concluded-round :concluded-round/last-received-event)]

    (->> es-r
         (map (fn [e-r]
                ;; TODO perhaps do median calculation on numbers, calcing median members on view side, when required
                (let [learned-by (-> e-r etr->ufw-les)
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

         (sort (fn [{re1-rt                              :received-event/received-time
                     {e1-ct :event/creation-time :as e1} :received-event/event}
                    {re2-rt                              :received-event/received-time
                     {e2-ct :event/creation-time :as e2} :received-event/event}]
                 (if (not= re1-rt re2-rt)
                   (< re1-rt re2-rt)
                   (if (not= e1-ct e2-ct)
                     (< e1-ct e2-ct)
                     (< (hash e1) (hash e2)))))) ;; TODO Add tie-breaking by XOR of witness signatures, as in spec https://hyp.is/_XoSCtShEe6VTx9ByOxu1w/www.swirlds.com/downloads/SWIRLDS-TR-2016-01.pdf

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
                 ?prev-received-event))))

(def concluded-round->stake-map cr-stake-map)
(def cr->stake-map concluded-round->stake-map)

;; --------------- Stake ----------------
(def total-stake 100)
(def many-stake 50 #_(-> total-stake (* 2) (/ 3))) ;; TODO rewrite to supermajority - (N+F)/2

(def db->stake-map :stake-map)
(def cr->db cr-db)


;; -------------- Derived DB --------------------
(defonce *tx-handlers (atom {}))
(defn reg-tx-handler! [tx-handler-id tx-handler]
  (swap! *tx-handlers assoc tx-handler-id tx-handler))

(reg-tx-handler! :inc-counter (fn [db _ _] (update db :counter inc)))

(defn ?tx-or-txes->?txes [?tx-or-txes]
  (cond (nil? ?tx-or-txes)             nil
        (-> ?tx-or-txes first vector?) ?tx-or-txes
        :else                          [?tx-or-txes]))

(defn apply-tx-handler [db {:event/keys [tx] :as event}]
  (l [:apply-tx-handler db event])
  (l (let [txes (?tx-or-txes->?txes tx)]
       (->> txes
            (reduce (fn [db-acc [tx-handler-id :as tx]]
                      (let [?tx-handler (get @*tx-handlers tx-handler-id)]
                        (cond-> db-acc
                          ?tx-handler (?tx-handler event tx))))
                    db)))))



(defonce *smartcontracts (atom []))
(defn apply-smartcontracts [db received-event]
  (l [:apply-smartcontracts db received-event])
  (l (->> @*smartcontracts
          (reduce (fn [db-acc smartcontract]
                    (smartcontract db-acc received-event))
                  db))))

;; there can be other strategies to order events,
;; and hence other versions of derived dbs, fit for different use-cases
;; e.g.,
;; lamport (based on depth + creator tie-breaker) - same order across peers, not stable
;; subjective (sp events first) - subjective order, stable, useful for co-structures

;; as well as mixed ordering strategies
;; e.g., hg + lamport ordering of not received events
;; e.g., hg + subjective - stable, until received

(m/=> prev-db+received-event->db [:=> [:cat :topic [:map]] :topic])
(defn prev-db+received-event->db [prev-db {:received-event/keys [r event]
                                           ?prev-re             :received-event/prev-received-event
                                           :as                  re}]
  (l [:prev-db+received-event->db prev-db re])
  (let [prev-re-db (if (or (nil? ?prev-re)
                           (not (hash= (:received-event/r ?prev-re)
                                       r)))
                     prev-db
                     (prev-db+received-event->db prev-db ?prev-re))]
    (cond-> prev-re-db
      (:event/tx event) (apply-tx-handler event)
      :always           (apply-smartcontracts re))))


;; A1                    ;; r1 wit d = 0, concluded, d = 0
;; A2(add mem) => added  ;; r2 wit d = 0, concluded, d = 2
;; A3 r2 B1 => (add mem) ;; r2 wit
;; A4                    ;; r3 wit
;;       B2              ;; r3 wit
;; A5                    ;; r4 wit                                                       r3
;;       B3              ;; r4 wit                                                       r3
;; A6                    ;; r5 wit, concludes r3, received: A3 <> B1, A4 (!), added C -> r3
;;       B4              ;; r3
;;           C1          ;; r3
;; A7                    ;; r3
;;      B5               ;; r4 wit

(reg-tx-handler! :assoc (fn [db [_ k v]] (assoc db k v)))
(deftest ->?concluded-round-test
  (testing "crs get concluded right away for 1-member stake-map"
    (let [a-e1    {:event/creator "a1"
                   :event/topic   {:stake-map {"a1" total-stake}}}
          a-e2    {:event/creator     "a1"
                   :event/tx          [:inc-counter]
                   :event/self-parent a-e1}
          a-e1-cr (->concluded-round a-e1)
          a-e2-cr (->concluded-round a-e2)
          topic   (-> a-e1 :event/topic)]
      (is (= {:concluded-round/r         0
              :concluded-round/db        topic
              :concluded-round/stake-map (:stake-map topic)}
             (select-keys a-e1-cr [:concluded-round/r
                                   :concluded-round/db
                                   :concluded-round/stake-map])))

      (is (= {:concluded-round/r    1
              :concluded-round/es-r [a-e2]}
             (select-keys a-e2-cr [:concluded-round/r
                                   :concluded-round/es-r])))
      (is (= 1 (-> a-e2-cr :concluded-round/db :counter))))))


(defn* ^:memoizing event->?sp-tip-tx-event [event]
  (if (some? (tx event))
    event
    (some-> (self-parent event) event->?sp-tip-tx-event)))

#_
(defn* ^:memoizing event->creator->sp-tip-tx-event [{:event/keys [creator tx self-parent other-parent] :as event}]
  (cond-> (or (merge-with max-sp
                          (some-> self-parent event->creator->sp-tip-tx-event)
                          (some-> other-parent event->creator->sp-tip-tx-event))
              (hash-map))
    tx
    (assoc creator event)))

(defn* ^:memoizing event->creator->sp-tip-tx-event [event]
  (->> event
       event->creator->unique-tip
       (filter-map-vals event->?sp-tip-tx-event)))

(defn* ^:memoizing event->creator->sp-tip-tx-event-received [event]
  (let [{:concluded-round/keys [creator->received-unique-tip]} (->concluded-round event)]
    (->> creator->received-unique-tip
         (filter-map-vals event->?sp-tip-tx-event))))

(defn* ^:memoizing event->sp-tip-tx-events-not-received [event]
  (let [creator->sp-tip-tx-event          (-> event event->creator->sp-tip-tx-event)
        creator->sp-tip-tx-event-received (-> event event->creator->sp-tip-tx-event-received)]
    (->> creator->sp-tip-tx-event
         (filter (fn [[creator sp-tip-tx-event]] (not (hash= sp-tip-tx-event (-> creator creator->sp-tip-tx-event-received)))))
         (map second))))

#_
(defn* ^:memoizing event->creator->sp-tip-tx-event-received [event]
  (let [cr (->concluded-round event)]
    (->> cr
         :concluded-round/es-r
         (reduce (fn [cr->creator->sp-tip-tx-events-acc {:event/keys [creator tx] :as event-received}]
                   (cond-> cr->creator->sp-tip-tx-events-acc
                     (and tx
                          (let [?current-creator-sp-tip-tx-event (get cr->creator->sp-tip-tx-events-acc creator)]
                            (or (nil? ?current-creator-sp-tip-tx-event)
                                (< (event->index ?current-creator-sp-tip-tx-event) (event->index event-received)))))
                     (assoc creator event-received)))
                 ;; could have cr in args instead of event so less traveling keys, but less pretty interface for app.topic that uses it
                 (or (some-> (:concluded-round/prev-concluded-round cr)
                             :concluded-round/witness-concluded
                             event->creator->sp-tip-tx-event-received)
                     (hash-map))))))


(defn evt->db [evt] (-> evt ->concluded-round :concluded-round/db))
(defn* ^:memoizing evt->max-members-across-time [evt]
  (max (some-> evt self-parent evt->max-members-across-time)
       (some-> evt evt->db :member-init-keys count)
       0))
