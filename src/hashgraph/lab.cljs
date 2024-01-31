(ns hashgraph.lab
  "A place to test the universe to get insight. A playground."
  (:require [hashgraph.utils :as utils]
            [hashgraph.app.playback :as hga-playback]
            [goog.object :as gobject]))


(let [l1 (list 1 2 3)]
  (= l1 (-> l1 rest (conj 1))))

(let [r (doall (range 100000))
      r2 (-> r (rest) (conj 0))
      m {r :val}]
  (time (get m r)))

(identical? (drop 1 (drop 1 (list 1 2 3 4)))
            (drop 2 (list 1 2 3 4)))



(let [r (range 100000)]
  (time (transduce (comp (filter even?)
                         (map inc))
                   +
                   (range 10000)))
  (time (reduce ((comp (filter even?)
                       (map inc))
                 +)
                (range 10000)))
  (time (-> r (->> (filter even?)
                   (map inc)
                   (reduce +))))
  )


#_
(profile
 {}
 (let [arg [(set (range 0 10000))
            (set (range 5000 15000))
            (set (range 10000 20000))
            ]]
   (doall (repeatedly 100 #(p ::union (doall (apply clojure.set/union arg)))))
   (doall (repeatedly 100 #(p ::reduce (doall (reduce into #{} arg)))))
   :finished))
;; union is generally faster than reduce
;; TODO check sets vs transducers for difference vs distinct + remove


(-> (sorted-map)
    (assoc (with-meta {} {:inspected/at-time (cljs.core/system-time)}) 1)
    (assoc (with-meta {} {:inspected/at-time (cljs.core/system-time)}) 2))
;; cannot compare a map with meta

(-> (sorted-map-by (fn [x y] (println x (meta x) y (meta y)) (if (= x y)
                                                               0
                                                               (> (meta x) (meta y)))))
    (assoc (with-meta {1 1} 1) 1)
    (assoc (with-meta {1 1} 2) 1)
    (assoc (with-meta {1 1} 3) 2)
    (assoc (with-meta {2 2} 0) 2))


(-> (sorted-set-by meta)
    (conj (with-meta {1 1} 1))
    (conj (with-meta {2 2} 2))
    (conj (with-meta {2 2} 2)))
;; Duplicate conjed.
;; This is because first arg to sorted-set-by should be comparator and not a key-fn that pulls vals to compare,
;; as I wrongly assumed, since that would be in-line with sort-by interface.
;; I.e., I'd expect that the interface of sorted-set-by is
;; ([key-fn & colls])
;; ([key-fn comparator & colls])
;;
;; In this implementation need to provide comparator as arg

(-> (sorted-set-by (fn [x y] (> (count (keys x)) (count (keys y)))))
    (conj {1 1})
    (conj {1 1}))
;; duplicate isn't conjed, as expected

(-> (sorted-set-by (fn [x y] (> (meta x) (meta y))))
    (conj (with-meta {1 1} 1))
    (conj (with-meta {1 1} 2)))
;; duplicate conjed, not expected
;; I'd expect that sorted set would behave like a set, and would not even try to add {1 1} the second time.
;; In this implementation need to return 0 in comparator for when x = y.

;; Side-note wonder, sorted-set is implemented atop PersistentTreeMap,
;; will it add duplicate key as well?
(-> (sorted-map-by (fn [x y] (> (meta x) (meta y))))
    (assoc (with-meta {1 1} 1) 1)
    (assoc (with-meta {1 1} 2) 2))
;; Yes, duplicate added, as not desired.

;; Trying sorted-set with comparator
(-> (sorted-set-by (fn [x y] (if (= x y)
                               0
                               (> (meta x) (meta y)))))
    (conj (with-meta {1 1} 1))
    (conj (with-meta {1 1} 2)))
;; works as desired

(-> (sorted-set-by (fn [x y] (if (= x y)
                               0
                               (> (meta x) (meta y)))))
    (conj (with-meta {1 1} 1))
    (conj (with-meta {1 1} 2))
    (conj (with-meta {2 2} 1)))
;; {2 2} is not conjed, since wrapping comparator treats returned falses from (compare x y) (compare y x) as 0
;; I'd expect that 1) {2 2} would be conjed 2) it would be sorted, nevermind the 0 comparation result.
;; In this implementation, need to either ensure that mirror comparator call returns different boolean or return numbers explicitly.

;; with different booleans
(-> (sorted-set-by (fn [x y] (if (= x y)
                               0
                               (>= (meta x) (meta y)))))
    (conj (with-meta {1 1} 1))
    (conj (with-meta {1 1} 2))
    (conj (with-meta {2 2} 1)))
;; works as desired

(-> (sorted-set-by (fn [x y] (if (= x y)
                               0
                               (<= (meta x) (meta y)))))
    (conj (with-meta {1 1} 1))
    (conj (with-meta {1 1} 2))
    (conj (with-meta {2 2} 1))
    (conj (with-meta {-1 -1} -1))
    (contains? {-1 -1}))
;; sorts

;; All-in-all, sorted-set-by does the job, but needs to be constructed with care
;; However, let's have a gotchas-free version.
hashgraph.utils/sorted-set-by*

(-> (vector (with-meta {1 1} 1))
    (-indexOf {1 1}))
;; meta is'n taken into account on lookup


#_
(do
  (time (do (doall (repeatedly 100000 #(-> @hga-playback/*playback :events first :event/creator))) 1))
  (time (do (doall (repeatedly 100000 #(-> @hga-playback/*playback :events first (-hash)))) 1)) ;; hash lookup is generally faster than ^
  (time (do (doall (repeatedly 100000 #(-> @hga-playback/*playback :events first hga-playback/event->event-id))) 1)))



(extend-type cljs.core/TransientHashMap
  ISeqable
  (-seq [^js coll]
    (when (pos? (.-count coll))
      (let [root ^js (.-root coll)
            s (when-not (nil? root) (.inode-seq root))]
        (if (.-has_nil_QMARK_ coll)
          (cons (MapEntry. nil (.-nil_val coll) nil) s)
          s)))))

(let [nths (doall (range 0 100000))]
  (let [tm (transient (hash-map))]
    (time (doseq [nth nths]
            (assoc! tm nth nth)))
    (time (doseq [[k v] tm] 1)))

  (let [obj (js-obj)]
    (time (doseq [nth nths]
            (gobject/set obj nth nth)))
    (time (gobject/forEach obj (fn [k v _] nil)))))
;; both creation and iteration is many times faster on plain js-obj

#_
(doseq [[k v] (doto (transient (hash-map)) (assoc! 1 1) (assoc! 2 2))]
       (println k v))


(type
 (let [tm (transient (hash-map))]
   (loop [tm* tm
          nth 2]
     (if (pos? nth)
       (let [new-tm* (assoc! tm* nth nth)]
         (recur new-tm* (dec nth)))
       tm*))))
;; cljs transient hash map in cljs can be bashed in place
;; in clj it's a transient array map, so some assoc! on 8th el will return transient hash map - can't bash in place


;; is it faster to use string keys or int keys on js-obj?
(let [nths     (doall (range 0 100000))
      str-nths (doall (map str nths))
      obj1 (js-obj)
      obj2 (js-obj)
      [obj1-t _] (utils/timed
                  (doseq [nth nths]
                    (goog.object/set obj1 nth nth)
                    (goog.object/get obj1 (dec nth))))
      [obj2-t _] (utils/timed
                  (doseq [str-nth str-nths]
                    (goog.object/set obj2 str-nth str-nth)
                    (goog.object/get obj2 str-nth)))]
  [obj1-t obj2-t (/ obj1-t obj2-t)])
;; str keys ~1-2 times faster

;; is it faster to str on clj side or on goog.object side?
(let [nths     (doall (range 0 100000))
      obj1 (js-obj)
      obj2 (js-obj)
      [obj1-t _] (utils/timed
                  (doseq [nth nths]
                    (goog.object/set obj1 nth nth)
                    (goog.object/get obj1 (dec nth))))
      [obj2-t _] (utils/timed
                  (doseq [nth nths]
                    (goog.object/set obj2 (str nth) nth)
                    (goog.object/get obj2 (str nth))))]
  [obj1-t obj2-t (/ obj1-t obj2-t)])
;; it's faster on goog.object side



;; is it faster to strore mem args in hash-map by args or by (-hash args)?
(let [nths     (doall (range 0 1000))
      argss    (doall (map list (map utils/nested-range-map nths)))
      thm1     (transient (hash-map))
      thm2     (transient (hash-map))
      [t1 _] (utils/timed
                  (doseq [args argss]
                    (assoc! thm1 args args)
                    (get thm1 args)))
      [t2 _] (utils/timed
                  (doseq [args argss]
                    (assoc! thm2 (-hash args) args)
                    (get thm2 (-hash args))))]
  [t1 t2 (/ t1 t2)])
;; associng args is ~200-600 TIMES slower than their hash
;; I suspect it won't be such a big difference on smaller args






(defn bench
  [argss f]
  (first
   (utils/timed
    (doseq [args argss]
      (f args)))))

(defn bench-many
  [args & fs]
  (doall (for [f fs]
           (bench args f))))

(defn bench-compare
  [args named-fs]
  (assert (seqable? args))
  (doall args)
  (let [results
        (->> (for [[n f] named-fs]
               [n (bench args f)])
             (sort-by second)
             (into (array-map)))
        [fastest-n fastest-t] (first results)]
    (array-map :fastest fastest-n
               :faster-than-by-times (into (array-map) (map (fn [[n t]] [n (/ t fastest-t)]) (rest results)))
               :results results)))

(bench-compare (range 0 100) {:f1 #(reduce + (range %))
                              :f2 identity})

;; is it faster to (js-obj 1 1) or (goog.object/create 1 1)?
(bench-compare (range 10000) {:js-obj #(js-obj % %)
                              :goog   #(goog.object/create % %)})
;; js-obj is faster by ~1-1.3 times

;; is it faster to use volatile or js-obj to store last mem?
(let [vol      (volatile! nil)
      obj      (js-obj)
      argss    (utils/nested-range-map 100000)]
  (bench-compare argss {:vol #(vreset! vol {(-hash %) (-hash %)})
                        :obj #(do (goog.object/clear obj)
                                  (goog.object/set obj (-hash %) (-hash %)))}))
;; volatile is faster than js-obj

(= (hash (list {1 1} {2 2}))
   (hash (list {1 1} {2 2})))

;; is it faster to (hash (list)) or (-hash (list))?
(def *last-time (volatile! 1))
(let [n      10000
      from-n (* @*last-time n)
      to-n   (* (inc @*last-time) n)
      argss (map list (utils/nested-range-map from-n to-n))]
  (bench-compare argss {:hash #(hash %)
                        :-hash #(-hash %)}))
;; -hash is ~1-5 times faster


((fn [& args]
   (type args))
 {1 {2 3}} 2)

(type (list 1 2 3))
(cljs.core/-hash (list 1 2 3))



;; what's faster for mem, js-obj or (transient (hash-map)) ?
(def last-time (volatile! 1))
(let [n     100000
      thm   (transient (hash-map))
      obj   (js-obj)
      from  (* n @last-time)
      to    (* n (inc @last-time))
      argss (doall (utils/nested-range-map from to))]
  (vreset! last-time (inc @last-time))
  (doseq [args argss]
    (-hash args))
  (let [[thm-add-t _] (timed
                       (doseq [args argss]
                         (assoc! thm (-hash args) args)))
        [obj-add-t _] (timed
                       (doseq [args argss]
                         (goog.object/set obj (-hash args) args)))
        [thm-get-t _] (timed
                       (doseq [args argss]
                         (get thm (-hash args))))
        [obj-get-t _] (timed
                       (doseq [args argss]
                         (goog.object/get obj (-hash args))))]
    [[thm-add-t obj-add-t (/ thm-add-t obj-add-t)]
     [thm-get-t obj-get-t (/ thm-get-t obj-get-t)]]))
;; about the same speed

;; let's try storing more-real-world args (nested data structures)
(def last-time10 (volatile! 1))
(let [n     100
      thm   (transient (hash-map))
      obj   (js-obj)
      from  (* n @last-time)
      to    (* n (inc @last-time))
      nths     (doall (range from to))
      args-arr (doall (->> nths (mapv (fn [nth] (utils/nested-range-map from nth)))))]
  (vreset! last-time10 (inc @last-time))
  (let [[thm-t _] (timed
                   (loop [prev-args nil
                          [args & next-args] args-arr]
                     (when args
                       (when prev-args
                         (get thm prev-args))
                       (assoc! thm args args)
                       (recur args next-args))))

        [obj-t _] (timed
                   (loop [prev-args nil
                          [args & next-args] args-arr]
                     (when args
                       (when prev-args
                         (goog.object/get obj prev-args))
                       (goog.object/set obj args args)
                       (recur args next-args))))]
    [thm-t obj-t (/ thm-t obj-t)]))
;; storing cljs data structures in js-obj is daaaammmn slow, even on n set to 100
;; any better when storing by hash?

(def last-time11 (volatile! 1))
(let [n     1000
      thm   (transient (hash-map))
      obj   (js-obj)
      from  (* n @last-time)
      to    (* n (inc @last-time))
      nths     (doall (range from to))
      args-arr (doall (->> nths (mapv (fn [nth] (list (utils/nested-range-map from nth) (utils/nested-range-map from nth))))))]
  (vreset! last-time11 (inc @last-time))
  (let [[thm-t _] (timed
                   (loop [prev-args nil
                          [args & next-args] args-arr]
                     (when args
                       (when prev-args
                         (get thm (-hash prev-args)))
                       (assoc! thm (-hash args) args)
                       (recur args next-args))))

        [obj-t _] (timed
                   (loop [prev-args nil
                          [args & next-args] args-arr]
                     (when args
                       (when prev-args
                         (goog.object/get obj (-hash prev-args)))
                       (goog.object/set obj (-hash args) args)
                       (recur args next-args))))]
    [thm-t obj-t (/ thm-t obj-t)]))
;; ~300-600 TIMES faster to store by hash in js-obj, WOW.
;; js-obj is daaammmn fast with hashes

#_
(cljs.pprint/pprint
 (macroexpand
  '(defn* ^:timing ^:memoizing timing-test-fn []
     [*->time* (*->time*)])))

#_(timing-test-fn)



(defn bench [reps f]
    (timing
     (do (doall (repeatedly reps f))
         (*->time*))))

(defn bench-memoizing-versions []
  (def test-fn
    (utils/memoizing
     {}
     (fn [num] (let [prev (if (zero? (dec num))
                            0
                            (test-fn (dec num)))]
                 (+ prev num)))))
  (def test-fn-only-last
    (utils/memoizing
     {:only-last? true}
     (fn [num] (let [prev (if (zero? (dec num))
                            0
                            (test-fn-only-last (dec num)))]
                 (+ prev num)))))
  (def test-fn-warmed
    (utils/memoizing
     {:recur-by   (fn [num] [(dec num)])
      :recur-stop (fn [num] (zero? num))}
     (fn [num] (let [prev (if (zero? (dec num))
                            0
                            (test-fn-warmed (dec num)))]
                 (+ prev num)))))

  (def test-fn-warmed-only-last
    (utils/memoizing
     {:recur-by   (fn [num] [(dec num)])
      :recur-stop (fn [num] (zero? num))
      :only-last? true}
     (fn [num] (let [prev (if (zero? (dec num))
                            0
                            (test-fn-warmed-only-last (dec num)))]
                 (+ prev num)))))

  (let [n    500000
        reps 100000]
    [#_(bench reps #(test-fn n))
     #_(bench reps #(test-fn-only-last n))
     (bench reps #(test-fn-warmed n))
     (bench reps #(test-fn-warmed-only-last n))]))

#_
(bench-memoizing-versions)

;; in order to track only-latest k->v,
;; is it fastest to keep it in a volatile in {}
;; or bash transient {} with dissoc! prev and assoc! new?
#_#_
(def last-time1 (volatile! 1))
(let [n 100
      nths (doall (range (* n @last-time1) (* n (inc @last-time1))))
      v (volatile! nil)
      t (transient (hash-map))]
  (vreset! last-time1 (inc @last-time1))
  (let [vt (timing ;; times faster, for n > 100
            (do (doseq [nth nths]
                  (get @v [(dec nth)])
                  (vreset! v {[nth] nth}))
                (*->time*)))
        tt (timing
            (do (doseq [nth nths]
                  (get t [(dec nth)])
                  (dissoc! t [(dec nth)])
                  (assoc! t [nth] nth))
                (*->time*)))]
    [vt tt (/ tt vt)]))

#_
(let [mem (volatile! nil)]
  (apply (fn [& args]
     (l (type args))
     (vreset! mem [args 1])
           (let [[from-mem-args _] (deref mem)]
       (l (type from-mem-args))
       (l (identical? args from-mem-args))
       ((fn [args2]
          (l args2)
          (l (type args2))
          (l (identical? args args2))
          (l (identical? args2 from-mem-args)))
        args))) [1 2 3])
  (apply (fn [& args]
     (l (type args))
           (let [[from-mem-args _] (deref mem)]
       (l (type from-mem-args))
       (l (identical? args from-mem-args))
       ((fn [args2]
          (l args2)
          (l (type args2))
          (l (identical? args args2))
          (l (identical? args2 from-mem-args)))
        args))) [1 2 3]))

#_#_
(def last-time (volatile! 1))
(let [n     10000
      v-hk  (volatile! nil)
      v-v   (volatile! nil)
      v-vec (volatile! nil)
      v-map (volatile! nil)
      from  (* n @last-time)
      to    (* n (inc @last-time))]
  (vreset! last-time (inc @last-time))
  (println "----")
  (time (loop [nth from]
          (when-not (= nth to)
            (let [hk (hash (list nth nth nth))]
              (identical? @v-hk (hash (dec nth)))
              @v-v
              (vreset! v-hk hk))
            (vreset! v-v nth)
            (recur (inc nth)))))
  (time (loop [nth from]
          (when-not (= nth to)
            (let [kh (hash (list nth nth nth))
                  [prev-kh prev-v] @v-vec]
              (identical? prev-kh (hash (dec nth)))
              prev-v
              (vreset! v-vec [kh nth]))
            (recur (inc nth)))))
  (time (loop [nth from]
          (when-not (= nth to)
            (let [new-k (list nth nth nth)]
              (get @v-map (dec nth))
              (vreset! v-map {new-k nth})) ;; fastest for both prev val lookup and current val lookup
            (recur (inc nth))))))

#_
(let [thm (transient (hash-map))]
  (loop [t* thm
         nth 5000]
    (if (zero? nth)
      t*
      (let [new-t* (assoc! t* [nth] {nth nth})]
        (when (not= (type t*) (type new-t*)) (println [(type t*) (type new-t*)]))
        (recur new-t* (dec nth))))))