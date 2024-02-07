(ns hashgraph.lab
  "A place to test the universe to get insight. A playground."
  (:require [hashgraph.utils.core :refer [*->time*] :refer-macros [timed timing] :as utils]
            [hashgraph.app.playback :as hga-playback]
            [goog.object :as gobject]
            [goog.array]
            [rum.core :as rum]))


(let [l1 (list 1 2 3)]
  (= l1 (-> l1 rest (conj 1))))

#_
(let [r (doall (range 100000))
      r2 (-> r (rest) (conj 0))
      m {r :val}]
  (time (get m r)))

(identical? (drop 1 (drop 1 (list 1 2 3 4)))
            (drop 2 (list 1 2 3 4)))


#_
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

#_
(-> (sorted-map)
    (assoc (with-meta {} {:inspected/at-time (cljs.core/system-time)}) 1)
    (assoc (with-meta {} {:inspected/at-time (cljs.core/system-time)}) 2))
;; cannot compare a map with meta

#_
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
hashgraph.utils.core/sorted-set-by*

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

#_
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
#_
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
#_
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
(def bench-mem-by-args-vs-by-args-hash
  (let [*last-time (atom 0)]
    (fn []
      (let [n      1000
            from-n (* @*last-time n)
            to-n   (* (inc @*last-time) n)
            nths   (doall (range from-n to-n))
            argss  (doall (map list (doall (map utils/nested-range-map nths))))
            argss-shuffled (shuffle argss)
            argss1 (take (/ n 2) argss-shuffled)
            argss2 (drop (/ n 2) argss-shuffled)
            thm1   (transient (hash-map))
            thm2   (transient (hash-map))
            [t1 _] (utils/timed
                    (doall
                     (doseq [args argss1]
                       (assoc! thm1 args args))))
            [t2 _] (utils/timed
                    (doall
                     (doseq [args argss2]
                       (assoc! thm2 (-hash args) args))))

            [get-t1 _] (utils/timed
                        (doall
                         (doseq [args argss1]
                           (get thm1 args))))
            [get-t2 _] (utils/timed
                        (doall
                         (doseq [args argss2]
                           (get thm2 (-hash args)))))]
        (swap! *last-time inc)
        {:add [t1 t2 (/ t1 t2)]
         :get [get-t1 get-t2 (/ get-t1 get-t2)]}))))
#_(bench-mem-by-args-vs-by-args-hash)
;; adding is about the same
;; getting is ~1-2 times faster by hash


;; is it faster to compare args by identical? vs lookup by args?
;; will only be of use for only-last mem
;; or for a hybrid mem of hash-map + last-call-mem
identical?



(defn bench
  [argss f]
  (first
   (utils/timed
    (doseq [args argss]
      (f args)))))

(defn bench-many
  [argss & fs]
  (doall (for [f fs]
           (bench argss f))))

(defn bench-compare
  [inputs named-fs]
  (assert (seqable? inputs))
  (doall (map doall inputs))
  (doall inputs)
  (let [results
        (->> (for [[n f] named-fs]
               [n (bench inputs f)])
             (sort-by second)
             (into (array-map)))
        [fastest-n fastest-t] (first results)]
    (array-map :fastest fastest-n
               :faster-than-by-times (into (array-map) (map (fn [[n t]] [n (/ t fastest-t)]) (rest results)))
               :results results)))

#_
(bench-compare (range 0 100) {:f1 #(reduce + (range %))
                              :f2 identity})

;; is it faster to (js-obj 1 1) or (goog.object/create 1 1)?
#_
(bench-compare (range 10000) {:js-obj #(js-obj % %)
                              :goog   #(goog.object/create % %)})
;; js-obj is faster by ~1-1.3 times

;; is it faster to use volatile or js-obj to store last mem?
#_
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
#_#_
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
(def bench-js-obj-vs-transient-hash-map
  (let [last-time (volatile! 1)]
    (fn []
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
           [thm-get-t obj-get-t (/ thm-get-t obj-get-t)]])))))
#_
(bench-js-obj-vs-transient-hash-map)
;; about the same speed

;; let's try storing more-real-world args (nested data structures)
(def bench-js-obj-vs-transient-hash-map-on-map-args
  (let [last-time (volatile! 1)]
    (fn []
      (let [n        100
            thm      (transient (hash-map))
            obj      (js-obj)
            from     (* n @last-time)
            to       (* n (inc @last-time))
            nths     (doall (range from to))
            args-arr (doall (->> nths (mapv (fn [nth] (utils/nested-range-map from nth)))))]
        (vreset! last-time (inc @last-time))
        (let [[thm-t _] (timed
                         (loop [prev-args          nil
                                [args & next-args] args-arr]
                           (when args
                             (when prev-args
                               (get thm prev-args))
                             (assoc! thm args args)
                             (recur args next-args))))

              [obj-t _] (timed
                         (loop [prev-args          nil
                                [args & next-args] args-arr]
                           (when args
                             (when prev-args
                               (goog.object/get obj prev-args))
                             (goog.object/set obj args args)
                             (recur args next-args))))]
          [thm-t obj-t (/ thm-t obj-t)])))))
#_
(bench-js-obj-vs-transient-hash-map-on-map-args)
;; storing cljs data structures in js-obj is daaaammmn slow, even on n set to 100
;; any better when storing by hash?

(def bench-obj-vs-transient-hash-map
  (let [last-time11 (volatile! 100)]
    (fn []
      (let [n        2000
            thm      (transient (hash-map))
            obj      (js-obj)
            from     (* n @last-time11)
            to       (* n (inc @last-time11))
            nths     (doall (range from to))
            args-arr
            (loop [out []
                   [nth & rest-nths] nths]
              (if-not nth
                out
                (recur (conj out (list (utils/nested-range-map from nth) (utils/nested-range-map from nth))) rest-nths)))
            #_(trampoline (fn build-args [out [nth & rest-nths]]
                                   (if-not nth
                                     out
                                     (let [new-out (conj out (list (utils/nested-range-map from nth) (utils/nested-range-map from nth)))]
                                       #(build-args new-out rest-nths))))
                                 []
                                 nths)]
        (println "built args" (count args-arr))
        (vreset! last-time11 (inc @last-time11))
        (let [[hashing-t _] (timed
                             (doall
                              (doseq [args args-arr]
                                (-hash args))))
              [thm-add-t _] (timed
                             (doall
                              (doseq [args args-arr]
                                (assoc! thm (-hash args) args))))

              [obj-add-t _] (timed
                             (doall
                              (doseq [args args-arr]
                                (goog.object/set obj (-hash args) args))))

              shuffled-args  (doall (shuffle args-arr))
              shuffled-args2 (doall (shuffle args-arr))
              shuffled-args3 (doall (shuffle args-arr))
              [thm-get-t _] (timed
                             (do (doall
                                  (doseq [args shuffled-args]
                                    (get thm (-hash args))))
                                 (doall
                                  (doseq [args shuffled-args2]
                                    (get thm (-hash args))))
                                 (doall
                                  (doseq [args shuffled-args3]
                                    (get thm (-hash args))))))

              [obj-get-t _] (timed
                             (do (doall
                                  (doseq [args shuffled-args]
                                    (goog.object/get obj (-hash args))))
                                 (doall
                                  (doseq [args shuffled-args2]
                                    (get thm (-hash args))))
                                 (doall
                                  (doseq [args shuffled-args3]
                                    (get thm (-hash args))))))]
          [hashing-t
           [thm-add-t obj-add-t (/ thm-add-t obj-add-t)]
           [thm-get-t obj-get-t (/ thm-get-t obj-get-t)]])))))

#_
(bench-obj-vs-transient-hash-map)
;; on n 1000,
;; about the same performance on add and get
;; although js-obj is generally a bit faster, more so on read

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
(def bench-volatile-vs-transient-for-only-last-mem
  (let [last-time (volatile! 1)]
    (fn []
      (let [n    100
            nths (doall (range (* n @last-time) (* n (inc @last-time))))
            v    (volatile! nil)
            t    (transient (hash-map))]
        (vreset! last-time (inc @last-time))
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
          [vt tt (/ tt vt)])))))
#_
(bench-volatile-vs-transient-for-only-last-mem)

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


;; is it faster to take-while&drop, split-with or alternative split-with, that does it in one pass?

(defn split-with* [pred coll]
  (loop [coll* coll
         tw-acc []]
    (if (empty? coll*)
      [tw-acc coll*]
      (let [[el & rest-coll] coll*]
        (if-not (pred el)
          [tw-acc coll*]
          (recur rest-coll (conj tw-acc el)))))))

(def bench-split-with-versions
  (fn []
    (let [n    10000
          m    5000
          r    (doall (range n))
          pred #(do (doall (range 1000)) (< % m))
          [twd-t twd-r] (timed
                         (let [taken   (doall (take-while pred r))
                               to-drop (count taken)
                               dropped (doall (drop to-drop r))]
                           [taken dropped]))
          [sw-t sw-r]   (timed
                         (let [sw-r    (split-with pred r)
                               [tw dw] sw-r]
                           (doall tw)
                           (doall dw)
                           sw-r))
          [swa-t swa-r] (timed
                         (let [swa-r   (split-with* pred r)
                               [tw dw] swa-r]
                           (doall tw)
                           (doall dw)
                           swa-r))]
      (assert (= twd-r sw-r swa-r))
      [twd-t sw-t swa-t])))
#_
(bench-split-with-versions)
;; split-with is generally faster on a fast pred, and ~x2 times slower on a more costly pred
;; split-with* is a bit faster than take&drop on a more costly pred


;; what's the fastest to get butlast of a seq, (butlast coll) or (rest (reverse coll))
#_
(do (let [r (vec (doall (range 0 1000000)))]
      (time (butlast r)))
    (let [r (doall (range 0 10000))]
      (time (doall (rest (doall (reverse r))))))
    1)
;; butlast is damm slow


;; is it faster to concat two vectors or into?
#_
(let [v1 (doall (vec (range 0 10000)))
      v2 (doall (vec (range 10000 20000)))]
  (time (doall (concat v1 v2)))
  (time (doall (into v1 v2)))
  1)
;; concatting is ~1-2 times faster


;; is it faster to (concat (reverse)) or into?
#_
(let [s1> (doall (reverse (range 0 100)))
      s2< (doall (range 100 110))]
  (time (into s1> s2<))
  (time (concat (reverse s2<) s1>)))
;; into's faster


;; what's the fastest way to lookup if key's in js-obj?
#_
(let [n 50000
      r (range 0 n)
      obj (apply js-obj (interleave r r))
      nth (rand-nth r)]
  (time (boolean (goog.object/get obj nth))) ;; almost always instant
  (time (goog.object/contains obj nth))      ;; many times slower
  (time (goog.object/containsKey obj nth)))  ;; almost instant
;; contains many times slower
;; get is fastest, a bit faster than containsKey




;; Are hashes reused?
;; We would see speedup on second call to a non-identical but equivalent seq
(let [r1 (doall (range 0 10000))
      r2 (doall (range 0 10000))
      [h1t h1] (timed (-hash r1))
      [h2t h2] (timed (-hash r2))]
  [(= h1 h2) h1t h2t (/ h1t h2t)])
;; hashes are the same, but speedup does not happen
;; thus, for benching on the same inputs, we can re-create inputs for each solution to bench

(defn bench
  [inputs f]
  (first
   (utils/timed
    (doseq [input inputs]
      (f input)))))

(defn bench-compare
  [inputs-builder named-fs]
  (assert (fn? inputs-builder))
  (assert (= (inputs-builder) (inputs-builder)))
  (let [results
        (->> (for [[n f] named-fs]
               [n (bench (inputs-builder) f)])
             (sort-by second)
             (apply concat)
             (apply array-map))
        [fastest-n fastest-t] (first results)]
    (array-map :fastest fastest-n
               :faster-than-by-times (into (array-map) (map (fn [[n t]] [n (/ t fastest-t)]) (rest results)))
               :results results)))

;; Rum's request-render may schedule component to be rendered multiple times, causing unnecessary computation.
;; Upgrading React and ReactDom to 18.2.0 did not solve multiple renders (and there happened serious performance drop).
;; So it's not solved on the React side..
;;
;; To escape that we could a) only schedule for render once b) schedule as is, ensure we haven't rendered it on render
;; a) seems better, as there won't be duplicates in render-queue, making going through it faster.
;; However, it will require not scheduling duplicates on request-render, making it a bit more costly.
;; Let's bench a)
(let [request-queue          (volatile! [])
      request-queue2         (volatile! [])
      request-queue-set      (volatile! #{})
      render-times           (range 1000)
      render-comps-builder   #(interleave (range 100) (range 100)) ;; simulates two render requests per comp
      ]
  (reduce
   (fn [t-acc _render-time]
     (let [[rq-t _] (timed
                     (do (doseq [c (render-comps-builder)]
                           (vswap! request-queue conj c))
                         @request-queue
                         (vreset! request-queue [])))
           [rqd-t _] (timed
                      (do (doseq [c (render-comps-builder)]
                            (vswap! request-queue2 conj c))
                          (distinct @request-queue2)
                          (vreset! request-queue2 [])))
           [rqs-t _] (timed
                      (do (doseq [c (render-comps-builder)]
                            (vswap! request-queue-set conj c))
                          @request-queue-set
                          (vswap! request-queue-set #{})))]
       (-> t-acc
           (update :rq-t + rq-t)
           (update :rqd-t + rqd-t)
           (update :rqs-t + rqs-t))))
   (hash-map)
   render-times)
  #_(bench-compare
   #(range 100)
   {:rq  (fn [nth] (vswap! request-queue conj nth))
    :rqd (fn [nth] (vswap! request-queue-distinct conj nth))}))
;; distinct performs on-pair with set, slower than original

(def *react-comps (atom []))
(rum/defcs test-comp <
  {:did-mount (fn [state] (swap! *react-comps conj (:rum/react-component state)) state)}
  [state nth]
  [:div nth])
(rum/defc test-root-comp [nths]
  (doall
   (for [nth nths]
     (test-comp nth))))
(rum/mount (test-root-comp (range 100)) (.getElementById js/document "root"))

@*react-comps
(js/console.log (test-comp))

(let [request-queue          (volatile! [])
      render-times           (range 1000)
      render-comps-builder   #(interleave @*react-comps @*react-comps) ;; simulates two render requests per comp
      ]
  (def vec-tr (transient []))
  (def set-tr (transient #{}))
  (def garr   (js/Array))
  (def arr    (js/Array))
  (def gobj   (js-obj))
  (def gobj2  (js-obj))
  (def js-map (new js/Map))
  (def js-set (new js/Set))
  (def js-set2 (new js/Set))
  (->> (reduce
        (fn [t-acc _render-time]
          (let [[t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (vswap! request-queue conj c))
                     @request-queue
                     (vreset! request-queue [])))

                [vec-tr-t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (conj! vec-tr c))
                     (distinct vec-tr)
                     (set! vec-tr (transient []))))

                [set-tr-t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (conj! set-tr c))
                     set-tr
                     (set! set-tr (transient #{}))))

                [garr-t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (goog.array/insert garr c))
                     (goog.array/removeDuplicates garr)
                     garr
                     (set! garr (js/Array))))

                arr-only-unique (fn [value index array] (identical? (.indexOf array value) index))
                [arr-t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (goog.array/insert arr c))
                     (.filter arr arr-only-unique)
                     (set! arr (js/Array))))

                [gobj-t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (goog.object/set gobj c true))
                     (goog.object/getKeys gobj)
                     (set! gobj (js-obj))))

                [gobj2-t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (goog.object/setIfUndefined gobj2 c true))
                     (goog.object/getKeys gobj2)
                     (set! gobj2 (js-obj))))

                [js-map-t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (.set js-map c true))
                     (js/Array.from (.keys js-map))
                     (set! js-map (new js/Map))))

                [js-set-t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (.add js-set c true))
                     (.values js-set)
                     (set! js-set (new js/Set))))

                [js-set2-t _]
                (timed
                 (do (doseq [c (render-comps-builder)]
                       (.add js-set c true))
                     (.values js-set)
                     (.clear js-set)))]
            (-> t-acc
                (update :t + t)
                (update :vec-tr-t + vec-tr-t)
                (update :set-tr-t + set-tr-t)
                (update :garr-t + garr-t)
                (update :arr-t + arr-t)
                (update :gobj-t + gobj-t)
                (update :gobj2-t + gobj2-t)
                (update :js-map-t + js-map-t)
                (update :js-set-t + js-set-t)
                (update :js-set2-t + js-set2-t))))
        (hash-map)
        render-times)
       (sort-by val)
       (apply concat)
       #_(apply array-map))
  #_(bench-compare
   #(range 100)
   {:rq  (fn [nth] (vswap! request-queue conj nth))
    :rqd (fn [nth] (vswap! request-queue-distinct conj nth))}))
;; js-set is fastest on 2 comp duplicates per 100 comps per render
;; js-set is generally faster than js-arr on no duplicates, generally outperforms vatilla by 1.5 times


;; does js/Map preserve order in which keys were added?
(let [js-map (new js/Map)]
  (doseq [nth (range 1000)]
    (.set js-map nth nth))
  (js/console.log (.keys js-map))
  )
;; it does, but .keys returns iterator,
;; may not be suitable for


;; does React re-renders the same comp requested multiple times
(rum/defcs test-comp2 < (rum/local 0 ::*a) (rum/local 0 ::*b) rum/reactive
  {:did-mount (fn [state] (js/setTimeout #(do (swap! (::*a state) inc)
                                              (swap! (::*b state) inc)))
                state)}
  [{::keys [*a *b]} _]
  (js/console.log "render" @*a @*b)
  [:div
   @*a @*b])

(rum/mount (test-comp2) (.getElementById js/document "root"))
;; gets in Rum request-queue two times, renders by React once


;; poking around trying to make js-obj lookapabale as (:k obj) or (obj :k)
;; no luck thus far
#_
(do
(extend-type object
  IFn
  (-invoke [coll kw]
    (js/console.log coll)
    (goog.object/get coll kw)))
(deftype My [])
(extend-type My
  cljs.core/IFn
    (cljs.core/-invoke [sym coll]
    (get coll sym))
    (cljs.core/-invoke [sym coll not-found]
    (get coll sym not-found)))

(extend-type cljs.core/Keyword
    IFn
  (-invoke [sym coll]
    (get coll sym))
  (-invoke [sym coll not-found]
    (get coll sym not-found))
  )
#_(goog.object/set js/Object "call" (fn [& args] (println args)))
#_(set! (.. js/Object -prototype -call) (fn [_ k] (println k) (this-as this (goog.object/get this k)))) ;; works!
(js/console.log js/Object)


(let [obj (js-obj :a 2)]
  (obj :a)) ;; works
#_((js-obj :a 2) :a) ;; works!
)

(extend-type object
  IAssociative
  (-assoc [coll k v]
    ;; key is a string, so setting collections as key will be slow and not readable back
    ;; if daaamn needed, could be implemenetd as two objs, key-hash->val key-hash->key
    (goog.object/set coll k v)
    coll))

;; how much slower does it get with using that ^ vs plain goog.object/set
(let [obj1 (js-obj)
      obj2 (js-obj)]
  (bench-compare #(range 10000) {:plain #(goog.object/set obj1 % %)
                                 :alternative #(assoc obj2 % %)}))
;; plain is times faster

(assoc (js-obj) 1 1)


;; is it faster to store vals on js-obj under number key string or full strings?
(def ^:dynamic obj1 (js-obj))
(def ^:dynamic obj2 (js-obj))
(def ^:dynamic js-map (new js/Map))
(let [nths (doall (range 10000))]
  (let [[obj1-t _]
        (timed
         (doseq [nth nths]
           (goog.object/set obj1 "0" 1)
           (goog.object/set obj1 "1" 10)
           (goog.object/set obj1 "2" 20)
           (goog.object/get obj1 "0")
           (goog.object/get obj1 "1")
           (goog.object/get obj1 "2")
           (set! obj1 (js-obj))))

        [obj2-t _]
        (timed
         (doseq [nth nths]
           (goog.object/set obj2 ":opacity" 1)
           (goog.object/set obj2 ":x" 10)
           (goog.object/set obj2 ":y" 20)
           (goog.object/get obj2 ":opacity")
           (goog.object/get obj2 ":x")
           (goog.object/get obj2 ":y")
           (set! obj2 (js-obj))))

        [js-map-t _]
        (timed
         (doseq [nth nths]
           (.set js-map :opacity 1)
           (.set js-map :x 10)
           (.set js-map :y 20)
           (.get js-map :opacity)
           (.get js-map :x)
           (.get js-map :y)
           (set! js-map (new js/Map))))

        [js-map2-t _]
        (timed
         (doseq [nth nths]
           (.set js-map ":opacity" 1)
           (.set js-map ":x" 10)
           (.set js-map ":y" 20)
           (.get js-map ":opacity")
           (.get js-map ":x")
           (.get js-map ":y")
           (set! js-map (new js/Map))))

        [js-map3-t _]
        (timed
         (doseq [nth nths]
           (.set js-map 1 1)
           (.set js-map 2 10)
           (.set js-map 3 20)
           (.get js-map 1)
           (.get js-map 2)
           (.get js-map 3)
           (set! js-map (new js/Map))))

        opacity-key (-hash :opacity)
        x-key       (-hash :x)
        y-key       (-hash :y)
        [js-map4-t _]
        (timed
         (doseq [nth nths]
           (.set js-map opacity-key 1)
           (.set js-map x-key 10)
           (.set js-map y-key 20)
           (.get js-map opacity-key)
           (.get js-map x-key)
           (.get js-map y-key)
           (set! js-map (new js/Map))))]
    [obj1-t obj2-t js-map-t js-map2-t js-map3-t js-map4-t]))
;; suprisingly, full-names are faster on js-obj
;; how does it compare with js/Map?
;; js/Map with idx keys are by far the fastest
;; how does it compare with js/Array? can't update val at index - no use.
