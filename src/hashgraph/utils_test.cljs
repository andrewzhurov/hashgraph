(ns hashgraph.utils-test
  (:require [hashgraph.app.inspector :refer [*log]]
            [hashgraph.utils :refer-macros [defn* defnml timing timed assert-warn l]
             :refer [*time-start* *time* *->time* *mem* *from-mem*]
             :as utils]
            [taoensso.tufte :as tufte]))

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

#_
(cljs.pprint/pprint
 (macroexpand
  '(defn* ^:timing ^:memoizing timing-test-fn []
     [*->time* (*->time*)])))

#_(timing-test-fn)


(defn test []
  (let [timed-result (timed (+ 1 1))]
    (assert-warn (vector? timed-result))
    (let [[took result] timed-result]
      (assert-warn (number? took))
      (assert-warn (= 2 result))))

  #_#_#_#_
  (defn* ^:timing ^:memoizing ^{:tracing {:enabled? false}} timing-test-fn []
    [*->time* (*->time*)])
  (let [[->time time] (timing-test-fn)]
    (assert-warn (fn? ->time))
    (assert-warn (number? time)))


  (defn* ^:memoizing ^{:tracing {:enabled? false}} memoizing-test-fn []
    (reduce + (range 1000000)))

  (let [time1 (timing (do (memoizing-test-fn) (*->time*)))
        time2 (timing (do (memoizing-test-fn) (*->time*)))]
    (assert-warn (> time1 10)   (str "memoized fn's first run must be long, but actually took: " time1 "ms"))
    (assert-warn (< time2 0.3)  (str "memoized fn's second run must be instanteneous, but actually took: " time2 "ms"))
    [time1 time2]
    )

  ;; (def *profile-info (atom #{}))
  ;; (tufte/add-handler! :profiling-test-fn "*"
  ;;                     (fn [m]
  ;;                       (swap! *profile-info conj m)))
  ;; (defn* ^:profiling profiling-test-fn []
  ;;   (reduce + (range 100000)))
  ;; (assert-warn (empty? @*profile-info))
  ;; (profiling-test-fn)
  ;; (assert-warn (= 1 (count @*profile-info)))
  ;; (profiling-test-fn)
  ;; (= 2 (count @*profile-info))
  ;; (tufte/remove-handler! :profiling-test-fn)


  (defn* ^:tracing tracing-test-fn-sum [arg1 arg2] (+ arg1 arg2))
  (defn* ^:tracing tracing-test-fn-multiply-sums [arg1 arg2] (* (tracing-test-fn-sum arg1 arg2)
                                                                (tracing-test-fn-sum arg1 arg2)))
  (binding [*log (atom {})]
    (assert-warn (empty? (:traces @*log)))
    (tracing-test-fn-multiply-sums 1 1)
    (assert-warn (= (:traces @*log)
                    [[['(tracing-test-fn-multiply-sums 1 1)] {}] ;; TODO have sym with ns
                     [['(tracing-test-fn-multiply-sums 1 1) '(tracing-test-fn-sum 1 1)] {}]
                     [['(tracing-test-fn-multiply-sums 1 1) '(tracing-test-fn-sum 1 1)] {}]])
                 (str "found instead " (with-out-str (cljs.pprint/pprint (:traces @*log))))))

  ;; (defn* ^{:tracing :time} tracing-time-test-fn-sum [arg1 arg2] (+ arg1 arg2))
  ;; (defn* ^{:tracing :time} tracing-time-test-fn-multiply-sums [arg1 arg2] (repeatedly 100 #(tracing-time-test-fn-multiply-sums arg1 arg2)))
  ;; (binding [*log (atom {})]
  ;;   (assert-warn (empty? (:traces @*log)))
  ;;   (tracing-test-fn-multiply-sums 1 1)
  ;;   (let [[trace1 trace2 & _trace-rest]] (:traces @*log)
  ;;        (let [[trace1-path trace1-result]]
  ;;          (assert-warn (= 'tracing-time-test-fn-multiply-sums )))
  ;;        (assert-warn (= (take 2 (:traces @*log))
  ;;                        [[['(tracing-test-fn-multiply-sums 1 1)] {}] ;; TODO have sym with ns
  ;;                         [['(tracing-test-fn-multiply-sums 1 1) '(tracing-test-fn-sum 1 1)] {}]
  ;;                         [['(tracing-test-fn-multiply-sums 1 1) '(tracing-test-fn-sum 1 1)] {}]])
  ;;                     (str "found instead " (with-out-str (cljs.pprint/pprint (:traces @*log)))))))
  )
