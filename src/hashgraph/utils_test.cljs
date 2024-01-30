(ns hashgraph.utils-test
  (:require [hashgraph.app.inspector :refer [*log]]
            [hashgraph.utils :refer-macros [defn* defnml timing timed assert-warn l]
             :refer [*time-start* *time* *->time* *mem* *from-mem*]
             :as utils]
            [taoensso.tufte :as tufte]
            [goog.object :as gobject]))

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
