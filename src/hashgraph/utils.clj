(ns hashgraph.utils
  (:require [hashgraph.app.inspector :as hga-inspector]))

;; def fn memoized
(defmacro defnm [fn-name & forms]
  (let [[fn-doc fn-inputs & fn-bodies] (if (string? (first forms))
                                         forms
                                         (cons "" forms))]
    (assert (string? fn-doc))
    (assert (every? symbol? fn-inputs) "defnml only supports symbol inputs atm")
    `(def ~fn-name (memoize (fn ~fn-inputs ~@fn-bodies)))))

;; def fn memoized logging
(defmacro defnml [fn-name & forms]
  (assert (symbol? fn-name))
  (let [[fn-doc fn-inputs & fn-bodies] (if (string? (first forms))
                                         forms
                                         (cons "" forms))]
    (assert (string? fn-doc))
    (assert (every? symbol? fn-inputs) "defnml only supports symbol inputs atm")
    `(def ~fn-name (memoize (fn ~fn-inputs (hga-inspector/log-relative
                                            [~(keyword fn-name) ~(apply array-map (interleave (mapv keyword fn-inputs)
                                                                                              fn-inputs))]
                                            (let [result# (do ~@fn-bodies)]
                                              (hga-inspector/log-set! [:result]
                                                                      result#))))))))
