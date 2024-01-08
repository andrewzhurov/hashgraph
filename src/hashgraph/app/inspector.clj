(ns hashgraph.app.inspector)

(def ^:private ^:dynamic *log-path* nil)
(defmacro log-relative [path & exprs]
  (assert (vector? path))
  `(binding [*log-path* (if *log-path* (into *log-path* ~path) (vec ~path))]
     ~@exprs))
