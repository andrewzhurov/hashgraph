(ns hashgraph.app.tutorial
  (:require [hashgraph.app.inspector :as hga-inspector]
            #_[hashgraph.utils.core :refer [hash=]]))

(defmacro i [el el-name & more]
  `(hga-inspector/inspectable-el ~el {:->inspected? (fn [ips# val#] (hashgraph.utils.core/hash= ips# val#))}
                                 [:div.ref ~el-name ~@more]))
