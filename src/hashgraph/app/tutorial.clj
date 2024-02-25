(ns hashgraph.app.tutorial
  (:require [hashgraph.app.inspector :as hga-inspector]
            [hashgraph.utils.core :as utils]))

(defmacro i [el el-name & more]
  `(hga-inspector/inspectable-el ~el {:->inspected? (fn [ips# val#] (and (= 1 (count ips#)) (utils/hash= (first ips#) val#)))}
                                 [:div.ref ~el-name ~@more]))
