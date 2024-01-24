(ns hashgraph.app.core
  {:dev/always true}
  (:require [cljs-thread.env :as env]
            [cljs-thread.core :as thread :refer [future spawn in =>> dbg break in? pmap pcalls pvalues]]))

(defn init []
  (js/console.log "CORE INIT")
  (-> (range 100)
      (->> (pmap #(js/console.log %)))
      doall)
  )
