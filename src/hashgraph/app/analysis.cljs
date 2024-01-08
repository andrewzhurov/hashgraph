(ns hashgraph.app.analysis
  (:require [hashgraph.main-analysis :as main-analysis]
            ["force-graph" :refer [default]]
            [rum.core :as rum]
            [taoensso.timbre :refer-macros [spy info]]))

(defn make-node [node-name]
  {"id"    node-name
   "name"  node-name
   "value" 1})

(defn make-link [from to]
  {"source" from
   "target" to})

(def graph-data
  (->> main-analysis/var-usages
       (reduce
        (fn [acc {:keys [from-ns from-var to-ns to-var]}]
          (let [from (str from-ns "/" from-var)
                to (str to-ns "/" to-var)]
            (cond-> acc
              (not= from to)
              (-> (update :nodes into [(make-node from)
                                       (make-node to)])
                  (update :links conj (make-link from to))))))
        {:nodes #{} :links #{}})))

(rum/defc graph-view
  < {:did-mount (fn [state]
                  (js/console.log (rum/dom-node state))
                  (-> ((default) (rum/dom-node state))
                      (.graphData (clj->js graph-data))
                      (.linkDirectionalArrowLength 6)
                      #_(.linkDirectionalParticles 20)
                      #_(.linkDirectionalParticleSpeed (fn [d] 10))

                      #_(.dagMode "td"))
                  state)}
  [state]
  [:div#graph-view])
