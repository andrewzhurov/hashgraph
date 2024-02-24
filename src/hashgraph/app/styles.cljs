(ns hashgraph.app.styles
  (:require [rum.core :as rum]
            [garden.core :refer [css]]
            [hashgraph.utils.core :refer-macros [l]]))

(def *id->kind->styles (atom {}))
(defn reg-styles! [id bare & [horizontal vertical]]
  (swap! *id->kind->styles assoc id (cond-> {:bare bare}
                                      horizontal (assoc :horizontal horizontal)
                                      vertical   (assoc :vertical vertical))))

(defn kind->css [kind] (-> (rum/react *id->kind->styles)
                           vals
                           (->> (map #(get % kind))
                                (reduce into))
                           css))
