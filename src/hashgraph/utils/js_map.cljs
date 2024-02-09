(ns hashgraph.utils.js-map
  (:require-macros [hashgraph.utils.js-map :as js-map]))

(defn js-map->clj-map* [?js-map ->keyword-key]
  (-> ?js-map
      (.entries)
      (js/Array.from)
      cljs.core/js->clj
      (->> (map (fn [[k v]] [(or (->keyword-key k) k) (if-not (js-map/js-map? v)
                                                        v
                                                        (js-map->clj-map* v ->keyword-key))]))
           (apply concat)
           (apply array-map))))
