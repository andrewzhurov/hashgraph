(ns hooks.hashgraph
  (:require [clj-kondo.hooks-api :as api]))

(defn rewrite [node]
  (let [args (rest (:children node))
        new-node (with-meta
                   (api/list-node
                    (list* (api/token-node 'defn)
                           args))
                   (meta node))]
    (spit "/home/user1/log/print.txt" (pr-str new-node))
    new-node))

(defn defnm [{:keys [:node]}]
  (let [new-node (rewrite node)]
    {:node new-node}))
