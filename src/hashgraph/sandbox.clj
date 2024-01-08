(ns hashgraph.sandbox
  (:require
   [nextjournal.clerk :as clerk]
   #_[nextjournal.clerk.graph-visualizer :as clerk-graph-viz]
   [arrowic.core :as arrowic]
   [nextjournal.clerk.analyzer :as analyzer]
   [weavejester.dependency :as dep]
   [cljs.analyzer.api :as cljs-analyzer]
   [rewrite-clj :as rc]))

(cljs-analyzer/analyze-file "hashgraph/main.cljs" {:cache-analysis true
                                                   :output-dir "/home/user1/gits/hashgraph/"})
(cljs-analyzer/ns-interns 'hashgraph.main)
(cljs-analyzer/read-analysis-cache)
#_
(clerk/example
 (+ 1 2)
 (+ 41 1)
 (-> 42 range shuffle)
 (macroexpand '(example (+ 1 2)))
 (clerk/html [:h1 "👋"])
 (range)
 (javax.imageio.ImageIO/read (java.net.URL. "https://nextjournal.com/data/QmeyvaR3Q5XSwe14ZS6D5WBQGg1zaBaeG3SeyyuUURE2pq?filename=thermos.gif&content-type=image/gif")))

(defn dep-svg [{:keys [graph ->analysis-info]}]
  (clerk/html
   (arrowic/as-svg
    (arrowic/with-graph (arrowic/create-graph)
      (let [vars->verticies (into {} (map (juxt identity arrowic/insert-vertex!)) (keys ->analysis-info))]
        (doseq [var (keys ->analysis-info)]
          (doseq [dep (dep/immediate-dependencies graph var)]
            (when (and (vars->verticies var)
                       (vars->verticies dep))
              (arrowic/insert-edge! (vars->verticies var) (vars->verticies dep))))))))))


(defn graph-file [file]
  (-> (analyzer/analyze-file {:graph (dep/graph)} file)
      analyzer/build-graph
      #_dep-svg))

;; ^{::clerk/width :full}
;; (graph-file "src/hashgraph/main.cljs"
;;             #_"src/hashgraph/sandbox.clj"
;;             #_"src/nextjournal/clerk/classpath.clj")
