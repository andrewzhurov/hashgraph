(ns hashgraph.clerk
  (:require
   [nextjournal.clerk :as clerk]))

(clerk/clear-cache!)
(clerk/serve! {:browse true :watch-paths ["src"] :port 6677})
#_(clerk/show! "src/hashgraph/sandbox.clj")
