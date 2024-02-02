(ns hashgraph.app.state)

(def *viz-scroll-top (atom 0))
(def *scroll-by! (atom nil))
(def *scroll! (atom nil))
(def *playback-attached-to-scroll? (atom true))
@*viz-scroll-top
