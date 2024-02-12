(ns hashgraph.app.state)

(def *viz-scroll (atom 0))
(def *viz-scroll-by! (atom nil))
(def *viz-scroll! (atom nil))
(def *playback-attached-to-viz-scroll? (atom true))
(def *playback-playing? (atom true))
@*viz-scroll
