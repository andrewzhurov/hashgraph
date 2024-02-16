(ns hashgraph.app.state)

(defonce *viz-scroll (atom 0))
(defonce *viz-scroll-by! (atom nil))
(defonce *viz-scroll! (atom nil))
(defonce *playback-attached-to-viz-scroll? (atom true))
(defonce *playback-playing? (atom false))

(defonce *just-played<   (atom '()))
(defonce *just-rewinded> (atom '()))
(defonce *last-concluded-round (atom nil))

(defonce *show-rounds? (atom false))
(defonce *show-witnesses? (atom false))
(defonce *show-stake-map? (atom false))
(defonce *show-votes? (atom false))
