(ns hashgraph.app.utils
  (:require
   [hashgraph.app.view :as hga-view]
   [hashgraph.utils :refer [log!] :refer-macros [l letl] :as utils]))

(defn ->view-bound-min [scroll-top]
  scroll-top)

(defn ->view-bound-max [scroll-top]
  (+ scroll-top js/window.innerHeight))

(defn ->view-bounds [scroll-top]
  (let [view-bound-min (->view-bound-min scroll-top)
        view-bound-max (->view-bound-max scroll-top)]
    [view-bound-min view-bound-max]))

(defn ->above-view? [y scroll-top]
  (> y (->view-bound-max scroll-top)))

(defn ->below-view? [y scroll-top]
  (< y (->view-bound-min scroll-top)))

(defn ->above-playback-view? [y scroll-top]
  (let [playback-view-bound-max (-> (->view-bound-max scroll-top)
                                    (- hga-view/load-area-size hga-view/members-size))]
    (> y playback-view-bound-max)))
