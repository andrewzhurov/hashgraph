(ns hashgraph.app.infini-events
  (:require [rum.core :as rum]
            [hashgraph.app.events :as hga-events]
            [taoensso.timbre :refer-macros [spy]]))

(def load-area-height 200)

(def mixin
  (merge
   (rum/local 0 :*scroll-top)
   {:did-mount
    ;; track scroll
    (fn [{:keys [*scroll-top] :as state}]
      (.addEventListener (rum/dom-node state) "scroll" #(reset! *scroll-top (.-scrollTop (.-target %))))
      (reset! *scroll-top 1)
      state)

    ;; saturate with events when scrolled close to end
    :did-update
    (fn [{:keys [*scroll-top] :as state}]
      (let [viz-wrapper (js/document.getElementById "viz-wrapper")
            viz         (js/document.getElementById "viz")
            scroll-top  @*scroll-top]
        (when (and viz viz-wrapper)
          (let [scroll-height (.-scrollHeight viz-wrapper)
                scroll-rest   (- scroll-height (+ scroll-top (.-clientHeight viz-wrapper)))
                viz-height    (-> viz (.getBBox) (.-height))]
            (-> viz (.setAttribute "height" (+ viz-height (* load-area-height 2))))
            (when (< scroll-rest load-area-height)
              (hga-events/issue!)))))
      state)}))
