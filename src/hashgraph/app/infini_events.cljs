(ns hashgraph.app.infini-events
  (:require [rum.core :as rum]
            [hashgraph.app.view :as hga-view]
            [hashgraph.app.state :as hga-state]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.playback :as hga-playback]
            [hashgraph.utils.core :refer-macros [l] :as utils]
            [taoensso.timbre :refer-macros [spy]]))

(def load-area-size (* (+ hga-view/hgs-padding hga-view/evt-s) 1.5))
(def load-area-buffer-height (* load-area-size 5))


#_
(def mixin
  (merge
   (rum/local 0 :*scroll)
   (rum/local 0 :*view-height)
   {:did-mount
    ;; track scroll
    (fn [{:keys [*scroll] :as state}]
      (.addEventListener (rum/dom-node state) "scroll" #(reset! *scroll (.-scrollTop (.-target %))))
      (reset! *scroll 1)
      state)

    ;; saturate with events when scrolled close to end
    :did-update
    (fn [{:keys [*scroll] :as state}]
      (let [viz-wrapper (js/document.getElementById "viz-wrapper")
            viz         (js/document.getElementById "viz")
            scroll  @*scroll]
        (when (and viz viz-wrapper)
          (let [scroll-height (.-scrollHeight viz-wrapper)
                scroll-rest   (- scroll-height (+ scroll (.-clientHeight viz-wrapper)))
                viz-height    (-> viz (.getBBox) (.-height))]
            (-> viz (.setAttribute "height" (+ viz-height (* load-area-size 2))))
            (when (< scroll-rest load-area-size)
              (hga-events/issue!)))))
      state)}))

#_#_(defonce *scroll (atom 0))
(defonce *dom-node (atom nil))



#_(def *issue-more-events?
  (rum/derived-atom [hga-events/*creator-hg-map *scroll *dom-node] ::*issue-more-events?
    (fn [creator-hg-map scroll ?dom-node]
      (js/console.log "issue more events?")
      (and ?dom-node
           (let [viz-height-val (viz-height creator-hg-map)
                 scroll-rest    (- viz-height-val (+ scroll (.-clientHeight ?dom-node)))
                 #_#_viz-height (-> viz (.getBBox) (.-height))]
             (< scroll-rest load-area-size))))))

#_(rum/derived-atom [*issue-more-events?] ::issue-to-fill
  (fn [issue-more-events?]
    (when issue-more-events?
      (js/console.log "needs more events, issuing!")
      (hga-events/issue!))))

#_
(def mixin
  (merge
   (rum/local 0 :*scroll)
   {:did-mount
    ;; track scroll
    (fn [{:keys [*scroll] :as state}]
      (.addEventListener (rum/dom-node state) "scroll" #(reset! *scroll (.-scrollTop (.-target %))))
      #_(reset! *dom-node (rum/dom-node state))
      (hga-events/issue!)
      #_(infini-issue)
      state)

    ;; saturate with events when scrolled close to end
    :did-update
    (fn [{:keys [*scroll] :as state}]
      (when-let [dom-node (rum/dom-node state)]
        (let [viz-height-val (viz-height @hga-events/*creator-hg-map)
              scroll-rest    (- viz-height-val (+ @*scroll (.-clientHeight dom-node)))
              #_#_viz-height (-> viz (.getBBox) (.-height))]
          (when (< scroll-rest load-area-size)
            (hga-events/issue!))))
      state)
    #_ (fn issue-to-fill [{:keys [*scroll] :as state}]
         (let [viz-wrapper (js/document.getElementById "viz-wrapper")
               viz         (js/document.getElementById "viz")
               scroll  @*scroll]
          (when (and viz viz-wrapper)
            (let [scroll-height  (.-scrollHeight viz-wrapper)
                  scroll-rest    (- scroll-height (+ scroll (.-clientHeight viz-wrapper)))
                  #_#_viz-height (-> viz (.getBBox) (.-height))]
              (when (< scroll-rest load-area-size)
                (let [new-creator-hg-map (hga-events/issue!)
                      highest-y
                      (-> new-creator-hg-map
                          vals
                          (->> (map hga-view/evt->y))
                          (some->> (apply max))
                          (or 0))]
                  (-> viz (.setAttribute "height" (+ highest-y (* 2 load-area-size)))))
                (issue-to-fill state)))))
        state)}))


;; buffers events on scroll, so there are always some to playback
#_
(def infini-events
  (add-watch hga-state/*viz-scroll ::infini-events
             (utils/async-sequential
              (fn [_ _ old-viz-scroll new-viz-scroll]
                (when (and @hga-state/*infini-events-enabled?
                           (> new-viz-scroll old-viz-scroll))
                  (let [->enough?
                        (fn [some-c->hg]
                          ;; maybe switch to buffering N events :left to playback
                          (let [viz-height      (evt->viz-height some-c->hg)
                                window-height   js/window.innerHeight
                                viz-height-rest (- viz-height (+ new-viz-scroll window-height))]
                            (>= viz-height-rest (- load-area-buffer-height members-height load-area-size))))]
                    (when-not (->enough? (-> @hga-events/*events hga-view/events->c->hg))
                      (hga-events/issue! ->enough?))))))))
