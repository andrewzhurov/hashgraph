(ns hashgraph.app.state
  (:require [rum.core :as rum]
            [hashgraph.main :as hg]
            [hashgraph.utils.core :refer [defn*]]
            [hashgraph.utils.lazy-derived-atom :refer [lazy-derived-atom]]))

(defonce *viz-scroll (atom 0))
(defonce *viz-scroll-by! (atom nil))
(defonce *viz-scroll! (atom nil))
(defonce *playback-attached-to-viz-scroll? (atom true))
(defonce *playback-playing? (atom false))

(defonce *show-members? (atom false))
(defonce *show-rounds? (atom false))
(defonce *show-witnesses? (atom false))
(defonce *show-stake-map? (atom false))
(defonce *show-votes? (atom false))

(defonce *overide-viz-height (atom nil))

(defonce *just-played<   (atom '()))
(defonce *just-rewinded> (atom '()))

(defonce *main-tip (atom nil))
(add-watch *just-played< ::sync-*main-tip-with-*just-played<
           (fn [_ _ _ just-played<]
             (when-let [new-main-tip (->> just-played<
                                          reverse
                                          (some (fn [evt] (when (= (hg/creator evt) hg/main-creator) evt))))]
               (reset! *main-tip new-main-tip))))

(add-watch *just-rewinded> ::sync-*main-tip-with-*just-rewinded>
           (fn [_ _ _ just-rewinded>]
             (when-let [last-rewinded-main-tip (->> just-rewinded>
                                                    reverse
                                                    (some (fn [evt] (when (= (hg/creator evt) hg/main-creator) evt))))]
               (reset! *main-tip (hg/self-parent last-rewinded-main-tip)))))

(defonce *last-cr (lazy-derived-atom [*main-tip] ::derive-last-concluded-round hg/->concluded-round))
(defonce *stake-map (lazy-derived-atom [*last-cr] ::derive-stake-map hg/concluded-round->stake-map))

(defn* ^:memoizing ?received-event->event->received-event
  [?received-event]
  (if (nil? ?received-event)
    (hash-map)
    (let [prev (?received-event->event->received-event (:received-event/prev-received-event ?received-event))]
      (assoc prev (:received-event/event ?received-event) ?received-event))))

(defonce *event->received-event (lazy-derived-atom [*last-cr] ::derive-event->received-event
                                                   (fn [cr] (-> cr :concluded-round/last-received-event ?received-event->event->received-event))))
