(ns hashgraph.app.state
  (:require [rum.core :as rum]
            [hashgraph.main :as hg]
            [hashgraph.members :as hg-members]
            [hashgraph.utils.core :refer [map-vals] :refer-macros [defn* l]]
            [hashgraph.utils.lazy-derived-atom :refer [lazy-derived-atom] :refer-macros [deflda]]

            [hashgraph.app.avatars :as hga-avatars]
            [hashgraph.app.view :as hga-view]))

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

(defonce *override-viz-height (atom nil))

(defonce *just-played<   (atom '()))
(defonce *just-rewinded> (atom '()))

(defonce *main-tip (atom nil))
#_
(add-watch *just-played< ::sync-*main-tip-with-*just-played<
           (fn [_ _ _ just-played<]
             (when-let [new-main-tip (->> just-played<
                                          reverse
                                          (some (fn [evt] (when (= (hg/creator evt) hg/main-creator) evt))))]
               (reset! *main-tip new-main-tip))))

#_
(add-watch *just-rewinded> ::sync-*main-tip-with-*just-rewinded>
           (fn [_ _ _ just-rewinded>]
             (when-let [last-rewinded-main-tip (->> just-rewinded>
                                                    reverse
                                                    (some (fn [evt] (when (= (hg/creator evt) hg/main-creator) evt))))]
               (reset! *main-tip (hg/self-parent last-rewinded-main-tip)))))

(deflda *last-cr [*main-tip] (fn [main-tip] (some-> main-tip hg/->concluded-round)))
(deflda *stake-map [*last-cr] (fn [last-cr] (some-> last-cr hg/concluded-round->stake-map)))

(defn* ^:memoizing ?received-event->event->received-event
  [?received-event]
  (if (nil? ?received-event)
    (hash-map)
    (let [prev (-> ?received-event :received-event/prev-received-event ?received-event->event->received-event)]
      (assoc prev (:received-event/event ?received-event) ?received-event))))

(deflda *event->received-event [*last-cr] (fn [cr] (-> cr :concluded-round/last-received-event ?received-event->event->received-event)))

(defonce ^:dynamic *aids (atom #{}))

(defn* ^:memoizing aid->creation-time [aid]
  (-> aid l :key-event/anchors first :aid/creation-time l))

(deflda *aid->creation-time [*aids]
  (fn [aids]
    (->> aids
         (map (fn [aid]
                [aid (-> aid l aid->creation-time l)]))
         (into (hash-map)))))

 ;; aid-> [1; 10]
(deflda *aid->seed [*aid->creation-time]
  (fn [aid->creation-time]
    (->> aid->creation-time
         (map-vals (fn [creation-time]
                     (inc (int (last (str creation-time)))))))))

(deflda *aid->avatar [*aid->seed]
  (fn [aid->seed]
    (l [:derive-*aid->avatar aid->seed])
    (l (->> aid->seed
            (map-vals (fn [seed]
                        (case (rem seed 2)
                          0 hga-avatars/male-avatar
                          1 hga-avatars/female-avatar)))))))

(deflda *aid->color [*aid->seed]
  (fn [aid->seed]
    (->> aid->seed
         (map-vals (fn [seed] (get hg-members/palette1 seed))))))
