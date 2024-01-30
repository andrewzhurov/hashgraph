(ns hashgraph.app.playback
  (:require [rum.core :as rum]
            [hashgraph.app.state :as hga-state]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.utils :refer [->below-view? ->above-view?] :as hga-utils]
            [hashgraph.utils :refer-macros [defn* l] :as utils]))

(def tt 500) ;; transition time ms
;; Playback state is kept explicitly rather than being a derived view,
;; this is to get a performance gain, sacrificing simplicity.
(defonce *playback (atom {:played '() ;; note, first element is the last played event
                          :left   '({:event/creator       "Alice"
                                     :event/creation-time 0})}))
(-> @*playback :played count)
(-> @*playback :left count)

(def sync-playback-with-scroll
  (add-watch hga-state/*viz-scroll-top ::sync-playback-with-scroll
             (utils/async-sequential
              (fn [_ _ old-scroll-top new-scroll-top]
                (let [delta (- new-scroll-top old-scroll-top)
                      play-forward? (pos? delta)]
                  (swap! *playback (fn [{:keys [played left] :as playback}]
                                     #_(js/console.log "played")
                                     (if play-forward?
                                       (let [to-play (take-while #(not (hga-utils/->above-playback-view?
                                                                        (hga-events/evt-view-position-y %)
                                                                        new-scroll-top))
                                                                 left)]
                                         {:played      (into played to-play)
                                          :left        (drop (count to-play) left)
                                          :just-played to-play})
                                       (let [to-rewind (take-while #(hga-utils/->above-playback-view?
                                                                     (hga-events/evt-view-position-y %)
                                                                     new-scroll-top)
                                                                   played)]
                                         {:played        (drop (count to-rewind) played)
                                          :left          (into left to-rewind)
                                          :just-rewinded to-rewind})))))))))

;; Ensures there are always some events left to playback by issuing new ones
(def max-buffered-size 200)
(def min-buffered-size 10)
(def buffer-playback-left-events-on-exhaust
  (add-watch *playback ::buffer-playback-left-events
             (utils/async-sequential
              (fn [_ _ _ {:keys [played left] :as playback}]
                (when (< (count left) min-buffered-size)
                  #_(js/console.log "buffering more events on exhaust")
                  (let [buffered-size (count left)
                        new-events (hga-events/issue (into played left) (fn [new-events] (>= (+ (count new-events) buffered-size) min-buffered-size)))]
                    (swap! *playback update :left concat new-events)))))))

(def issue-on-iddle-for-ms (/ 16.6 2))
(defn buffer-playback-left-events-async-on-idle! []
  (js/requestIdleCallback
   (fn []
     (when (< (count (:left @*playback)) max-buffered-size)
       #_(js/console.log "on idle buffering more events")
       (utils/timing
        (swap! *playback (fn [{:keys [played left] :as playback}]
                           (let [new-events (hga-events/issue (into played left)
                                                              (fn [new-events] (> (utils/*->time*) issue-on-iddle-for-ms))
                                                              #_(constantly true)
                                                              #_(fn [new-events] (>= (count new-events) 10)))]
                             {:played played
                              :left   (concat left new-events)})))))
     (buffer-playback-left-events-async-on-idle!))))

(buffer-playback-left-events-async-on-idle!)

#_(defn play-forward [{:keys [played left] :as playback}]
  (-> playback
      (update :played conj (first left))
      (update :left rest)))

(def *playing? (atom false))
(defn play! []
  (when @*playing?
    (when-let [scroll-by! hga-state/*scroll-by!]
      (scroll-by! (+ hga-events/evt-view-s hga-events/sp-padding)))
    (js/setTimeout play! (/ tt 2))))

(add-watch *playing? ::run-play
           (fn [_ _ old new]
             (when (and (not old)
                        new)
               (play!))))

(defonce *played-c->hg (rum/derived-atom [*playback] ::*played-c->hg
                         (fn [playback]
                           (-> playback :played hga-events/events->c->hg))))

(defn load-playback! []
  (-> (js/window.showOpenFilePicker)
      (.then (fn [[new-handle]] (-> new-handle
                                    (.getFile)
                                    (.then (fn [file] (-> file
                                                          (.text)
                                                          (.then (fn [edn-str] (let [playback (clojure.edn/read-string edn-str)]
                                                                                 (reset! *playback playback))))))))))))

(defn save-playback! []
  (-> (js/window.showSaveFilePicker (clj->js {"suggestedName" (let [date    (new js/Date)
                                                                    year    (.getFullYear date)
                                                                    month   (inc (.getMonth date))
                                                                    day     (.getDay date)
                                                                    hours   (.getHours date)
                                                                    minutes (.getMinutes date)
                                                                    seconds (.getSeconds date)]
                                                                (str "hashgraph-" year "-" month "-" day "_" hours "-" minutes "-" seconds ".edn"))}))
      (.then (fn [new-handle] (-> new-handle
                                  (.createWritable)
                                  (.then (fn [writable-stream]
                                           (-> writable-stream (.write (pr-str @*playback)))
                                           (-> writable-stream (.close)))))))))

(def playback-controls
  [{:description "Load hashgraph playback from disk"
    :short       "Open"
    :action      load-playback!}
   {:description "Save hashgraph playback to disk"
    :short       "Save to..."
    :action      save-playback!}
   {:description "Set playback position to start"
    :short       "<--"
    :action      #(swap! *playback assoc :position 0)
    :shortcut    #{:ctrl :->}}
   {:description "Rewind playback once backwards"
    :short       "<-"
    :action      #(swap! *playback update :position (fn [position] (max 0 (dec position))))
    :shortcut    #{:ctrl :shift :->}}
   {:description "Play"
    :short       "Play"
    :action      #(reset! *playing? true)}
   {:description "Pause"
    :short       "Pause"
    :action      #(reset! *playing? false)}
   {:description "Rewind playback once forwards"
    :short "->"
    :action #(swap! *playback (fn [{:keys [events position] :as playback}] (assoc playback :position (min (count events) (inc position)))))
    :shortcut #{:ctrl :->}}
   {:description "Set playback position to end"
    :short "->>"
    :action #(swap! *playback (fn [{:keys [events] :as playback}] (assoc playback :position (count events))))
    :shortcut #{:ctrl :shift :<-}}])



(rum/defc playback-controls-view < rum/reactive
  []
  [:div
   [:input {:type          :range
            :min           0
            :max           (count (:events (rum/react *playback)))
            :value         (:position (rum/react *playback))
            :on-change     #(swap! *playback assoc :position (int (-> % .-target .-value)))}]
   (for [{:keys [description short action]} playback-controls]
     [:button {:key short
               :title description
               :on-click action}
      short])])
