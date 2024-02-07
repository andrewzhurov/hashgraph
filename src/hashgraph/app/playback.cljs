(ns hashgraph.app.playback
  (:require [clojure.set :as set]
            [cognitect.transit :as transit]
            [rum.core :as rum]
            [hashgraph.main :as hg]
            [hashgraph.members :as hg-members]
            [hashgraph.app.view :as hga-view]
            [hashgraph.app.state :as hga-state]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.transitions :as hga-transitions]
            [hashgraph.app.utils :as hga-utils]
            [hashgraph.app.inspector]
            [hashgraph.utils.core :refer-macros [defn* l letl letp] :as utils]
            [taoensso.tufte :refer [profile p]]))


;; Playback state is kept explicitly rather than being a derived view,
;; this is to get a performance gain, sacrificing simplicity.
(defonce *left< (atom (list {:event/creator       hg/main-creator
                             :event/creation-time 1})))

;; Playback is split to four states.
;;
;; *left< is used to buffer more events ahead-of-time.
;; They are issued and memoized on idle and on exhaus.
;; On idle issuing allows to buffer events and memoize main algorithm for them for faster playback.
;;
;; :behind> is used to store events that are behind the view, as there's no point in rendering them.
;; They are kept in descending (>) order, so pulling from them to :played< on rewind is fast.
;;
;; :played< stores events that are in-view (rendered), in ascending order.
;;
;; :rewinded< stores events that been rewinded. They are capped to just few events.
;; These events are also rendered, in order to display rewinding transition.
;; Even though rewinded events are rendered, they do not contribute to main algorithm / they are no more present.
(defonce *playback (atom {:behind>   '()   ;; on play we'll read from the first when putting to played V
                          :played<   '()   ;; on play we'll read from the first when putting to behind ^
                          :rewinded< '() ;; on play we'll read from the first when putting to played ^
                          }))
#_(log! :playback @*playback)

(defonce *played<   (rum/cursor *playback :played<))
(defonce *rewinded> (rum/cursor *playback :rewinded>))


(def *frame (atom 0))
(def *tracking? (atom false))
(defn track-frame! []
  (swap! *frame inc)
  (js/requestAnimationFrame track-frame!))
(when-not @*tracking?
  (reset! *tracking? true)
  (track-frame!))

;; As events viz is scrolled forward, more events get "created".
;; As events viz is scrolled backwards, time's rewinded.

(def sync-playback-with-viz-scroll
  (add-watch hga-state/*viz-scroll ::sync-playback-with-scroll
             (fn [_ _ old-viz-scroll new-viz-scroll]
               (let [delta                               (- new-viz-scroll old-viz-scroll)
                     play-forward?                       (pos? delta)
                     {:keys [behind> played< rewinded<]} @*playback]
                 (if play-forward?
                   (do
                     #_(js/console.log "play")
                     ;; TODO switch to split-with*
                     (let [[to-behind< new-played<]          (->> played< (split-with #(hga-view/->before-viz-viewbox? (hga-view/evt->y %) new-viz-scroll)))
                           new-behind>                       (into behind> to-behind<) ;; TODO perhaps try storing in separate atom
                           [to-play-rewinded< new-rewinded<] (->> rewinded< (split-with #(not (hga-view/->after-viz-playback-viewbox? (hga-view/evt->y %) new-viz-scroll))))
                           new-just-played<                  to-play-rewinded<
                           new-played<                       (concat new-played< to-play-rewinded<)]
                       (if (not (empty? new-rewinded<))
                         (do (reset! hga-transitions/*just-played< new-just-played<)
                             (reset! *playback
                                     {:behind>   new-behind>
                                      :played<   new-played<
                                      :rewinded< new-rewinded<}))
                         (let [left<                     @*left<
                               [to-play-left< new-left<] (->> left< (split-with #(not (hga-view/->after-viz-playback-viewbox? (hga-view/evt->y %) new-viz-scroll))))
                               new-played<               (concat new-played< to-play-left<)
                               new-just-played<          (concat new-just-played< to-play-left<)]
                           (reset! hga-transitions/*just-played< new-just-played<) ;; fire first, so transitions are ready, brittle :/
                           (reset! *playback
                                   {:behind>   new-behind>
                                    :played<   new-played<
                                    :rewinded< '()})
                           (reset! *left< new-left<)))))

                   (do #_(js/console.log "rewind")
                       (let [played>                  (reverse played<)
                             [to-play> new-behind>]   (->> behind>     (split-with #(not (hga-view/->before-viz-viewbox? (hga-view/evt->y %) new-viz-scroll))))
                             new-played>              (concat played> to-play>)
                             [to-rewind> new-played>] (->> new-played> (split-with #(hga-view/->after-viz-playback-viewbox? (hga-view/evt->y %) new-viz-scroll)))
                             new-just-rewinded>       to-rewind>
                             [new-rewinded< to-left<] (->> rewinded< (split-with hga-transitions/evt->in-transition?))
                             new-rewinded<            (into new-rewinded< to-rewind>)
                             new-played<              (reverse new-played>)
                             ]
                         (reset! hga-transitions/*just-rewinded> new-just-rewinded>)
                         (reset! *playback
                                 {:behind>   new-behind>
                                  :played<   new-played<
                                  :rewinded< new-rewinded<})
                         (when (not-empty to-left<)
                           (let [new-left< (concat to-left< @*left<)]
                             (reset! *left< new-left<))))))))))

;; Ensures there are always some events left to playback by issuing new ones
(def max-buffered-size 200)
(def min-buffered-size 10)

(defn ->playback-events< [{:keys [behind> played< rewinded<]}]
  (-> behind>
      (into played<)
      (into rewinded<)
      reverse))

(defn ->all-events<! []
  (let [playback @*playback
        left<    @*left<]
    (concat (->playback-events< playback) left<)))

(def buffer-playback-left-events-on-exhaust
  (add-watch *left< ::buffer-playback-left-events-on-exhaust
             (fn [_ _ _ left<]
               (when (< (count left<) min-buffered-size)
                 (let [buffered-size    (count left<)
                       playback-events< (->playback-events< @*playback)
                       new-left<        (hga-events/issue playback-events< left< (fn [new-events<] (>= (+ (count new-events<) buffered-size) min-buffered-size)))]
                   (reset! *left< new-left<))))))

(def issue-on-iddle-for-ms (/ 16.6 3))
(defn buffer-playback-left-events-async-on-idle! []
  (js/requestIdleCallback
   (fn []
     (utils/timing
      (let [left<                               @*left<]
        (when (< (count left<) max-buffered-size)
          #_(js/console.log "on idle buffering more events")
          (let [playback-events< (->playback-events< @*playback)
                new-left< (hga-events/issue playback-events< left<
                                            (fn [new-events<] (> (utils/*->time*) issue-on-iddle-for-ms))
                                            #_(constantly true) ;; issue one at a time
                                            #_(fn [new-events] (>= (count new-events) 10)))]
            (reset! *left< new-left<)))))
     (buffer-playback-left-events-async-on-idle!))))

(buffer-playback-left-events-async-on-idle!)

#_(defn play-forward [{:keys [played left] :as playback}]
  (-> playback
      (update :played conj (first left))
      (update :left rest)))


(defn pack [events<]
  (let [writer (transit/writer :json)]
    (transit/write writer events<)))

(defn pack! []
  ;; Playback is screen-resolution dependent, so it's state is not reusable across devices.
  ;; We export playback as events.
  ;; And on import they can be played on a different screen-resolution.
  ;; We could additionally remember the last played event and re-create playback state on different device,
  ;; but that's some hustle and the result is not needed atm.
  ;; To note: events that are in left< are not needed, as user doesn't even know they're there.
  ;;
  ;; Also events have a TON of denormalized, simply to pr-str them will blow up the page.
  ;; We could pack them by selecting tips, and on import unpack from tips.
  ;; But there still will be plenty of denormalization, and it blows up the browser on even ~100 events.
  ;; Alternative is to use transit that does normalization. Let's stick with it.
  ;; UPD: transit hangs.
  (let [playback-events< (->playback-events< @*playback)]
    (pack playback-events<)))

(defn unpack [packed]
  (let [reader (transit/reader :json)]
    (transit/read reader packed)))


#_
(let [all-events< (->all-events<!)]
  (log! :before all-events<)
  (-> all-events<
      pack
      (->> (log! :packed))
      unpack
      (->> (log! :unpacked))
      (= all-events<))
  )

(let [m1 {:a :a}
      m2 {:a m1}
      m3 {:a m1}]
  (identical? (:a m2) (:a m3)))

#_
(let [playback-events< (->playback-events< @*playback)]
  (log! :before playback-events<)
  (-> playback-events<
      pack
      (->> (log! :packed))
      unpack
      (->> (log! :unpacked))
      (= playback-events<))
     )

(defn save-playback! []
  (-> (js/window.showSaveFilePicker (clj->js {"suggestedName" (let [date    (new js/Date)
                                                                    year    (.getFullYear date)
                                                                    month   (inc (.getMonth date))
                                                                    day     (.getDay date)
                                                                    hours   (.getHours date)
                                                                    minutes (.getMinutes date)
                                                                    seconds (.getSeconds date)]
                                                                (str "hashgraph-" year "-" month "-" day "_" hours "-" minutes "-" seconds ".json"))}))
      (.then (fn [new-handle] (-> new-handle
                                  (.createWritable)
                                  (.then (fn [writable-stream]
                                           (-> writable-stream (.write (pack!)))
                                           (-> writable-stream (.close)))))))))

(defn load-playback! []
  (-> (js/window.showOpenFilePicker)
      (.then (fn [[new-handle]] (-> new-handle
                                    (.getFile)
                                    (.then (fn [file] (-> file
                                                          (.text)
                                                          (.then (fn [packed]
                                                                   (let [playback-events< (unpack packed)]
                                                                     (reset! *left< playback-events<))))))))))))


(defn viz-scroll-to-event! [evt]
  (let [evt-pos        (hga-view/evt->y evt)
        evt-viz-scroll-pos (- evt-pos hga-view/playback-size)]
    (@hga-state/*viz-scroll! evt-viz-scroll-pos :smooth? true)))

(defn play-backwards! []
  (if-let [prev-evt (second (reverse @*played<))]
    (viz-scroll-to-event! prev-evt)
    (@hga-state/*viz-scroll! 0 :smooth? true)))

(defn play-forwards! []
  (if-let [rewinded-evt (first (not-empty (:rewinded< @*playback)))]
    (viz-scroll-to-event! rewinded-evt)
    (if-let [left-event (first (not-empty @*left<))] ;; better ensure left's are populated
      (viz-scroll-to-event! left-event)
      nil)))


(def *playing? (atom false))
(defn play! []
  (when @*playing?
    (when-let [viz-scroll-by! @hga-state/*viz-scroll-by!]
      (viz-scroll-by! (* hga-view/evt-offset)))
    (js/setTimeout play! (/ hga-transitions/tt 5))))

(add-watch *playing? ::run-play
           (fn [_ _ old new]
             (when (and (not old)
                        new)
               (play!))))

(defn play-to-end! []
  (let [last-evt? (or (last (:rewinded< @*playback))
                      (last (:played< @*playback)))]
    (when last-evt?
      (viz-scroll-to-event! last-evt?))))

#_
(defn play! []
  (js/requestAnimationFrame
   (fn []
     (js/setTimeout (fn [] (play-forwards!) (play!))))))

(def playback-controls
  [{:description "Load hashgraph playback from disk"
    :short       "Open"
    :action      load-playback!}
   {:description "Save hashgraph playback to disk"
    :short       "Save to..."
    :action      save-playback!}
   {:description "Set playback position to start"
    :short       "<--"
    :action      #(@hga-state/*viz-scroll! 0)
    :shortcut    #{:ctrl :->}}
   {:description "Rewind playback once backwards"
    :short       "<-"
    :action      play-backwards!
    :shortcut    #{:ctrl :shift :->}}
   {:description "Play"
    :short       "Play"
    :action      #(reset! *playing? true)}
   {:description "Pause"
    :short       "Pause"
    :action      #(reset! *playing? false)}
   {:description "Play next event"
    :short       "->"
    :action      play-forwards!
    :shortcut    #{:ctrl :->}}
   {:description "Set playback position to end"
    :short "->>"
    ;; :action      play-to-end!
    :shortcut #{:ctrl :shift :<-}}])



(rum/defc playback-controls-view < rum/reactive
  []
  [:div
   (for [{:keys [description short action]} playback-controls]
     [:button {:key short
               :title description
               :on-click action}
      short])])
