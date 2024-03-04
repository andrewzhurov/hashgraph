(ns hashgraph.app.playback
  (:require-macros [hashgraph.utils.js-map :as js-map])
  (:require [clojure.set :as set]
            [cognitect.transit :as transit]
            [garden.core :refer [css]]
            [garden.units :refer [px]]
            [garden.selectors :as gs]
            [rum.core :as rum]
            [hashgraph.main :as hg]
            [hashgraph.members :as hg-members]
            [hashgraph.app.view :refer [t] :as hga-view]
            [hashgraph.app.styles :refer [reg-styles!]]
            [hashgraph.app.icons :as hga-icons]
            [hashgraph.app.state :as hga-state]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.keyboard :as hga-keyboard]
            [hashgraph.app.transitions :refer [tt] :as hga-transitions]
            [hashgraph.app.utils :as hga-utils]
            [hashgraph.utils.lazy-derived-atom :refer [lazy-derived-atom]]
            [hashgraph.app.inspector]
            [hashgraph.utils.core :refer-macros [defn* l letl letp] :as utils]
            [taoensso.tufte :refer [profile p]]
            [cljs.core :as core]))


;; Playback state is kept explicitly rather than being a derived view,
;; this is to get a performance gain, sacrificing simplicity.
;;
;; Playback is split to three states:
;;
;; :behind> is used to store events that are behind the view, as there's no point in rendering them.
;;          They are kept in descending (>) order, so pulling from them to :played< on rewind is fast.
;;
;; :played< stores events that are in-view (rendered), in ascending order.
;;
;; :rewinded< stores events that been rewinded. They are capped to just few events.
;;            These events are also rendered, in order to display rewinding transition.
;;            Even though rewinded events are rendered,
;;            they do not contribute to main algorithm / they are no more present.
(defonce ^:dynamic *left< nil)

(defonce *playback (atom {:behind>   '()   ;; on play we'll read from the first when putting to played V
                          :played<   '()   ;; on play we'll read from the first when putting to behind ^
                          :rewinded< '() ;; on play we'll read from the first when putting to played ^
                          }))

(defonce *behind>   (rum/cursor *playback :behind>))
(defonce *played<   (rum/cursor *playback :played<))
(defonce *rewinded< (rum/cursor *playback :rewinded<))

;; As events viz is scrolled forward, advancing time, more events get "created".
;; As events viz is scrolled backwards, time's rewinded.

(defn sync-playback-with-viz-scroll! []
  (add-watch hga-state/*viz-scroll ::sync-playback-with-scroll
             (fn [_ _ old-viz-scroll new-viz-scroll]
               (let [delta                               (- new-viz-scroll old-viz-scroll)
                     play-forward?                       (pos? delta)
                     {:keys [behind> played< rewinded<]} @*playback]
                 (if play-forward?
                   ;; TODO switch to split-with*
                   (let [[to-behind< new-played<]          (->> played< (split-with #(hga-view/->before-viz-viewbox? (hga-view/evt->y %) new-viz-scroll)))
                         new-behind>                       (into behind> to-behind<) ;; TODO perhaps try storing in separate atom
                         [to-play-rewinded< new-rewinded<] (->> rewinded< (split-with #(not (hga-view/->after-viz-playback-viewbox? (hga-view/evt->y %) new-viz-scroll))))
                         new-just-played<                  to-play-rewinded<
                         new-played<                       (concat new-played< to-play-rewinded<)]
                     (if (not (empty? new-rewinded<))
                       (do (reset! hga-state/*just-played< new-just-played<)
                           (reset! *playback
                                   {:behind>   new-behind>
                                    :played<   new-played<
                                    :rewinded< new-rewinded<}))
                       (let [left<                     @*left<
                             [to-play-left< new-left<] (->> left< (split-with #(not (hga-view/->after-viz-playback-viewbox? (hga-view/evt->y %) new-viz-scroll))))
                             new-played<               (concat new-played< to-play-left<)
                             new-just-played<          (concat new-just-played< to-play-left<)]
                         (reset! hga-state/*just-played< new-just-played<) ;; fire first, so transitions are ready, brittle :/
                         (reset! *playback
                                 {:behind>   new-behind>
                                  :played<   new-played<
                                  :rewinded< '()})
                         (reset! *left< new-left<))))

                   (let [played>                  (reverse played<)
                         [to-play> new-behind>]   (->> behind>     (split-with #(not (hga-view/->before-viz-viewbox? (hga-view/evt->y %) new-viz-scroll))))
                         new-played>              (concat played> to-play>)
                         [to-rewind> new-played>] (->> new-played> (split-with #(hga-view/->after-viz-playback-viewbox? (hga-view/evt->y %) new-viz-scroll)))
                         new-just-rewinded>       to-rewind>
                         new-rewinded<            (into rewinded< to-rewind>)
                         new-played<              (reverse new-played>)]
                     (reset! hga-state/*just-rewinded> new-just-rewinded>)
                     (reset! *playback
                             {:behind>   new-behind>
                              :played<   new-played<
                              :rewinded< new-rewinded<})))))))

;; Events are crafted as an infinite lazy sequence,
;; elements are created as it's being read.
;; Creation may be slow, as it uses main algorithm to determine active members,
;; as only they send events to each other.
;;
;; In order to speed up creation, events-seq gets eagerly resolved when browser's idle,
;; just few at a time, to keep the browser responsive.

;; Ensures there are always some events left to playback by buffering more
;; Maybe use IdleDeadline.timeRemaining()
;; However, in my browser it's not available.. which's strange.
(def issue-on-iddle-for-ms (/ 16.6 3))
(defn resolve-more-events-on-idle! [resolved-events-nexts]
  (js/requestIdleCallback
   (fn []
     (let [new-resolved-events-nexts (if (-> (hg/creation-time (first resolved-events-nexts))
                                             (- (hg/creation-time (first @*left<)))
                                             (<= hga-view/playback-size))
                                       (do (js/console.log "on idle buffering more events")
                                           (drop 5 resolved-events-nexts))
                                       resolved-events-nexts)]
       (resolve-more-events-on-idle! new-resolved-events-nexts)))))





(defn init-playback! [events<]
  (set! *left< (atom events<))
  (add-watch *left< ::resolve-next-left< (fn [_ _ _ left<] (take 10 left<)))
  (sync-playback-with-viz-scroll!)
  (resolve-more-events-on-idle! events<))


(defn ->playback-events< [{:keys [behind> played< rewinded<]}]
  (-> behind>
      (into played<)
      (into rewinded<)
      reverse))

(defn ->all-events<! []
  (let [playback @*playback
        left<    @*left<]
    (concat (->playback-events< playback) left<)))

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
  (let [evt-pos            (hga-view/evt->y evt)
        evt-viz-scroll-pos (- evt-pos hga-view/playback-size)]
    (@hga-state/*viz-scroll! evt-viz-scroll-pos :smooth? true)))

(defn rewind-all! []
  (@hga-state/*viz-scroll! (- hga-view/window-y-span) :smooth? true))

(defn rewind-once! []
  (if-let [prev-evt (second (reverse @*played<))]
    (viz-scroll-to-event! prev-evt)
    (rewind-all!)))

(defn play-once! []
  (if-let [rewinded-evt (first (not-empty (:rewinded< @*playback)))]
    (viz-scroll-to-event! rewinded-evt)
    (if-let [left-event (first (not-empty @*left<))] ;; better ensure left's are populated
      (viz-scroll-to-event! left-event)
      nil)))

(def play-scroll-px-per-s (* (+ hga-view/hgs-padding hga-view/evt-r) 15))
(def play-scroll-px-per-ms (/ play-scroll-px-per-s 1000))
(defn play! [& [lt]]
  (let [lt (or lt (cljs.core/system-time))]
    (when @hga-state/*playback-playing?
      (let [t (cljs.core/system-time)]
        (when-let [viz-scroll-by! @hga-state/*viz-scroll-by!]
          (let [dt (- t lt)
                px (-> play-scroll-px-per-ms (* dt))]
            (viz-scroll-by! px :smooth? false)))
        (hga-utils/schedule (partial play! t))))))

(add-watch hga-state/*playback-playing? ::run-play
           (fn [_ _ old new]
             (when (and (not old)
                        new)
               (play!))))


(defn play-all! []
  (when-let [last-evt (or (last (:rewinded< @*playback))
                          (last (:played< @*playback)))]
    (let [to-viz-t (-> last-evt hga-view/evt->y)]
      (reset! hga-state/*overide-viz-height (+ to-viz-t hga-view/load-area-size hga-view/after-viz-buffer-size))
      (js/setTimeout #(viz-scroll-to-event! last-evt) 300)
      (js/setTimeout #(reset! hga-state/*overide-viz-height nil) 3000))))

(def *rewind-able?
  (rum/derived-atom [*played<] ::derive-rewind-able?
    (fn [played<]
      (not (empty? (l played<))))))

(def *play-all-able?
  (rum/derived-atom [*rewinded<] ::derive-rewind-able?
    (fn [rewinded<]
      (not (empty? (l rewinded<))))))

(def playback-controls
  [#_#_{:description "Load hashgraph playback from disk"
        :short       "Open"
        :action      load-playback!}
   {:description "Save hashgraph playback to disk"
    :short       "Save to..."
    :action      save-playback!}

   ;; |<--
   {:id          :rewind-all
    :description "Set playback position to start"
    :short       (hga-icons/icon :solid :backward-fast)
    :*able?      *rewind-able?
    :action      rewind-all!
    :shortcut    #{:ctrl :shift :<-}}
   ;; |<-
   {:id          :rewind-once
    :icon        (hga-icons/icon :solid :backward-step)
    :description "Rewind playback once backwards"
    :short       "<-"
    :*able?      *rewind-able?
    :action      (utils/silence tt rewind-once!)
    :shortcut    #{:ctrl :<-}}
   ;; |> ||
   {:id          :play-pause
    :action      #(swap! hga-state/*playback-playing? not)
    :shortcut    #{:space}
    :*state      hga-state/*playback-playing?
    :on-state    {true {:short "Pause"
                        :icon  (hga-icons/icon :solid :pause)}
                  false {:short "Play"
                         :icon (hga-icons/icon :solid :play)}}}
   ;; ->|
   {:id          :play-once
    :description "Play next event"
    :short       (hga-icons/icon :solid :forward-step)
    :action      (utils/silence tt play-once!)
    :shortcut    #{:ctrl :->}}
   ;; ->>|
   {:id          :play-all
    :description "Set playback position to end"
    :short       (hga-icons/icon :solid :forward-fast)
    :*able?      *play-all-able?
    :action      play-all!
    :shortcut    #{:ctrl :shift :->}}])

(def playback-controls-styles
  [[:.playback-controls {:position        :fixed
                         :bottom          (px hga-view/controls-y-start)
                         :left            "50%"
                         :right           "0px"
                         :transform       "translateX(-50%)"
                         :z-index         100
                         :width           :fit-content
                         :display         :flex
                         :justify-content :center}
    [:.playback-control
     {:width           "56px"
      :height          "56px"
      :display         :inline-flex
      :justify-content :center
      :align-items     :center}
     [:svg {:transition (t :color 400)}]
     [:&.able {:cursor :pointer}]
     [(gs/& (gs/not :.able)) {:pointer-events :none}
      [:svg {:color "lightgray !important"}]]]]])

(reg-styles! ::playback-controls playback-controls-styles)


(rum/defc playback-controls-view < rum/reactive
  {:will-mount (fn [state]
                 (doseq [{:keys [shortcut action]} playback-controls]
                     (hga-keyboard/reg-shortcut! shortcut action))
                 state)}
  []
  [:div.playback-controls
   (for [{:keys [*state on-state *able?] :as playback-control} playback-controls]
     (let [{:keys [id icon description short action]}
           (if-not *state
             playback-control
             (merge playback-control
                    (get on-state (rum/react *state))))]
       [:div.playback-control {:class    [(name id)
                                          (when (or (nil? *able?)
                                                    (rum/react *able?))
                                            "able")]
                               :key      (name id)
                               :title    description
                               :on-click action}
        (or icon short)]))])
