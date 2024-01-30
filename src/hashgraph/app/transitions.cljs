(ns hashgraph.app.transitions
  (:require [rum.core :as rum]
            [hashgraph.app.timing :as hga-timing]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.playback :refer [tt] :as hga-playback]
            [hashgraph.utils :refer [safe-assoc! safe-assoc-in!
                                     safe-update! safe-update-in!
                                     log!]
             :refer-macros [l letl]
             :as utils]
            [hashgraph.main :as hg]
            [goog.object]
            [goog.object :as gobject]
            [cljs.core :as core]))

(defonce event-hash->current-view-state (js-obj))
(defonce event-hash->prop->t (js-obj)) ;; property -> transition
(defn reset-view-states! []
  (set! event-hash->current-view-state (js-obj))
  (set! event-hash->prop->t (js-obj)))
#_(l event-hash->current-view-state)
#_(l event-hash->desired-view-state)

(def debug? true)
(if debug?
  (do (def t-time-start-key "t-time-start")
      (def t-val-from-key "t-from")
      (def t-val-to-key "t-to")

      (def opacity-key "opacity")
      (def x-key "x")
      (def y-key "y"))
  (do (def t-time-start-key "0")
      (def t-val-from-key "1")
      (def t-val-to-key "2")

      (def opacity-key "0")
      (def x-key "1")
      (def y-key "2")))
;; TODO check if there's performance gain from having enum keys

(defn ->left-desired? []
  (not (goog.object/isEmpty event-hash->prop->t)))

(defn ->current [event-hash]
  (or (goog.object/get event-hash->current-view-state event-hash)
      (let [current (js-obj)]
        (goog.object/set event-hash->current-view-state event-hash current)
        current)))

(defn ->prop->t [event-hash]
  (or (goog.object/get event-hash->prop->t event-hash)
      (let [desired (js-obj)]
        (goog.object/set event-hash->prop->t event-hash desired)
        desired)))

(defn make-t [val-from val-to time-start]
  (js-obj t-val-from-key val-from
          t-val-to-key val-to
          t-time-start-key time-start))

(rum/derived-atom [hga-playback/*playback] ::transitions-on-playback
  (fn [{:keys [just-played just-rewinded] :as playback}]
    (let [tt-start (cljs.core/system-time)]
      (when just-played
        (doseq [evt just-played]
          (let [evt-hash    (-> evt (-hash))
                ?op         (-> evt :event/other-parent)
                ?op-current (some-> ?op (-hash) ->current)
                ?from-x     (some-> ?op-current (goog.object/get x-key))
                ?from-y     (some-> ?op-current (goog.object/get y-key))
                to-x        (-> evt hga-events/evt-view-position-x)
                to-y        (-> evt hga-events/evt-view-position-y)
                current     (-> evt-hash ->current)
                prop->t     (-> evt-hash ->prop->t)]
            (if ?from-x
              (do (goog.object/set current x-key ?from-x)
                  (goog.object/set prop->t x-key (make-t ?op-current to-x tt-start)))
              (goog.object/set current x-key to-x))
            (if ?from-y
              (do (goog.object/set current y-key ?from-y)
                  (goog.object/set prop->t y-key (make-t ?op-current to-y tt-start)))
              (goog.object/set current y-key to-y))

            (goog.object/set current opacity-key 0)
            (goog.object/set prop->t opacity-key (make-t (js-obj opacity-key 0) 1 tt-start)))))

      )))

(def *stats (atom {:from  (cljs.core/system-time)
                   :times 0}))
(let [{:keys [from to times]} @*stats]
  (/ times (/ (- to from) 1000)))
#_(def *last-tick-time-end (atom (cljs.core/system-time)))
(defn current->desired-run! []
  #_(log! :current->desired-run! event-hash->prop->t)
  (when (->left-desired?)
    #_(swap! *stats (fn [stats] (-> stats
                                  (update :times inc)
                                  (assoc :to (cljs.core/system-time)))))
    (let [t-time-now (cljs.core/system-time)]
      (goog.object/forEach
       event-hash->prop->t
       (fn [prop->t event-hash _obj]
         (let [current (->current event-hash)]
           (goog.object/forEach
            prop->t
            (fn [t prop _obj]
              (let [t-time-start (goog.object/get t t-time-start-key)
                    t-time-delta (- t-time-now t-time-start)
                    t-time-pos   (min 1 (/ t-time-delta tt))
                    t-mod        (-> t-time-pos hga-timing/ease-in-out-cubic)

                    t-val-from    (-> t
                                      (goog.object/get t-val-from-key)
                                      (goog.object/get prop))
                    t-val-to      (goog.object/get t t-val-to-key)
                    t-val-delta   (- t-val-to t-val-from)
                    t-val-current (+ t-val-from (* t-val-delta t-mod))]
                (goog.object/set current prop t-val-current)

                (when (= 1 t-time-pos)
                  (goog.object/remove prop->t prop)))))
           (when (goog.object/isEmpty prop->t)
             (goog.object/remove event-hash->prop->t event-hash))))))))

#_
(def sync-transition
  (add-watch hga-playback/*playback ::sync-transition
             (fn [_ _ {old-position :position :as old-playback} {new-position :position :as new-playback}]
               (let [position-delta (- new-position old-position)
                     backwards? (neg? position-delta)]
                 (if backwards?
                   (if (zero? new-position)
                     (reset-view-states!)
                     (let [events-rewinded (-> old-playback
                                               playback->events-played
                                               (->> (take (* -1 position-delta))))]
                       (doseq [event-rewinded events-rewinded]
                         (dissoc! event-hash->coords (-hash event-rewinded))
                         (dissoc! event-hash->transition (-hash event-rewinded)))))
                   )))))

#_
(def *rendered-transitioning-evt-infos
  (rum/derived-atom [*rendered-evt-infos *last-tick-time-end] ::*rendered-animated-evts
    (fn [rendered-evt-infos _last-tick-time-end]
      (let [t (cljs.core/system-time)
            ;; dt (- t last-tick-time-end)
            result
            (->> rendered-evt-infos
                 (map (fn [{:event-info/keys [event coords] :as evt-info}]
                        (let [transition-time-start (or (get hga-playback/id->transition-time-start (-hash event))
                                                        (do (assoc! hga-playback/id->transition-time-start (-hash event) t)
                                                            t))
                              dt                    (max 1 (- t transition-time-start))]
                          (if (< dt hga-playback/tt)
                            (let [time-in     (/ dt hga-playback/tt)
                                  timing-fn   easeInOutCubic #_(fn [t-in] (/ t-in (- 1 (/ t-in 5))))
                                  trans-in    (timing-fn time-in)
                                  op          (-> event hg/other-parent)
                                  from-coords (-> op evt-view-position)
                                  coords-now  (mapv (fn [from to] (-> from
                                                                      (+ (* (- to from) trans-in))))
                                                  from-coords coords)]
                              (assoc evt-info
                                     :event-info/coords  coords-now
                                     :event-info/opacity trans-in))
                            evt-info)))))]
        (js/setTimeout #(reset! *last-tick-time-end t) 1) ;; trigger next tick
        result))))



#_
(def *sync-playback-with-scroll
  (rum/derived-atom [hga-state/*playback-attached-to-scroll? hga-state/*viz-scroll-top *playback] ::*sync-playback-with-scroll
                    (fn [playback-attached-to-scroll? viz-scroll-top {:keys [events] :as playback}]
                      (when playback-attached-to-scroll?
                        (let [evt-based-on-scroll (-> events
                                                      (->> (drop-while #(->above-view? % viz-scroll-top)))
                                                      first)
                              position-based-on-scroll
                              (- (count events) (-indexOf events evt-based-on-scroll))]
                          (swap! *playback assoc :position position-based-on-scroll))))))
