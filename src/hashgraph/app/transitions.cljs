(ns hashgraph.app.transitions
  (:require [rum.core :as rum]
            [hashgraph.app.timing :as hga-timing]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.playback :refer [tt] :as hga-playback]
            [hashgraph.app.inspector :refer [log!]]
            [hashgraph.utils :refer [safe-assoc! safe-assoc-in!
                                     safe-update! safe-update-in!]
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

      (def prop-val-key "prop-val")

      (def opacity-key "opacity")
      (def x-key "x")
      (def y-key "y"))
  (do (def t-time-start-key "0")
      (def t-val-from-key "1")
      (def t-val-to-key "2")

      (def prop-val-key "0")

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
      (let [prop->t (js-obj)]
        (goog.object/set event-hash->prop->t event-hash prop->t)
        prop->t)))

(defn make-t [val-from val-to time-start]
  (js-obj t-val-from-key val-from
          t-val-to-key val-to
          t-time-start-key time-start))

(add-watch hga-playback/*just-played< ::transitions-on-playback
           (fn [_ _ _ just-played<]
             #_(log! :just-played< just-played<)
             (let [tt-start (cljs.core/system-time)]
               (doseq [evt just-played<]
                 (let [evt-hash    (-> evt (-hash))
                       ?op         (-> evt :event/other-parent)
                       ?op-current (some-> ?op (-hash) ->current)
                       to-x        (-> evt hga-events/evt-view-position-x)
                       to-y        (-> evt hga-events/evt-view-position-y)
                       current     (-> evt-hash ->current)
                       prop->t     (-> evt-hash ->prop->t)]
                   (if ?op-current
                     (do (goog.object/set current x-key (goog.object/get ?op-current x-key))
                         (goog.object/set current y-key (goog.object/get ?op-current y-key))
                         (goog.object/set prop->t x-key (make-t ?op-current to-x tt-start))
                         (goog.object/set prop->t y-key (make-t ?op-current to-y tt-start)))
                     (do (goog.object/set current x-key to-x)
                         (goog.object/set current y-key to-y)))

                   (goog.object/set prop->t opacity-key (make-t 0 1 tt-start)))))))

(add-watch hga-playback/*just-rewinded> ::transitions-on-playback
           (fn [_ _ _ just-rewinded>]
             #_(log! :just-rewinded> just-rewinded>)
             (let [tt-start (cljs.core/system-time)]
               (doseq [evt just-rewinded>]
                 (let [evt-hash     (-> evt (-hash))
                       ?op          (-> evt :event/other-parent)
                       ?op-current  (some-> ?op (-hash) ->current)
                       current      (-> evt-hash ->current)
                       from-x       (goog.object/get current x-key)
                       from-y       (goog.object/get current y-key)
                       from-opacity (goog.object/get current opacity-key)
                       prop->t      (-> evt-hash ->prop->t)]
                   (when ?op-current
                     (goog.object/set prop->t x-key (make-t from-x ?op-current tt-start))
                     (goog.object/set prop->t y-key (make-t from-y ?op-current tt-start)))

                   (goog.object/set prop->t opacity-key (make-t from-opacity 0 tt-start)))))))

(def *stats (atom {:from  (cljs.core/system-time)
                   :times 0}))
(let [{:keys [from to times]} @*stats]
  (/ times (/ (- to from) 1000)))
#_(def *last-tick-time-end (atom (cljs.core/system-time)))

(defn current->desired-run! []
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
            (let [t-time-start  (goog.object/get t t-time-start-key)
                  t-time-delta  (- t-time-now t-time-start)
                  t-time-pos    (min 1 (/ t-time-delta tt))
                  t-mod         (-> t-time-pos hga-timing/ease-in-out-cubic)
                  t-val-from    (let [t-val-from* (goog.object/get t t-val-from-key)]
                                  (if (object? t-val-from*)
                                    (goog.object/get t-val-from* prop)
                                    t-val-from*))
                  t-val-to      (let [t-val-to* (goog.object/get t t-val-to-key)]
                                  (if (object? t-val-to*)
                                    (goog.object/get t-val-to* prop)
                                    t-val-to*))
                  t-val-delta   (- t-val-to t-val-from)
                  t-val-current (+ t-val-from (* t-val-delta t-mod))]
              (goog.object/set current prop t-val-current)

              (when (= 1 t-time-pos)
                (goog.object/remove prop->t prop)))))
         (when (goog.object/isEmpty prop->t)
           (goog.object/remove event-hash->prop->t event-hash)))))))
