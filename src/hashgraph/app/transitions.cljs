(ns hashgraph.app.transitions
  (:require-macros [hashgraph.utils.js-map :refer [js-map] :as js-map])
  (:require [rum.core :as rum]
            [goog.object]
            [cljs.core :as core]
            [hashgraph.main :as hg]
            [hashgraph.members :as hg-members]
            [hashgraph.app.view :as hga-view]
            [hashgraph.app.timing :as hga-timing]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.inspector]
            [hashgraph.utils.core :refer [log!
                                          safe-assoc! safe-assoc-in!
                                          safe-update! safe-update-in!]
             :refer-macros [l letl defn*]
             :as utils]))

(def tt 500) ;; transition time ms

;; These states are used to trigger view transitions.
;; *just-played< events transition from other-parent (as though they are being sent over the wire).
;; *just-rewinded> events transition from their current position back to other-parent (as though time's rewinded)
(defonce *just-played<   (atom '()))
(defonce *just-rewinded> (atom '()))
(defonce *last-concluded-round (atom nil))

(defonce view-id->current-view-state (js-map))
(defonce view-id->prop->t (js-map)) ;; property -> transition
(defonce *view-id->react-comps (atom {})) ;; subscriptions from react components to view-id changes

#_
(defn reset-view-states! []
  (set! view-id->current-view-state (js-map))
  (set! view-id->prop->t (js-map)))
#_(l view-id->current-view-state)
#_(l view-id->desired-view-state)
#_(js/console.log view-id->current-view-state)
#_(js/console.log view-id->prop->t)
#_(defonce _make-obj-lookapable
  (set! (.. js/Object -prototype -call) (fn [_ k] (println k) (this-as this (goog.object/get this k)))))

#_
(defn ->left-desired? []
  (not (js-map/empty? view-id->prop->t)))

(defn subscribe-to-view-state-change [view-id react-comp]
  (swap! *view-id->react-comps update view-id utils/conjs react-comp))

(defn unsubscribe-to-view-state-change [view-id react-comp]
  (swap! *view-id->react-comps update view-id disj react-comp))

(defn notify-on-view-state-change [view-id]
  (let [comps-to-notify (get @*view-id->react-comps view-id)]
    (doseq [comp comps-to-notify]
      (rum/request-render comp))))



(defn view-id->in-transition? [view-id]
  (js-map/contains? view-id->prop->t view-id))

(defn evt->in-transition? [evt]
  (view-id->in-transition? (-hash evt)))

(defn ->current [view-id]
  (or (js-map/get view-id->current-view-state view-id)
      (let [current (js-map)]
        (js-map/assoc! view-id->current-view-state view-id current)
        current)))

(defn ->prop->t [view-id]
  (or (js-map/get view-id->prop->t view-id)
      (let [prop->t (js-map)]
        (js-map/assoc! view-id->prop->t view-id prop->t)
        prop->t)))

(defn make-t [val-from val-to time-start]
  (js-map :transition/val-from val-from
          :transition/val-to val-to
          :transition/time-start time-start))

(add-watch *just-played< ::transitions-on-playback
           (fn [_ _ _ just-played<]
             #_(log! :just-played< just-played<)
             (let [tt-start (cljs.core/system-time)]
               (doseq [evt just-played<]
                 (let [evt-hash    (-> evt (-hash))
                       ?op         (-> evt :event/other-parent)
                       ?op-current (some-> ?op (-hash) ->current)
                       to-x        (-> evt hga-view/evt->x)
                       to-y        (-> evt hga-view/evt->y)
                       current     (-> evt-hash ->current)
                       prop->t     (-> evt-hash ->prop->t)]
                   (if ?op-current
                     (do (js-map/assoc! current :x (js-map/get ?op-current :x))
                         (js-map/assoc! current :y (js-map/get ?op-current :y))
                         (js-map/assoc! prop->t :x (make-t ?op-current to-x tt-start))
                         (js-map/assoc! prop->t :y (make-t ?op-current to-y tt-start)))
                     (do (js-map/assoc! current :x to-x)
                         (js-map/assoc! current :y to-y)))

                   (js-map/assoc! prop->t :opacity (make-t 0 1 tt-start)))))))

(add-watch *just-rewinded> ::transitions-on-playback
           (fn [_ _ _ just-rewinded>]
             #_(log! :just-rewinded> just-rewinded>)
             (let [tt-start (cljs.core/system-time)]
               (doseq [evt just-rewinded>]
                 (let [evt-hash     (-> evt (-hash))
                       ?op          (-> evt :event/other-parent)
                       ?op-current  (some-> ?op (-hash) ->current)
                       current      (-> evt-hash ->current)
                       from-x       (js-map/get current :x)
                       from-y       (js-map/get current :y)
                       from-opacity (js-map/get current :opacity)
                       prop->t      (-> evt-hash ->prop->t)]
                   (when ?op-current
                     (js-map/assoc! prop->t :x (make-t from-x ?op-current tt-start))
                     (js-map/assoc! prop->t :y (make-t from-y ?op-current tt-start)))

                   (js-map/assoc! prop->t :opacity (make-t from-opacity 0 tt-start)))))))

(defn* ^:memoizing ?cr->to-y [?cr]
  (let [#_#_
        ?last-main-creator-re-y
        (some-> ?cr
                :concluded-round/last-received-event
                (->> (iterate :received-event/prev-received-event)
                     (take-while some?)
                     (some (fn [{:received-event/keys [event]}] (when (= (hg/creator event) hg/main-creator) event))))
                hga-view/evt->y)
        ]
    (max (+ 0 hga-view/wit-r)
         (or
          ;; first-main-creator-e-nr-to-y
          (some-> ?cr
                  :concluded-round/es-nr
                  (->> (filter (fn [e] (= (hg/creator e) hg/main-creator)))
                       (sort-by :event/creation-time))
                  first
                  hga-view/evt->y
                  (- hga-view/sp-padding))
          ;; main-creator-w-to-y
          (some-> ?cr
                  :concluded-round/ws
                  (->> (some (fn [w] (when (= (hg/creator w) hg/main-creator) w))))
                  hga-view/evt->y
                  (- hga-view/sp-padding))))))

(def cr-x (hga-view/idx->x (-indexOf hg-members/names hg/main-creator)))
(def cr-tt-delay (/ tt 20))
(def **last-cr-tt-start (volatile! (cljs.core/system-time)))
(def **last-cr-direction (volatile! nil))
(add-watch *last-concluded-round ::transitions-on-concluded-round
           (fn [_ _ prev-cr current-cr]
             (when (not (identical? prev-cr current-cr))
               (if (> (:concluded-round/r current-cr)
                      (:concluded-round/r prev-cr))
                 (let [new-crs (into [] (comp (take-while some?)
                                              (take-while (fn [cr] (not= cr prev-cr))))
                                     (iterate :concluded-round/prev-concluded-round current-cr))]
                   (vswap! **last-cr-tt-start (fn [last-tt-start] (if (= @**last-cr-direction :forwards)
                                                                    (max last-tt-start (cljs.core/system-time))
                                                                    (cljs.core/system-time))))
                   (vreset! **last-cr-direction :forwards)
                   (doseq [new-cr (reverse new-crs)]
                     (letl [received-events (->> (:concluded-round/last-received-event new-cr)
                                                (iterate :received-event/prev-received-event)
                                                (take-while some?)
                                                (take-while #(= (:received-event/r %) (:concluded-round/r new-cr))))
                           new-cr-from-y   (-> new-cr :concluded-round/prev-concluded-round ?cr->to-y)
                           new-cr-to-y     (-> new-cr ?cr->to-y)
                           to-y-offset     (-> new-cr-to-y
                                               (- new-cr-from-y)
                                               (/ (count (:concluded-round/es-r new-cr))))
                           ->to-y          (fn [re] (+ new-cr-from-y (* to-y-offset (:received-event/r-idx (l re)))))
                           to-x            cr-x]
                       (doseq [re (reverse received-events)]
                         (let [tt-start     (vswap! **last-cr-tt-start + cr-tt-delay)
                               evt          (:received-event/event re)
                               evt-hash     (-hash evt)
                               current      (-> evt-hash ->current)
                               prop->t      (-> evt-hash ->prop->t)
                               to-y         (->to-y re)
                               from-opacity (or (js-map/get current :fill-opacity)
                                                (do (js-map/assoc! current :fill-opacity 0)
                                                    0))]
                           (js-map/assoc! current :fill (:received-event/color re))
                           (js-map/assoc! prop->t :x            (make-t (js-map/get current :x) to-x tt-start))
                           (js-map/assoc! prop->t :y            (make-t (js-map/get current :y) to-y tt-start))
                           (js-map/assoc! prop->t :fill-opacity (make-t from-opacity 1 tt-start)))))))

                 (let [crs-to-rewind (into [] (comp (take-while some?)
                                                    (take-while (fn [cr] (not= cr current-cr))))
                                           (iterate :concluded-round/prev-concluded-round prev-cr))]
                   (vswap! **last-cr-tt-start (fn [last-tt-start] (if (= @**last-cr-direction :backwards)
                                                                    (max last-tt-start (cljs.core/system-time))
                                                                    (cljs.core/system-time))))
                   (vreset! **last-cr-direction :backwards)
                   (doseq [cr-to-rewind crs-to-rewind]
                     (let [received-events (->> (:concluded-round/last-received-event cr-to-rewind)
                                                (iterate :received-event/prev-received-event)
                                                (take-while some?)
                                                (take-while #(= (:received-event/r %) (:concluded-round/r cr-to-rewind))))]
                       (doseq [re received-events]
                         (let [evt           (:received-event/event re)
                               evt-hash      (-hash evt)
                               current       (-> evt-hash ->current)
                               prop->t       (-> evt-hash ->prop->t)
                               from-x        (js-map/get current :x)
                               from-y        (js-map/get current :y)
                               from-opacity  (js-map/get current :fill-opacity)
                               to-x          (hga-view/evt->x evt)
                               to-y          (hga-view/evt->y evt)
                               to-opacity    0]
                           #_(when (or (not= from-x to-x)
                                     (not= from-y to-y)
                                     (not= from-opacity to-opacity)))
                           (let [tt-start (vswap! **last-cr-tt-start + cr-tt-delay)]
                             (js-map/assoc! prop->t :x            (make-t from-x to-x tt-start))
                             (js-map/assoc! prop->t :y            (make-t from-y to-y tt-start))
                             (js-map/assoc! prop->t :fill-opacity (make-t from-opacity to-opacity tt-start))))))))))))


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
    (.forEach
     view-id->prop->t
     (fn [prop->t view-id _js-map]
       (let [current (->current view-id)]
         (.forEach
          prop->t
          (fn [t prop _js-map]
            (let [t-time-start  (js-map/get t :transition/time-start)
                  t-time-delta  (- t-time-now t-time-start)]
              (when (pos? t-time-delta)
                (let [t-time-pos    (min 1 (/ t-time-delta tt))
                      t-mod         (-> t-time-pos hga-timing/ease-in-out-cubic)
                      t-val-from    (let [t-val-from* (js-map/get t :transition/val-from)]
                                      (if (js-map/js-map? t-val-from*)
                                        (js-map/get t-val-from* prop)
                                        t-val-from*))
                      t-val-to      (let [t-val-to* (js-map/get t :transition/val-to)]
                                      (if (js-map/js-map? t-val-to*)
                                        (js-map/get t-val-to* prop)
                                        t-val-to*))
                      t-val-delta   (- t-val-to t-val-from)
                      t-val-current (+ t-val-from (* t-val-delta t-mod))]
                  (js-map/assoc! current prop t-val-current)
                  (notify-on-view-state-change view-id)

                  (when (= 1 t-time-pos)
                    (js-map/dissoc! prop->t prop)))))))
         (when (js-map/empty? prop->t)
           (js-map/dissoc! view-id->prop->t view-id)))))))

(defn run-each-frame! []
  (current->desired-run!)
  (js/requestAnimationFrame run-each-frame!))

(run-each-frame!)

(defn mixin [id args->view-id]
  {:will-mount    (fn [state]
                    (if-let [view-id (apply args->view-id (:rum/args state))]
                      (do (subscribe-to-view-state-change view-id (:rum/react-component state))
                          (-> state
                              (assoc-in [::id->view-id id] view-id)
                              (assoc id (->current view-id))))
                      state))

   :will-unmount  (fn [state]
                    (when-let [view-id (get-in state [::id->view-id id])]
                      (unsubscribe-to-view-state-change view-id (:rum/react-component state)))
                    state)})
