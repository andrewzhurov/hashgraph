(ns hashgraph.app.transitions
  (:require-macros [hashgraph.utils.js-map :refer [js-map] :as js-map] :reload-all)
  (:require-macros [hashgraph.app.transitions :refer [t!]])
  (:require [rum.core :as rum]
            [goog.object]
            [garden.units :as gu]
            [garden.types]
            [cljs.core :as core]
            [hashgraph.main :as hg]
            [hashgraph.members :as hg-members]
            [hashgraph.app.view :as hga-view]
            [hashgraph.app.timing :as hga-timing]
            [hashgraph.app.state :as hga-state]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.utils :as hga-utils]
            [hashgraph.utils.core :refer [log!
                                          safe-assoc! safe-assoc-in!
                                          safe-update! safe-update-in!]
             :refer-macros [l letl defn*]
             :as utils]))

(def tt 500) ;; transition time ms

;; These states are used to trigger view transitions.
;; *just-played< events transition from other-parent (as though they are being sent over the wire).
;; *just-rewinded> events transition from their current position back to other-parent (as though time's rewinded)
(def view-id->view-state (js-map))
(def view-state->with-t? (js-map))

(defn subscribe-to-view-state-change [view-state react-comp]
  (js-map/assoc-in! view-state [:view-state/subscribed-react-comps react-comp] true))

(defn unsubscribe-to-view-state-change [view-state react-comp]
  (js-map/update! view-state :view-state/subscribed-react-comps js-map/dissoc! react-comp))

(defn notify-view-state-change [view-state]
  (when-let [subscribed-react-comps (js-map/get view-state :view-state/subscribed-react-comps)]
    (.forEach
     subscribed-react-comps
     (fn [_ comp _]
       (rum/request-render comp)))))

(defn ->view-state [view-id]
  (or (js-map/get view-id->view-state view-id)
      (let [view-state (js-map/js-map)]
        (js-map/assoc! view-id->view-state view-id view-state)
        view-state)))

(defn ->current [view-state]
  (or (js-map/get view-state :view-state/current)
      (let [current (js-map/js-map)]
        (js-map/assoc! view-state :view-state/current current)
        current)))

(defn ->desired [view-state]
  (or (js-map/get view-state :view-state/desired)
      (let [current (js-map/js-map)]
        (js-map/assoc! view-state :view-state/desired current)
        current)))

(defn ->prop->t [view-state]
  (or (js-map/get view-state :view-state/prop->t)
      (let [prop->t (js-map/js-map)]
        (js-map/assoc! view-state :view-state/prop->t prop->t)
        prop->t)))


(add-watch hga-state/*just-played< ::just-played-transitions-on-playback
           (fn [_ _ _ just-played<]
             #_(log! :just-played< just-played<)
             (let [tt-start (cljs.core/system-time)
                   tt-end   (+ tt-start tt)]
               (doseq [evt just-played<]
                 (let [view-id     (-> evt (-hash))
                       view-state  (-> view-id ->view-state)
                       ?op         (-> evt :event/other-parent)
                       ?op-current (some-> ?op (-hash) ->view-state ->current)
                       to-x        (-> evt hga-view/evt->x)
                       to-y        (-> evt hga-view/evt->y)]
                   (if ?op-current
                     (t! view-state
                         :x tt-start ?op-current to-x tt-end
                         :y tt-start ?op-current to-y tt-end
                         :opacity tt-start 0 1 tt-end)
                     (t! view-state
                         :x       tt-start to-x to-x tt-end ;; use the same flow so :view-state/desired get's updated
                         :y       tt-start to-y to-y tt-end
                         :opacity tt-start 0    1    tt-end)))))))

(add-watch hga-state/*just-rewinded> ::just-rewinded-transitions-on-playback
           (fn [_ _ _ just-rewinded>]
             #_(log! :just-rewinded> just-rewinded>)
             (let [tt-start (cljs.core/system-time)
                   tt-end   (+ tt-start tt)]
               (doseq [evt just-rewinded>]
                 (let [view-id      (-> evt (-hash))
                       view-state   (-> view-id ->view-state)
                       current      (-> view-state ->current)
                       ?op          (-> evt :event/other-parent)
                       ?op-current  (some-> ?op (-hash) ->view-state ->current)
                       from-x       (js-map/get current :x)
                       from-y       (js-map/get current :y)
                       from-opacity (js-map/get current :opacity)]
                   (if ?op-current
                     (t! view-state
                         :x       tt-start from-x       ?op-current tt-end
                         :y       tt-start from-y       ?op-current tt-end
                         :opacity tt-start from-opacity 0           tt-end)
                     (t! view-state
                         :opacity tt-start from-opacity 0           tt-end)))))))

(defn* ^:memoizing ?cr->to-y [?cr]
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
                (- hga-view/sp-padding))

        ;; last received evt y
        (some-> ?cr
                :concluded-round/last-received-event
                :received-event/event
                hga-view/evt->y))))

(def cr-x (hga-view/idx->x (-indexOf hg-members/names hg/main-creator)))
(def cr-tt-delay (/ tt 20))
(def **last-cr-tt-start (volatile! (cljs.core/system-time)))
(def **last-cr-direction (volatile! nil))
(add-watch hga-state/*last-cr ::transitions-on-concluded-round
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
                     (let [received-events (->> (:concluded-round/last-received-event new-cr)
                                                (iterate :received-event/prev-received-event)
                                                (take-while some?)
                                                (take-while #(= (:received-event/r %) (:concluded-round/r new-cr))))
                           new-cr-from-y   (-> new-cr :concluded-round/prev-concluded-round ?cr->to-y)
                           new-cr-to-y     (-> new-cr ?cr->to-y)
                           to-y-offset     (-> new-cr-to-y
                                               (- new-cr-from-y)
                                               (/ (count (:concluded-round/es-r new-cr))))
                           ->to-y          (fn [re] (+ new-cr-from-y (* to-y-offset (:received-event/r-idx re))))
                           to-x            cr-x]
                       (doseq [re (reverse received-events)]
                         (let [tt-start     (vswap! **last-cr-tt-start + cr-tt-delay)
                               evt          (:received-event/event re)
                               view-id      (-hash evt)
                               view-state   (-> view-id ->view-state)
                               current      (-> view-state ->current)
                               to-y         (->to-y re)
                               from-fill-opacity (or (js-map/get current :fill-opacity)
                                                     (do (js-map/assoc! current :fill-opacity 0)
                                                         0))]
                           (js-map/assoc! current :fill (:received-event/color re))
                           (t! view-state
                               :x tt-start (js-map/get current :x) to-x nil ;; blank tt-end, as it's not used atm by the engine anyway
                               :y tt-start (js-map/get current :y) to-y nil
                               :fill-opacity tt-start from-fill-opacity 1 nil))))))

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
                               view-id       (-hash evt)
                               view-state    (-> view-id ->view-state)
                               current       (-> view-state ->current)
                               from-x        (js-map/get current :x)
                               from-y        (js-map/get current :y)
                               from-fill-opacity  (js-map/get current :fill-opacity)
                               to-x          (hga-view/evt->x evt)
                               to-y          (hga-view/evt->y evt)
                               to-fill-opacity    0]
                           #_(when (or (not= from-x to-x)
                                     (not= from-y to-y)
                                     (not= from-opacity to-opacity)))
                           (let [tt-start (vswap! **last-cr-tt-start + cr-tt-delay)]
                             (t! view-state
                                 :x tt-start from-x to-x nil
                                 :y tt-start from-y to-y nil
                                 :fill-opacity tt-start from-fill-opacity to-fill-opacity nil)))))))))))


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
     view-state->with-t?
     (fn [_with-t? view-state _js-map]
       (let [current (-> view-state (->current))
             prop->t (-> view-state (->prop->t))]
         (.forEach
          prop->t
          (fn [t prop _js-map]
            (let [t-time-start (js-map/get t :transition/time-start)
                  t-time-delta (- t-time-now t-time-start)]
              (when (pos? t-time-delta)
                (let [t-time-pos    (min 1 (/ t-time-delta tt))
                      t-mod         (-> t-time-pos hga-timing/ease-in-out-cubic)
                      t-val-from    (let [t-val-from* (js-map/get t :transition/val-start)]
                                      (if (js-map/js-map? t-val-from*)
                                        (js-map/get t-val-from* prop)
                                        t-val-from*))
                      t-val-to      (let [t-val-to* (js-map/get t :transition/val-end)]
                                      (if (js-map/js-map? t-val-to*)
                                        (js-map/get t-val-to* prop)
                                        t-val-to*))
                      t-val-delta   (- t-val-to t-val-from)
                      t-val-current (+ t-val-from (* t-val-delta t-mod))]
                  (js-map/assoc! current prop t-val-current)
                  (when (= 1 t-time-pos)
                    (js-map/dissoc! prop->t prop)))))))
         (notify-view-state-change view-state)
         (when (js-map/empty? prop->t)
           (js-map/dissoc! view-state->with-t? view-state)))))))

(def run-each-frame!
  (let [schedule (or hga-utils/?request-animation-frame
                     (partial hga-utils/before-render ::run-transitions))]
    (fn []
      (current->desired-run!)
      (schedule run-each-frame!))))

(defonce _runner (run-each-frame!))

(defn mixin [id args->view-id]
  {:will-mount    (fn [state]
                    (if-let [view-id (apply args->view-id (:rum/args state))]
                      (let [view-state (-> view-id ->view-state)]
                        (subscribe-to-view-state-change view-state (:rum/react-component state))
                        (-> state
                            (assoc-in [::id->view-state id] view-state)
                            (assoc id (-> view-state ->current))))
                      state))

   :will-unmount  (fn [state]
                    (when-let [view-state (get-in state [::id->view-state id])]
                      (unsubscribe-to-view-state-change view-state (:rum/react-component state)))
                    state)})
