(ns hashgraph.app.transitions
  (:require-macros [hashgraph.utils.js-map :refer [js-map] :as js-map] :reload-all)
  (:require-macros [hashgraph.app.transitions :refer [->view-state t!]])
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
                                          safe-update! safe-update-in!
                                          hash=]
             :refer-macros [l letl letl2 defn*]
             :as utils]))

(def tt 500) ;; transition time ms

(defn num [maybe-num] (when (number? maybe-num) maybe-num))

;; These states are used to trigger view transitions.
;; *just-played< events transition from other-parent (as though they are being sent over the wire).
;; *just-rewinded> events transition from their current position back to other-parent (as though time's rewinded)
(def view-id-path->view-state (js-map))
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

(defn view-state->current [view-state]
  (js-map/get! view-state :view-state/current (js-map/js-map)))

(defn view-state->desired [view-state]
  (js-map/get! view-state :view-state/desired (js-map/js-map)))

(defn view-state->prop->t [view-state]
  (js-map/get! view-state :view-state/prop->t (js-map/js-map)))

(declare current->desired-run!)
(defn play! [topic-path creator->x evts<]
  #_(l [::play! topic-path creator->x evts<])
  (let [tt-start (cljs.core/system-time)
        tt-end   (+ tt-start tt)]
    (doseq [evt evts<]
      (let [view-state (-> [(hash topic-path) (hash evt)] ->view-state)
            ?p         (or (-> evt :event/other-parent)
                             (-> evt :event/self-parent))
            ?p-current (when-let [p ?p]
                           (-> [(hash topic-path) (hash p)] ->view-state view-state->current))
            to-x       (creator->x (hg/creator evt))
            to-y       (hga-view/evt->y evt)]
        (if ?p-current
          (t! view-state
            :x tt-start ?p-current to-x tt-end
            :y tt-start ?p-current to-y tt-end
            :opacity tt-start 0 1 tt-end)
          (t! view-state
            :x       tt-start to-x to-x tt-end ;; use the same flow so :view-state/desired get's updated
            :y       tt-start to-y to-y tt-end
            :opacity tt-start 0    1    tt-end))))))


(defn rewind! [topic-path evts>]
  #_(l [::rewind! evts>])
  (let [tt-start (cljs.core/system-time)
        tt-end   (+ tt-start tt)]
    (doseq [evt evts>]
      (let [view-state   (-> [(hash topic-path) (hash evt)] ->view-state)
            current      (-> view-state view-state->current)
            ?p           (or (-> evt :event/other-parent)
                             (-> evt :event/self-parent))
            ?p-current   (when-let [p ?p]
                           (-> [(hash topic-path) (hash p)] ->view-state view-state->current))
            from-x       (js-map/get current :x)
            from-y       (js-map/get current :y)
            from-opacity (js-map/get current :opacity)]
        (if ?p-current
          (t! view-state
            :x       tt-start from-x       ?p-current tt-end
            :y       tt-start from-y       ?p-current tt-end
            :opacity tt-start from-opacity 0          tt-end)
          (t! view-state
            :opacity tt-start from-opacity 0          tt-end))))))


(defn* ^:memoizing initial-tip-taped+initial-cr+cr->?to-initial-y [initial-tip-taped initial-cr cr]
  (let [initial-sub-crs<   (->> initial-cr
                                (iterate :concluded-round/prev-concluded-round)
                                (take-while some?)
                                reverse)
        from-y             hga-view/evt-offset-y
        initial-to-y       (-> initial-tip-taped hga-view/evt->y)
        initial-y-range    (- initial-to-y from-y)
        initial-cr-delta-y (if (> (count initial-sub-crs<) 1)
                             (/ initial-y-range (dec (count initial-sub-crs<)))
                             initial-y-range)]
    (+ from-y (* (:concluded-round/r cr) initial-cr-delta-y))))

(defn* ^:memoizing initial-tip-taped+initial-cr+cr->to-y [initial-tip-taped initial-cr cr]
  (if (not= (hg/creator (:concluded-round/witness-concluded cr)) (hg/creator initial-tip-taped))
    (initial-tip-taped+initial-cr+cr->?to-initial-y initial-tip-taped initial-cr cr)
    (let [witness-concluded (:concluded-round/witness-concluded cr)]
      (or (some-> cr
                  :concluded-round/creator->received-unique-tip
                  (get (hg/creator witness-concluded))
                  hga-view/evt->y)
          (-> witness-concluded
              hg/first-self-parent
              hga-view/evt->y
              (- hga-view/sp-padding))))))

(def cr-tt-delay (/ tt 20))
(def **last-cr-tt-start  (volatile! (cljs.core/system-time)))
(def **last-cr-direction (volatile! nil))
(defn weave-cr! [topic-path initial-tip-taped initial-cr ?creator->x prev-cr current-cr]
  #_(l [::weave-cr! topic-path prev-cr current-cr])

  ;; trigger manually, and not wait for engine that runs on next frame, as some may not be processed and we need them
  (current->desired-run!)

  (let [main-creator (hg/creator initial-tip-taped)
        new-crs      (into [] (comp (take-while some?)
                               (take-while (fn [cr] (not (hash= cr prev-cr)))))
                      (iterate :concluded-round/prev-concluded-round current-cr))
        to-x         (or (get ?creator->x main-creator)
                         0)]
    (vswap! **last-cr-tt-start (fn [last-tt-start] (if (= @**last-cr-direction :forwards)
                                                     (max last-tt-start (cljs.core/system-time))
                                                     (cljs.core/system-time))))
    (vreset! **last-cr-direction :forwards)
    (doseq [new-cr (reverse new-crs)]
      (let [received-events (->> (:concluded-round/last-received-event new-cr)
                                 (iterate :received-event/prev-received-event)
                                 (take-while some?)
                                 (take-while #(= (:received-event/r %) (:concluded-round/r new-cr))))
            prev-cr         (:concluded-round/prev-concluded-round new-cr)
            prev-cr-to-y    (initial-tip-taped+initial-cr+cr->to-y initial-tip-taped initial-cr prev-cr)
            new-cr-to-y     (initial-tip-taped+initial-cr+cr->to-y initial-tip-taped initial-cr new-cr)
            to-y-offset     (-> new-cr-to-y
                                (- prev-cr-to-y)
                                (/ (count (:concluded-round/es-r new-cr))))
            ->to-y          (fn [re] (+ prev-cr-to-y (* to-y-offset (inc (:received-event/r-idx re)))))]
        (doseq [re (reverse received-events)]
          (let [tt-start          (vswap! **last-cr-tt-start + cr-tt-delay)
                evt               (:received-event/event re)
                view-state        (-> [(hash topic-path) (hash evt)] ->view-state)
                current           (-> view-state view-state->current)
                to-y              (->to-y re)
                from-fill-opacity (or (num (js-map/get current :fill-opacity))
                                      (do (js-map/assoc! current :fill-opacity 0)
                                          0))]
            (js-map/assoc! current :fill (:received-event/color re))
            (t! view-state
              :x tt-start (js-map/get current :x) to-x nil ;; blank tt-end, as it's not used atm by the engine anyway
              :y tt-start (js-map/get current :y) to-y nil
              :fill-opacity tt-start from-fill-opacity 1 nil)))))))

(defn unweave-cr! [topic-path ?creator->x prev-cr current-cr]
  #_(l [::unweave-cr! topic-path ?creator->x prev-cr current-cr])

  ;; trigger manually, and not wait for engine that runs on next frame, as some may not be processed and we need them
  (current->desired-run!)

  (let [crs-to-rewind (into [] (comp (take-while some?)
                                     (take-while (fn [cr] (not (hash= cr current-cr)))))
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
          (let [evt               (:received-event/event re)
                view-state        (-> [(hash topic-path) (hash evt)] ->view-state)
                current           (-> view-state view-state->current)
                from-x            (js-map/get current :x)
                from-y            (js-map/get current :y)
                from-fill-opacity (or (num (js-map/get current :fill-opacity))
                                      (do (js-map/assoc! current :fill-opacity 0)
                                          0))
                to-x              (or (get ?creator->x (hg/creator evt))
                                      from-x)
                to-y              (hga-view/evt->y evt)
                to-fill-opacity   0]
            #_(when (or (not= from-x to-x)
                        (not= from-y to-y)
                        (not= from-opacity to-opacity)))
            (let [tt-start (vswap! **last-cr-tt-start + cr-tt-delay)]
              (t! view-state
                :x tt-start from-x to-x nil
                :y tt-start from-y to-y nil
                :fill-opacity tt-start from-fill-opacity to-fill-opacity nil))))))))

(defn re-position! [topic-path old-x->new-x]
  #_(l [::re-position! topic-path old-x->new-x])
  (let [view-id-path*->view-state (js-map/get! view-id-path->view-state (hash topic-path) (js-map/js-map))
        tt-start                  (cljs.core/system-time)
        tt-end                    (+ tt-start tt)]
    (.forEach
     view-id-path*->view-state
     (fn [view-state _view-id-path* _js-map]
       (let [desired   (-> view-state view-state->desired)
             desired-x (js-map/get desired :x)]
         (when-let [new-x (get old-x->new-x desired-x)]
           (t! view-state
             :x tt-start desired-x new-x tt-end)))))))

#_(add-watch hga-state/*last-cr ::run-transitions-on-cr-change
           (fn [_ _ old-last-cr new-last-cr]
             (l [::run-transitions-on-cr-change old-last-cr new-last-cr])
             (transition-on-cr-change! old-last-cr new-last-cr)))


#_(def *stats (atom {:from  (cljs.core/system-time)
                     :times 0}))
#_(let [{:keys [from to times]} @*stats]
  (/ times (/ (- to from) 1000)))
#_(def *last-tick-time-end (atom (cljs.core/system-time)))

(defn current->desired-run! []
  #_(swap! *stats (fn [stats] (-> stats
                                       (update :times inc)
                                       (assoc :to (cljs.core/system-time)))))
  #_(l :current->desired-run!)
  (let [t-time-now (cljs.core/system-time)]
    (.forEach
     view-state->with-t?
     (fn [_with-t? view-state _js-map]
       (let [current (-> view-state view-state->current)
             prop->t (-> view-state view-state->prop->t)]
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

(defn js-get! [?js-map k]
  (let [js-map (or ?js-map (js-map/js-map))]
    (.get js-map k)))

(defn js-get-in! [?js-map [k & rest-ks]]
  (if (empty? rest-ks)
    (js-get! ?js-map k)
    (js-get-in! (js-get! ?js-map k) rest-ks)))

(defn mixin [id args->view-id-path]
  {:will-mount (fn [state]
                 (if-let [view-id-path (apply args->view-id-path (:rum/args state))]
                   (let [view-state (js-get-in! view-id-path->view-state view-id-path)]
                     (subscribe-to-view-state-change view-state (:rum/react-component state))
                     (-> state
                         (assoc-in [::id->view-state id] view-state)
                         (assoc id (-> view-state view-state->current))))
                   state))

   :will-unmount (fn [state]
                   (when-let [view-state (get-in state [::id->view-state id])]
                     (unsubscribe-to-view-state-change view-state (:rum/react-component state)))
                   state)})
