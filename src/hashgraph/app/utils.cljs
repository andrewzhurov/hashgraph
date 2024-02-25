(ns hashgraph.app.utils
  (:require
   [rum.core :as rum]
   [hashgraph.app.view :as hga-view]
   [hashgraph.utils.core :refer [log!] :refer-macros [l letl] :as utils]))

(def render-always-mixin
  {:after-render (fn [state] (rum/request-render (:rum/react-component state)) state)})

(rum/defcs plug < render-always-mixin
  {:did-mount (fn [state] (assoc state ::dom-node (rum/dom-node state)))}
  [{::keys [dom-node] :as state} plug-name]
  [:div {:style {:position        :relative
                 :width           "100%"
                 :height          "100%"
                 :border          "1px solid lightgray"
                 :display         :flex
                 :align-items     "center"
                 :justify-content "center"
                 :z-index         -1000}}
   (when dom-node
     (let [w (.-clientWidth dom-node)
           h (.-clientHeight dom-node)]
       [:svg {:width  w
              :height h
              :style  {:position :absolute
                       :left     "0px"
                       :right    "0px"}}
        [:line {:x1           0
                :y1           0
                :x2           w
                :y2           h
                :stroke       "lightgray"
                :stroke-width "1px"}]
        [:line {:x1           0
                :y1           h
                :x2           w
                :y2           0
                :stroke       "lightgray"
                :stroke-width "1px"}]]))
   [:div {:style {:color "lightgray"}} plug-name]])


(def schedule
  (or (and (exists? js/window)
           (or js/window.requestAnimationFrame
               js/window.webkitRequestAnimationFrame
               js/window.mozRequestAnimationFrame
               js/window.msRequestAnimationFrame))
      #(js/setTimeout % 16)))

(defn on-next-frame! [callback]
  (js/setTimeout #(js/requestAnimationFrame callback)))

(defn on-animation-frame! [callback]
  (js/requestAnimationFrame callback))

(defn async-idle
  "Async evokations of this function will evoke f only when it's not currently running - is idle."
  [f]
  (let [idle?* (volatile! true)]
    (fn [& args]
      (when @idle?*
        (vreset! idle?* false)
        (js/setTimeout #(do (apply f args)
                            (vreset! idle?* true)))))))

(defn once-per-render
  [f]
  (let [*ran? (volatile! false)]
    (fn [& args]
      (when-not @*ran?
        (vreset! *ran? true)
        (apply f args)
        (js/requestAnimationFrame (fn [] (vreset! *ran? false)))))))

(defn per-animation-frame-while [per-animation-frame-cb while-pred]
  (js/requestAnimationFrame #(when (while-pred)
                               (per-animation-frame-cb)
                               (per-animation-frame-while per-animation-frame-cb while-pred))))



(def static-by-hashes
  {:should-update (fn [old-state new-state]
                    (let [old-args (:rum/args old-state)
                          new-args (:rum/args new-state)]
                      (or (not= (count old-args) (count new-args))
                          (loop [old-args-left old-args
                                 new-args-left new-args]
                            (cond (empty? new-args-left)
                                  false

                                  (not= (hash (first old-args-left))
                                        (hash (first new-args-left)))
                                  true

                                  :else
                                  (recur (rest old-args-left) (rest new-args-left)))))))})

(rum/defc static-by-hashes-testee-comp <
  static-by-hashes
  [args]
  (l [:testee args])
  [:div (pr-str args)])

(rum/defcs static-by-hashes-tester-comp < (rum/local 0 ::*counter) (rum/local 0 ::*counter2)
  [{::keys [*counter *counter2]}]
  (l [:tester @*counter @*counter2])
  [:div {:style {:position         :fixed
                 :z-index          100000
                 :background-color "white"}}
   [:button {:on-click #(swap! *counter inc)}
    "inc 1st"]
   [:button {:on-click #(swap! *counter2 inc)}
    "inc 2nd"]
   [:button {:on-click #(reset! *counter @*counter)}
    "same 1st"]
   [:button {:on-click #(reset! *counter @*counter)}
    "same 2nd"]
   (static-by-hashes-testee-comp @*counter @*counter)])


(def key-fn-by-hash {:key-fn (fn [arg] (-hash arg))})
(def static-by-hash {:should-update (fn [old-state new-state]
                                      #_(log! [:should-update] {:event         (:event-info/event (first (:rum/args old-state)))
                                                              :should-update (not= (-hash (first (:rum/args old-state)))
                                                                                   (-hash (first (:rum/args new-state))))
                                                              :old-hash      (-hash (first (:rum/args old-state)))
                                                              :new-hash      (-hash (first (:rum/args new-state)))
                                                              :old-args      (first (:rum/args old-state))
                                                              :new-args      (first (:rum/args new-state))})
                                      (not= (-hash (first (:rum/args old-state)))
                                            (-hash (first (:rum/args new-state)))))})
