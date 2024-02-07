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
