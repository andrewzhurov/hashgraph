(ns hashgraph.app.page
  (:require
   [cljs.reader]
   [garden.core :refer [css]]
   [hashgraph.app.events :as hga-events]
   [hashgraph.app.infini-events :as hga-infini-events]
   [hashgraph.main :as hg]
   [rum.core :as rum]
   [taoensso.timbre :refer-macros [spy]]
   [hashgraph.members :as hg-members]))

(def styles
  [[:html {:height "100%" :overflow-y :hidden}]
   [:body {:height "100%" :overflow-y :hidden}]
   [:#root {:height "100%" :overflow-y :hidden}]
   [:#viz-wrapper {:height "100%" :overflow-y :scroll}]
   [:#viz {:width "100%"}]])

(def evt-view-r 20)
(def evt-view-s (* evt-view-r 2))
(def vote-view-r (/ evt-view-r 2))
(def vote-view-circumferance (* 2 js/Math.PI vote-view-r))
(def evt-padding 30)

(defn evt-view-position [evt]
  (let [x             (-> (-> evt :creator hg-members/members :member/idx)
                          (* (+ evt-padding evt-view-s))
                          (+ evt-view-r evt-padding))
        y             (-> (hg/index evt)
                          (* (+ evt-view-s evt-padding))
                          (+ evt-view-r evt-padding))]
    [x y]))

(defn evts-height [evts]
  (-> evts
      hg/tips
      (->> (map evt-view-position))
      (some->> not-empty (apply max))
      (or 0)))

(defonce *hovered-evt (atom nil))

(defn color-rgba-str
  ([rgb-vec] (color-rgba-str rgb-vec 1))
  ([[red green blue] alpha] (str "rgba(" red "," green "," blue "," alpha  ")")))

(rum/defcs viz <
  rum/static
  rum/reactive
  hga-infini-events/mixin
  [_state evts]
  (let [hovered-evt  (rum/react *hovered-evt)
        highlighted? (fn [evt]
                       (when-let [hovered-evt hovered-evt]
                         (or (= hovered-evt evt)
                             (hg/ancestor? hovered-evt evt))))]

    [:div#viz-wrapper
     [:style (css styles)]
     [:svg#viz
      (for [evt  evts
            :let [[x y] (evt-view-position evt)]]
        [:g.references
         (when-let [[self-parent-x self-parent-y] (some-> (hg/self-parent evt) evt-view-position)]
           (let [line-highlighted? (and (highlighted? evt)
                                        (highlighted? (hg/self-parent evt)))]
             [:line {:x1    x
                     :y1    y
                     :x2    self-parent-x
                     :y2    self-parent-y
                     :style {:stroke       (if line-highlighted? :blue :lightgray)
                             :stroke-width "2px"}}]))
         (when-let [[parent-x parent-y] (some-> (hg/parent evt) evt-view-position)]
           (let [line-highlighted? (and (highlighted? evt)
                                        (highlighted? (hg/parent evt)))]
             [:line {:x1    x
                     :y1    y
                     :x2    parent-x
                     :y2    parent-y
                     :style {:stroke       (if line-highlighted? :blue :lightgray)
                             :stroke-width "2px"}}]))])

      (for [evt  evts
            :let [[x y] (evt-view-position evt)]
            :let [round (hg/round evt)
                  member (-> evt :creator hg-members/members)]]
        [:g.event
         [:circle {:r                  evt-view-r
                   :cx                 x
                   :cy                 y
                   :stroke            (color-rgba-str (:member/color-rgb member))
                   #_(cond (hg/witness? evt)                                     "red"
                                             (some (fn [evt-y] (hg/see-many-see? evt evt-y)) evts) "orange"
                                             (some (fn [evt-y] (hg/see? evt evt-y)) evts)          "yellow"
                                             :else                                                 "gray")
                   :stroke-width       "4px"
                   :fill               :white
                   :on-mouse-enter     #(reset! *hovered-evt evt)
                   #_#_:on-mouse-leave #(reset! *hovered-evt nil)}]
         [:g.votes
          (for [seen-by-member (-> evts
                                   hg/tips
                                   (->> (map (fn [tip]
                                               (when (hg/see? tip evt)
                                                 (:creator tip))))
                                        (filter some?)
                                        (map hg-members/members)))]
            (let [vote-circumferance (/ vote-view-circumferance (count hg-members/members))
                  vote-circumferance-start (-> vote-view-circumferance
                                               (/ hg-members/members-count)
                                               (* (:member/idx seen-by-member)))]
              [:circle {:r                vote-view-r
                        :cx               x
                        :cy               y
                        :fill             :transparent
                        :stroke           (color-rgba-str (:member/color-rgb seen-by-member) 0.5)
                        :stroke-width     (* vote-view-r 2)
                        :stroke-dasharray (str "0 " vote-circumferance-start " " vote-circumferance " " vote-view-circumferance)
                        #_#_:transform (str "rotate("(* (:member/idx seen-by-member)
                                                    (/ 360 hg-members/members-count)) ")")}]))]
         [:text {:x (+ x evt-view-r)
                 :y (- y evt-view-r)
                 :font-size "12px"} round]])]]))

(rum/defc view < rum/reactive
  []
  (viz (rum/react hga-events/*evts)))
