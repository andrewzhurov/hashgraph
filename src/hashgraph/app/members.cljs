(ns hashgraph.app.members
  (:require [rum.core :as rum]
            [garden.units :refer [px]]
            [hashgraph.main :as hg]
            [hashgraph.members :as hg-members]
            [hashgraph.app.view :as hga-view]
            [hashgraph.app.styles :refer [reg-styles!]]
            [hashgraph.app.icons :as hga-icons]
            [hashgraph.app.avatars :as hga-avatars]
            [hashgraph.app.playback :as hga-playback]
            [hashgraph.app.transitions :as hga-transitions]
            [hashgraph.utils.core
             :refer [merge-attr-maps color-rgba-str timing *->time*]
             :refer-macros [defn* l letl]
             :as utils]))

(def styles
  [[:#members
    [:.member
     [:.member-name {:position   :absolute
                     :left       (px (/ hga-view/avatar-size 2))
                     :margin-top (px -6)
                     :transform  "translateX(-50%)"
                     ;; :font-size (px hga-view/member-name-font-size)
                     :color      :black}]
     [:.connectivity {:display       :none
                      :position      :absolute
                      :bottom        (px -2)
                      :right         (px -2)
                      #_#_:transform "translate(-100%, -50%)"}
      [:&.poor {:display :block}
       [:svg {:vertical-align :middle}]]]]]])

(def styles-horizontal
  [[:#members {:position         :fixed
               :top              (px (/ hga-view/window-height 3))
               :bottom           (px (/ hga-view/window-height 3))
               :right            (px hga-view/members-y-start)
               :width            (px hga-view/members-height)
               :background-color hga-view/members-background-color}
    [:.member {:position         :absolute
               :left             (px hga-view/members-padding-y)
               :transition       (str "opacity " (/ hga-transitions/tt 2) "ms")
               :opacity          0}
     [:&.active {:opacity 1}]]]])

(def styles-vertical
  [[:#members {:position         :fixed
               :left             (px hga-view/viz-margin-x)
               :right            (px hga-view/viz-margin-x)
               :bottom           (px hga-view/members-y-start)
               :height           (px hga-view/members-height)
               :background-color hga-view/members-background-color}
    [:.member {:position   :absolute
               :top        (px hga-view/members-padding-y)
               :transition (str "opacity " (/ hga-transitions/tt 2) "ms")
               :opacity    0}
     [:&.active {:opacity 1}]]]])

(reg-styles! ::members styles styles-horizontal styles-vertical)

(rum/defc view < rum/reactive []
  (let [?main-tip             (hg/events>->main-tip (reverse (rum/react hga-playback/*played<)))
        ?last-concluded-round (some-> ?main-tip hg/->concluded-round)
        stake-map             (hg/concluded-round->stake-map ?last-concluded-round)]
    [:div#members
     (for [member-name hg-members/names]
       (let [member    (hg-members/member-name->person member-name)
             active?   (contains? stake-map member-name)
             stake-pos (/ (get stake-map member-name) hg/total-stake)
             hardly-reachable? (hg-members/hardly-reachable-member-names member-name)]
         [:div.member {:key   member-name
                       :class [(when active? "active")]
                       :style {(if hga-view/view-mode-horizontal? :top :left) (- (hga-view/idx->x (:member/idx member))
                                                                                 (/ hga-view/avatar-size 2))}}
          (case (:member/gender member)
            :male   (hga-avatars/male-avatar   (color-rgba-str (:member/color-rgb member) 1) (color-rgba-str (:member/color-rgb member) stake-pos))
            :female (hga-avatars/female-avatar (color-rgba-str (:member/color-rgb member) 1) (color-rgba-str (:member/color-rgb member) stake-pos))
            [:div "unknown gender"])

          [:div.connectivity {:class (when hardly-reachable? "poor")
                              :title "Poor connectivity"}
           (hga-icons/icon :solid :poor-connectivity :size :sm)]

          [:div.member-name
           member-name]]))]))
