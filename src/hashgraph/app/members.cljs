(ns hashgraph.app.members
  (:require [rum.core :as rum]
            [garden.units :refer [px]]
            [hashgraph.main :as hg]
            [hashgraph.members :as hg-members]
            [hashgraph.app.view :as hga-view]
            [hashgraph.app.avatars :as hga-avatars]
            [hashgraph.app.playback :as hga-playback]
            [hashgraph.app.transitions :as hga-transitions]
            [hashgraph.utils.core
             :refer [merge-attr-maps color-rgba-str timing *->time*]
             :refer-macros [defn* l letl]
             :as utils]))

(def styles-horizontal-mode
  [[:#members {:position         :fixed
               :top              (px (/ hga-view/window-height 3))
               :right            (px 0)
               :width            (px hga-view/members-height)
               :height           (px (/ hga-view/window-height 3))
               :background-color "rgba(255,255,255,0.5)"}
    [:.member {:position         :absolute
               :left             (px hga-view/members-padding-y)
               :transition       (str "opacity " (/ hga-transitions/tt 2) "ms")
               :opacity          0}
     [:&.active {:opacity 1}]
     [:.member-name {:position :absolute
                     :transform "translateX(-50%)"}]]]])

(def styles-vertical-mode
  [])

(rum/defc view < rum/reactive []
  (let [?main-tip             (hg/events>->main-tip (reverse (rum/react hga-playback/*played<)))
        ?last-concluded-round (some-> ?main-tip hg/->concluded-round)
        stake-map             (hg/concluded-round->stake-map ?last-concluded-round)]
    [:div#members
     (for [member-name hg-members/names]
       (let [member    (hg-members/member-name->person member-name)
             active?   (contains? stake-map member-name)
             stake-pos (/ (get stake-map member-name) hg/total-stake)]
         [:div.member {:key   member-name
                       :class (when active? "active")
                       :style {(if hga-view/view-mode-horizontal? :top :left) (- (hga-view/idx->x (:member/idx member))
                                                                                 (/ hga-view/avatar-size 2))}}
          (case (:member/gender member)
            :male   (hga-avatars/male-avatar   (color-rgba-str (:member/color-rgb member) 1) (color-rgba-str (:member/color-rgb member) stake-pos))
            :female (hga-avatars/female-avatar (color-rgba-str (:member/color-rgb member) 1) (color-rgba-str (:member/color-rgb member) stake-pos))
            [:div "unknown gender"])
          [:div.member-name {:style {:color (if (hg-members/hardly-reachable-member-names member-name)
                                              "lightgray" "black")
                                     :left (/ hga-view/avatar-size 2)
                                     :margin-top "-6px"}}
           member-name]]))]))
