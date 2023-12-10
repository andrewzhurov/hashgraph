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

(def evt-view-position
  (memoize
   (fn [evt]
     (let [x (-> (-> evt :creator hg-members/members :member/idx)
                 (* (+ evt-padding evt-view-s))
                 (+ evt-view-r evt-padding))
           y (-> (hg/index evt)
                 (* (+ evt-view-s evt-padding))
                 (+ evt-view-r evt-padding))]
       [x y]))))

(defn evts-height [evts]
  (-> evts
      hg/tips
      (->> (map evt-view-position))
      (some->> not-empty (apply max))
      (or 0)))

(defonce *hovered-evt (atom nil))

(def color-rgba-str
  (memoize
   (fn ([rgb-vec] (color-rgba-str rgb-vec 1))
       ([[red green blue] alpha] (str "rgba(" red "," green "," blue "," alpha  ")")))))

(rum/defc event-refs-view < rum/static
  [evt]
  (let [[x y] (evt-view-position evt)]
    [:g.refs
     (when-let [[self-parent-x self-parent-y] (some-> (hg/self-parent evt) evt-view-position)]
       (let [#_#_line-highlighted? (and (highlighted? evt)
                                    (highlighted? (hg/self-parent evt)))]
         [:line {:x1    x
                 :y1    y
                 :x2    self-parent-x
                 :y2    self-parent-y
                 :style {:stroke       :lightgray #_(if line-highlighted? :blue :lightgray)
                         :stroke-width "2px"}}]))

     (when-let [[parent-x parent-y] (some-> (hg/parent evt) evt-view-position)]
       (let [#_#_line-highlighted? (and (highlighted? evt)
                                    (highlighted? (hg/parent evt)))]
         [:line {:x1    x
                 :y1    y
                 :x2    parent-x
                 :y2    parent-y
                 :style {:stroke       :lightgray #_(if line-highlighted? :blue :lightgray)
                         :stroke-width "2px"}}]))]))

(rum/defc event-view < rum/static
  [evt]
  (let [witness? (hg/witness? evt)
        [x y]    (evt-view-position evt)
        round    (hg/round evt)
        member   (-> evt :creator hg-members/members)
        color    (color-rgba-str (:member/color-rgb member))]
    [:g.event
     [:circle {:r                  evt-view-r
               :cx                 x
               :cy                 y
               :stroke             color
               #_                  (cond (hg/witness? evt)                                     "red"
                                         (some (fn [evt-y] (hg/see-many-see? evt evt-y)) evts) "orange"
                                         (some (fn [evt-y] (hg/see? evt evt-y)) evts)          "yellow"
                                         :else                                                 "gray")
               :stroke-width       (if witness? 4 1)
               :fill               :white
               #_#_:on-mouse-enter #(reset! *hovered-evt evt)
               #_#_:on-mouse-leave #(reset! *hovered-evt nil)
               }]

     [:text.round
      {:x         (+ x evt-view-r)
       :y         (- y evt-view-r)
       :font-size "12px"} round]]))

(rum/defcs viz <
  rum/static
  rum/reactive
  hga-infini-events/mixin
  [_state evts]
  (let [hovered-evt   (rum/react *hovered-evt)
        highlighted?  (fn [evt]
                        (when-let [hovered-evt hovered-evt]
                          (or (= hovered-evt evt)
                              (hg/ancestor? hovered-evt evt))))
        top-witnesses (-> evts
                          hg/tips
                          (->> (map hg/self-witness)))]

    #_(when hovered-evt
      (spy
       (-> evts hg/tips
           (->> (map (fn [tip]
                       {:tip tip
                        :votes (hg/votes tip hovered-evt)
                        :voting-concluded? (hg/voting-concluded? tip hovered-evt)
                        :votes-fract-true (hg/votes-fract-true tip hovered-evt)}))))))

    [:div#viz-wrapper
     [:svg#viz
      (for [evt  evts]
        (event-refs-view evt))

      [:g.events
       (for [evt evts]
         [:g
          (event-view evt)

          (when (hg/witness? evt)
            [:g.event-received-votes
             (for [top-witness top-witnesses]
               (let [member                   (-> top-witness :creator hg-members/members)
                     vote-circumferance       (/ vote-view-circumferance (count hg-members/members))
                     vote-circumferance-start (-> vote-view-circumferance
                                                  (/ hg-members/members-count)
                                                  (* (:member/idx member)))
                     [x y] (evt-view-position evt)]
                 [:circle {:r                vote-view-r
                           :cx               x
                           :cy               y
                           :fill             :transparent
                           :stroke           (color-rgba-str (:member/color-rgb member)
                                                             #_(cond (hg/see-many-see? top-witness evt) 1
                                                                     (hg/see? top-witness evt)          0.5
                                                                     :else                              0)
                                                             (or (and (hg/voting-concluded? top-witness evt)
                                                                      (or (and (hg/vote top-witness evt) 1)
                                                                          0))
                                                                 (hg/votes-fract-true top-witness evt)))
                           :stroke-width     (* vote-view-r 2)
                           :stroke-dasharray (str "0 " vote-circumferance-start " " vote-circumferance " " vote-view-circumferance)
                           #_#_:transform    (str "rotate("(* (:member/idx seen-by-member)
                                                              (/ 360 hg-members/members-count)) ")")}]))])])]]]))

(rum/defc view < rum/reactive
  []
  [:<>
   [:style (css styles)]
   (viz (rum/react hga-events/*evts))])
