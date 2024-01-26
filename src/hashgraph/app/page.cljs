(ns hashgraph.app.page
  (:require
   [garden.core :refer [css ]]
   [garden.stylesheet :refer [at-keyframes]]
   [garden.color :as gc]
   [clojure.string :as str]
   [goog.string :refer [format]]
   [hashgraph.app.state :as hga-state]
   [hashgraph.app.utils :refer [->below-view? ->above-view?]:as hga-utils]
   [hashgraph.app.events :refer [evt-view-r
                                 evt-view-s
                                 wit-view-r
                                 vote-view-r
                                 vote-view-circumferance
                                 evt-padding
                                 idx-view-position-x
                                 evt-view-position-x
                                 t->view-position-y
                                 evt-view-position-y
                                 evt-view-position
                                 wit-view-r
                                 vote-view-circumferance
                                 vote-view-stroke-width]
    :as hga-events]
   [hashgraph.app.playback :as hga-playback]
   [hashgraph.app.infini-events :as hga-infini-events]
   [hashgraph.app.analysis :as hga-analysis]
   [hashgraph.app.inspector :refer [log! unlog! log-set!
                                    inspect! uninspect! toggle-inspect!
                                    inspectable
                                    view]
    :rename {view inspector-view}
    :as hga-inspector]
   [hashgraph.main :as hg]
   [hashgraph.app.avatars :as hga-avatars]
   [hashgraph.members :as hg-members]
   [hashgraph.utils
    :refer [merge-attr-maps color-rgba-str timing *->time*]
    :refer-macros [defn* l]]
   [rum.core :as rum]
   [taoensso.timbre :refer-macros [spy info]]
   [taoensso.tufte :as tufte :refer [defnp fnp p profiled profile]]
   [cljs.math :refer [floor ceil pow]]
   [clojure.set :as set]))

(def animate-scale
  [:animateTransform
   {:attributeName "transform"
    :attributeType "XML"
    :type "scale"
    :from "1"
    :to "1.5"
    :dur "0.5s"
    #_#_:repeatCount 1}])

(def styles
  [(at-keyframes "appear"
                 [:from {:transform "scale(0)"}]
                 [:to {:transform "scale(1)"}])

   [:html {:height "100%" :overflow-y :hidden}]
   [:body {:height "100%" :overflow-y :hidden}]
   [:#root {:height "100%" :overflow-y :hidden}]
   [:#view {:display :flex}]
   [:#viz-wrapper {:height "100vh" :width "50vw" :overflow-y :scroll}]
   [:#viz {:width "100%"}
    [:.inspectable {:transform-box :fill-box
                    :transform-origin :center}
     [:&.peeked {:scale 1.3}]
     [:&.dimm {:opacity 0.33}]]]
   [:.event {:transition       "0.5s"
             ;; :animation-name "appear"
             ;; :animation-duration "0.5s"
             ;; :animation-fill-mode "both"
             }
    [:.witness {:opacity 0.30}]

    [:.round {:color     "gray"
              :font-size "14px"}]

    [:.stake-map {:opacity 0.33}]
    [:.votes {:opacity 0.66}
     [:.vote {}]]

    [:&.r-final
     [:.witness {:opacity 1}]
     [:.round {:color "black"
               :font-size "18px"
               :font-weight :bold}]]

    [:&.r-concluded
     [:.votes {:opacity 1}]]]])

(rum/defc event-info-refs-view < rum/static
  [{:event-info/keys [coords event opacity]}]
  (let [[x y] coords]
    [:g.refs
     (when-let [[self-parent-x self-parent-y] (some-> (hg/self-parent event) evt-view-position)]
       (let [#_#_line-highlighted? (and (highlighted? evt)
                                    (highlighted? (hg/self-parent evt)))]
         [:line {:opacity opacity
                 :x1    x
                 :y1    y
                 :x2    self-parent-x
                 :y2    self-parent-y
                 :style {:stroke       :lightgray #_(if line-highlighted? :blue :lightgray)
                         :stroke-width "2px"}}]))

     (when-let [[parent-x parent-y] (some-> (hg/other-parent event) evt-view-position)]
       (let [#_#_line-highlighted? (and (highlighted? evt)
                                    (highlighted? (hg/parent evt)))]
         [:line {:opacity opacity
                 :x1    x
                 :y1    y
                 :x2    parent-x
                 :y2    parent-y
                 :style {:stroke       :lightgray #_(if line-highlighted? :blue :lightgray)
                         :stroke-width "2px"}}]))]))

(def vote-circumferance-start+for+end
  (memoize
   (fn [member-name stake-map]
     (if (nil? member-name)
       [0 0 0]
       (let [prev-voter-idx                          (-> member-name hg-members/member-name->person :member/idx dec)
             prev-voter-name                         (some (fn [person] (when (= (:member/idx person) prev-voter-idx)
                                                                          (:member/name person)))
                                                           hg-members/people)
             [_ _ prev-voter-end-vote-circumferance] (vote-circumferance-start+for+end prev-voter-name stake-map)
             start-vote-circumferance                prev-voter-end-vote-circumferance
             stake                                   (get stake-map member-name)
             vote-circumferance                      (-> vote-view-circumferance
                                                         (/ hg/total-stake)
                                                         (* stake))
             end-vote-circumferance                  (+ start-vote-circumferance vote-circumferance)]
         [start-vote-circumferance vote-circumferance end-vote-circumferance])))))


(def event-view-key-fn {:key-fn (fn [{:event-info/keys [coords]}] (str coords))})

(rum/defcs event-view < rum/static rum/reactive event-view-key-fn
  [state {:event-info/keys [event round r-concluded? coords opacity share-stake? color fill witness? stake-map votes received-event] :as event-info}]
  (let [{r        :round/number
         r-final? :round/final?} round
        [x y] coords
        share-stake? (some-> event :event/tx :tx/fn-id (= :share-stake))]
    [:g {:opacity opacity}
     [:g.event {:class     [(when r-final? "r-final")
                            (when r-concluded? "r-concluded")
                            (when share-stake? "tx-share-stake")]
                :width     (* wit-view-r 2)
                :height    (* wit-view-r 2)
                ;; :transform  (str "translate(" delta-x "," delta-y ")")
                #_#_:style {
                            :transition "0.5s all 0.5s"}}

      (when witness?
        [:circle.witness
         {:r            wit-view-r
          :cx           x
          :cy           y
          :stroke       :black
          :stroke-width 1
          :fill         :transparent}])

      (when stake-map
        [:g.stake-map (inspectable stake-map)
         (for [[stake-holder _stake-holded] stake-map]
           [:g.stake {:key stake-holder}
            (let [member                                                                (-> stake-holder hg-members/member-name->person)
                  [start-vote-circumferance vote-circumferance _end-vote-circumferance] (vote-circumferance-start+for+end stake-holder stake-map)]
              [:circle {:r                vote-view-r
                        :cx               x
                        :cy               y
                        :fill             :transparent
                        :stroke           (color-rgba-str (:member/color-rgb member) 1)
                        :stroke-width     vote-view-stroke-width
                        :stroke-dasharray (str "0 " start-vote-circumferance " " vote-circumferance " " vote-view-circumferance)}])])])

      (when votes
        [:g.votes
         (for [{:vote/keys [from-event value] :as vote} votes]
           (let [member-name                                                           (:event/creator from-event)
                 member                                                                (-> member-name hg-members/member-name->person)
                 [start-vote-circumferance vote-circumferance _end-vote-circumferance] (vote-circumferance-start+for+end member-name (:vote/stake-map vote))]
             (assert stake-map (:vote/stake-map vote))
             [:g.vote (merge {:key member-name} (inspectable vote))
              [:circle {:r                vote-view-r
                        :cx               x
                        :cy               y
                        :fill             :transparent
                        :stroke           (if value
                                            (color-rgba-str (:member/color-rgb member) 1)
                                            "cyan")
                        :stroke-width     vote-view-stroke-width
                        :stroke-dasharray (str "0 " start-vote-circumferance " " vote-circumferance " " vote-view-circumferance)}]]))])

      [:circle.event
       {:r            evt-view-r
        :cx           x
        :cy           y
        :stroke       color
        :stroke-width 1
        :fill         fill}]

      (when share-stake?
        [:image.event-tx
         {:width  evt-view-s
          :height evt-view-s
          :x      x
          :y      y
          :href   "https://www.svgrepo.com/show/446085/transfer.svg"
          :style  {:transform-box    "fill-box"
                   :transform-origin "center"
                   :transform        "scale(0.7)"
                   ;; :filter "invert(100%)"
                   }
          #_      "url(https://vectorified.com/images/invite-icon-3.png)"}])

      [:text.round
       {:x (+ x (/ wit-view-r 2))
        :y (+ y (/ (- wit-view-r) 2))}
       r]]

     (when received-event
       (let [{:received-event/keys [event middle-learned-events received-time concluded-round r-idx color]} received-event

             received-y (t->view-position-y received-time)
             received-x (-> (idx-view-position-x (count hg-members/names))
                            (+ (* r-idx hga-events/evt-view-s)))]
         [:g

          #_
          (let [parent-evt (:received-event/event (:received-event/prev-received-event received-event))
                [r-x r-y]  (evt-view-position evt)
                [p-x p-y]  (evt-view-position parent-evt)]
            [:line
             {:x1           r-x
              :y1           r-y
              :x2           p-x
              :y2           p-y
              :stroke       (:received-event/color received-event)
              :stroke-width "2px"}])

          (when (hga-inspector/->in-peeked? received-event)
            (for [mle middle-learned-events]
              [:line {:x1           (evt-view-position-x mle)
                      :y1           (evt-view-position-y mle)
                      :x2           received-x
                      :y2           received-y
                      :stroke       "blue"
                      :stroke-width "2px"}]))

          [:g.receivend-event-view (merge {:key (str (:coords event-info))}
                                          (inspectable received-event))
           [:circle.event
            {:r            evt-view-r
             :cx           received-x
             :cy           received-y
             :stroke       color
             :stroke-width 1
             :fill         fill}]]]))]))




(def *rendered-evts
  (rum/derived-atom [hga-playback/*c->hg hga-state/*viz-scroll-top] ::*rendered-evts
    (fn [creator->hg scroll-top]
      (->> creator->hg
           vals
           (mapcat (fn [hg] (->> hg
                                 (iterate hg/self-parent)
                                 (take-while some?)
                                 (drop-while (fn [evt] (when-let [sp-evt (hg/self-parent evt)]
                                                         (->above-view? (evt-view-position-y sp-evt) scroll-top)))) ;; more costly as deeper in history you get
                                 (take-while (fn [evt] (not (->below-view? (evt-view-position-y evt) scroll-top)))))))))))

(def *?alice-tip
  (rum/derived-atom [hga-playback/*c->hg] ::*alice-tip
    (fn [c->hg]
      (hg/creator-hg-map->?alice-tip c->hg))))

(def *?last-concluded-round
  (rum/derived-atom [*?alice-tip] ::*last-concluded-round
    (fn [?alice-tip]
      (some-> ?alice-tip hg/->concluded-round))))

(def *?last-received-event
  (rum/derived-atom [*?last-concluded-round] ::*last-received-event
    (fn [?last-concluded-round]
      (some-> ?last-concluded-round :concluded-round/last-received-event))))

(def ?received-event->event->received-event
  (memoize
   (fn [?received-event]
     (if (nil? ?received-event)
       {}
       (into {}
             (map (juxt :received-event/event identity))
             (take-while some? (iterate :received-event/prev-received-event ?received-event)))))))

(defn event->color [event]
  (-> event
      hg/event->person
      :member/color-rgb
      color-rgba-str))

(defn* ^:memoizing ->enriched-event-info
  [{:event-info/keys [event
                      round
                      witness?
                      to-receive-votes?
                      receives-votes?
                      received-votes?
                      voting-by
                      ?concluded-in-cr
                      received-event]
    :as              event-info}]
  (let [coords (evt-view-position event)
        color  (event->color event)


        {r        :round/number
         r-final? :round/final?
         r-cr     :round/cr} round

        ?atop-cr (when to-receive-votes?
                   (some->> r-cr
                            (iterate :concluded-round/prev-concluded-round)
                            (take-while some?)
                            (some (fn [cr] (when (< (:concluded-round/r cr) (:round/number round))
                                             cr)))))

        fill
        (if-let [{:keys [received-event/color]} received-event]
          color
          "white")]

    (cond-> event-info
      :always
      (merge #:event-info{:coords       coords
                          :color        color
                          :fill         fill
                          :r-concluded? received-votes?})
      to-receive-votes?
      (assoc :event-info/stake-map (hg/concluded-round->stake-map ?atop-cr))

      (or received-votes? receives-votes?)
      (assoc :event-info/votes (:concluded-voting/votes (hg/->?concluded-voting voting-by event ?atop-cr))))))

(def *rendered-evt-infos
  (rum/derived-atom [*rendered-evts *?alice-tip *?last-concluded-round *?last-received-event] ::*rendered-evt-infos
    (fn [rendered-evts ?alice-tip ?last-concluded-round ?last-received-event]
      (let [event->received-event (?received-event->event->received-event ?last-received-event)]
        (->> rendered-evts
             (map (fn [event]
                    (let [id (hga-playback/event->event-id event)
                          {r        :round/number
                           r-final? :round/final?
                           r-cr     :round/cr :as round} (hg/->round event ?last-concluded-round) ;; been very costly :hashgraph.app.page/round

                          witness?          (hg/witness? event r-cr)
                          to-receive-votes? (and witness?
                                                 r-final?)
                          ?concluded-in-cr  (when to-receive-votes?
                                              (some->> ?last-concluded-round
                                                       (iterate :concluded-round/prev-concluded-round)
                                                       (take-while some?)
                                                       (some (fn [cr] (when (= (:concluded-round/r cr) r) cr)))))

                          received-votes? (boolean ?concluded-in-cr)
                          receives-votes? (and to-receive-votes?
                                               (not received-votes?)
                                               ?alice-tip
                                               (hg/ancestor? ?alice-tip event)) ;; more costly the further tip is from the roots
                          ?received-event (event->received-event event)
                          voting-by       (if received-votes?
                                            (-> ?concluded-in-cr :concluded-round/witness-concluded)
                                            ?alice-tip)]
                      ;; concluded stuff will be memoized
                      (->enriched-event-info (cond-> #:event-info{:id                id
                                                                  :event             event
                                                                  :round             round
                                                                  :witness?          witness?
                                                                  :to-receive-votes? to-receive-votes?
                                                                  :received-votes?   received-votes?
                                                                  :receives-votes?   receives-votes?
                                                                  :voting-by         voting-by
                                                                  :?concluded-in-cr  ?concluded-in-cr}
                                               ?received-event (assoc :event-info/received-event ?received-event)))))))))))

;; https://gizma.com/easing/#easeInOutCubic
(defn easeInOutCubic [x]
  (if (< x 0.5)
    (* 4 x x x)
    (-> 1
        (- (/ (pow (-> -2 (* x) (+ 2)) 3)
              2)))))

(def *last-tick-time-end (atom (cljs.core/system-time)))
(def *rendered-transitioning-evt-infos
  (rum/derived-atom [*rendered-evt-infos *last-tick-time-end] ::*rendered-animated-evts
    (fn [rendered-evt-infos _last-tick-time-end]
      (let [t (cljs.core/system-time)
            ;; dt (- t last-tick-time-end)
            result
            (->> rendered-evt-infos
                 (map (fn [{:event-info/keys [id event coords] :as evt-info}]
                        (let [transition-time-start (or (get hga-playback/id->transition-time-start id)
                                                        (do (assoc! hga-playback/id->transition-time-start id t)
                                                            t))
                              dt                    (max 1 (- t transition-time-start))]
                          (if (< dt hga-playback/tt)
                            (let [time-in     (/ dt hga-playback/tt)
                                  timing-fn   easeInOutCubic #_(fn [t-in] (/ t-in (- 1 (/ t-in 5))))
                                  trans-in    (timing-fn time-in)
                                  op          (-> event hg/other-parent)
                                  from-coords (-> op evt-view-position)
                                  coords-now  (mapv (fn [from to] (-> from
                                                                      (+ (* (- to from) trans-in))))
                                                  from-coords coords)]
                              (assoc evt-info
                                     :event-info/coords  coords-now
                                     :event-info/opacity trans-in))
                            evt-info)))))]
        (js/setTimeout #(reset! *last-tick-time-end t) 1) ;; trigger next tick
        result))))

(rum/defc viz < rum/reactive
  []
  (let [rendered-evts      (rum/react *rendered-evts)
        rendered-evt-infos (rum/react *rendered-transitioning-evt-infos #_*rendered-evt-infos)]
    [:svg#viz {:height (hga-infini-events/->viz-height (rum/react hga-playback/*c->hg))}
     [:g.events-view
      (for [evt-info rendered-evt-infos]
        (event-info-refs-view evt-info))

      (for [evt-info rendered-evt-infos]
        (event-view evt-info))]]))

#_
(rum/defc member-stake-view [member stake-map]
  (let [svg-size (+ (* vote-view-r 2) 10)]
    [:svg {:width  svg-size
           :height svg-size}
     [:g.member-stake
      (let [stake-holder (:member/name member)]
        (let [member                                                                (-> stake-holder hg-members/member-name->person)
              [start-vote-circumferance vote-circumferance _end-vote-circumferance] (vote-circumferance-start+for+end stake-holder stake-map)]
          [:circle {:r                vote-view-r
                    :cx               (/ svg-size 2)
                    :cy               (/ svg-size 2)
                    :fill             :transparent
                    :stroke           (color-rgba-str (:member/color-rgb member) 1)
                    :stroke-width     vote-view-stroke-width
                    :stroke-dasharray (str "0 " start-vote-circumferance " " vote-circumferance " " vote-view-circumferance)}]))]]))

(rum/defc members-view < rum/reactive []
  (let [last-cr (rum/react *?last-concluded-round)
        stake-map (hg/concluded-round->stake-map last-cr)]
    [:div.members {:style {:position :absolute
                           :bottom "0px"
                           :height "70px"
                           :background-color "rgba(255,255,255,0.5)"}}
     (for [[member-name _stake] stake-map]
       (let [member (hg-members/member-name->person member-name)]
         [:div.member {:style {:position :absolute
                               :left (idx-view-position-x (:member/idx member))
                               :transform "translateX(-50%)"
                               :display :flex
                               :flex-direction :column
                               :align-items :center}}
          #_(member-stake-view member stake-map)
          (case (:member/gender member)
            :male   (hga-avatars/male-avatar   (color-rgba-str (:member/color-rgb member) 1))
            :female (hga-avatars/female-avatar (color-rgba-str (:member/color-rgb member) 1))
            [:div "unknown gender"])
          [:div.member-name member-name]]))]))


(rum/defc viz-wrapper
  <
  rum/reactive
  {:did-mount
   (fn [state]
     (.addEventListener (rum/dom-node state) "scroll" #(reset! hga-state/*viz-scroll-top (.-scrollTop (.-target %))))
     state)}
  []
  [:div#viz-wrapper
   (viz)])

(rum/defc viz-section-controls-view < rum/reactive
  []
  [:div {:style {:position :absolute
                  :top "0px"}}
   [:button {:on-click #(swap! hga-state/*infini-events-enabled? not)}
    (str (if (rum/react hga-state/*infini-events-enabled?) "disable" "enable") " issue events to fill")]
   [:button {:on-click #(swap! hga-state/*playback-attached-to-scroll? not)}
    (str (if (rum/react hga-state/*playback-attached-to-scroll?) "detach" "attach") " playback to scroll")]
   (hga-playback/playback-controls-view)])

(rum/defc viz-section-view
  []
  [:div {:style {:position :relative}}
   (viz-section-controls-view)

   (viz-wrapper)

   (members-view)])

(rum/defc view []
  [:div#view
   [:style (css styles)]
   #_(hga-analysis/graph-view)
   (inspector-view)
   (viz-section-view)])
