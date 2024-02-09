(ns hashgraph.app.page
  (:require-macros [hashgraph.utils.js-map :as js-map])
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [cljs.math :refer [round floor ceil pow]]
   [garden.core :as garden]
   [garden.stylesheet :refer [at-keyframes]]
   [garden.color :as gc]
   [garden.units :refer [px]]
   [goog.string :refer [format]]
   [goog.object :as gobject]
   [rum.core :as rum]
   [taoensso.timbre :refer-macros [spy info]]
   [taoensso.tufte :as tufte :refer [defnp fnp p profiled profile]]

   [hashgraph.app.state :as hga-state]
   [hashgraph.app.events :as hga-events]
   [hashgraph.app.view :as hga-view]
   [hashgraph.app.members :as hga-members]
   [hashgraph.app.playback :as hga-playback]
   [hashgraph.app.transitions :as hga-transitions]
   [hashgraph.app.infini-events :as hga-infini-events]
   [hashgraph.app.analysis :as hga-analysis]
   [hashgraph.app.tutorial :as hga-tutorial]
   [hashgraph.app.inspector :refer [inspectable]
    :as hga-inspector]
   [hashgraph.app.icons :as hga-icons]
   [hashgraph.app.timing :as hga-timing]
   [hashgraph.app.utils :as hga-utils]
   [hashgraph.utils.core
    :refer [log! merge-attr-maps color-rgba-str timing *->time*]
    :refer-macros [defn* l letl]
    :as utils]
   [hashgraph.main :as hg]
   [hashgraph.members :as hg-members]))

(def animate-scale
  [:animateTransform
   {:attributeName   "transform"
    :attributeType   "XML"
    :type            "scale"
    :from            "1"
    :to              "1.5"
    :dur             "0.5s"
    #_#_:repeatCount 1}])

(def styles-vertical-mode
  (into
   [[:#viz
     [:#render {:transition "height 0.5s"}]]]
   hga-members/styles-vertical-mode))

(def styles-horizontal-mode
  (into
   [[:body {:overflow-y :hidden}]
    [:#root {:overflow-y :hidden}]
    [:#root-view {:overflow-y :hidden}]
    [:#page-view {:flex-direction :row
                  :overflow-x     :scroll
                  :overflow-y     :hidden}]

    (let [flex-equaly "1 1 0"]
      [:#viz-section {:height         "100vh"
                      :display        :flex
                      :flex-direction :column}
       [:#tutorial {:flex flex-equaly}]
       [:#viz {:flex flex-equaly}
        [:#render {:transition "width 0.5s"}]]
       [:#inspector {:flex     flex-equaly
                     :position :sticky
                     :left     "0px"
                     :width    "100vw"}]

       [:#viz {:position :relative}
        ]])]
   hga-members/styles-horizontal-mode))

(def styles
  [(at-keyframes "appear"
                 [:from {:transform "scale(0)"}]
                 [:to {:transform "scale(1)"}])

   [:html {}]
   [:body {:margin "0px"}]
   [:#root]
   [:#root-view]

   [:#page-view {:height  "100vh"
                 :width   "100vw"
                 :display :flex}]

   [:#controls {:width            "36px"
                :height           "36px"
                :position         :fixed
                :top              "5px"
                :right            "5px"
                :padding          "4px"
                :border           "1px solid lightgray"
                :border-radius    "4px"
                :background-color "white"}
    [:.toggler {:width           (px hga-view/controls-icon-size)
                :height          (px hga-view/controls-icon-size)
                :background      "url('https://www.svgrepo.com/show/532195/menu.svg')"
                :background-size :cover
                :border          :none
                :cursor          :pointer
                :padding         "0px"}]]

   [:#home {:width      "100vw"
            :height     "100vh"
            :min-width  "100vw"
            :min-height "100vh"}]

   #_
   [:.members {:position         :absolute
               :bottom           "0px"
               :height           (str hga-view/members-height "px")
               :background-color "rgba(255,255,255,0.5)"}
    [:.member {:position       :absolute
               :transform      "translateX(-50%)"
               :display        :flex
               :flex-direction :column
               :align-items    :center
               :transition     "opacity 0.4s"
               :opacity        0}
     [:&.active {:opacity 1}]]]

   [:#viz
    [:.inspectable {:transform-box    :fill-box
                    :transform-origin :center}
     [:&.peeked {:scale 1.3}]
     [:&.dimm {:opacity 0.33}]]]

   [:.event {}
    [:.witness {:opacity 0.30}]

    [:.round {:color     "gray"
              :font-size "14px"}]

    [:.stake-map {:opacity 0.33}]
    [:.votes {:opacity 0.66}
     [:.vote {}]]

    [:&.r-final
     [:.witness {:opacity 1}]
     [:.round {:color       "black"
               :font-size   "18px"
               :font-weight :bold}]]

    [:&.r-concluded
     [:.votes {:opacity 1}]]]])

(def key-fn-by-hash {:key-fn (fn [arg] (-hash arg))})
(def static-by-hash {:should-update (fn [old-state new-state]
                                      (not= (-hash (first (:rum/args old-state)))
                                            (-hash (first (:rum/args new-state)))))})

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
             vote-circumferance                      (-> hga-view/vote-circumferance
                                                         (/ hg/total-stake)
                                                         (* stake))
             end-vote-circumferance                  (+ start-vote-circumferance vote-circumferance)]
         [start-vote-circumferance vote-circumferance end-vote-circumferance])))))

(def event-view-key-fn {:key-fn (fn [{:event-info/keys [event]}] (-hash event))})

(rum/defcs event-view <
  event-view-key-fn
  static-by-hash
  rum/reactive
  (hga-transitions/mixin ::view-state    (fn [event-info] (some-> event-info :event-info/event -hash)))
  (hga-transitions/mixin ::sp-view-state (fn [event-info] (some-> event-info :event-info/event hg/self-parent -hash)))
  (hga-transitions/mixin ::op-view-state (fn [event-info] (some-> event-info :event-info/event hg/other-parent -hash)))
  [{::keys [view-state sp-view-state op-view-state]} {:event-info/keys [event round r-concluded? share-stake? color witness? stake-map votes received-event] :as event-info}]
  #_(log! [:render-event-view @hga-playback/*frame] (-hash event))
  (let [x (js-map/get view-state :x)
        y (js-map/get view-state :y)
        opacity (js-map/get view-state :opacity)
        {r        :round/number
         r-final? :round/final?} round
        share-stake?             (some-> event :event/tx :tx/fn-id (= :share-stake))]
    [:g.event-info {:opacity opacity}

     [:g ;; refs
      (when sp-view-state
        [:line {hga-view/x1   x
                hga-view/y1   y
                hga-view/x2   (js-map/get sp-view-state :x)
                hga-view/y2   (js-map/get sp-view-state :y)
                :stroke       :lightgray
                :stroke-width "2px"
                :style        {:opacity (- 1 (js-map/get view-state :fill-opacity))}}])

      (when op-view-state
        [:line {hga-view/x1   x
                hga-view/y1   y
                hga-view/x2   (js-map/get op-view-state :x)
                hga-view/y2   (js-map/get op-view-state :y)
                :stroke       :lightgray
                :stroke-width "2px"
                :style        {:opacity (- 1 (js-map/get view-state :fill-opacity))}}])]

     [:g.event (merge-attr-maps
                {:class  [(when r-final? "r-final")
                          (when r-concluded? "r-concluded")
                          (when share-stake? "tx-share-stake")]
                 :width  (* hga-view/wit-r 2)
                 :height (* hga-view/wit-r 2)}
                (inspectable event))

      (when witness?
        [:circle.witness
         {hga-view/cx   x
          hga-view/cy   y
          :r            hga-view/wit-r
          :stroke       :black
          :stroke-width 1
          :fill         :transparent}])

      (when stake-map
        [:g.stake-map (inspectable stake-map)
         (for [[stake-holder _stake-holded] stake-map]
           [:g.stake {:key stake-holder}
            (let [member                                                                (-> stake-holder hg-members/member-name->person)
                  [start-vote-circumferance vote-circumferance _end-vote-circumferance] (vote-circumferance-start+for+end stake-holder stake-map)]
              [:circle {hga-view/cx       x
                        hga-view/cy       y
                        :r                hga-view/vote-r
                        :fill             :transparent
                        :stroke           (color-rgba-str (:member/color-rgb member) 1)
                        :stroke-width     hga-view/vote-stroke-width
                        :stroke-dasharray (str "0 " start-vote-circumferance " " vote-circumferance " " hga-view/vote-circumferance)}])])])

      (when votes
        [:g.votes
         (for [{:vote/keys [from-event value] :as vote} votes]
           (let [member-name                                                           (:event/creator from-event)
                 member                                                                (-> member-name hg-members/member-name->person)
                 [start-vote-circumferance vote-circumferance _end-vote-circumferance] (vote-circumferance-start+for+end member-name (:vote/stake-map vote))]
             (assert stake-map (:vote/stake-map vote))
             [:g.vote (merge {:key member-name} (inspectable vote))
              [:circle {hga-view/cx       x
                        hga-view/cy       y
                        :r                hga-view/vote-r
                        :fill             :transparent
                        :stroke           (if value
                                            (color-rgba-str (:member/color-rgb member) 1)
                                            "cyan")
                        :stroke-width     hga-view/vote-stroke-width
                        :stroke-dasharray (str "0 " start-vote-circumferance " " vote-circumferance " " hga-view/vote-circumferance)}]]))])

      ;; always rendering white background to hide refs
      [:circle.event
       {hga-view/cx   x
        hga-view/cy   y
        :r            hga-view/evt-r
        :stroke       color
        :stroke-width 1
        :fill         "white"}]

      ;; received event color
      (when-let [{:keys [red green blue]} (js-map/get view-state :fill)]
        (let [fill-opacity (js-map/get view-state :fill-opacity)
              fill (str "rgba("red","green","blue","fill-opacity")")]
          [:circle.received-event
           {hga-view/cx   x
            hga-view/cy   y
            :r            hga-view/evt-r
            :stroke       color
            :stroke-width 1
            :fill         fill}]))

      (when share-stake?
        (hga-icons/transfer {hga-view/x (- x hga-view/evt-r)
                             hga-view/y (- y hga-view/evt-r)
                             :width     hga-view/evt-s
                             :height    hga-view/evt-s
                             :fill      (gc/hsl->hex (gc/lighten (gc/hsl 0 0 0) (* 100 (js-map/get view-state :fill-opacity))))}))

      [:text.round
       (if-not hga-view/view-mode-horizontal?
         {hga-view/x (+ x (/ hga-view/wit-r 2))
          hga-view/y (+ y (/ (- hga-view/wit-r) 2))}
         {hga-view/x (+ x (/ (- hga-view/wit-r) 2))
          hga-view/y (+ y (/ hga-view/wit-r 2))})
       r]]]))

(defn* ^:memoizing ?received-event->event->received-event
  [?received-event]
  (if (nil? ?received-event)
    (hash-map)
    (let [prev (?received-event->event->received-event (:received-event/prev-received-event ?received-event))]
      (assoc prev (:received-event/event ?received-event) ?received-event))))

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
  (let [color            (event->color event)
        {r-cr :round/cr} round

        ?atop-cr (when to-receive-votes?
                   (some->> r-cr
                            (iterate :concluded-round/prev-concluded-round)
                            (take-while some?)
                            (some (fn [cr] (when (< (:concluded-round/r cr) (:round/number round))
                                             cr)))))]

    (-> event-info
        (assoc :event-info/color color
               :event-info/r-concluded? received-votes?)
        (cond->
          to-receive-votes?
          (assoc :event-info/stake-map (hg/concluded-round->stake-map ?atop-cr))

          (or received-votes? receives-votes?)
          (assoc :event-info/votes (:concluded-voting/votes (hg/->?concluded-voting voting-by event ?atop-cr)))))))

(def *rendered-evt-infos
  (rum/derived-atom [hga-playback/*playback] ::*rendered-evt-infos
    (fn [{:keys [behind>
                 played<   ;; asc
                 rewinded< ;; asc
                 ]}]
      (let [played>               (reverse played<)
            ?main-tip             (hg/events>->main-tip played>)
            ?last-concluded-round (some-> ?main-tip hg/->concluded-round)
            _                     (reset! hga-transitions/*last-concluded-round ?last-concluded-round)
            ?last-received-event  (some-> ?last-concluded-round :concluded-round/last-received-event)
            event->received-event (?received-event->event->received-event ?last-received-event)
            ->event-info          (fn [event]
                                    (let [{r        :round/number
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
                                                               ?main-tip
                                                               (hg/ancestor? ?main-tip event)) ;; more costly the further tip is from the roots
                                          ?received-event (event->received-event event)
                                          voting-by       (if received-votes?
                                                            (-> ?concluded-in-cr :concluded-round/witness-concluded)
                                                            ?main-tip)]
                             ;; concluded stuff will be memoized
                             ;; TODO enhance memoization to memoize based on predicate
                             (->enriched-event-info (cond-> (hash-map :event-info/event             event
                                                                      :event-info/round             round
                                                                      :event-info/witness?          witness?
                                                                      :event-info/to-receive-votes? to-receive-votes?
                                                                      :event-info/received-votes?   received-votes?
                                                                      :event-info/receives-votes?   receives-votes?
                                                                      :event-info/voting-by         voting-by
                                                                      :event-info/?concluded-in-cr  ?concluded-in-cr)
                                                      ?received-event (assoc :event-info/received-event ?received-event)))))]
        [(->> behind>
              (take 10)
              (map ->event-info))
         (->> played>
              (map ->event-info))
         (->> (reverse rewinded<)
              (map ->event-info))]))))


(rum/defc viz < rum/reactive
  []
  (let [[behind-evt-infos> played-evt-infos> rewinded-evt-infos>] (rum/react *rendered-evt-infos)]
    [:div#viz
     (hga-members/view)
     [:svg#render {(if hga-view/view-mode-horizontal? :width :height) (hga-view/evt->viz-height (-> played-evt-infos> first :event-info/event))
                   (if hga-view/view-mode-horizontal? :height :width) hga-view/viz-size}
      [:g.events-view
       (for [evt-info (concat behind-evt-infos> played-evt-infos> rewinded-evt-infos>)]
         (event-view evt-info))]]]))

(rum/defc controls-view < rum/reactive []
  [:div#controls
   [:button.toggler]
   #_#_[:button {:on-click #(swap! hga-state/*playback-attached-to-scroll? not)}
        (str (if (rum/react hga-state/*playback-attached-to-scroll?) "detach" "attach") " playback to scroll")]
   (hga-playback/playback-controls-view)])

(rum/defc viz-section-view
  []
  [:div#viz-section {:style {:position :relative}}
   (controls-view)
   #_(hga-tutorial/view)
   (viz)
   (hga-inspector/view)])

(rum/defc home-view []
  [:div#home
   (hga-utils/plug "home")])

(rum/defc page-view <
  {:did-mount
   (fn [state]
     (let [dom-node (rum/dom-node state)]
       (reset! hga-state/*viz-scroll-by! (fn [viz-px & {:keys [smooth?]}]
                                           (let [px (+ viz-px hga-view/window-size)]
                                             (.scrollBy dom-node (if smooth?
                                                                   (js-obj "top" px
                                                                           "behavior" "smooth")
                                                                   (js-obj "top" px))))))
       (reset! hga-state/*viz-scroll! (fn [viz-px & {:keys [smooth?]}]
                                        (let [px (+ viz-px hga-view/window-size)]
                                          #_(set! (.-scrollTop dom-node) px)
                                          (.scroll dom-node (if smooth?
                                                              (js-obj "top" px
                                                                      "behavior" "smooth")
                                                              (js-obj "top" px)))) ;; doesn't scroll pixel-perfect on zoom
                                        ))
       (.addEventListener dom-node "scroll"
                          ;; V may cause stale viz-scroll
                          (hga-utils/once-per-render
                           #(reset! hga-state/*viz-scroll (- (if hga-view/view-mode-horizontal?
                                                               (.-scrollLeft (.-target %))
                                                               (.-scrollTop (.-target %)))
                                                             hga-view/window-size)))))
     state)}
  []
  [:div#page-view
   #_(hga-analysis/graph-view)
   (home-view)
   (viz-section-view)])

(rum/defc view []
  [:div#root-view
   [:style (garden/css styles)]
   [:style (garden/css (if hga-view/view-mode-horizontal?
                         styles-horizontal-mode
                         styles-vertical-mode))]
   (page-view)])
