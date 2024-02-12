(ns hashgraph.app.page
  (:require-macros [hashgraph.utils.js-map :as js-map]
                   [hashgraph.app.inspector :refer [inspectable*]])
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [cljs.math :refer [round floor ceil pow]]
   [garden.core :as garden]
   [garden.selectors :as gs]
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
   [hashgraph.app.view :refer [t] :as hga-view]
   [hashgraph.app.members :as hga-members]
   [hashgraph.app.playback :as hga-playback]
   [hashgraph.app.transitions :refer [tt] :as hga-transitions]
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
    :refer-macros [defn* l letl merge-attr-maps*]
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
   [
    [:#page-view {:flex-direction :row
                  :overflow-x     :scroll
                  :overflow-y     :hidden}]

    [:#viz-section {:height         "100vh"
                    :display        :flex
                    :flex-direction :column}
     [:>* {:flex "1 1 0"}]
     [:.bins-view {:position :sticky
                   :left     "0px"
                   :width    "100vw"}
      (let [controls-end-height (+ hga-view/scrollbar-height hga-view/control-margin hga-view/control-size hga-view/control-margin)]
        [:.bins {:max-height (str "calc((100vh / 3) - " hga-view/scrollbar-height "px)")}
         #_{:max-height (str "calc((100vh / 3) - " controls-end-height "px)")}])]

     [:#viz
      [:#render {:transition "width 0.5s"}]]
     [:#inspector {:position :sticky
                   :left     "0px"
                   :width    "100vw"}]

     [:#viz {:position :relative}
      ]]]
   hga-members/styles-horizontal-mode))

(def control-size-style
  {:max-width  (px hga-view/control-size)
   :max-height (px hga-view/control-size)
   :width      (px hga-view/control-size)
   :height     (px hga-view/control-size)})

(def styles
  [(at-keyframes "fade-in"
                 [:from {:opacity 0}]
                 [:to   {:opacity 1}])

   [:button {:border           :none
             :padding          "0px"
             :background-color :unset
             :width            :fit-content
             :height           :fit-content
             :cursor           :pointer}]

   [:html {}]
   [:body {:overflow-y :hidden
           :overflow-x :hidden
           :margin "0px"}]
   [:#root {:overflow-y :hidden
            :overflow-x :hidden}]
   [:#root-view {:overflow-y :hidden
                 :overflow-x :hidden}]

   [:#page-view {:height  "100vh"
                 :width   "100vw"
                 :display :flex}]

   [:#controls]
   [:#menu-controls (merge control-size-style
                           {:position         :fixed
                            :bottom           (px (+ hga-view/scrollbar-height hga-view/control-margin))
                            :right            (px hga-view/control-margin)
                            :background-color "white"
                            :z-index          110})
    [:#menu-controls-toggler (merge control-size-style
                                    {:display         :inline-flex
                                     :justify-content :center
                                     :align-items     :center})]]

   [:#home {:width      "100vw"
            :height     "100vh"
            :min-width  "100vw"
            :min-height "100vh"}]

   [:#viz
    [:.inspectable {:transform-box    :fill-box
                    :transform-origin :center
                    :transition (t :scale (/ tt 2) :opacity (/ tt 2))}
     [:&.peeked {:scale 1.3
                 :filter "drop-shadow(0px 0px 2px rgb(0 0 0 / 0.4))"}]
     [(gs/& :.analysis (gs/not :.in-peeked) (gs/not :.in-inspected)) {:opacity 0.33}]]]

   [:.event-info {;; :filter "drop-shadow(0px 0px 2px rgb(0 0 0 / 0.4))"
                  }
    #_[:* {:transition-timing-function :linear}]
    [:.witness {:stroke       :black
                :stroke-width 1
                :fill         :transparent
                :opacity      0
                :r            hga-view/evt-r
                :transition   (t :opacity tt
                                 :r tt)}
     [:&.absent {}]
     [:&.present {:opacity 0.33
                  :r       hga-view/wit-r}
      [:&.final {:opacity 1}]]]

    [:.round-number {:font-size (px 14)
                     :fill :gray
                     :transition (t :fill tt)}
     [:&.final {:fill :black}]]

    [:.votes {:opacity    0
              :transition (t :opacity tt)
              :display    :none}
     [:&.present {:opacity 1
                  :display :block}]
     [:.vote {:transition (t :stroke tt)}]]
    [:&.r-concluded
     [:.votes {:opacity 1}]]]])

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


(rum/defc event-round-view < rum/static rum/reactive
  [{:round/keys [number final?] :as round}]
  [:g (inspectable round)
   [:text.round-number {:class [(when final? "final")]
                        :x (/ hga-view/wit-r 2)
                        :y (/ (- hga-view/wit-r) 2)}
    number]])

(rum/defc event-tx-view < rum/static rum/reactive
  [{:tx/keys [fn-id args]}]
  (case fn-id
    :share-stake
    (hga-icons/transfer {:width     hga-view/evt-s
                         :height    hga-view/evt-s
                         :x         (- hga-view/evt-r)
                         :y         (- hga-view/evt-r)})))

(defn translate-based-on-view-mode [x y]
  (if hga-view/view-mode-horizontal?
    (str "translate(" y "px," x "px)")
    (str "translate(" x "px," y "px)")))

(def event-view-key-fn {:key-fn (fn [{:event-info/keys [event]}] (-hash event))})
(rum/defcs event-view <
  event-view-key-fn
  static-by-hash
  rum/reactive
  (hga-transitions/mixin ::view-state    (fn [event-info] (some-> event-info :event-info/event -hash)))
  (hga-transitions/mixin ::sp-view-state (fn [event-info] (some-> event-info :event-info/event hg/self-parent -hash)))
  (hga-transitions/mixin ::op-view-state (fn [event-info] (some-> event-info :event-info/event hg/other-parent -hash)))
  [{::keys [view-state sp-view-state op-view-state]} {:event-info/keys [event color round round-concluded? witness? stake-map concluded-voting received-event] :as event-info}]
  ;;(js/console.log event-info)
  #_(log! [:render-event-view @hga-playback/*frame] (-hash event))
  (let [x                        (js-map/get view-state :x)
        y                        (js-map/get view-state :y)
        opacity                  (or (js-map/get view-state :opacity) 0)
        fill-opacity             (or (js-map/get view-state :fill-opacity) 0)
        {round-final? :round/final?} round
        share-stake?             (some-> event :event/tx :tx/fn-id (= :share-stake))]
    (when (not (zero? opacity))
      [:g {:key     (-hash event)
           :opacity opacity}

       [:g.refs
        (when sp-view-state
          [:line {hga-view/x1   x
                  hga-view/y1   y
                  hga-view/x2   (js-map/get sp-view-state :x)
                  hga-view/y2   (js-map/get sp-view-state :y)
                  :stroke       :lightgray
                  :stroke-width "2px"
                  :style        {:opacity (- 1 fill-opacity)}}])

        (when op-view-state
          [:line {hga-view/x1   x
                  hga-view/y1   y
                  hga-view/x2   (js-map/get op-view-state :x)
                  hga-view/y2   (js-map/get op-view-state :y)
                  :stroke       :lightgray
                  :stroke-width "2px"
                  :style        {:opacity (- 1 fill-opacity)}}])]

       [:g.event-info {:width  (* hga-view/wit-r 2)
                       :height (* hga-view/wit-r 2)
                       :style  {:transform (translate-based-on-view-mode x y)}}

        [:g (inspectable :witness)
         [:circle.witness
          {:key   "witness"
           :class [(if witness? "present" "absent")
                        (when (and witness? round-final?) "final")]}]]

        [:g.votes {:class [(when stake-map "present")]}
         (when stake-map
           (let [member-name->vote (->> concluded-voting :concluded-voting/votes (into {} (map (fn [vote] [(-> vote :vote/from-event hg/creator) vote]))))
                 voted?            (some? concluded-voting)]
             (for [[stake-holder stake-amount] stake-map]
               (let [member                                                                (-> stake-holder hg-members/member-name->person)
                     [start-vote-circumferance vote-circumferance _end-vote-circumferance] (vote-circumferance-start+for+end stake-holder stake-map)
                     ?vote                                                                 (member-name->vote stake-holder)
                     ?vote-value                                                           (:vote/value ?vote)]
                 [:g {:key stake-holder}
                  [:g (inspectable (if ?vote ?vote {:stake/holder stake-holder
                                                    :stake/amount stake-amount}))
                   [:circle.vote {:r                hga-view/vote-r
                                  :fill             :transparent
                                  :stroke-width     hga-view/vote-stroke-width
                                  :stroke-dasharray (str "0 " start-vote-circumferance " " vote-circumferance " " hga-view/vote-circumferance)
                                  :style            {:stroke (cond (not voted?)
                                                                   (color-rgba-str (:member/color-rgb member) 0.33)

                                                                   (and voted? ?vote-value)
                                                                   (color-rgba-str (:member/color-rgb member) 1)

                                                                   :else
                                                                   "lightgray")}}]]]))))]

        ;; always rendering white background to hide refs
        [:g (inspectable event)
         [:circle.event
          {:key          "event-refs-hide"
           :r            hga-view/evt-r
           :stroke       color
           :stroke-width 1
           :fill         "white"}]]

        (when-not (zero? fill-opacity)
          (let [{:keys [red green blue]} (js-map/get view-state :fill)
                fill                     (str "rgba("red","green","blue","fill-opacity")")]
            [:g (inspectable received-event)
             [:circle.received-event
              {:r    hga-view/evt-r
               :fill (if (zero? fill-opacity) "white" fill)}]]))

        (when-let [tx (:event/tx event)]
          [:g {:fill (gc/hsl->hex (gc/lighten (gc/hsl 0 0 0) (* 100 (js-map/get view-state :fill-opacity))))}
           (event-tx-view tx)])

        (event-round-view round)]])))

(defn* ^:memoizing ?received-event->event->received-event
  [?received-event]
  (if (nil? ?received-event)
    (hash-map)
    (let [prev (?received-event->event->received-event (:received-event/prev-received-event ?received-event))]
      (assoc prev (:received-event/event ?received-event) ?received-event))))

(defn* ^:memoizing concluded-round->witness->cr+cv
  [{:concluded-round/keys [r concluded-votings prev-concluded-round] :as cr}]
  (if (= 0 r)
    (hash-map)
    (let [prev (concluded-round->witness->cr+cv prev-concluded-round)]
      (reduce (fn [acc {:concluded-voting/keys [about] :as cv}]
                (assoc acc about [cr cv]))
              prev
              concluded-votings))))

(defn event->color [event]
  (-> event
      hg/event->person
      :member/color-rgb
      color-rgba-str))

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
            ?witness->cr+cv       (some-> ?last-concluded-round concluded-round->witness->cr+cv)
            ->event-info          (fn [event]
                                    (let [{r-final? :round/final?
                                           r-cr     :round/cr :as round} (hg/->round event ?last-concluded-round)

                                          witness?          (hg/witness? event r-cr)
                                          to-receive-votes? (and witness?
                                                                 r-final?)
                                          [?cr ?cv]         (when to-receive-votes?
                                                              (get ?witness->cr+cv event))
                                          received-votes?   (some? ?cv)

                                          receives-votes? (and to-receive-votes?
                                                               (not received-votes?)
                                                               ?main-tip
                                                               (hg/ancestor? ?main-tip event))
                                          ?cv             (or ?cv
                                                              (when receives-votes?
                                                                (hg/->?concluded-voting ?main-tip event r-cr)))
                                          stake-map       (if ?cr
                                                            (hg/concluded-round->stake-map (:concluded-round/prev-concluded-round ?cr))
                                                            (hg/concluded-round->stake-map r-cr))
                                          ?received-event (event->received-event event)

                                          color           (event->color event)]
                                      (cond-> (hash-map :event-info/color    color
                                                        :event-info/event    event
                                                        :event-info/round    round)
                                        ?cr               (assoc :event-info/round-concluded? ?cr)
                                        witness?          (assoc :event-info/witness? witness?)
                                        to-receive-votes? (assoc :event-info/stake-map stake-map)
                                        ?cv               (assoc :event-info/concluded-voting ?cv)
                                        ?received-event   (assoc :event-info/received-event ?received-event))))]
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
       (for [evt-info rewinded-evt-infos>]
         (event-view evt-info))
       (for [evt-info played-evt-infos>]
         (event-view evt-info))
       (for [evt-info behind-evt-infos>]
         (event-view evt-info))]]]))

(rum/defc menu-controls []
  [:div#menu-controls
   [:button#menu-controls-toggler
    (hga-icons/icon :solid :bars :2xl)]
   #_[:button {:on-click #(swap! hga-state/*playback-attached-to-scroll? not)}
    (str (if (rum/react hga-state/*playback-attached-to-scroll?) "detach" "attach") " playback to scroll")]])

(rum/defc controls-view < rum/reactive []
  [:div#controls
   (menu-controls)
   (hga-playback/playback-controls-view)])

(rum/defc viz-section-view
  []
  [:div#viz-section {:style {:position :relative}}
   (hga-tutorial/view)
   (viz)
   (hga-inspector/bins-view)
   ])

(rum/defc home-view []
  [:div#home
   (hga-utils/plug "home")])

(def scroll-coord (if hga-view/view-mode-horizontal? "left" "top"))
(rum/defc page-view <
  {:did-mount
   (fn [state]
     (let [dom-node (rum/dom-node state)]
       (reset! hga-state/*viz-scroll-by! (fn [viz-px & {:keys [smooth?]}]
                                           (let [px (+ viz-px hga-view/window-size)]
                                             (.scrollBy dom-node (if smooth?
                                                                   (js-obj scroll-coord px
                                                                           "behavior" "smooth")
                                                                   (js-obj scroll-coord px))))))
       (reset! hga-state/*viz-scroll! (fn [viz-px & {:keys [smooth?]}]
                                        (let [px (+ viz-px hga-view/window-size)]
                                          #_(set! (.-scrollTop dom-node) px)
                                          (.scroll dom-node (if smooth?
                                                              (js-obj scroll-coord px
                                                                      "behavior" "smooth")
                                                              (js-obj scroll-coord px)))) ;; doesn't scroll pixel-perfect on zoom
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
   (viz-section-view)
   (controls-view)])

(rum/defc view []
  [:div#root-view
   [:style (garden/css styles)]
   [:style (garden/css (if hga-view/view-mode-horizontal?
                         styles-horizontal-mode
                         styles-vertical-mode))]
   (page-view)])
