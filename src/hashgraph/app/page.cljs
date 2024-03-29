(ns hashgraph.app.page
  (:require-macros [hashgraph.utils.js-map :as js-map]
                   [hashgraph.app.inspector :refer [inspectable*]]
                   :reload-all)
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [cljs.math :refer [round floor ceil pow]]
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
   [hashgraph.app.styles :refer [reg-styles! kind->css]]
   [hashgraph.app.members :as hga-members]
   [hashgraph.app.playback :as hga-playback]
   [hashgraph.app.transitions :refer [tt] :as hga-transitions]
   [hashgraph.app.infini-events :as hga-infini-events]
   [hashgraph.app.analysis :as hga-analysis]
   [hashgraph.app.home :as hga-home]
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

(def styles-horizontal
  [[:#page-view {:flex-direction :row
                 :overflow-x     :scroll
                 :overflow-y     :hidden}]

   [:#viz-section {:height         "100vh"
                   :display        :flex
                   :flex-direction :column}
    [:>* {:flex "1 1 0"}]
    [:.bins-view {:position :sticky
                  :left     "0px"
                  :width    "100vw"}
     [:.bins {:max-height (str "calc((100vh / 3) - " hga-view/scrollbar-height "px)")}]]

    [:#viz
     [:#render {:overflow   :visible
                :transition "width 0.5s"}]]
    [:#inspector {:position :sticky
                  :left     "0px"
                  :width    "100vw"}]

    [:#viz {:position :relative}]]])

(def styles-vertical
  [[:#page-view {:flex-direction :column
                 :overflow-x     :hidden
                 :overflow-y     :scroll}]
   [:#viz-section {:width "100vw"}

    [:.bins-view {:display :none}]

    [:#tutorial]

    [:#viz {:position       :absolute
            :left           (px hga-view/viz-margin-x)
            :right          (px hga-view/viz-margin-x)
            :pointer-events :none
            :z-index        hga-view/viz-z}
     [:#render {:transition "height 0.5s"}
      [:.inspectable.active {:pointer-events :all}]]]]])

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
                 :display :flex
                 :outline :none}]

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

   [:#viz
    [:#render {:overflow :visible}]
    [:.inspectable {:transform-box    :fill-box
                    :transform-origin :center
                    :transition (t :scale (/ tt 2) :opacity (/ tt 2))}
     [:&.accented {:scale  1.2
                   :filter "drop-shadow(0px 0px 2px rgb(0 0 0 / 0.4))"}
      [:.inspectable.accented {:scale  1
                               :filter :none}]]
     [(gs/& :.analysis (gs/not :.inspected) (gs/not :.nested)) {:opacity 0.33}]]
    [:.refs {:opacity 0.20}
     [:.ref {:stroke       :black
             :stroke-width (px 2)}]
     [:.inspectable
      [:&.accented {:scale 1
                    :filter "drop-shadow(0px 0px 1px rgb(0 0 0 / 0.6))"}]]]]

   [:.event-info {;; :filter "drop-shadow(0px 0px 2px rgb(0 0 0 / 0.4))"
                  }
    #_[:* {:transition-timing-function :linear}]
    [:.witness-view-wrapper
     [:.witness-view {:stroke       :black
                      :stroke-width 1
                      :fill         :transparent
                      :opacity      0
                      :transition   (t :r tt :opacity tt)}
      [:&.present {:opacity 0.33}
       [:&.final {:opacity 1}]]]

     [:.witness-view-hit-slope {:stroke       :transparent
                                :stroke-width 15
                                :fill         :transparent
                                }]]

    [:.round-number {:font-size (px 14)
                     :fill :gray
                     :opacity 0
                     :transition (t :fill tt
                                    :opacity tt)}
     [:&.shown {:opacity 1}]
     [:&.final {:fill :black}]]

    [:.votes-wrapper {:opacity    0
                      :transition (t :opacity tt)
                      :display    :none}
     [:&.present {:opacity 1
                  :display :block}]
     [:.vote {:transition (t :stroke tt)}]]
    [:&.r-concluded
     [:.votes-wrapper {:opacity 1}]]]])

(reg-styles! ::page styles styles-horizontal styles-vertical)

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


(rum/defcs event-witness-view <
  {:key-fn (fn [_] "witness")}
  hga-utils/static-by-hashes
  rum/reactive
  (rum/local nil ::*paths)
  [{::keys [*paths]} {:round/keys [final? number cr]} witness? event]
  (let [witness-shown? (and witness? (rum/react hga-state/*show-witnesses?))]
    [:g.witness-view-wrapper (when (and witness-shown? #_@*paths)
                               (inspectable :witness #_@*paths {:passive? true}))
     [:circle.witness-view {:key   "witness-view"
                            :r     (if witness-shown?
                                     hga-view/wit-r
                                     hga-view/evt-r)
                            :class [(if witness-shown? "present" "absent")
                                    (when (and witness? final?) "final")]}]
     (when witness-shown?
       ;; even though calc is optimized, highlight of affected ones is darn slow
       ;; it'll be way faster to highlight them by tweaking their view-state
       ;; a separate view-state of inspectables can be kept
       [:circle.witness-view-hit-slope {:r              hga-view/wit-r
                                        #_#_:on-mouse-enter #(when-not @*paths
                                                           (reset! *paths (hg/->strongly-see-r-paths event cr (dec number))))}])]))

(rum/defcs event-votes-view <
  hga-utils/static-by-hashes
  rum/reactive
  [_ round votes witness? event]
  (let [round-final? (:round/final? round)
        round-cr (:round/cr round)]
    [:g.votes-wrapper {:key "vote-wrapper"
                       :class [(when (and witness? round-final? (rum/react hga-state/*show-stake-map?)) "present")]}
     (when (and witness? round-final?)
       (let [stake-map          (hg/concluded-round->stake-map round-cr)
             voted?             (some? votes)
             ?member-name->vote (when voted? (->> votes (into (hash-map) (map (fn [vote] [(-> vote :vote/voter hg/creator) vote])))))]
         [:g.votes (merge {:key "votes"} (when-not voted? (inspectable stake-map)))
          (for [[stake-holder stake-amount] stake-map]
            (let [stake-color-rgb                                 (-> stake-holder hg-members/member-name->person :member/color-rgb)
                  [start-vote-circumferance vote-circumferance _] (vote-circumferance-start+for+end stake-holder stake-map)
                  ?vote                                           (get ?member-name->vote stake-holder)
                  ?vote-value                                     (:vote/value ?vote)]
              [:g.vote-wrapper (merge {:key stake-holder}
                                      (when voted? (inspectable (or ?vote {stake-holder "didn't vote"}))))
               [:circle.vote
                (cond-> {:r                hga-view/vote-r
                         :fill             :transparent
                         :stroke-width     hga-view/vote-stroke-width
                         :stroke-dasharray (str "0 " start-vote-circumferance " " vote-circumferance " " hga-view/vote-circumferance)
                         :style            {:stroke (cond (not voted?)
                                                          (color-rgba-str stake-color-rgb 0.33)

                                                          (and voted? ?vote-value)
                                                          (color-rgba-str stake-color-rgb 1)

                                                          :else
                                                          "lightgray")}})]]))]))]))

(rum/defc event-round-view < hga-utils/static-by-hash rum/reactive
  [{:round/keys [event number next? final? cr] :as round}]
  [:g (inspectable round)
   [:text.round-number {:class [(when (rum/react hga-state/*show-rounds?) "shown")
                                (when final? "final")]
                        :x     (/ hga-view/wit-r 2)
                        :y     (/ (- hga-view/wit-r) 2)}
    number]])

(rum/defc event-tx-view < hga-utils/static-by-hashes rum/reactive
  [{:tx/keys [fn-id args] :as tx} {:keys [color]}]
  [:svg.tx {:width  hga-view/evt-s
            :height hga-view/evt-s
            :x      (- hga-view/evt-r)
            :y      (- hga-view/evt-r)}
   (case fn-id
     :share-stake
     (hga-icons/icon :solid :transfer :color color)
     :inc-counter
     (hga-icons/icon :solid :plus :color color))])

(defn translate-based-on-view-mode [x y]
  (if hga-view/view-mode-horizontal?
    (str "translate(" y "px," x "px)")
    (str "translate(" x "px," y "px)")))

(def event-view-key-fn {:key-fn (fn [{:event-info/keys [event]}] (-hash event))})
(rum/defcs event-view <
  event-view-key-fn
  hga-utils/static-by-hash
  rum/reactive
  (hga-transitions/mixin ::view-state    (fn [event-info] (some-> event-info :event-info/event -hash)))
  (hga-transitions/mixin ::sp-view-state (fn [event-info] (some-> event-info :event-info/event hg/self-parent -hash)))
  (hga-transitions/mixin ::op-view-state (fn [event-info] (some-> event-info :event-info/event hg/other-parent -hash)))
  [{::keys [view-state sp-view-state op-view-state]} {:event-info/keys [event color round witness? cr votes received-event] :as event-info}]
  ;;(js/console.log event-info)
  #_(log! [:render-event-view @hga-playback/*frame] (-hash event))
  (let [x                        (js-map/get view-state :x)
        y                        (js-map/get view-state :y)
        opacity                  (or (js-map/get view-state :opacity) 0)
        fill-opacity             (or (js-map/get view-state :fill-opacity) 0)
        {r            :round/number
         round-final? :round/final?
         round-next?  :round/next?
         round-cr     :round/cr} round]
    (when (not (zero? opacity))
      [:g {:opacity opacity}

       (let [ref-opacity (- 1 fill-opacity)
             ->accented?  (fn [_accented ips els] (->> ips (some (fn [ip] (and (map? ip) (some-> (:sees/path ip) (utils/->neighbours? els)))))))
             ->inspected? (fn [ips els]           (->> ips (some (fn [ip] (and (map? ip) (some-> (:sees/path ip) (utils/->neighbours? els)))))))]
         (when-not (zero? ref-opacity)
           [:g.refs
            (when sp-view-state
              [:g.ref.sp (inspectable [event (hg/self-parent event)] {:passive? true :->inspected? ->inspected? :->accented? ->accented?})
               [:line {hga-view/x1 x
                       hga-view/y1 y
                       hga-view/x2 (js-map/get sp-view-state :x)
                       hga-view/y2 (js-map/get sp-view-state :y)
                       :style      {:opacity ref-opacity}}]])

            (when op-view-state
              [:g.ref.op (inspectable [event (hg/other-parent event)] {:passive? true :->inspected? ->inspected? :->accented? ->accented?})
               [:line {hga-view/x1 x
                       hga-view/y1 y
                       hga-view/x2 (js-map/get op-view-state :x)
                       hga-view/y2 (js-map/get op-view-state :y)
                       :style      {:opacity ref-opacity}}]])]))

       [:g.event-info {:width  (* hga-view/wit-r 2)
                       :height (* hga-view/wit-r 2)
                       :style  {:transform (translate-based-on-view-mode x y)}}

        (event-witness-view round witness? event)

        (event-votes-view round votes witness? event)

        ;; always rendering white background to hide refs
        (let [{:keys [red green blue] :as fill} (js-map/get view-state :fill)
              tx (:event/tx event)]
          [:g (inspectable (cond-> [event]
                             received-event (conj received-event)
                             tx             (conj tx))
                           {:->inspected?   (fn [ips els]           (->> els (some (fn [el] (hga-inspector/->in ips el)))))
                            :->accented?    (fn [accented _ips els] (->> els (some (fn [el] (hga-inspector/->in accented el)))))})
           [:circle.event {:r            hga-view/evt-r
                           :stroke       color
                           :stroke-width 1
                           :fill         "white"}]
           (when (> fill-opacity 0)
             [:circle.received-event {:class (when received-event "received")
                                      :r     hga-view/evt-r
                                      :fill  (str "rgba(" red "," green "," blue "," fill-opacity ")")}])
           (when tx
             [:g.tx-wrapper {:style {:scale "0.8"}}
              (event-tx-view tx {:color (gc/hsl->hex (gc/lighten (gc/hsl 0 0 0) (* 100 fill-opacity)))})])])

        (event-round-view round)]])))


(defn* ^:memoizing concluded-round->witness->cr+vote
  [{:concluded-round/keys [r w->vote prev-concluded-round] :as cr}]
  (if (= 0 r)
    (hash-map)
    (let [prev (concluded-round->witness->cr+vote prev-concluded-round)]
      (merge prev w->vote))))

(defn event->color [event]
  (-> event
      hg/event->person
      :member/color-rgb
      color-rgba-str))

(defn cr+r->?cr [cr r]
  (let [cr-r (:concluded-round/r cr)]
    (cond (= cr-r r) cr
          (> cr-r r) (recur (:concluded-round/prev-concluded-round cr) r))))

(def *rendered-evt-infos
  (rum/derived-atom [hga-playback/*playback hga-state/*main-tip hga-state/*last-cr hga-state/*event->received-event] ::*rendered-evt-infos
    (fn [{:keys [behind>
                 played<   ;; asc
                 rewinded< ;; asc
                 ]}
         ?main-tip
         last-cr
         event->received-event]
      (let [->event-info
            (fn [event]
              (let [{r        :round/number
                     r-final? :round/final?
                     r-cr     :round/cr :as round} (hg/->round event last-cr)

                    witness?            (hg/witness? event r-cr)
                    will-receive-votes? (and witness? r-final?)
                    receives-votes?     (and will-receive-votes? ?main-tip
                                             (hg/voting-round? ?main-tip event r-cr)
                                             (hg/ancestor? ?main-tip event))
                    ?cr                 (when receives-votes?
                                          (cr+r->?cr last-cr r))
                    ?votes              (when receives-votes?
                                          (if-let [cr ?cr]
                                            (hg/->votes (:concluded-round/witness-concluded cr) event (:concluded-round/prev-concluded-round cr))
                                            (hg/->votes ?main-tip event last-cr)))
                    ?received-event     (event->received-event event)

                    color      (event->color event)
                    event-info (cond-> (hash-map :event-info/color    color
                                                 :event-info/event    event
                                                 :event-info/round    round)
                                 witness?        (assoc :event-info/witness? witness?)
                                 ?cr             (assoc :event-info/cr ?cr)
                                 ?votes          (assoc :event-info/votes ?votes)
                                 ?received-event (assoc :event-info/received-event ?received-event))]
                event-info))]
        [(->> rewinded<
              (take 10)
              reverse
              (map ->event-info))
         (->> played<
              reverse
              (map ->event-info))
         (->> behind>
              (take 10)
              (map ->event-info))
         ]))))


(rum/defc viz < rum/reactive
  []
  (let [[behind-evt-infos> played-evt-infos> rewinded-evt-infos>] (rum/react *rendered-evt-infos)]
    [:div#viz
     [:svg#render {(if hga-view/view-mode-horizontal? :width :height) (or (rum/react hga-state/*overide-viz-height)
                                                                          (hga-view/evt->viz-height (-> played-evt-infos> first :event-info/event)))
                   (if hga-view/view-mode-horizontal? :height :width) hga-view/viz-x-span}
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
    (hga-icons/icon :solid :bars :size :2xl)]
   #_[:button {:on-click #(swap! hga-state/*playback-attached-to-scroll? not)}
    (str (if (rum/react hga-state/*playback-attached-to-scroll?) "detach" "attach") " playback to scroll")]])

(rum/defc controls-view < rum/reactive []
  [:div#controls
   #_(menu-controls)
   (hga-playback/playback-controls-view)])

(rum/defc viz-section-view
  []
  [:div#viz-section {:style {:position :relative}}
   (hga-tutorial/view)
   (viz)
   (hga-members/view)
   (hga-inspector/bins-view)
   ])

(def scroll-coord (if hga-view/view-mode-horizontal? "left" "top"))
(def scroll-attr (if hga-view/view-mode-horizontal? "scrollLeft" "scrollTop"))
(rum/defc page-view <
  {:did-mount
   (fn [state]
     (let [dom-node (rum/dom-node state)]
       (reset! hga-state/*viz-scroll-by! (fn [viz-px & {:keys [smooth?]}]
                                           (let [px viz-px]
                                             (.scrollBy dom-node (if smooth?
                                                                   (js-obj scroll-coord px
                                                                           "behavior" "smooth")
                                                                   (js-obj scroll-coord px))))))
       (reset! hga-state/*viz-scroll! (fn [viz-px & {:keys [smooth?]}]
                                        (let [px (+ viz-px hga-view/window-y-span)]
                                          #_(set! (.-scrollTop dom-node) px)
                                          (.scroll dom-node (if smooth?
                                                              (js-obj scroll-coord px
                                                                      "behavior" "smooth")
                                                              (js-obj scroll-coord px)))) ;; doesn't scroll pixel-perfect on zoom
                                        ))
       (.addEventListener dom-node "scroll"
                          (hga-utils/once-per-render
                           (fn [e]
                             (reset! hga-state/*viz-scroll (- (-> e (.-target) (goog.object/get scroll-attr))
                                                              hga-view/window-y-span)))))
       (.focus dom-node
               ;; focusVisible works only in Firefox atm https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/focus#browser_compatibility
               ;; overriding :outline manually in styles
               ^js {"focusVisible" false}))
     state)}
  []
  [:div#page-view {:tab-index  -1} ;; to be able to focus on load ^, so keyboard events trigger scroll
   (hga-utils/render-cbs-trigger)
   (hga-home/view)
   (viz-section-view)
   (controls-view)
   (when js/goog.DEBUG
     (hga-inspector/debug-view))])

(rum/defc view < rum/reactive []
  [:div#root-view
   [:style (kind->css :bare)]
   (let [horizontal-css (kind->css :horizontal)
         vertical-css (kind->css :vertical)]
     [:style (if hga-view/view-mode-horizontal?
               horizontal-css
               vertical-css)])

   (page-view)])
