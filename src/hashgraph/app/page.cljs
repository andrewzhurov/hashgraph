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
   hga-members/styles-vertical-mode)
  #_
  [[:#page-view {:flex-direction :column
                 :overflow-x     :hidden
                 :overflow-y     :scroll}]
   ])

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
#_(log! styles-horizontal-mode)

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

   [:.event {;; :transition       "0.5s"
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
     [:.round {:color       "black"
               :font-size   "18px"
               :font-weight :bold}]]

    [:&.r-concluded
     [:.votes {:opacity 1}]]]])

(def key-fn-by-hash {:key-fn (fn [arg] (-hash arg))})
(def static-by-hash {:should-update (fn [old-state new-state]
                                      (not= (-hash (first (:rum/args old-state)))
                                            (-hash (first (:rum/args new-state)))))})

(defn async-idle
  "Async evokations of this function will evoke f only when it's not currently running - is idle."
  [f]
  (let [idle?* (volatile! true)]
    (fn [& args]
      (when @idle?*
        (vreset! idle?* false)
        (js/setTimeout #(do (apply f args)
                            (js/console.log "ran async idle")
                            (vreset! idle?* true)))))))

(defn once-per-render
  [f]
  (let [*ran? (volatile! false)]
    (fn [& args]
      (when-not @*ran?
        (vreset! *ran? true)
        (apply f args)
        (js/requestAnimationFrame (fn [] (vreset! *ran? false)))))))

(def request-current->desired-run! (once-per-render hga-transitions/current->desired-run!))
rum/reactive
(defn with-view-state [id args->view-id]
  {:will-mount    (fn [state] (if-let [view-id (apply args->view-id (:rum/args state))]
                                (-> state
                                    (assoc id (hga-transitions/->current view-id))
                                    (assoc-in [::id->view-id id] view-id))
                                state))
   :before-render (fn [state]
                    ;; would be nicer to make view-state lookapable as-though it's a clj map,
                    ;; so we don't craft clj map from it on each transition render, getting better efficiency
                    (request-current->desired-run!)
                    (when (hga-transitions/view-id->in-transition? (get-in state [::id->view-id id]))
                      (rum/request-render (:rum/react-component state)))
                    #_
                    (if-let [view-state (get-in state [id ::view-state])]
                      (do
                        #_(log! [:render @hga-playback/*frame] view-id)
                        (let [x       (goog.object/get view-state hga-transitions/x-key)
                              y       (goog.object/get view-state hga-transitions/y-key)
                              opacity (goog.object/get view-state hga-transitions/opacity-key)]
                          (if false #_hga-view/view-mode-horizontal?
                              (update state id assoc
                                      :x y
                                      :y x
                                      :opacity opacity)
                              (update state id assoc
                                      :x x
                                      :y y
                                      :opacity opacity))))
                      state)
                    state)})

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

(def received-event-y-offset (/ hga-view/evt-s 3))
(def received-events-width-count (count hg-members/names))
(def received-events-width (-> received-events-width-count dec hga-view/idx->x #_evt-s))
(def received-events-area-x (hga-view/idx->x (count hg-members/names)))

(rum/defc received-event-view <
  rum/reactive
  key-fn-by-hash
  static-by-hash
  [received-event]
  (with-redefs [utils/log!
                (if-not (hga-inspector/->peeked? received-event)
                  identity
                  utils/log!)]
    (let [{:received-event/keys [event idx middle-learned-events received-time concluded-round r-idx color]} received-event

          ;; received-y (t->y received-time)
          ;; received-x (-> (idx->x (count hg-members/names))
          ;;                (+ (* r-idx hga-view/evt-s)))

          from-y            (or (some-> concluded-round :concluded-round/prev-concluded-round :concluded-round/ufws (some->> (map :event/creation-time)) hg/median hga-view/t->y)
                               0)
          to-y              (-> concluded-round :concluded-round/ufws (->> (map :event/creation-time)) hg/median hga-view/t->y (- hga-view/evt-s))
          cr-size           (-> concluded-round :concluded-round/es-r count)
          size-y            (- to-y from-y)
          fitting-evts-size (/ size-y hga-view/evt-s)
          evt-y-offset      (* (/ fitting-evts-size
                                 cr-size)
                              hga-view/evt-s)

          lapses                      (ceil (/ cr-size
                                    (* received-events-width-count 2)))
          lapse-size                  (/ cr-size lapses)
          lapse-idx                   (quot r-idx lapse-size)
          lapse-height                (/ size-y lapses)
          lapse-y                     (+ (* (/ size-y lapses) lapse-idx) from-y)
          within-lapse-idx            (mod r-idx lapse-size)
          within-half-lapse-idx       (mod within-lapse-idx (/ lapse-size 2))
          within-lapse-pos            (/ within-lapse-idx (dec lapse-size))
          within-half-lapse-pos       (mod within-lapse-pos (/ (dec lapse-size) 2))
          within-lapse-before-middle? (< within-lapse-pos 0.5)
          within-lapse-y-mod          (hga-timing/sharp-in-sharp-out within-half-lapse-pos)
          within-lapse-y              (* within-lapse-y-mod lapse-height)
          ;; within-lapse-y-modded (cond-> within-lapse-y
          ;;                         within-lapse-before-middle?       ((fn [in]
          ;;                                                              (l in)
          ;;                                                              (l within-lapse-y-mod)
          ;;                                                              (l (/ in within-lapse-y-mod))
          ;;                                                              (if (= within-lapse-y-mod 0)
          ;;                                                                in
          ;;                                                                (* in within-lapse-y-mod))))
          ;;                         (not within-lapse-before-middle?) (* within-lapse-y-mod)
          ;;                         )

          received-y (-> within-lapse-y #_(* r-idx evt-y-offset)
                         (+ lapse-y #_from-y))

          evt-x-offset (-> (l received-events-width)
                           (/ (dec lapse-size)) ;; account for first not getting offset (?)
                           (* 2))
          received-x
          (->
           (if (> within-lapse-pos 0.5)
             (-> (- received-events-width (* (- within-lapse-idx (/ lapse-size 2))
                                             evt-x-offset)))
             (* within-lapse-idx
                evt-x-offset))
           (+ received-events-area-x))
          #_
          (-> (if (odd? (quot idx received-events-width-count))
                (- received-events-width (* (mod idx received-events-width-count) evt-s))
                (* (mod idx received-events-width-count) evt-s))
              (+ received-events-area-x))
          ]
      [:g

       #_
       (let [parent-evt (:received-event/event (:received-event/prev-received-event received-event))
             [r-x r-y]  (evt->x+y evt)
             [p-x p-y]  (evt->x+y parent-evt)]
         [:line
          {:x1           r-x
           hga-view/y1           r-y
           hga-view/x2           p-x
           hga-view/y2           p-y
           :stroke       (:received-event/color received-event)
           :stroke-width "2px"}])

       (when (hga-inspector/->in-peeked? received-event)
         (for [mle middle-learned-events]
           [:line {hga-view/x1           (hga-view/evt->x mle)
                   hga-view/y1           (hga-view/evt->y mle)
                   hga-view/x2           received-x
                   hga-view/y2           received-y
                   :stroke       "blue"
                   :stroke-width "2px"}]))

       [:g.receivend-event-view (inspectable received-event)
        [:circle.event
         {:r            hga-view/evt-r
          hga-view/cx           received-x
          hga-view/cy           received-y
          :stroke       color
          :stroke-width 1
          :fill         color}]]])))

(def event-view-key-fn {:key-fn (fn [{:event-info/keys [event]}] (-hash event))})

(rum/defcs event-view <
  event-view-key-fn
  static-by-hash
  rum/reactive
  (with-view-state ::view-state    (fn [event-info] (some-> event-info :event-info/event -hash)))
  (with-view-state ::sp-view-state (fn [event-info] (some-> event-info :event-info/event hg/self-parent -hash)))
  (with-view-state ::op-view-state (fn [event-info] (some-> event-info :event-info/event hg/other-parent -hash)))
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
        [:line {hga-view/x1      x
                hga-view/y1      y
                hga-view/x2      (js-map/get sp-view-state :x)
                hga-view/y2      (js-map/get sp-view-state :y)
                :style   {:stroke       :lightgray
                          :stroke-width "2px"}}])

      (when op-view-state
        [:line {hga-view/x1      x
                hga-view/y1      y
                hga-view/x2      (js-map/get op-view-state :x)
                hga-view/y2      (js-map/get op-view-state :y)
                :style   {:stroke       :lightgray
                          :stroke-width "2px"}}])]

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

      [:circle.event
       {hga-view/cx   x
        hga-view/cy   y
        :r            hga-view/evt-r
        :stroke       color
        :stroke-width 1
        :fill         (or (when-let [{:keys [red green blue]} (js-map/get view-state :fill)]
                            (let [fill-opacity (js-map/get view-state :fill-opacity)]
                              (str "rgba("red","green","blue","fill-opacity")")))
                          "white")
        #_#_:style    (when-let [{:received-event/keys [r-idx]} received-event]
                        (let [re-tt (/ hga-transitions/tt 5)]
                          {:transition (str "fill " re-tt "ms" " " (* re-tt r-idx) "ms" " ease-out")}))}]

      (when share-stake?
        (hga-icons/transfer {hga-view/x (- x hga-view/evt-r)
                             hga-view/y (- y hga-view/evt-r)
                             :width     hga-view/evt-s
                             :height    hga-view/evt-s
                             :fill      (if received-event "white" "black")}))

      [:text.round
       (if-not hga-view/view-mode-horizontal?
         {hga-view/x (+ x (/ hga-view/wit-r 2))
          hga-view/y (+ y (/ (- hga-view/wit-r) 2))}
         {hga-view/x (+ x (/ (- hga-view/wit-r) 2))
          hga-view/y (+ y (/ hga-view/wit-r 2))})
       r]]

     #_
     (when received-event
       (received-event-view received-event))]))

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
    (fn [{:keys [played<   ;; asc
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
        [(->> (reverse played<)
              (map ->event-info))
         (->> (reverse rewinded<)
              (map ->event-info))]))))


(rum/defc viz < rum/reactive
  []
  (let [[played-evt-infos> rewinded-evt-infos>] (rum/react *rendered-evt-infos)]
    [:div#viz
     (hga-members/view)
     [:svg#render {(if hga-view/view-mode-horizontal? :width :height) (hga-view/evt->viz-height (-> played-evt-infos> first :event-info/event))
                   (if hga-view/view-mode-horizontal? :height :width) hga-view/viz-size}
      [:g.events-view
       (for [evt-info played-evt-infos>]
         (event-view evt-info))
       (for [evt-info rewinded-evt-infos>]
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
                          (once-per-render #(do #_(reset! hga-playback/*playing? false)
                                                (reset! hga-state/*viz-scroll (- (if hga-view/view-mode-horizontal?
                                                                                   (.-scrollLeft (.-target %))
                                                                                   (.-scrollTop (.-target %)))
                                                                                 hga-view/window-size))))))
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
