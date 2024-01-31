(ns hashgraph.app.page
  (:require
   [garden.core :refer [css ]]
   [garden.stylesheet :refer [at-keyframes]]
   [garden.color :as gc]
   [clojure.string :as str]
   [goog.string :refer [format]]
   [hashgraph.app.state :as hga-state]
   [hashgraph.app.utils :refer [->below-view? ->above-view?] :as hga-utils]
   [hashgraph.app.events :refer [evt-view-r
                                 evt-view-s
                                 wit-view-r
                                 vote-view-r
                                 vote-view-circumferance
                                 hgs-padding
                                 sp-padding
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
   [hashgraph.app.transitions :as hga-transitions]
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
   [hashgraph.app.icons :as hga-icons]
   [hashgraph.app.timing :as hga-timing]
   [hashgraph.members :as hg-members]
   [hashgraph.utils
    :refer [merge-attr-maps color-rgba-str timing *->time*]
    :refer-macros [defn* l letl]
    :as utils]
   [rum.core :as rum]
   [taoensso.timbre :refer-macros [spy info]]
   [taoensso.tufte :as tufte :refer [defnp fnp p profiled profile]]
   [cljs.math :refer [floor ceil pow]]
   [clojure.set :as set]
   [goog.object :as gobject]))

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
     [:.round {:color "black"
               :font-size "18px"
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

(defn with-event-view-state [id args->event]
  {:before-render (fn [state]
                    (request-current->desired-run!)
                    (if-let [event (apply args->event (:rum/args state))]
                      (let [event-hash         (-hash event)
                            current-view-state (hga-transitions/->current event-hash)
                            x                  (goog.object/get current-view-state hga-transitions/x-key)
                            y                  (goog.object/get current-view-state hga-transitions/y-key)
                            opacity            (goog.object/get current-view-state hga-transitions/opacity-key)]
                        (when (goog.object/get hga-transitions/event-hash->prop->t event-hash)
                          (-> state :rum/react-component rum/request-render))
                        (assoc state id {:x       x
                                         :y       y
                                         :opacity opacity}))
                      state))})


(rum/defcs event-refs-view <
  key-fn-by-hash
  static-by-hash
  (with-event-view-state ::view-state (fn [event] event))
  (with-event-view-state ::sp-view-state (fn [event] (:event/self-parent event)))
  (with-event-view-state ::op-view-state (fn [event] (:event/other-parent event)))
  [{::keys [view-state sp-view-state op-view-state]} event]
  [:g.refs
   (when sp-view-state
     [:line {:opacity (:opacity view-state)
             :x1      (:x view-state)
             :y1      (:y view-state)
             :x2      (:x sp-view-state)
             :y2      (:y sp-view-state)
             :style   {:stroke       :lightgray
                       :stroke-width "2px"}}])

   (when op-view-state
     [:line {:opacity (:opacity view-state)
             :x1      (:x view-state)
             :y1      (:y view-state)
             :x2      (:x op-view-state)
             :y2      (:y op-view-state)
             :style   {:stroke       :lightgray
                       :stroke-width "2px"}}])])

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

(def received-event-y-offset (/ evt-view-s 3))
(def received-events-width-count (count hg-members/names))
(def received-events-width (-> received-events-width-count dec idx-view-position-x #_evt-view-s))
(def received-events-area-x (idx-view-position-x (count hg-members/names)))

(rum/defc received-event-view <
  rum/reactive
  key-fn-by-hash
  static-by-hash
  [received-event]
  (with-redefs [utils/log!
                (if-not (hga-inspector/->peeked? received-event)
                  identity
                  utils/log!)]
    (letl [{:received-event/keys [event idx middle-learned-events received-time concluded-round r-idx color]} received-event

           ;; received-y (t->view-position-y received-time)
           ;; received-x (-> (idx-view-position-x (count hg-members/names))
           ;;                (+ (* r-idx hga-events/evt-view-s)))

           from-y           (or (some-> concluded-round :concluded-round/prev-concluded-round :concluded-round/ufws (some->> (map :event/creation-time)) hg/median t->view-position-y)
                                0)
           to-y             (-> concluded-round :concluded-round/ufws (->> (map :event/creation-time)) hg/median t->view-position-y (- evt-view-s))
           cr-size          (-> concluded-round :concluded-round/es-r count)
           size-y           (- to-y from-y)
           fitting-evts-size (/ size-y evt-view-s)
           evt-y-offset     (* (/ fitting-evts-size
                                  cr-size)
                               evt-view-s)

           lapses           (ceil (/ cr-size
                                     (* received-events-width-count 2)))
           lapse-size       (/ cr-size lapses)
           lapse-idx        (quot r-idx lapse-size)
           lapse-height     (/ size-y lapses)
           lapse-y          (+ (* (/ size-y lapses) lapse-idx) from-y)
           within-lapse-idx (mod r-idx lapse-size)
           within-half-lapse-idx (mod within-lapse-idx (/ lapse-size 2))
           within-lapse-pos (/ within-lapse-idx (dec lapse-size))
           within-half-lapse-pos (mod within-lapse-pos (/ (dec lapse-size) 2))
           within-lapse-before-middle? (< within-lapse-pos 0.5)
           within-lapse-y-mod (hga-timing/sharp-in-sharp-out within-half-lapse-pos)
           within-lapse-y     (* within-lapse-y-mod lapse-height)
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
                 (- received-events-width (* (mod idx received-events-width-count) evt-view-s))
                 (* (mod idx received-events-width-count) evt-view-s))
               (+ received-events-area-x))
           ]
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

           [:g.receivend-event-view (inspectable received-event)
            [:circle.event
             {:r            evt-view-r
              :cx           received-x
              :cy           received-y
              :stroke       color
              :stroke-width 1
              :fill         color}]]])))

(def event-view-key-fn {:key-fn (fn [{:event-info/keys [event]}] (-hash event))})

(rum/defcs event-view <
  event-view-key-fn
  static-by-hash
  rum/reactive
  (with-event-view-state ::view-state (fn [event-info] (:event-info/event event-info)))
  [{::keys [view-state]} {:event-info/keys [event round r-concluded? share-stake? color fill witness? stake-map votes received-event] :as event-info}]
  (let [{:keys [x y opacity]} view-state
        {r        :round/number
         r-final? :round/final?} round
        share-stake?             (some-> event :event/tx :tx/fn-id (= :share-stake))]
    [:g {:opacity opacity}
     [:g.event (merge-attr-maps
                {:class  [(when r-final? "r-final")
                          (when r-concluded? "r-concluded")
                          (when share-stake? "tx-share-stake")]
                 :width  (* wit-view-r 2)
                 :height (* wit-view-r 2)}
                (inspectable event))

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
        (hga-icons/transfer {:width  evt-view-s
                             :height evt-view-s
                             :x      (- x evt-view-r)
                             :y      (- y evt-view-r)
                             :fill   (if received-event "white" "black")}))

      [:text.round
       {:x (+ x (/ wit-view-r 2))
        :y (+ y (/ (- wit-view-r) 2))}
       r]]

     #_
     (when received-event
       (received-event-view received-event))]))


(def *rendered-evts
  (rum/derived-atom [hga-playback/*played-c->hg hga-state/*viz-scroll-top] ::*rendered-evts
    (fn [creator->hg scroll-top]
      (->> creator->hg
           vals
           ;; TODO switch to transducers
           (mapcat (fn [hg] (->> hg
                                 (iterate hg/self-parent)
                                 (take-while some?)
                                 ;; this only needed for refs viz, perhaps viz refs always instead
                                 ;; actually, when rewinding playback these events will fold onto oher-parents
                                 (drop-while (fn [evt] (and (when-let [sp-evt (hg/self-parent evt)]  ;; more costly as deeper in history you get
                                                              (->above-view? (evt-view-position-y sp-evt) scroll-top))
                                                            (when-let [op-evt (hg/other-parent evt)]
                                                              (->above-view? (evt-view-position-y op-evt) scroll-top)))))
                                 (take-while (fn [evt] (not (->below-view? (evt-view-position-y evt) scroll-top)))))))))))

(def *?alice-tip
  (rum/derived-atom [hga-playback/*played-c->hg] ::*alice-tip
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
  (let [color            (event->color event)
        {r-cr :round/cr} round

        ?atop-cr (when to-receive-votes?
                   (some->> r-cr
                            (iterate :concluded-round/prev-concluded-round)
                            (take-while some?)
                            (some (fn [cr] (when (< (:concluded-round/r cr) (:round/number round))
                                             cr)))))

        fill
        (if-let [{:keys [received-event/color]} received-event]
          color
          "white")
        ]

    (-> event-info
        (assoc :event-info/color color
               :event-info/fill fill
               :event-info/r-concluded? received-votes?)
        (cond->
          to-receive-votes?
          (assoc :event-info/stake-map (hg/concluded-round->stake-map ?atop-cr))

          (or received-votes? receives-votes?)
          (assoc :event-info/votes (:concluded-voting/votes (hg/->?concluded-voting voting-by event ?atop-cr)))))))

(def *rendered-evt-infos
  (rum/derived-atom [*rendered-evts *?alice-tip *?last-concluded-round *?last-received-event] ::*rendered-evt-infos
    (fn [rendered-evts ?alice-tip ?last-concluded-round ?last-received-event]
      (let [event->received-event (?received-event->event->received-event ?last-received-event)]
        (->> rendered-evts
             (map (fn [event]
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
                                               ?alice-tip
                                               (hg/ancestor? ?alice-tip event)) ;; more costly the further tip is from the roots
                          ?received-event (event->received-event event)
                          voting-by       (if received-votes?
                                            (-> ?concluded-in-cr :concluded-round/witness-concluded)
                                            ?alice-tip)]
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
                                               ?received-event (assoc :event-info/received-event ?received-event)))))))))))


(defn on-next-frame! [callback]
  (js/setTimeout #(js/requestAnimationFrame callback)))

(defn on-animation-frame! [callback]
  (js/requestAnimationFrame callback))

(rum/defc viz < rum/reactive
  []
  (let [rendered-evts      (rum/react *rendered-evts)
        rendered-evt-infos (rum/react *rendered-evt-infos)]
    [:svg#viz {:height (hga-events/->viz-height (rum/react hga-playback/*played-c->hg))}
     [:g.events-view
      (for [evt rendered-evts]
        (event-refs-view evt))

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

(def members-style
  [[:.members {:position         :absolute
               :bottom           "0px"
               :height           (str hga-events/members-height "px")
               :background-color "rgba(255,255,255,0.5)"}
    [:.member {:position       :absolute
               :transform      "translateX(-50%)"
               :display        :flex
               :flex-direction :column
               :align-items    :center
               :transition     "opacity 0.4s"
               :opacity        0}
     [:&.active {:opacity 1}]]]])

(def members-style-css (css members-style))

(rum/defc members-view < rum/reactive []
  (let [last-cr (rum/react *?last-concluded-round)
        stake-map (hg/concluded-round->stake-map last-cr)]
    [:<>
     [:style members-style-css]
     [:div.members
      (for [member-name hg-members/names]
        (let [member  (hg-members/member-name->person member-name)
              active? (contains? stake-map member-name)
              stake-pos (/ (get stake-map member-name) hg/total-stake)]
          [:div.member {:class (when active? "active")
                        :style {:left  (idx-view-position-x (:member/idx member))}}
           (case (:member/gender member)
             :male   (hga-avatars/male-avatar   (color-rgba-str (:member/color-rgb member) 1) (color-rgba-str (:member/color-rgb member) stake-pos))
             :female (hga-avatars/female-avatar (color-rgba-str (:member/color-rgb member) 1) (color-rgba-str (:member/color-rgb member) stake-pos))
             [:div "unknown gender"])
           [:div.member-name {:style {:color (if (hg-members/hardly-reachable-member-names member-name)
                                               "lightgray" "black")}}
            member-name]]))]]))


(rum/defc viz-wrapper
  <
  rum/reactive
  {:did-mount
   (fn [state]
     (let [dom-node (rum/dom-node state)]
       (reset! hga-state/*scroll-by! (fn [px] (.scrollBy dom-node 0 px)))
       (.addEventListener dom-node "scroll"
                          ;; V may cause stale viz-scroll-top
                          (once-per-render #(do #_(reset! hga-playback/*playing? false)
                                                (reset! hga-state/*viz-scroll-top (.-scrollTop (.-target %)))
                                                ))))
     state)}
  []
  [:div#viz-wrapper
   (viz)])

(rum/defc viz-section-controls-view < rum/reactive
  []
  [:div {:style {:position :absolute
                  :top "0px"}}
   [:button {:on-click #(swap! hga-state/*playback-attached-to-scroll? not)}
    (str (if (rum/react hga-state/*playback-attached-to-scroll?) "detach" "attach") " playback to scroll")]
   (hga-playback/playback-controls-view)])

(rum/defc viz-section-view
  []
  [:div {:style {:position :relative}}
   #_
   (viz-section-controls-view)

   (viz-wrapper)

   (members-view)])

(rum/defc view []
  [:div#view
   [:style (css styles)]
   #_(hga-analysis/graph-view)
   (inspector-view)
   (viz-section-view)])
