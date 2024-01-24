(ns hashgraph.app.page
  (:require
   [garden.core :refer [css ]]
   [garden.stylesheet :refer [at-keyframes]]
   [garden.color :as gc]
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
   [hashgraph.app.infini-events :as hga-infini-events]
   [hashgraph.app.analysis :as hga-analysis]
   [hashgraph.app.inspector :refer [log! unlog! log-set!
                                    inspect! uninspect! toggle-inspect!
                                    inspectable
                                    view]
    :rename {view inspector-view}
    :as hga-inspector]
   [hashgraph.main :as hg]
   [hashgraph.members :as hg-members]
   [hashgraph.utils
    :refer [merge-attr-maps color-rgba-str]
    :refer-macros [defn* l]]
   [hashgraph.utils2 :refer-macros [td]]
   [rum.core :as rum]
   [taoensso.timbre :refer-macros [spy info]]
   [taoensso.tufte :as tufte :refer [defnp fnp p profiled profile]]
   [cljs.math :refer [floor ceil]]
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

     (when-let [[parent-x parent-y] (some-> (hg/other-parent evt) evt-view-position)]
       (let [#_#_line-highlighted? (and (highlighted? evt)
                                    (highlighted? (hg/parent evt)))]
         [:line {:x1    x
                 :y1    y
                 :x2    parent-x
                 :y2    parent-y
                 :style {:stroke       :lightgray #_(if line-highlighted? :blue :lightgray)
                         :stroke-width "2px"}}]))]))

(rum/defc events-refs-view < rum/static
  [[evt & rest-evts]]
  [:<>
   (if (empty? rest-evts)
          [:<>]
          (events-refs-view rest-evts))
   (event-refs-view evt)])

(rum/defc evt-refs-view < rum/static
  [evt]
  [:<>
   (if (nil? (hg/self-parent evt))
     [:<>]
     (evt-refs-view (hg/self-parent evt)))
   (event-refs-view evt)])

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

#_(def event-view-error-on-final-round-change
  {:should-update (fn [{[{old-r :r old-r-final? :r-final? :as old-args}] :rum/args :as old-state}
                       {[{new-r :r new-r-final? :r-final? :as new-args}] :rum/args :as new-state}]
                    (when-let [error (cond (and old-r-final? (not new-r-final?)) (new js/Error (str "round changed from final to not final: " (pr-str old-args) " " (pr-str new-args)))
                                           (and old-r-final? (not= old-r new-r)) (new js/Error (str "round been final but changed to a different one: " (pr-str old-args) " " (pr-str new-args))))]
                      (js/throw error))
                    (not= old-state new-state))})

(rum/defcs event-view < rum/static rum/reactive event-view-key-fn
  [state {:event-info/keys [event round r-concluded? coords x y share-stake? color fill witness? stake-map votes received-event] :as event-info}]
  (let [{r        :round/number
         r-final? :round/final?} round
        [x y] coords
        share-stake?  (some-> event :event/tx :tx/fn-id (= :share-stake))]
    [:<>
     [:g.event (merge-attr-maps
                (inspectable event)
                {:class [(when r-final? "r-final")
                         (when r-concluded? "r-concluded")
                         (when share-stake? "tx-share-stake")]})

      [:circle.view-bounds
       {:r    wit-view-r
        :cx   x
        :cy   y
        :fill :transparent}]

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
          :x      (- x evt-view-r)
          :y      (- y evt-view-r)
          :href   "https://www.svgrepo.com/show/446085/transfer.svg"
          :style  {:transform-box    "fill-box"
                   :transform-origin "center"
                   :transform        "scale(0.7)"
                   ;; :filter "invert(100%)"
                   }
          #_      "url(https://vectorified.com/images/invite-icon-3.png)"}])

      [:text.round
       {:x (+ x evt-view-r)
        :y (- y evt-view-r)}
       r]]

     (when received-event
       (let [{:received-event/keys [event middle-learned-events received-time concluded-round r-idx color]} received-event

             received-y (t->view-position-y received-time)
             received-x (-> (idx-view-position-x (count hg-members/names))
                            (+ (* r-idx hga-events/evt-view-s)))]

         [:g.receivend-event-view (merge {:key (str (:coords event-info))}
                                         (inspectable received-event))
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

          (when (hga-inspector/->in-peeked? event)
            (for [mle middle-learned-events]
              [:line {:x1           (evt-view-position-x mle)
                      :y1           (evt-view-position-y mle)
                      :x2           received-x
                      :y2           received-y
                      :stroke       "blue"
                      :stroke-width "2px"}]))

          [:circle.event
           {:r            evt-view-r
            :cx           received-x
            :cy           received-y
            :stroke       color
            :stroke-width 1
            :fill         fill}]]))]))



#_
(rum/defc events-view < rum/static
  [[evt & rest-evts]]
  [:<>
   (if (empty? rest-evts)
     [:<>]
     (events-view rest-evts))
   (let [member   (-> evt hg/creator hg-members/members)
         color    "gray" #_(color-rgba-str (:member/color-rgb member))]
     (event-view {:evt      evt
                  :coords   (evt-view-position evt)
                  :witness? (hg/witness? evt)
                  :color    color}))])


#_
(defn with-received-votes-xf [witnesses]
  (map (fn [evt-info]
         (let [evt (:evt evt-info)]
           (cond-> evt-info
             (hg/witness? evt)
             (assoc :received-votes
                    (->> witnesses
                         (into {} (map (fn [w] [(:event/creator w)
                                                {:fract-true (hg/votes-fract-true w evt)
                                                 :concluded? (hg/voting-concluded? w evt)}]))))))))))

(defn with-hovered?-xf [hovered-evt]
  (map (fn [evt-info]
         (cond-> evt-info
           (= (:evt evt-info) hovered-evt)
           (assoc :hovered? true)))))

(def with-coords-xf
  (map (fn [evt-info]
         (assoc evt-info :coords (evt-view-position (:evt evt-info))))))



(defn ->view-bound-min [scroll-top]
  scroll-top)

(defn ->view-bound-max [scroll-top]
  (+ scroll-top js/window.innerHeight))

(defn ->view-bounds [scroll-top]
  (let [view-bound-min (->view-bound-min scroll-top)
        view-bound-max (->view-bound-max scroll-top)]
    [view-bound-min view-bound-max]))

(defn ->above-view? [y scroll-top]
  (> y (->view-bound-max scroll-top)))

(defn ->below-view? [y scroll-top]
  (< y (->view-bound-min scroll-top)))

#_(def in-view??
  (fn [scroll-top]
    (let [[view-bound-min view-bound-max] (view-bounds scroll-top)]
      (fn [evt]
        (let [y (evt-view-position-y evt)]
          (and (> y view-bound-min)
               (< y view-bound-max)))))))
#_#_#_#_
above-view? (fn [evt]
              (let [y (evt-view-position-y evt)]
                (> y view-bound-max)))
below-view? (fn [evt]
              (let [y (evt-view-position-y evt)]
                (< y view-bound-max)))
#_
((fn get-in-views [acc evt]
                 (let [stop?   (or (nil? (hg/self-parent evt))
                                   (and (some-> acc last hg/creator (= (hg/creator evt)))
                                        (some-> acc last in-view?)
                                        (not (in-view? evt))))
                       new-acc (or (and (or (in-view? evt)
                                            (and (above-view? evt)
                                                 (or (below-view? (hg/self-parent evt))
                                                     (below-view? (hg/other-parent evt)))))
                                        (conj acc evt))
                                   acc)]
                   (if stop?
                     new-acc
                     (recur new-acc (hg/self-parent evt)))))
               acc hg)

#_
(defn filter-in-view-xf [scroll-top]
  (let [in-view?? (in-view? scroll-top)]
    (filter in-view??)))


(def *scroll-top (atom 0))
(def *viz-dom-node (atom nil))
(def *issue-events (rum/derived-atom [*scroll-top *viz-dom-node hga-events/*c->hg] ::*issue-events
                     (fn [scroll-top viz-dom-node c->hg]
                       (when viz-dom-node
                         (let [enough? (fn [some-c->hg]
                                         (let [viz-height   (hga-infini-events/->viz-height some-c->hg)
                                               window-height js/window.innerHeight
                                               scroll-rest   (- viz-height (+ scroll-top window-height))]
                                           (> scroll-rest hga-infini-events/load-area-height)))]
                           (when-not (enough? c->hg)
                             (hga-events/issue! enough?)))))))

(def *rendered-evts
  (rum/derived-atom [hga-events/*c->hg *scroll-top] ::*rendered-evts
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
  (rum/derived-atom [hga-events/*c->hg] ::*alice-tip
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

      (or received-votes? receives-votes?) ;; been no votes to lately arrived evt of the last cr
      (assoc :event-info/votes (:concluded-voting/votes (hg/->?concluded-voting voting-by event ?atop-cr))))))

(def *rendered-evt-infos
  (rum/derived-atom [*rendered-evts *?alice-tip *?last-concluded-round *?last-received-event] ::*rendered-evt-infos
    (fn [rendered-evts ?alice-tip ?last-concluded-round ?last-received-event]
      (let [event->received-event (?received-event->event->received-event ?last-received-event)]
        (->> rendered-evts
             (map (fn [event]
                    (let [{r        :round/number
                           r-final? :round/final?
                           r-cr     :round/cr :as round} (hg/->round event ?last-concluded-round) ;; been very costly :hashgraph.app.page/round
                          ;; nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total
                          ;;     49        0ns        0ns        0ns      105ms      223ms      223ms        9ms ±188%      449ms    99%

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
                      ;; so we can memoize concluded stuff
                      (->enriched-event-info (cond-> #:event-info{:event             event
                                                                  :round             round
                                                                  :witness?          witness?
                                                                  :to-receive-votes? to-receive-votes?
                                                                  :received-votes?   received-votes?
                                                                  :receives-votes?   receives-votes?
                                                                  :voting-by         voting-by
                                                                  :?concluded-in-cr  ?concluded-in-cr}
                                               ?received-event (assoc :event-info/received-event ?received-event)))))))))))

(rum/defc viz <
  rum/reactive
  {:did-mount (fn [state] (reset! *viz-dom-node (rum/dom-node state)) state)}
  []
  (let [rendered-evts      (rum/react *rendered-evts)
        rendered-evt-infos (rum/react *rendered-evt-infos)]
    [:svg#viz {:height (hga-infini-events/->viz-height (rum/react hga-events/*c->hg))}

     [:g.hgs-refs
      (for [rendered-evt rendered-evts]
        (event-refs-view rendered-evt))]

     [:g.events-view
      (for [evt-info rendered-evt-infos]
        (event-view evt-info))]

     #_
     [:g.received-events-view
      (when-let [last-received-event ?last-received-event]
        (for [re (take-while some? (iterate :received-event/prev-received-event last-received-event))]
          (let [{:received-event/keys [event middle-learned-events received-time concluded-round r-idx color]} re

                received-y (t->view-position-y received-time)
                received-x (-> (idx-view-position-x (count hg-members/names))
                               (+ (* r-idx hga-events/evt-view-s)))

                #_#_
                color (-> event
                          hg/event->person
                          :member/color-rgb
                          color-rgba-str)
                event-info
                {:evt           event
                 :coords        [received-x received-y]
                 ;; :color    color
                 :analysis?     (hga-inspector/->analysis?)
                 :in-inspected? (hga-inspector/->in-inspected? event)
                 :in-peeked?    (hga-inspector/->in-peeked? event)
                 :fill          color}]

            [:g.receivend-event-view {:key (str (:coords event-info))}
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

             (when (hga-inspector/->in-peeked? event)
               (for [mle middle-learned-events]
                 [:line {:x1           (evt-view-position-x mle)
                         :y1           (evt-view-position-y mle)
                         :x2           received-x
                         :y2           received-y
                         :stroke       "blue"
                         :stroke-width "2px"}]))

             (event-view event-info)])))]
     ;; {:event/round
     ;;  :event/witness?
     ;;  :event/received-votes
     ;;  :event/voting-concluded?
     ;;  :event/creator
     ;;  :event/self-parent
     ;;  :event/other-parent}
     ;; {:received-event/event ordered-event
     ;;  :received-event/received-round r
     ;;  :received-event/by-events #{}
     ;;  :received-event/prev-received-event prev-event}
     ;; {:round/concluded? true
     ;;  :round/witnesses #{}
     ;;  :round/unique-famous-witnesses #{}
     ;;  :round/events #{}
     ;;  :round/received-events #{}
     ;;  :round/last-received-event evt
     ;;  :received-round/size (count ordered-events)
     ;;  :received-round/}


     #_(let [[view-bound-min view-bound-max] (view-bounds scroll-top)
             above-view?                     (above-view?? view-bound-max)
             below-view?                     (below-view?? view-bound-min)])
     #_
     (for [hg hgs]
       (into [:g.hg] (comp (take-while some?)
                           (map (fn [evt] {:evt   evt
                                           :color (-> evt
                                                      :event/creator
                                                      hg-members/members
                                                      :member/color-rgb
                                                      color-rgba-str )}))
                           with-coords-xf
                           #_(drop-while (comp above-view? :coords))
                           #_(take-while (comp (complement below-view?) :coords))
                           (with-hovered?-xf hovered-evt)
                           #_(with-received-votes-xf top-witnesses)
                           (map event-view))
             (iterate hg/self-parent hg)))



     #_
     [:g.round-concluded
      (for [top-witness top-witnesses
            r           (map inc (range 0 (hg/round top-witness)))
            :when       (hg/round-concluded? top-witness r)
            :let        [r-highest-wit-position-y (apply max (map evt-view-position-y (hg/round-witnesses top-witness r)))
                         concluding-witness? (= top-witness (some (fn [w] (and (= (:event/creator w) "Alice") w)) top-witnesses))]]
        [:g
         [:line {:x1    0
                 :y1    r-highest-wit-position-y
                 :x2    800
                 :y2    r-highest-wit-position-y
                 :style {:stroke       :gray
                         :stroke-width "2px"}}]

         (let [color1 "blue"
               color2 "green"]
           [:defs
            [:linearGradient {:id "even-round-color"}
             [:stop {:offset "0%" :stop-color color1}]
             [:stop {:offset "100%" :stop-color color2}]]
            [:linearGradient {:id "odd-round-color"}
             [:stop {:offset "0%" :stop-color color2}]
             [:stop {:offset "100%" :stop-color color1}]]])
         (let [color1 "blue"
               color2 "green"
               calculate-gradient-degrees
               (fn [[x1 y1] [x2 y2]]
                 (let [angle   (Math/atan2 (- y2 y1) (- x2 x1))
                       degrees (* (/ angle Math/PI) 180)]
                   degrees))

               calculate-gradient-stops
               (fn [total-segments current-segment]
                 (let [start-percent (str (* (/ current-segment total-segments) 100) "%")
                       end-percent   (str (* (/ (inc current-segment) total-segments) 100) "%")]
                   [start-percent end-percent]))

               calculate-gradient
               (fn [coord1 coord2 total-segments current-segment]
                 (let [degrees                     (calculate-gradient-degrees coord1 coord2)
                       [start-percent end-percent] (calculate-gradient-stops total-segments current-segment)]
                   [:linearGradient {:id (str coord1 coord2)}
                    [:stop {:offset start-percent :stop-color color1}]
                    [:stop {:offset end-percent :stop-color color2}]]))]
           (when concluding-witness?
             (let [received-evts (get (hg/round-whether-received-events top-witness r) true)
                   ordered-evts  (hg/order-events top-witness received-evts)
                   x             (idx-view-position-x hg-members/members-count)]
               [:g
                [:path {:d      (apply str
                                       "M " (evt-view-position-x (first ordered-evts)) " " (evt-view-position-y (first ordered-evts)) " "
                                       (map (comp (fn [[x y]]
                                                    (str "L " x " " y " "))
                                                           evt-view-position) (rest ordered-evts)))
                                 :stroke :blue #_ "url(#even-round-color)"
                                 :fill   :transparent}]

                         (when (not-empty ordered-evts)
                           #_(for [[idx ordered-evt] (map-indexed (fn [idx e] [idx e]) ordered-evts)]
                               (if (> idx 0)
                                 (let [prev-evt        (get ordered-evts (dec idx))
                                       [prev-x prev-y] (evt-view-position (spy prev-evt))
                                       [this-x this-y] (evt-view-position ordered-evt)]
                                   [:line {:x1    prev-x
                                           :y1    prev-y
                                           :x2    this-x
                                           :y2    this-y
                                           :style {:stroke       :gold
                                                   :stroke-width "2px"}}])))
                           (let [y-start (apply max (map evt-view-position-y (hg/round-witnesses top-witness (dec r))))]
                             (for [[idx received-evt] (map-indexed (fn [idx evt] [idx evt]) ordered-evts)
                                   :let               [member (-> received-evt :event/creator hg-members/members)
                                                       pad 4
                                                       y (-> y-start
                                                             (+ (/ evt-view-s 2))
                                                             (+ pad)
                                                             (+ (* idx (+ evt-view-s pad))))]]
                               (event-view {:evt    received-evt
                                            :coords [x y]
                                            :big    false
                                            :color  "gray"}))))])))])]]
             ))


(rum/defc viz-wrapper
  <
  #_ hga-infini-events/mixin
  {:did-mount
   (fn [state]
     ;; track scroll
     (.addEventListener (rum/dom-node state) "scroll" #(reset! *scroll-top (.-scrollTop (.-target %))))
     state)}
  []
  [:div#viz-wrapper
   (viz)])

(rum/defc view []
  [:div#view
   [:style (css styles)]
   #_(hga-analysis/graph-view)
   (inspector-view)
   (viz-wrapper)])
