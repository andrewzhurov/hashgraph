(ns hashgraph.app.page
  (:require
   [cljs.reader]
   [garden.core :refer [css ]]
   #_[garden.def :refer-macros [defkeyframes]]
   [garden.stylesheet :refer [at-keyframes]]
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
                                    merge-attr-maps]
    :as hga-inspector]
   [hashgraph.main :as hg]
   [hashgraph.utils2 :refer-macros [td time3]]
   [hashgraph.app.log]
   [rum.core :as rum]
   [taoensso.timbre :refer-macros [spy info]]
   [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
   [clojure.set]
   [garden.color :as gc]
   [cljs.math :refer [floor ceil]]
   [clojure.set :as set]
   [hashgraph.members :as hg-members]))

(defn l [msg value] (js/console.log msg value) value)
(defn cl [pred-or-bool msg value]
  (or (and (or (and (fn? pred-or-bool)
                    (pred-or-bool value))
               (and (not (fn? pred-or-bool))
                    pred-or-bool))
           (l msg value))
      value))

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
   [:.debug {:position :fixed
             :background-color "white"
             :top "0px"
             :right "0px"
             :white-space "pre-wrap"
             :width "60vw"
             :height "100vh"
             :scroll-y "scroll"}]
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

(def animate-scale
  [:animateTransform
   {:attributeName "transform"
    :attributeType "XML"
    :type "scale"
    :from "1"
    :to "1.5"
    :dur "0.5s"
    #_#_:repeatCount 1}])

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


(def event-view-key-fn {:key-fn (fn [{:keys [coords]}] (str coords))})

(def event-view-error-on-final-round-change
  {:should-update (fn [{[{old-r :r old-r-final? :r-final? :as old-args}] :rum/args :as old-state}
                       {[{new-r :r new-r-final? :r-final? :as new-args}] :rum/args :as new-state}]
                    (when-let [error (cond (and old-r-final? (not new-r-final?)) (new js/Error (str "round changed from final to not final: " (pr-str old-args) " " (pr-str new-args)))
                                           (and old-r-final? (not= old-r new-r)) (new js/Error (str "round been final but changed to a different one: " (pr-str old-args) " " (pr-str new-args))))]
                      (js/throw error))
                    (not= old-state new-state))})

(rum/defcs event-view < rum/static rum/reactive event-view-key-fn event-view-error-on-final-round-change
  [state {:keys [evt r r-final? r-concluded? coords x y share-stake? color fill witness? stake-map votes] :as event-info}]
  (let [[x y] coords
        share-stake?  (some-> evt :event/tx :tx/fn-id (= :share-stake))]
    [:g.event (merge-attr-maps
               (inspectable evt)
               {:class [(when r-final? "r-final")
                        (when r-concluded? "r-concluded")
                        (when share-stake? "tx-share-stake")]})

     [:circle.view-bounds
      {:r            wit-view-r
       :cx           x
       :cy           y
       :fill         :transparent}]

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
        (for [[stake-holder stake-holded] stake-map]
          [:g.stake {:key stake-holder}
           (let [member                                                               (-> stake-holder hg-members/member-name->person)
                 [start-vote-circumferance vote-circumferance end-vote-circumferance] (vote-circumferance-start+for+end stake-holder stake-map)]
             [:circle {:r                vote-view-r
                       :cx               x
                       :cy               y
                       :fill             :transparent
                       :stroke           (color-rgba-str (:member/color-rgb member) 1)
                       :stroke-width     vote-view-stroke-width
                       :stroke-dasharray (str "0 " start-vote-circumferance " " vote-circumferance " " vote-view-circumferance)}])])
        ;; shade concluded votes
        #_[:circle {:r    wit-view-r
                    :cx   x
                    :cy   y
                    :fill "rgba(0,0,0,0.5)"}]])

     (when votes
       [:g.votes
        (for [{:vote/keys [from-event to-event value] :as vote} votes]
          (let [member-name                                                          (:event/creator from-event)
                member                                                               (-> member-name hg-members/member-name->person)
                [start-vote-circumferance vote-circumferance end-vote-circumferance] (vote-circumferance-start+for+end member-name (:vote/stake-map vote))]
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
                       :stroke-dasharray (str "0 " start-vote-circumferance " " vote-circumferance " " vote-view-circumferance)}]]))
        ;; shade concluded votes
        #_[:circle {:r    wit-view-r
                    :cx   x
                    :cy   y
                    :fill "rgba(0,0,0,0.5)"}]])

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
      {:class [(when r-final? "final")]
       :x     (+ x evt-view-r)
       :y     (- y evt-view-r)}
      r]
     ]))



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

(defn view-bounds [scroll-top]
  (let [window-height js/window.innerHeight
        view-bound-min scroll-top
        view-bound-max (+ scroll-top window-height)]
    [view-bound-min view-bound-max]))

(def above-view??
  (fn [view-bound-max]
    (fn [[_x y]]
      (> y view-bound-max))))

(def below-view??
  (fn [view-bound-min]
    (fn [[_x y]]
      (< y view-bound-min))))

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

(rum/defcs viz <
  rum/static
  rum/reactive
  #_hga-render-bounds/mixin
  [state creator-hg-map]
  (profile
   {}
   (let [hgs          (vals creator-hg-map)
         #_#_
         highlighted? (fn [evt]
                        (when-let [hovered-evt hovered-evt]
                          (or (= hovered-evt evt)
                              (hg/ancestor? hovered-evt evt))))

         #_#_
         top-witnesses (->> creator-hg-map
                            (map (fn [[_ hg]] (if (hg/witness? hg)
                                                hg
                                                (hg/self-witness hg))))
                            (filter some?))



         #_#_#_#_
         view-tips     (-> view-evts hg/tips)
         rendered-evts (-> (set view-evts)
                           (into (-> view-evts
                                     hg/tips
                                     (->> (mapcat (fn [view-tip] (hg/children evts view-tip))))))
                           (into (-> view-evts
                                     hg/bottoms
                                     (->> (mapcat hg/parents)))))

         ;; view-evts+1
         ;; TODO fix disappearing ref line of from an event above view to event below view
         #_#_
         view-evts
         (->> hgs
              (filter some?)
              (map (fn [hg]
                     ((fn get-view-evts [acc evt]
                        (let [stop?   (or (nil? (hg/self-parent evt))
                                          (and (not-empty (:in-view-acc acc))
                                               (not (in-view? evt))))
                              new-acc (cond (in-view? evt)                        (update acc :in-view-acc conj evt)
                                            (or (in-view? (hg/self-parent evt))
                                                (in-view? (hg/other-parent evt))) (update acc :above-view-acc conj evt)
                                            :else                                 acc)]
                          (if stop?
                            new-acc
                            (recur new-acc (hg/self-parent evt)))))
                      {:in-view-acc    []
                       :above-view-acc []}
                      hg)))
              (mapcat vals)
              (apply concat)
              set)

         #_#_
         view-evts
         (reduce
          (fn [acc hg]
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
             acc hg))
          []
          hgs)

         #_ (->> hgs
                (filter some?)
                (map )
                (reduce into #{}))

         #_#_#_#_
         below-view-evts (->> view-evts hg/bottoms (mapcat hg/parents) set)
         above-view-evts (->> hgs
                              (mapcat (fn [hg] ((fn get-above-evts
                                                  [acc evt]
                                                  (cond (or (view-evts evt) (nil? (hg/self-parent evt))) acc
                                                        (some->> (hg/self-parent evt) (view-evts))       (get-above-evts (conj acc evt) (hg/self-parent evt))
                                                        :else                                            (get-above-evts acc (hg/self-parent evt))))
                                                [] hg)))
                              set)

         #_#_
         rendered-evts (-> view-evts
                           #_#_
                           (into below-view-evts)
                           (into above-view-evts))
         ?alice-tip            (hg/creator-hg-map->?alice-tip creator-hg-map)
         ?last-concluded-round (some-> ?alice-tip hg/->?concluded-round)
         ?last-received-event  (some-> ?last-concluded-round hg/concluded-round->received-event)
         ?e->received-evt
         (when ?last-received-event
           (clojure.set/index (take-while some? (iterate :received-event/prev-received-event ?last-received-event)) [:received-event/event]))
         ]

     ;; (when ?alice-tip            (log-set! [:alice-tip] ?alice-tip))
     ;; (when ?last-concluded-round (log-set! [:last-concluded-round] ?last-concluded-round))
     ;; (when ?last-received-event  (log-set! [:last-received-event] ?last-received-event))

     #_(log-set! (some->> ?last-concluded-round
                          (iterate :concluded-round/prev-concluded-round)
                          (take-while some?)))

     #_(log-set! ?last-received-event)
     #_(log-set! (some->> ?last-received-event
                          (iterate :received-event/prev-received-event)
                          (take-while some?)
                          (map (fn [re] {:re             re
                                         :db             (hg/?received-event->derived-db re)
                                         :sm             (hg/?received-event->stake-map re)
                                         :sm-total-stake (-> (hg/?received-event->stake-map re)
                                                             (->> (map val)
                                                                  (reduce +))
                                                             )
                                         :anomaly?       (-> (hg/?received-event->stake-map re)
                                                             (->> (map val)
                                                                  (reduce +))
                                                             (> hg/total-stake))}))))
     #_(when hovered-evt
         (spy
          (-> evts hg/tips
              (->> (map (fn [tip]
                          {:tip               tip
                           :votes             (hg/votes tip hovered-evt)
                           :voting-concluded? (hg/voting-concluded? tip hovered-evt)
                           :votes-fract-true  (hg/votes-fract-true tip hovered-evt)}))))))

     [:<>
      [:svg#viz {:height (hga-infini-events/viz-height creator-hg-map)}

       [:g.hgs-refs
        (for [hg hgs]
          (evt-refs-view hg))]

       [:g.events-view
        (for [hg  hgs
              evt (take-while some? (iterate hg/self-parent hg))
              :let [coords                                   (evt-view-position evt)
                    color                                    (-> evt
                                                                 hg/event->person
                                                                 :member/color-rgb
                                                                 color-rgba-str)
                    {r        :round/number
                     r-final? :round/final?
                     r-?cr    :round/?cr} (hg/round evt ?last-concluded-round nil)
                    ?atop-cr (some->> r-?cr
                                      (iterate :concluded-round/prev-concluded-round)
                                      (take-while some?)
                                      (some (fn [?cr] (when (< (:concluded-round/r ?cr) r) ?cr))))

                    witness? (hg/witness? evt ?last-concluded-round)

                    ?received-event (some-> ?e->received-evt
                                            (get {:received-event/event evt})
                                            first)

                    fill
                    (if-let [{:keys [received-event/color]} ?received-event]
                      color
                      "white")

                    to-receive-votes? (and witness?
                                           r-final? #_(<= r (inc (:concluded-round/r ?last-concluded-round))))

                    receives-votes? (and to-receive-votes?
                                         ?alice-tip
                                         (hg/ancestor? ?alice-tip evt))

                    event-info {:evt           evt
                                :r             r
                                :r-final?      r-final?
                                :r-concluded?  (and r-final? (>= (:concluded-round/r ?last-concluded-round) r))
                                :witness?      witness?
                                :coords        coords
                                :color         color
                                :fill          fill
                                :analysis?     (hga-inspector/->analysis?)
                                :in-inspected? (hga-inspector/->in-inspected? evt)
                                :in-peeked?    (hga-inspector/->in-peeked? evt)}]]

          (event-view (cond-> event-info
                        to-receive-votes?
                        (assoc :stake-map (hg/?concluded-round->stake-map ?atop-cr))

                        receives-votes? ;; no votes to lately arrived evt of the last cr
                        (assoc :votes (:concluded-voting/votes (hg/->?concluded-voting ?alice-tip evt ?atop-cr))))))]


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
      ]

     #_(when hovered-evt
         (let [top-witness (some (fn [w] (and (= (:event/creator w) "Alice") w)) top-witnesses)]
           [:div.debug
            (let [debug-info
                  {#_#_:evt        hovered-evt
                   :received-time  (hg/received-time top-witness hovered-evt)
                   #_#_:learned-by (hg/learned-member-learned-evt  hovered-evt)}]
              (with-out-str (cljs.pprint/pprint debug-info)))])))))

(rum/defcs viz-wrapper
  <
  rum/reactive
  hga-infini-events/mixin
  [_]
  [:div#viz-wrapper
   (viz (rum/react hga-events/*creator-hg-map))])

(rum/defc view < rum/reactive []
  [:div#view
   [:style (css styles)]
   #_(hga-analysis/graph-view)
   (hga-inspector/view)
   (viz-wrapper)])
