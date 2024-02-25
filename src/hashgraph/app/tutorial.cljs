(ns hashgraph.app.tutorial
  (:require-macros [hashgraph.app.tutorial :refer [i]])
  (:require [goog.object]
            [clojure.set :as set]
            [clojure.string :as str]
            [rum.core :as rum]
            [garden.core :refer [css]]
            [garden.units :refer [px]]
            [garden.selectors :as gs]
            [hashgraph.main :as hg]
            [hashgraph.app.state :as hga-state]
            [hashgraph.app.inspector :refer [inspectable] :refer-macros [inspectable-el] :as hga-inspector]
            [hashgraph.app.transitions :refer [tt] :as hga-transitions]
            [hashgraph.members :as hg-members]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.playback :as hga-playback]
            [hashgraph.app.view :refer [t] :as hga-view]
            [hashgraph.app.styles :refer [reg-styles!]]
            [hashgraph.app.utils :as hga-utils]
            [hashgraph.utils.core :refer [hash=] :refer-macros [l] :as utils]))

(defn ->events [& from-member-to-member<]
  (->> from-member-to-member<
       (reduce (fn [events-acc> [from-member to-member & [?to-tutor]]]
                 (let [c->hg         (hga-events/events>->c->hg events-acc>)
                       ?sp           (get c->hg to-member)
                       ?op           (get c->hg from-member)
                       creation-time (if (and (nil? ?sp) (nil? ?op))
                                       1
                                       (hga-events/->next-creation-time (max (hg/creation-time ?sp)
                                                                             (hg/creation-time ?op))))
                       event         (cond-> (hash-map hg/creator to-member
                                                       hg/creation-time creation-time)
                                       ?sp       (assoc hg/self-parent ?sp)
                                       ?op       (assoc hg/other-parent ?op)
                                       ?to-tutor (with-meta {::to-tutor ?to-tutor}))]
                   (conj events-acc> event)))
               '())
       reverse))

(defonce tutorial-events
  (let [m1 (rand-nth (hga-events/names-without-name hg-members/initial-member-names hg/main-creator))
        m2 hg/main-creator
        m3 (first (set/difference (set hg-members/initial-member-names) #{m1 m2}))]
    (->events
     [nil m2]
     [m2 m1]
     [m1 m2]
     [m2 m1]
     [m1 m3]
     [m3 m2 ::strongly-seeing]

     ;; r2 ws
     [m2 m1]
     [m1 m2 ::stake-map]
     [m2 m3 ::witnesses-will-vote]

     [m3 m1]
     [m1 m2 ::votes]
     [m2 m1 ::why-all-this-p1]
     [m1 m3 ::why-all-this-p2]
     [m3 m1 ::why-all-this-p3]

     ;; r4 w
     [m1 m2]

     [m2 m1 ::and-the-beat-goes-on]

     )))

(defn init-tutorial-events! []
  (reset! hga-playback/*just-left< tutorial-events))

(def cart-style
  {:padding       "10px"
   :border        "1px solid lightgray"
   :border-radius (px hga-view/border-radius)})

(def styles
  [[:#tutorial {:position "relative"}
    [:.tutor-wrapper (merge cart-style
                            {:position         :absolute
                             :box-sizing       :border-box
                             :background-color :lavender
                             :transition       (t :opacity (/ tt 1.5) "ease-in" (/ tt 5))
                             :z-index          hga-view/tutorial-z})
     [:.tutor {:white-space :pre-line}
      [(gs/& (gs/not :.unbound)) {:max-width (px hga-view/tutorial-size)}]
      [:.inspectable {:color     :maroon
                      :min-width :fit-content
                      :max-width :fit-content
                      :width     :fit-content
                      :display   :inline}]
      ]]]])

(def styles-horizontal
  [[:#tutorial
    [:.tutor-wrapper {:bottom    "30px"
                      :transform "translateX(-50%)"}]]])

(def styles-vertical
  [[:#tutorial
    [:.tutor-wrapper {;; :width "100%"
                      :transform "translateY(-50%)"
                      :left      "2vw"
                      :width     "96vw"
                      :opacity   "0.9"}
     [:.tutor {:max-width "100% !important"
               :width     "100% !important"}]]]])

(reg-styles! ::tutorial styles styles-horizontal styles-vertical)


(defn tutor [& text-with-is]
  (into [:div.tutor text-with-is]))

(def ->y hga-view/evt->y)

(declare *left-view->state-fn)
(declare *tutors-ordered)

(def view->state-fn
  {
   (rum/defc tutor-event-view < rum/reactive
     [event]
     [:div.tutor
      "Members create events, such as " (i event "this one") ".\n\n"

      (if hga-view/view-mode-horizontal?
        "(Ctrl+hover to peek, click to pin ^)"
        "(tap to highlight)")])
   (fn [event]
     (when (= 1 (:event/creation-time event))
       {::on-event event
        ::y        (->y event)
        ::args     [event]}))

   (rum/defc tutor-other-parent-view < rum/reactive
     [event]
     [:div.tutor.unbound
      "And gossip them to others.\n\n"

      "On receival of an event from another member\n"
      "a " (i event "new event") " is created, referencing\n"
      "just received " (i (hg/other-parent event) "other-parent event") "."])
   (fn [event]
     (when (hg/other-parent event)
       {::on-event event
        ::y        (->y event)
        ::args     [event]}))


   (rum/defc tutor-self-parent-view < rum/reactive
     [event]
     [:div.tutor.unbound
      "When " (i event "an event") " is created it also references\n"
      "previously created by that member event, if there's one,\n"
      "a so-called " (i (hg/self-parent event) "self-parent event") ".\n\n"

      "This alone builds the structure of how members\n"
      "communicated with each other - a (hash)graph of gossip!\n\n"

      "Well, was it shorter than expected? Sorry to disapoint. -_-\n\n"

      "...okaay! We didn't get events ordered yet,\n"
      "now to the virtual voting bit.. it'll be more juicy."])
   (fn [event]
     (when (hg/self-parent event)
       {::type ::self-parent
        ::on-event event
        ::y        (->y event)
        ::args     [event]}))

   (rum/defc tutor-round-view < rum/reactive
     [event round]
     [:div.tutor
      ;; "In order for an " (i event "event") " to be received, it's order must be concluded.\n"
      ;; "This is done by via virtual voting.\n"
      "Step #1 is to determine " (i round "event's round number") "."])
   (fn [event]
     (when (= 3 (hg/index event))
       (let [round (hg/->round event (hg/->concluded-round event))]
         {::on-event  event
          ::on-play   #(reset! hga-state/*show-rounds? true)
          ::on-rewind #(reset! hga-state/*show-rounds? false)
          ::y         (->y event)
          ::args      [event round]})))

   (rum/defc tutor-witness-view < rum/reactive
     [r1-ws]
     [:div.tutor
      "First event of each member in a round is considered to be a witness.\n\n"

      "These are " (i r1-ws "witnesses of 1st round") "."
      #_"This is one interesting twist of voting. Instead of voting on order of each event individually, we can vote on fame of witnesses."])
   (fn [event]
     (when (= (hg/creator event) hg/main-creator)
       (let [cr        (hg/->concluded-round event)
             r1-ws     (hg/round-witnesses event 1 cr)
             latest-rw (->> r1-ws (sort-by :event/creation-time >) first)]
         (when (= 3 (count r1-ws))
           {::on-event  latest-rw
            ::on-play   #(reset! hga-state/*show-witnesses? true)
            ::on-rewind #(reset! hga-state/*show-witnesses? false)
            ::y         (->y latest-rw)
            ::args      [r1-ws]}))))

   (rum/defc tutor-strongly-seeing-view < rum/reactive
     [event sees-m1 sees-m2]
     (let [m1-evt (:sees/seee sees-m1)
           m2-evt (:sees/seee sees-m2)
           m1     (hg/creator m1-evt)
           m2     (hg/creator m2-evt)
           ss-m1  (i sees-m1 "strongly sees")
           ss-m2  (i sees-m2 "strongly sees")]
       [:div.tutor
        "An event may see that many members see another event, a so-called 'strongly see'.\n\n"

        (i event "This event") " " ss-m1 " " (i m1-evt (str m1 "'s event")) "\n"
        " and also " ss-m2 " " (i m2-evt (str m2 "'s event")) "."]))
   (fn [event]
     (when (-> event meta ::to-tutor (= ::strongly-seeing))
       (let [cr                (hg/->concluded-round event)
             [sees-m1 sees-m2] (hg/->strongly-see-r-paths event cr 1)]
         {::on-event event
          ::y        (->y event)
          ::args     [event sees-m1 sees-m2]})))

   (rum/defc tutor-next-round-view < rum/reactive
     [event round [seen-m1 seen-m2 seen-m3]]
     (let [m1-evt (:sees/seee seen-m1)
           m2-evt (:sees/seee seen-m2)
           m3-evt (:sees/seee seen-m3)
           m1     (hg/creator m1-evt)
           m2     (hg/creator m2-evt)
           m3     (hg/creator m3-evt)
           ss-m1  (i [seen-m1] "strongly sees")
           ss-m2  (i [seen-m2] "strongly sees")
           ss-m3  (i [seen-m3] "strongly sees")]
       [:div.tutor
        (i round "Round number") " increases when an " (i event "event") " strongly sees events of a previous round from many members.\n\n"

        (i event "This event") " " ss-m1 " " (i m1-evt (str m1 "'s event")) ",\n"
        "as well " ss-m2 " " (i m2-evt (str m2 "'s event")) ",\n"
        "and finally " ss-m3 " " (i m3-evt (str m3 "'s event")) ",\n"
        "so events of a previous round from many members (all, in this case) "
        "are strongly seen, and so the round number is incremented."]))
   (fn [event] (let [cr    (hg/->concluded-round event)
                     round (hg/->round event cr)]
                 (when (= 2 (:round/number round))
                   (let [strongly-see-r-paths (hg/->strongly-see-r-paths event cr 1)]
                     {::on-event event
                      ::y        (->y event)
                      ::args     [event round strongly-see-r-paths]}))))

   (rum/defc tutor-stake-map-view < rum/reactive
     [rw stake-map]
     [:div.tutor
      (i rw "Round witnesses") " will receive virtual votes regarding their fame from next round witnesses, with vote's weight according to " (i stake-map "member's holded stake") "."])
   (fn [event]
     (when (-> event meta ::to-tutor (= ::stake-map))
       (let [cr        (hg/->concluded-round event)
             r1-rws    (hg/round-witnesses event 1 cr)
             stake-map (-> cr hg/concluded-round->stake-map)]
         {::on-event  event
          ::on-play   #(reset! hga-state/*show-stake-map? true)
          ::on-rewind #(reset! hga-state/*show-stake-map? false)
          ::y         (->y event)
          ::args      [r1-rws stake-map]})))

   (rum/defc tutor-witnesses-will-vote-view < rum/reactive
     [r2-ws r1-ws]
     [:div.tutor.unbound
      "These " (i r2-ws "round 2 witnesses") " will vote on fame\n"
      "of the " (i r1-ws "round 1 witnesses") ".\n"
      "Voting will be done by a round 3 witness.\n\n"

      "The voting is virtual, no actual votes are issued and send,\n"
      "rather each member will derive how other members _would_ vote,\n"
      "based on the history of the communication (a hashgraph) a member has."])
   (fn [event]
     (when (-> event meta ::to-tutor (= ::witnesses-will-vote))
       (let [cr    (hg/->concluded-round event)
             r2-ws (hg/round-witnesses event 2 cr)
             r1-ws (hg/round-witnesses event 1 cr)]
         {::on-event event
          ::y        (->y event)
          ::args     [r2-ws r1-ws]})))

   (rum/defc tutor-votes-view < rum/reactive
     [r3-mw r2-ws r1-ws votes]
     [:div.tutor.unbound
      (i r3-mw "This round 3 witness") " ran virtual voting regarding fame\n"
      "of " (i r1-ws "the round 1 witnesses") " by deriving " (i votes "virtual votes") " \n"
      "from " (i r2-ws "the round 2 witnesses") ".\n\n"

      "Witness is considered to be famous when it received > 2/3 'yes' votes.\n"
      "The next round witness vote 'yes' if it can see that witness.\n\n"

      "And so each of the " (i r2-ws "round 2 witnesses") " voted for each of the " (i r1-ws "round 1 witnesses") ".\n"
      "All of the votes are 'yes'.\n"
      "And so " (i r3-mw "the round 3 witness") " concludes all round 1 witnesses to be famous."])
   (fn [event]
     (when (-> event meta ::to-tutor (= ::votes))
       (let [cr    (hg/->concluded-round event)
             r3-mw  event
             r2-ws (-> (hg/round-witnesses event 2 cr) hga-inspector/accent-all)
             r1-ws (-> (hg/round-witnesses event 1 cr) hga-inspector/accent-all)
             r1-mw (->> r1-ws (some (fn [w] (when (= (hg/creator w) hg/main-creator) w))))
             r1-cr (-> r1-mw (hg/->concluded-round))
             votes (-> (mapcat (fn [r1-w] (hg/->votes r3-mw r1-w r1-cr)) r1-ws) hga-inspector/accent-all)]
         {::on-event r3-mw
          ::y        (->y event)
          ::args     [r3-mw r2-ws r1-ws votes]})))

   (rum/defc tutor-why-all-of-this-p1-view
     []
     [:div.tutor.unbound
      "Phew? Been plenty enough? Well, sorry, it's almost all there is.\n"
      "But what's all this hussle for, again?\n\n"

      "Events may carry a transaction - some arbitrary code to run.\n"
      "Transactions may conflict with each other, e.g., the famous double-spend.\n"
      "Having they be executed in parallel - we'd end up in a messed up state,\n"
      "how horrible would that be?!"])
   (fn [event]
     (when (-> event meta ::to-tutor (= ::why-all-this-p1))
       {::on-event event
        ::y        (->y event)
        ::args     []}))

   (rum/defc tutor-why-all-of-this-p2-view
     []
     [:div.tutor.unbound
      "But let's not PaNiC!\n"
      "One of the solutions to that is to find total order of transactions.\n\n"

      "... and having concluded fame of round witnesses we're able to do just that,\n"
      "events that are seen by all famous witnesses can be put in total order, ha!\n\n"

      "But what _is_ that order??"])
   (fn [event]
     (when (-> event meta ::to-tutor (= ::why-all-this-p2))
       {::on-event event
        ::y        (->y event)
        ::args     []}))

   (rum/defc tutor-why-all-of-this-p3-view < rum/reactive
     [last-re]
     [:div.tutor.unbound
      "That's a fair question, let's try to give a fair answer.\n\n"

      "We could derive 'received time' of " (i (:received-event/event last-re) "an event") " as median time\n"
      "of the " (i (:received-event/learned-by last-re) " events from each member that first learned about it") ".\n\n"

      "This way whatever event gets seen by the majority first\n"
      "is considered to be first - seems fair?"])
   (fn [event]
     (when (= (hg/creator event) hg/main-creator)
       (let [cr (hg/->concluded-round event)]
         (when (= 2 (:concluded-round/r cr))
           (let [last-re (:concluded-round/last-received-event cr)
                 p3-event (->> event hg/events (some (fn [e] (when (-> e meta ::to-tutor (= ::why-all-this-p3)) e))))]
             {::on-event p3-event
              ::y        (->y p3-event)
              ::args     [last-re]})))))

   (rum/defc tutor-events-received-view < rum/reactive
     [r4-cw r2-cr]
     (let [r2-ufws (-> (:concluded-round/ufws r2-cr) hga-inspector/accent-all)
           r2-er   (-> (:concluded-round/es-r r2-cr) hga-inspector/accent-all)]
       [:div.tutor.unbound
        "And so, "(i r2-er "those events") " that are seen\n"
        "by all " (i r2-ufws "unique famous witnesses of a round") " are received,\n"
        "ordered by their 'received time'."])) ;; TODO TADA!
   (fn [event]
     (when (= hg/main-creator (hg/creator event))
       (let [cr (hg/->concluded-round event)
             r  (hg/->round-number event cr)
             w? (hg/witness? event cr)]
         (when (and w?
                    (= 4 r))
           (let [r4-mw      event
                 r2-last-re (-> cr :concluded-round/last-received-event)]
             {::on-event r4-mw
              ::y        (->y r4-mw)
              ::args     [r4-mw cr]})))))

   (rum/defc tutor-and-the-beat-goes-on-view
     []
     [:div.tutor
      "And the beat goes onnn " (if hga-view/view-mode-horizontal?
                                  "-->>"
                                  "VvV")])
   (fn [event]
     (when (-> event meta ::to-tutor (= ::and-the-beat-goes-on))
       {::on-event event
        ::y        (->y event)
        ::args     []}))

   (rum/defc tutor-events-not-received-view < rum/reactive
     [cr es-nr ufws]
     [:div.tutor.unbound
      "When " (i cr "round's been concluded") " there may be " (i es-nr "some events that weren't received") ",\n"
      "that is because they weren't seen by all " (i ufws "the unique famous witnesses") ".\n\n"
      "Oh, well, bad for them. Better luck next time.\n"
      "Okaay, no fret, they'll be received in the next rounds.. why would we drop them?!"])
   (fn [event]
     (when (= hg/main-creator (hg/creator event))
       (let [cr     (hg/->concluded-round event)
             ?es-nr (not-empty (-> cr :concluded-round/es-nr))
             ?ufws  (not-empty (-> cr :concluded-round/ufws))]
         (when (and ?es-nr ?ufws)
           (let [last-e-nr (->> ?es-nr (sort-by hg/creation-time >) first)
                 es-nr     (-> ?es-nr hga-inspector/accent-all)
                 ufws      (-> ?ufws hga-inspector/accent-all)]
             {::on-event event
              ::y        (->y last-e-nr)
              ::args     [cr es-nr ufws]})))))

   (rum/defc tutor-inc-counter-tx-view < rum/reactive
     [[event tx :as event+tx]]
     [:div.tutor.unbound
      "Event may carry a transaction,\n"
      "that is to be executed when event gets received.\n\n"

      "\"Aha, finally the useful bit!\"?\n"
      "Yeeas, the whole point of this excercise.\n\n"

      (i event+tx "This event's transaction") ", issued by " (hg/creator event) ",\n"
      "increments a counter by 1.\n"
      "Not much of a transaction, I know.. " (hg/creator event) "\n"
      " could come up with something more interesting."])
   (fn [event]
     (when (-> event :event/tx :tx/fn-id (= :inc-counter))
       (let [event+tx [(hga-inspector/accent event) (:event/tx event)]]
         {::on-event event
          ::y        (->y event)
          ::args     [event+tx]})))

   (rum/defc tutor-share-stake-tx-view < rum/reactive
     [[event {[{:share-stake/keys [from to ratio]}] :tx/args :as tx} :as event+tx]]
     (let [pron   (if (= :male (-> from hg-members/member-name->person :member/gender))
                    "him"
                    "her")
           amount (if (= (first ratio) (second ratio))
                    "all"
                    (str (first ratio) "/" (second ratio)))]
       [:div.tutor.unbound
        (i event+tx "This event's transaction") ", issued by " (hg/creator event) ",\n"
        "transfers " amount " of holded by " pron " stake to " to ".\n"
        "Whoa! That's interesting, stake's dynamic! \n"
        to " will be invited to participate in this permissioned system.\n\n"

        "(stake holders can transfer only their own stake, in case you wonder,\n"
        " if so try to do others - their txes are considered invalid)"]))
   (fn [event]
     (when (-> event :event/tx :tx/fn-id (= :share-stake))
       (let [event+tx [(hga-inspector/accent event) (:event/tx event)] ]
         {::on-event event
          ::y        (->y event)
          ::args     [event+tx]})))

   (rum/defc tutor-stake-change-view < rum/reactive [cr e+tx+re stake-map-old stake-map-new]
     [:div.tutor
      "In this " (i cr "concluded round") " an " (i e+tx+re "event with share-stake tx been received") ", "
      "after execution of all received events, member's stake changed " (i stake-map-old "from") " " (i stake-map-new "to") ". "
      "Now all the calculations, such as round number, whether an event is a witness & the rest "
      "will be re-done based on the new stake map."])
   (fn [event]
     (when (= hg/main-creator (hg/creator event))
       (when-let [[cr re-with-share-stake-tx]
                  (->> (-> event hg/->concluded-round)
                       (iterate :concluded-round/prev-concluded-round)
                       (take-while some?)
                       reverse
                       (some (fn [cr]
                               (when-let [re-with* (->> cr
                                                        :concluded-round/last-received-event
                                                        (iterate :received-event/prev-received-event)
                                                        (take-while some?)
                                                        (some (fn [re]
                                                                (when (some-> re
                                                                              :received-event/event
                                                                              :event/tx
                                                                              :tx/fn-id
                                                                              (= :share-stake))
                                                                  re))))]
                                 [cr re-with*]))))]
         (let [last-cr-w     (->> cr :concluded-round/ufws (sort-by hg/creation-time >) first)
               wc            (->> cr :concluded-round/witness-concluded)
               stake-map-old (->> cr :concluded-round/prev-concluded-round hg/concluded-round->stake-map)
               stake-map-new (->> cr hg/concluded-round->stake-map)
               e             (:received-event/event re-with-share-stake-tx)
               e+tx+re       [e (:event/tx e) re-with-share-stake-tx]]
           {::on-event wc
            ::y        (->y last-cr-w)
            ::args     [cr e+tx+re stake-map-old stake-map-new]}))))

   (rum/defc tutor-all-there-is-view []
     [:div.tutor.unbound
      "You've seen all there's to it, huh!\n\n"

      "Yes, Hashgraph is _that_ darn simple,\n"
      "yet the power it gives is immense\n"
      "- an exemplar of tech beauty.\n\n"

      "I hope this viz made the simple thing easy.\n"
      "Love it? Hate it? " [:a {:href hga-view/discussions-link :target "_blank"} "Let me know!"]
      #_#_#_"If not, well.. " [:a {:href hga-view/discussions-link :target "_blank"} "reach out"] " with a spare tomato. (:"])
   (fn [event]
     (when (= 1 (count @*left-view->state-fn))
       (let [last-on-event-creation-time (->> @*tutors-ordered last ::on-event hg/creation-time)]
         (when (> (hg/creation-time event) (-> last-on-event-creation-time (+ hga-view/window-size)))
           {::on-event event
            ::y        (->y event)
            ::args     []}))))
   })


(def *tutors (atom []))
(def *tutors-ordered (rum/derived-atom [*tutors] ::derive-ordered-tutors
                       (fn [tutors]
                         (sort-by ::y tutors))))

(def *on-event->tutor (rum/derived-atom [*tutors] ::derive-on-event->tutor
                              (fn [tutors]
                                (->> tutors (into (hash-map) (map (fn [tutor] [(::on-event tutor) tutor])))))))

(defonce *left-view->state-fn
  (rum/derived-atom [*tutors] ::*derive-left-tutors
    (fn [tutors]
      (let [tutored-views (set (map ::view tutors))]
        (->> view->state-fn
             (into {} (remove (comp tutored-views key))))))))

(defn event->tutors [event]
  (let [left-view->state-fn @*left-view->state-fn]
    (->> left-view->state-fn
         (into [] (comp (map (fn [[view state-fn]]
                               (when-let [state (state-fn event)]
                                 (assoc state
                                        ::view view))))
                        (filter some?))))))

(defn maybe-tutor! [event]
  (when-let [tutors (not-empty (doall (event->tutors event)))]
    (let [distinct-on-event-tutors (-> tutors
                                       (->> (group-by ::on-event)
                                            (map (fn [[_ [first-tutor]]] first-tutor))))]
      (swap! *tutors into distinct-on-event-tutors))))


(add-watch hga-playback/*just-left< ::derive-event->tutor
           (fn [_ _ _ just-left<]
             (doall
              (doseq [event just-left<]
                (maybe-tutor! event)))

             (when (empty? @*left-view->state-fn)
               (l :removing-watch)
               (remove-watch hga-playback/*just-left< ::derive-event->tutor))))

(defonce *seen-events (atom #{}))
(defonce *hidden-events
  (rum/derived-atom [*tutors *seen-events] ::derive-hidden-events
    (fn [tutors seen-events]
      (set/difference (set (map ::on-event tutors))
                      seen-events))))

(add-watch hga-state/*just-played< ::sync-tutors-with-just-played
           (fn [_ _ _ just-played<]
             (when-let [hidden-events (not-empty @*hidden-events)]
               (when-let [to-show-events (not-empty (set/intersection hidden-events (set just-played<)))]

                 ;; trigger on-play
                 (let [on-event->tutor @*on-event->tutor]
                   (doseq [to-show-event to-show-events]
                     (when-let [to-show-tutor (get on-event->tutor to-show-event)]
                       (when-let [on-play (::on-play to-show-tutor)]
                         (on-play)))))

                 (swap! *seen-events set/union to-show-events)))))

(add-watch hga-state/*just-rewinded> ::sync-tutors-with-just-played
           (fn [_ _ _ just-rewinded>]
             (when-let [seen-events (not-empty @*seen-events)]
               (when-let [to-hide-events (not-empty (set/intersection seen-events (set just-rewinded>)))]

                 ;; trigger on-rewind
                 (let [on-event->tutor @*on-event->tutor]
                   (doseq [to-hide-event to-hide-events]
                     (when-let [to-hide-tutor (get on-event->tutor to-hide-event)]
                       (when-let [on-rewind (::on-rewind to-hide-tutor)]
                         (on-rewind)))))

                 (swap! *seen-events set/difference to-hide-events)))))

(defonce *seen-latest-event
  (rum/derived-atom [*seen-events] ::derive-only-one-seen-event
    (fn [seen-events]
      (->> seen-events
           (sort-by ->y >)
           (first)))))

(defonce *seen-latest-tutor
  (rum/derived-atom [*seen-latest-event *tutors] ::derive-only-one-tutor-to-show
    (fn [seen-latest-event tutors]
      (->> tutors (some (fn [{::keys [on-event] :as tutor}]
                          (when (= (hash on-event) (hash seen-latest-event))
                            tutor)))))))

(def smooth-tt 250)
(def smooth-render-styles
  [[:.smooth-render
    [:>div {:opacity    0
            :transition (t :opacity smooth-tt smooth-tt)}]
    [:.showing {:opacity 1}]
    [:.hiding {:opacity 0}]]])

(reg-styles! ::smooth-render smooth-render-styles)

(defn maybe-hide-keys! [smooth-comp to-maybe-hide-keys]
  (let [t (cljs.core/system-time)
        *state (-> smooth-comp
                   .-state
                   (goog.object/get ":rum/state"))
        *hidden? (volatile! false)]
    (vswap! *state update ::key->state
            (fn [key->state]
              (reduce (fn [key->state-acc to-maybe-hide-key]
                        (let [?state   (get key->state-acc to-maybe-hide-key)
                              to-hide? (and ?state
                                            (= (::t-state ?state) ::hiding)
                                            (>= t (::hiding-t-end ?state)))]
                          (if to-hide?
                            (do (vreset! *hidden? true)
                                (dissoc key->state-acc to-maybe-hide-key))
                            key->state-acc)))
                      key->state
                      to-maybe-hide-keys)))
    (when @*hidden? (rum/request-render smooth-comp))))

;; ENHANCEMENT mount comps only after prev unmounted
;; I.e., do the render in two phases: 1) hide no-more-active 2) show novel active
(rum/defcs smooth-render < rum/static
  {:will-mount   (fn [state]
                   (let [key->state (->> (:rum/args state) (into (hash-map) (map (fn [comp] [(.-key comp) (hash-map ::comp  comp ::t-state ::to-show)]))))]
                     (assoc state
                            ::key->state key->state
                            ::new-keys-to-show (keys key->state))))

   :will-remount (fn [old-state new-state]
                   (let [old-key->state   (::key->state old-state)

                         new-comps        (:rum/args new-state)

                         old-keys         (set (keys old-key->state))
                         new-keys         (->> new-comps (into #{} (map (fn [comp] (.-key comp)))))
                         old-keys-to-hide (set/difference old-keys new-keys)
                         new-keys-to-show (set/difference new-keys old-keys)

                         hiding-t-end     (+ (cljs.core/system-time) smooth-tt)
                         old-key->state*  (reduce (fn [old-key->state-acc old-key-to-hide]
                                                    (update old-key->state-acc old-key-to-hide assoc ::t-state ::hiding ::hiding-t-end hiding-t-end))
                                                  old-key->state
                                                  old-keys-to-hide)

                         new-key->state  (reduce
                                          (fn [old-key->state-acc new-comp]
                                            (let [key (.-key new-comp)]
                                              (if-let [old-state (get old-key->state-acc key)]
                                                (assoc old-key->state-acc key (assoc old-state ::t-state ::showing ::comp new-comp))
                                                (assoc old-key->state-acc key (hash-map ::t-state ::to-show ::comp new-comp)))))
                                          old-key->state*
                                          new-comps)]

                     (when (not (empty? old-keys-to-hide))
                       (js/setTimeout #(maybe-hide-keys! (:rum/react-component new-state) old-keys-to-hide) smooth-tt))

                     (assoc new-state
                            ::key->state new-key->state
                            ::new-keys-to-show new-keys-to-show)))}

  {:after-render (fn [state]
                   (let [new-keys-to-show (::new-keys-to-show state)]
                     (if (empty? new-keys-to-show)
                       state
                       (let [old-key->state (::key->state state)
                             new-key->state (reduce (fn [new-key->state-acc new-key-to-show]
                                                      (update new-key->state-acc new-key-to-show assoc ::t-state ::showing))
                                                    old-key->state
                                                    new-keys-to-show)]
                         (-> state
                             (assoc ::key->state new-key->state)
                             (assoc ::request-render? true)
                             (dissoc ::new-keys-to-show))))))}
  {:after-render (fn [state]
                   (if (::request-render? state)
                     (do (js/setTimeout #(rum/request-render (:rum/react-component state)))
                         (dissoc state ::request-render?))
                     state))}

  [{::keys [key->state] :as state} _]
  [:div.smooth-render
   (for [[key {::keys [comp t-state]}] key->state]
     [:div {:key   key
            :class [(when (= ::showing t-state) "showing")]}
      comp])])

(rum/defc tutor-view < rum/static rum/reactive
  {:key-fn (fn [{::keys [y]}] y)}
  [{::keys [view args y] :as tutor}]
  [:div.tutor-wrapper {:style (if hga-view/view-mode-horizontal?
                                {:left y}
                                {:top y})}
   (apply view args)])

(rum/defc view < rum/reactive
  []
  [:div#tutorial
   (when-let [seen-latest-tutor (rum/react *seen-latest-tutor)]
     (smooth-render
      (tutor-view seen-latest-tutor)))])
