(ns hashgraph.main
  (:refer-clojure
   :exclude [parents])
  (:require [cljs.math :refer [floor ceil]]
            [hashgraph.members :as hg-members]
            [hashgraph.utils :refer-macros [defnm defnml]]
            [hashgraph.utils2 :refer-macros [td time3]]
            [hashgraph.app.inspector :refer [log-relative log! log-set! log-flush!] :as hga-inspector]
            [taoensso.timbre :refer-macros [spy] :refer [log info]]
            [taoensso.tufte :as tufte :refer [defnp p pspy profiled profile]]
            [clojure.set :refer [union] :as set]))

(log-flush!)

;; (system-time)
;; (cljs.pprint/pprint (macroexpand-1 '(time3 (doall (range 1 10000)))))
;; (cljs.pprint/pprint (macroexpand '(td3 100 (range 1 100000))))
;; (js* "debugger;")
;; (td3 101 (and (reduce + 0 (range 1 10000000)) 1))
;; (td3 100 (+ 1 1))
;; (macroexpand-1 (== 1 1))
(defn oho []
  (let [a (+ 1 1)
        b (* a a)]
    (+ a b (td 1 (reduce + (range 1 100000))))
    ))
;; (time (range 1 10000))
;; (oho)

(def creator (memoize (fn [x] (:event/creator x))))
(def self-parent (memoize (fn [x] (:event/self-parent x))))
(def other-parent (memoize (fn [x] (:event/other-parent x))))

(def parents
  "Set of events that are parents of x."
  (memoize
   (fn [x]
     (cond-> #{}
       (other-parent x) (conj (other-parent x))
       (self-parent x) (conj (self-parent x))))))

(def self-parent? (memoize (fn [x y] (= (self-parent x) y))))
(def other-parent? (memoize (fn [x y] (= (other-parent x) y))))

(def parent?
  (memoize
   (fn [x y]
     (or (self-parent? x y)
         (other-parent? x y)))))

(def ancestor?
  (memoize
   (fn [x y]
     (or (parent? x y)
         #_(some-> (self-parent x) (ancestor? y))
         #_(some-> (other-parent x) (ancestor? y))
         (some #(ancestor? % y) (parents x))))))

(def self-ancestor?
  (memoize
   (fn [x y]
     (and (some? (self-parent x))
          (or (self-parent? x y)
              (self-ancestor? (self-parent x) y))))))

#_
(def many-creators?
  (memoize
   (fn [events]
     (-> events
         (->> (map creator))
         distinct
         count
         (> hg-members/many)))))

#_
(def events
  "Returns a set of events known to x."
  (memoize
   (fn [x]
     (cond-> #{}
       (self-parent x)  (-> (conj (self-parent x))
                            (into (events (self-parent x))))
       (other-parent x) (-> (conj (other-parent x))
                            (into (events (other-parent x))))))))


#_
(def children
  "Set of events that are children of y."
  (memoize
   (fn [evts y]
     (->> evts (filter (fn [z] (or (parent? z y)
                                   (self-parent? z y))))))))

#_
(def self-children
  "Set of events that are self-children of y, as known to x."
  (memoize
   (fn [x y]
     (-> x
         events
         (->> (filter (fn [z] (self-parent? z x))))))))

#_
(def tips "Latest events by members"
  (memoize (fn [evts]
             (->> evts (into #{} (remove (fn [x] (some (fn [y] (self-parent? y x)) evts))))))))
#_
(def bottoms "Earliest events by members"
  (memoize (fn [evts]
             (->> evts (into #{} (remove (fn [x] (some (fn [y] (self-parent? x y)) evts))))))))

(def index
  "Index position of event x, as max of index position of self-parent and parent"
  (memoize
   (fn [x]
     (if (empty? (parents x))
       0
       (inc (apply max (map index (parents x))))))))

#_
(def self-ancestors
  (memoize
   (fn [x]
     (if (nil? (self-parent x))
       '()
       (conj (self-ancestors (self-parent x)) (self-parent x))))))

#_
(def self-descendants
  "Returns a set of self-descendants of y, as known by x."
  (memoize
   (fn [x y]
     (-> x
         events
         (->> (filter (fn [z] (self-ancestor? z y))))))))

#_
(def in-fork?
  "Whether y is in a fork, as known to x."
  (memoize
   (fn [x y]
     (or (some-> y
                 self-parent
                 (->> (self-children x))
                 count
                 (> 1))
         (some->> y self-parent (in-fork? x))))))

(def see?
  (memoize
   (fn [x y]
     (or (= x y)
         (ancestor? x y))
     #_(and
        (not (in-fork? x y))))))

(def ->seen-by
  "Events that seen y, as known to x."
  (memoize
   (fn [x y]
     #_(l {:id [:seen-by [x y]]} {:x x :y y})
     #_(l {:id [:seen-by [x y]] :key :seen-by})
     (cond-> #{}
       (see? x y)                         (conj x)
       (some-> (self-parent x) (see? y))  (into (->seen-by (self-parent x) y))
       (some-> (other-parent x) (see? y)) (into (->seen-by (other-parent x) y))))))

(declare many-stake)
(declare ?concluded-round->stake-map)
(def see-many-see? ;; strongly-see
  "Whether x see many stake from events that see y."
  (memoize
   (fn [x y ?cr]
     (let [seen-by (->seen-by x y)
           seen-stake (reduce + 0 (map (comp (?concluded-round->stake-map ?cr) :event/creator) seen-by))] ;; TODO make creators distinct
       (> seen-stake many-stake)))))

#_#_
(def merge-rws (fn [& rws] (apply merge-with #(merge-with into %1 %2) rws)))
(def round->witnesses->seen-by-creators
  "Round and its witnesses seen by creators, based on a concluded round."
  (memoize
   (fn [x ?cr] ;; maybe pass tip, but be wary of recursion loop
     (if (empty? (parents x))
       {1 {x #{(creator x)}}}

       (let [merged-r-w-sb (-> (parents x)
                            (->> (map (fn [px] (round->witnesses->seen-by-creators px ?cr)))
                                 (apply merge-rws)))
             max-r      (-> merged-r-w-sb keys (->> (apply max)))
             max-r-w-sb (get merged-r-w-sb max-r)
             ;; add seen-by creator
             new-max-r-w-sb (->> max-r-w-sb (into {} (map (fn [[w sb]] [w (conj sb (creator x))]))))

             latest-applicable-stake-map (?concluded-round->stake-map ?cr)
             strongly-seen-many?
             (-> new-max-r-w-sb
                 (->> (filter (fn [[_w sb]]
                                (-> sb
                                    (->> (map latest-applicable-stake-map sb)
                                         (reduce + 0))
                                    (> many-stake))))
                      (map (fn [[w _sb]] (latest-applicable-stake-map (:event/creator w))))
                      (reduce + 0))
                 (> many-stake))]
         (cond
           ;; became witness in new round?
           strongly-seen-many?
           {(inc max-r) {x #{(creator x)}}}

           ;; became witness in this round?
           (not (some (fn [[w _s]] (= (creator w) (creator x))) new-max-r-w-sb))
           {max-r (assoc new-max-r-w-sb x #{(creator x)})}

           ;; collect-my-seeing
           :else
           {max-r new-max-r-w-sb}))))))

(declare round-witnesses)

;; TODO perhaps rename to ->round
(def round ;; see-many-see-many-see ;; see-many-strongly-see
  "Round number of y, as known to a previous round concluded x.
   It's either 1 if y has no parents,
   or a max round of events strongly seen by many (as known to x) +1."
  (memoize
   (fn
     ([x ?cr]
      (:round/number (round x ?cr nil)))
     ([x ?cr ?acc]
      (if (empty? (parents x))
        {:round/event  x
         :round/number 1
         :round/final? true}
        (or (when-let [cr ?cr]
              (let [?prev-cr   (:concluded-round/prev-concluded-round cr)
                    prev-round (round x ?prev-cr nil)]
                (assoc prev-round :round/from-?cr cr)
                (when (:round/final? prev-round)
                  prev-round)))
            (let [max-pr       (apply max (map #(round % ?cr) (parents x)))
                  max-pr-ws    (cond-> #{}
                                 (some-> (self-parent x)  (round ?cr) (= max-pr)) (set/union (round-witnesses (self-parent  x) max-pr ?cr))
                                 (some-> (other-parent x) (round ?cr) (= max-pr)) (set/union (round-witnesses (other-parent x) max-pr ?cr)))
                  stake-map    (?concluded-round->stake-map ?cr)
                  round-number (cond-> max-pr
                                 (-> max-pr-ws
                                     (->> (filter (fn [w] (see-many-see? x w ?cr)))
                                          (map (comp stake-map :event/creator))
                                          (reduce + 0))
                                     (> many-stake))
                                 inc)

                  ;; Once round r is settled, the _future_ rounds will reshuffle,
                  ;; and the calculations for round r + 1 famous witnesses will be done using the new stake record.
                  ;; source: https://hyp.is/QxYPUqyAEe6hUtsMYuakKQ/www.swirlds.com/downloads/SWIRLDS-TR-2016-01.pdf

                  ;; So, once there is a cr that gave round number less or equal to cr - it's final.
                  ;; We need _less_ for cases, when stake changed, and more events received cr's round number.
                  round-final? (<= round-number (or (inc (:concluded-round/r ?cr)) 1))]
              {:round/event     x
               :round/number    round-number
               :round/final?    round-final?
               :round/max-pr    max-pr
               :round/max-pr-ws max-pr-ws
               :round/stake-map stake-map
               :round/?cr       ?cr}

              #_(log :debug {:max-pr max-pr :max-pr-ws max-pr-ws :stake-map stake-map :x x :?cr ?cr})
              )
            #_
            (-> (round->witnesses->seen-by-creators x ?cr)
                keys
                first))))

     #_
     (-> x
         events
         (->> (filter (fn [y] (see-many-see? x y)))
              (group-by round)
              (filter (comp many-creators? val))
              (map first))
         (some->> not-empty (apply max) inc)
         (or 1)))))

(def witness?
  "Whether y is a witness, as known to x."
  (memoize
   (fn
     #_ ([x] (witness? x x)) ;; commented to ensure I upgrade
     ([x ?cr]
      (or (nil? (self-parent x))
          (< (round (self-parent x) ?cr)
             (round x ?cr)))))))

(def rounds-diff
  "Rounds diff between y and z, as known to x."
  (memoize
   (fn [x y ?cr]
     (- (round x ?cr)
        (round y ?cr)))))

(def d 1) ;; round in which voting starts
(def c 10) ;; each c's round is a coin flip round
(def voting-round?
  (memoize
   (fn [x y ?cr] (> (rounds-diff x y ?cr) d))))

(def voting-coin-flip-round?
  (memoize
   (fn
     [x y ?cr]
     (and (voting-round? x y ?cr)
          (-> (rounds-diff x y ?cr) (mod c) (= 0))))))

(defn signature [evt] (:signature evt))
(defn middle-bit [_sig] 1)

(def self-witness
  "Self-witness of y, as known to concluded witness."
  (memoize
   (fn [x ?cr]
     (cond (nil? (self-parent x))         nil
           (witness? (self-parent x) ?cr) (self-parent x)
           :else                          (self-witness (self-parent x) ?cr)))))

(def witness-or-self-witness
  (memoize
   (fn [x ?cr]
     (or (and (witness? x ?cr)
              x)
         (self-witness x ?cr)))))

(def round-witnesses
  "Round r witnesses, known to y, as known to x."
  (memoize
   (fn [x r ?cr]
     #_(js* "debugger;")
     (cond-> #{}
       (and (= (round x ?cr) r) ;; can be optimized to not run if round > r
            (witness? x ?cr)) (conj x)
       (>= (round x ?cr) r)   (cond->
                                       (self-parent  x) (set/union (round-witnesses (self-parent  x) r ?cr))
                                       (other-parent x) (set/union (round-witnesses (other-parent x) r ?cr)))))))

(declare many-stake)
(declare votes-stake-true)
#_
(def round-unique-famous-witnesses
  (memoize
   (fn [witness-concluded round-concluded]
     (let [rws (round-witnesses witness-concluded round-concluded)
           stake-map (witness-concluded+round-concluded->stake-map )]
       (->> rws
            (filter (fn [rw] (> (votes-stake-true witness-concluded rw stake-map) many-stake))))))))

#_
(def round-events
  "Events that are the same round as y, as known to x."
  (memoize
   (fn
     [x r cr]
     (cond-> #{}
       (= (round x cr) r) ;; can be optimized to not run if round > r
       (conj x)

       (>= (round x cr) r)
       (cond->
           (self-parent  x) (into (round-events (self-parent  x) r cr))
           (other-parent x) (into (round-events (other-parent x) r cr)))))))

#_
(def evt-learned-by-evts-map
  "Return a map of events to events that learned about them (have it as other-parent), as known to x."
  (memoize
   (fn [x]
     )))

#_
(profile
 {}
 (let [arg [(set (range 0 10000))
            (set (range 5000 15000))
            (set (range 10000 20000))
            ]]
   (doall (repeatedly 100 #(p ::union (doall (apply clojure.set/union arg)))))
   (doall (repeatedly 100 #(p ::reduce (doall (reduce into #{} arg)))))
   :finished))
;; union is generally faster than reduce
;; TODO check sets vs transducers for difference vs distinct + remove

#_
(defn merge-es-lm-le [& es-lm-les]
  (apply merge-with merge es-lm-les))

#_
(def
  events-learned-member-learned-event-map
  (memoize
   (fn [x]
     (let [m (:event/creator x)
           sp-es-lm-le (or (some-> (self-parent x) events-learned-member-learned-event-map)
                                    {})
           op-es-lm-le (or (some-> (other-parent x) events-learned-member-learned-event-map)
                                    {})
           es-lm-le (merge-es-lm-le
                     sp-es-lm-le
                     op-es-lm-le)]
       (-> es-lm-le
           (->> (into {} (map (fn [[e lm-le]]
                                [e (cond-> lm-le
                                     (not (get lm-le m))
                                     (update lm-le m x))]))))
           (cond-> (other-parent x)
             (assoc (other-parent x) {m x})))))))


(def make-vote
  (memoize
   (fn [?cr from-event to-event vote-type vote-value & [vote-rest]]
     (let [stake-map (?concluded-round->stake-map ?cr)]
       (merge
        {:vote/from-event from-event
         :vote/to-event   to-event
         :vote/type       vote-type
         :vote/value      vote-value
         :vote/stake      (get stake-map (:event/creator from-event))
         :vote/stake-map  stake-map
         :vote/?prev-concluded-round ?cr}
        vote-rest)))))

(def vote-see? (memoize (fn [x y ?cr] (= (rounds-diff x y ?cr) 1))))
(def vote-see (memoize (fn [x y ?cr] (make-vote ?cr x y :see (boolean (see? x y))))))

(declare ->?concluded-voting)
(declare vote)
#_#_
(def vote-copy?
  (memoize
   (fn [x y ?cr]
     (some-> (self-witness x ?cr) (->?concluded-voting y ?cr)))))
(def vote-copy
  (memoize
   (fn [x y cr]
     (let [vote-copied (vote (self-witness x) y cr)]
       (make-vote cr x y :copy (:vote/value vote-copied) {:vote/vote-copied vote-copied})))))

(declare votes-stake-fract-true)
(declare many-stake)
(def vote-coin-flip?
  (memoize
   (fn [x y ?cr]
     (and (voting-coin-flip-round? x y ?cr)
          (>= (votes-stake-fract-true x y ?cr) (-> many-stake (/ 3)))
          (<= (votes-stake-fract-true x y ?cr) (-> many-stake (/ 3) (* 2)))))))
(defn vote-coin-flip [x y cr] (make-vote cr x y :coin-flip (= 1 (middle-bit (signature x)))))


(def vote-for-majority
  (memoize
   (fn [x y ?cr] (make-vote ?cr x y :for-majority (>= (votes-stake-fract-true x y ?cr) (/ 1 2))))))

(def vote
  "Vote of y about fame of z, as known to x."
  (memoize
   (fn [x y ?cr]
     (cond (vote-see? x y ?cr)       (vote-see x y ?cr)
           ;; (vote-copy? x y cr)      (vote-copy x y cr) ;; done in ?concluded-voting?
           (vote-coin-flip? x y ?cr) (vote-coin-flip x y ?cr)
           :else                     (vote-for-majority x y ?cr)))))

(def votes
  "Votes on fame of x from witnesses seen by many in the round before x, based on concluded-round."
  (memoize
   (fn [x y ?cr]
     (-> x
         (round-witnesses (dec (round x ?cr)) ?cr)
         (->> (filter (fn [w] (see-many-see? x w ?cr)))
              (map (fn [w] (vote w y ?cr))))))))

(declare event->stake)
(def votes-stake-true
  (memoize
   (fn [x y ?cr]
     ;; TODO maybe switch to transduce
     (reduce + 0 (map :vote/stake (filter :vote/value (votes x y ?cr)))))))

(def votes-stake-false
  (memoize
   (fn [x y ?cr]
     (reduce + 0 (map :vote/stake (remove :vote/value (votes x y ?cr)))))))

(def votes-stake-fract-true
  (memoize
   (fn [x y ?cr]
     (let [stake-true (votes-stake-true x y ?cr)
           stake-false (votes-stake-false x y ?cr)]
       (/ stake-true
          (max 1 (+ stake-true stake-false)))))))

(declare many-stake)
(declare ->?concluded-round)

(defnm ->?concluded-voting
  "Whether fame of y is concluded, as known to x.
   Fame is concluded when in a regular round there are votes with sum of their stakes more than many stake."
  [x y ?cr]
  (when (voting-round? x y ?cr)
    (or (when-let [sp-x (self-parent x)]
          (when-let [prev-concluded-voting (->?concluded-voting sp-x y ?cr)]
            (when (:concluded-voting/conclusion prev-concluded-voting)
              prev-concluded-voting)))
        (when (and (witness? x ?cr)
                   (not (voting-coin-flip-round? x y ?cr)))
          (let [?conclusion (or (and (> (votes-stake-true x y ?cr) many-stake) :famous) ;; takes a while
                                (and (> (votes-stake-false x y ?cr) many-stake) :not-famous))] ;; !!!!! nil conclusions when they are meant to be concluded as not-famous, votes are shown in viz, but not here
            ;; Maybe log state, instead of returning it. So it can be disabled for a faster and less memory-hungry eval.
            ;; Maybe just access state ad-hoc by re-creating eval where needed.
            (cond-> {:concluded-voting/by    x
                     :concluded-voting/about y
                     :concluded-voting/votes (votes x y ?cr)}
              ?conclusion (assoc :concluded-voting/conclusion ?conclusion)))))))


(def tries-to-conclude-round?
  (memoize
   (fn [x r ?cr]
     )))

(declare concluded-round->received-event)
(declare ?received-event->stake-map)
(def ->?concluded-round
  "Whether the round has fame of all it's witnesses concluded,
   and the previous round, if there is one, been concluded, as known to x."
  (memoize
   (fn
     ([x]
      (let [?prev-concluded-round (some-> (self-parent x) (->?concluded-round))]
        (->?concluded-round x ?prev-concluded-round)))
     ([x ?prev-cr]
      (if (not (witness? x ?prev-cr))
        ?prev-cr

        (let [next-cr-r  (inc (:concluded-round/r ?prev-cr))
              next-crw-r (-> next-cr-r (+ d) inc)
              wx-r       (round x ?prev-cr)]
          #_(when (and (= next-cr-r 7)
                     (< wx-r next-crw-r))
            (spy [wx wx-r])
            (js* "debugger;")) ;; huh, no sf reaches round 11 atop prev-cr ?

          (if (< wx-r next-crw-r)
            ?prev-cr

            ;; x is a witness able to try to conclude
            (let [ws                      (round-witnesses x next-cr-r ?prev-cr)
                  maybe-concluded-votings (->> ws (map (fn [w] (->?concluded-voting x w ?prev-cr))))]

              (if-not (every? :concluded-voting/conclusion maybe-concluded-votings)
                ?prev-cr

                (let [ufws    (->> maybe-concluded-votings
                                   (filter (comp #{:famous} :concluded-voting/conclusion))
                                   (map :concluded-voting/about))
                      next-cr (cond-> {:concluded-round/r                 next-cr-r
                                       :concluded-round/witness-concluded x
                                       :concluded-round/ws                ws
                                       :concluded-round/ufws              ufws
                                       :concluded-round/concluded-votings maybe-concluded-votings}
                                ?prev-cr (assoc :concluded-round/prev-concluded-round ?prev-cr))]
                  ;; try recur
                  (->?concluded-round x next-cr)))))))))))

(def ?cr+r->?cr
  (memoize
   (fn [?cr r]
     (assert (pos? r))
     (cond (or (nil? ?cr)
               (< (:concludedr-round/r ?cr) r))
           nil

           (= (:concluded-round/r ?cr) r)
           ?cr

           :else (recur (:concluded-round/prev-concluded-round ?cr) r)))))

(def x+r->?cr
  (memoize
   (fn [x r]
     (assert (map? x))
     (assert (pos? r))
     (?cr+r->?cr (->?concluded-round x) r))))


(declare concluded-round->events-not-received)
(declare ?received-event->stake-map)
(declare concluded-round->received-event)

(defn l [msg value] (js/console.log msg value) value)
(def concluded-round->novel-events
  (memoize
   (fn
     ([cr] (let [novel-events (apply set/union (->> (:concluded-round/ws cr)
                                                    (map (fn [w] (concluded-round->novel-events cr w)))))]
             novel-events))
     ([{:concluded-round/keys [prev-concluded-round] :as cr} x]
      (if-let [novel? (or (nil? prev-concluded-round)
                          (->> prev-concluded-round
                               (iterate :concluded-round/prev-concluded-round)
                               (take-while some?)
                               (map concluded-round->novel-events)
                               (some (fn [prev-ne] (contains? prev-ne x)))
                               not)
                          #_(-> prev-concluded-round
                              (concluded-round->novel-events)
                              (contains? x)
                              not))]
            (set/union #{x}
                       (some->> (self-parent x) (concluded-round->novel-events cr))
                       (some->> (other-parent x) (concluded-round->novel-events cr)))

        #{})))))

(def concluded-round->whether-events-received
  "Returns a map from whether received to events in a a concluded round by maybe previous concluded round and concluded votings."
  (memoize ;; sufficient to memoize only for previous round
   (fn [{:concluded-round/keys [prev-concluded-round r ufws] :as cr}]
     (when (nil? (:concluded-round/r cr))
       (js* "debugger;"))
     (if (< r 1)
       {true  #{} ;; redundant, this fn won't be called without a cr
        false #{}}
       (let [;; TODO perhaps pass x instead of stake-map
             ;; TODO move to concluded-round->ufws
             novel-events      (concluded-round->novel-events cr)
             events-to-receive (set/union novel-events
                                          (some-> prev-concluded-round (concluded-round->events-not-received)))
             ]

         (reduce
          (fn [acc evt]
            (let [whether-received (->> ufws (every? (fn [ufw] (ancestor? ufw evt))))]
              (update acc whether-received conj evt)))
          {true  #{}
           false #{}}
          events-to-receive))))))

(def concluded-round->events-received
  (memoize
   (fn [concluded-round]
     (get (concluded-round->whether-events-received concluded-round) true))))

(def concluded-round->events-not-received
  (memoize
   (fn [concluded-round]
     (get (concluded-round->whether-events-received concluded-round) false))))



(def ->learned-evt
  "First event of the creator of x that see y."
  (memoize
   (fn [x y]
     (and (see? x y)
          (or (some-> (self-parent x) (->learned-evt y))
              x)))))

(def median
  (memoize
   (fn [numbers]
     (let [sorted-numbers (sort numbers)
           len            (count sorted-numbers)]
       (if (even? len)
         (/ (+ (nth sorted-numbers (quot len 2))
               (nth sorted-numbers (dec (quot len 2))))
            2)
         (nth sorted-numbers (quot len 2)))))))

#_
(def received-time
  "Received time of y, as known to x."
  (memoize
   (fn [x y]
     (let [learned-events (vals (learned-member-learned-evt x y))]
       (median (map :event/creation-time learned-events))))))

#_
(def order-events
  (memoize
   (fn [x evts]
     (->> evts
          (sort (fn [e1 e2]
                  (let [rt1 (received-time x e1)
                        rt2 (received-time x e2)]
                    (cond (not= rt1 rt2)
                          (if (> rt1 rt2)
                            1 -1)

                          :else
                          (if (> (:event/creation-time e1)
                                 (:event/creation-time e2))
                            1 -1)))))
          vec))))

(def concluded-round->received-event
  (memoize
   (fn [{:concluded-round/keys [r ufws] :as concluded-round} & [debug?]]
     (when debug? (js* "debugger;"))
     (if (= r 1)
       nil
       (let [events-received         (concluded-round->events-received concluded-round)
             received-round-size     (count events-received)
             ?prev-concluded-round   (:concluded-round/prev-concluded-round concluded-round)
             ?prev-received-event    (some-> ?prev-concluded-round (concluded-round->received-event))]

         (->> events-received
              (map (fn [er]
                     (let [learned-by (->> ufws (into #{} (map (fn [ufw] (->learned-evt ufw er)))))
                           middle-learned-events
                           (let [len (count learned-by)]
                             (-> learned-by
                                 (->> (sort-by :event/creation-time))
                                 (cond->
                                     (even? len)
                                   ((juxt #(nth % (quot len 2)) #(nth % (dec (quot len 2)))))

                                   (odd? len)
                                   ((juxt #(nth % (quot len 2)))))
                                 (set)))

                           received-time (-> middle-learned-events
                                             (->> (map :event/creation-time)
                                                  (reduce +))
                                             (/ (count middle-learned-events)))]

                       {:received-event/learned-by            learned-by
                        :received-event/middle-learned-events middle-learned-events
                        :received-event/received-time         received-time
                        :received-event/concluded-round       concluded-round
                        :received-event/event                 er})))

              (sort-by :received-event/received-time)

              (reduce (fn [?prev-received-event re-part]
                        (let [r-idx (if (some-> ?prev-received-event :received-event/concluded-round :concluded-round/r (= r))
                                      (-> ?prev-received-event :received-event/r-idx inc)
                                      1)

                              ;; TODO perhaps move color calculation into view
                              palette1 [[38, 70, 83]
                                        ;; [42, 157, 143]
                                        [233, 196, 106]
                                        ;; [244, 162, 97]
                                        [231, 111, 81]]

                              [rgb1 rgb2 rgb3] palette1

                              [from-rgb to-rgb]
                              (case (mod r 3)
                                1 [rgb3 rgb1]
                                2 [rgb1 rgb2]
                                0 [rgb2 rgb3])

                              chroma
                              (map (fn [from-c to-c]
                                     (let [upper-c       (max from-c to-c)
                                           lower-c       (min from-c to-c)
                                           delta-c       (- upper-c lower-c)
                                           steps         received-round-size
                                           steps-delta-c (/ delta-c steps)
                                           step-delta-c  (* steps-delta-c r-idx)
                                           c             (if (= upper-c from-c)
                                                           (- from-c step-delta-c)
                                                           (+ from-c step-delta-c))]
                                       (ceil c)))
                                   from-rgb
                                   to-rgb)
                              color (let [[red green blue] chroma]
                                      (str "rgb(" red "," green "," blue ")"))]
                          (cond-> (merge re-part
                                         {:received-event/r-idx r-idx
                                          :received-event/color color})
                            ?prev-received-event (assoc :received-event/prev-received-event ?prev-received-event))))
               ?prev-received-event)))))))

(def ?concluded-round->stake-map
  (memoize
   (fn [?cr]
     (let [?re (some-> ?cr concluded-round->received-event)]
       (?received-event->stake-map ?re)))))

#_
(def ->?last-received-event
  (memoize
   (fn [wx]
     #_(assert (witness? wx))
     (when-let [last-concluded-round (->?last-concluded-round wx)]
       (-> last-concluded-round
           concluded-round->received-event)))))

(defn round-received
  "The first round in which all unique famous witnesess see it,
   given fame of all witnesses in previous rounds been decided (round been decided)."
  [x y]
  )


;; --------------- Stake ----------------
(def total-stake 99)
(def many-stake (-> total-stake (/ 3) (* 2)))
(def initial-share-stake-log
  (->> hg-members/initial-member-names
       (mapv (fn [member-name]
               {:share-stake/to     member-name
                :share-stake/amount (/ total-stake (count hg-members/initial-member-names))}))))

(def share-stake-log->stake-map
  (memoize
   (fn [share-stake-log]
     (if (empty? share-stake-log)
       {}
       (let [[{:share-stake/keys [from to amount]} & ss-rest] share-stake-log]
         (cond-> (-> ss-rest share-stake-log->stake-map)
           from (update from - amount)
           to   (update to   + amount)
           :sanitize (->> (into {} (remove (comp zero? val))))
           :sort (->> (sort-by first)
                      (apply concat)
                      (apply array-map))))))))

(def derived-db->stake-map
  (memoize
   (fn [derived-db]
     (-> derived-db
         :share-stake-log
         share-stake-log->stake-map))))

;; -------------- Derived DB --------------------
(def init-db
  {:share-stake-log initial-share-stake-log})

;; TODO add memoize
(def fns
  {:share-stake
   (memoize
    (fn [db {:share-stake/keys [from to percent]}]
      (update db :share-stake-log (fn [share-stake-log]
                                    (let [stake-map (share-stake-log->stake-map share-stake-log)
                                          amount           (-> stake-map
                                                               (get from)
                                                               (/ 100)
                                                               (* percent)
                                                               (floor))]
                                      (conj share-stake-log
                                            {:share-stake/from   from
                                             :share-stake/to     to
                                             :share-stake/amount amount}))))))})

(def ?received-event->derived-db
  (memoize
   (fn [?received-event]
     (if (nil? ?received-event)
       init-db
       (if-let [{:tx/keys [fn-id args]} (:event/tx (:received-event/event ?received-event))]
         (let [f (get fns fn-id)]
           (apply f (?received-event->derived-db (:received-event/prev-received-event ?received-event)) [args]))
         (?received-event->derived-db (:received-event/prev-received-event ?received-event)))))))

#_
(def event->derived-db
  (memoize
   (fn [event]
     (?received-event->derived-db (last-received-event event)))))

(def ?received-event->stake-map
  (memoize
   (fn [received-event]
     (-> received-event
         ?received-event->derived-db
         :share-stake-log
         share-stake-log->stake-map))))

(def witness->stake-map
  (memoize
   (fn [w]
     #_(assert (witness? w))
     (-> w
         ->?concluded-round
         ?concluded-round->stake-map))))

#_
(def witness-concluded->stake-map
  (memoize
   (fn [witness-concluded]
     (-> witness-concluded
         ->?last-received-event
         ?received-event->stake-map))))

(def event->person
  (memoize
   (fn [event]
     (let [member-name (creator event)]
       (hg-members/member-name->person member-name)))))

(def witness->stake
  (memoize
   (fn [w]
     (let [member-name (:event/creator w)
           stake-map (-> w witness->stake-map)]
       (get stake-map member-name)))))

#_
(def event->most-weigted-member
  (memoize
   (fn [event]
     (-> event
         event->stake-map
         (->> (sort-by second))
         last
         first))))


(def event->present-members
  (memoize
   (fn [event]
     (some-> event
             witness-or-self-witness
             witness->stake-map
             keys
             (->> (map hg-members/member-name->person))))))

(def creator-hg-map->main-top-witness
  (memoize
   (fn [creator-hg-map]
     (when-let [main-hg (get creator-hg-map "Alice")]
       (let [?cr (->?concluded-round main-hg)]
         (witness-or-self-witness main-hg ?cr))))))

(def creator-hg-map->?alice-tip
  (memoize
   (fn [creator-hg-map]
     (when-let [main-hg (get creator-hg-map "Alice")]
       main-hg))))
