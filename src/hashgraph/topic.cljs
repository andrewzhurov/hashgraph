(ns hashgraph.topic
  (:require [hashgraph.main :refer [idx? root-event? a0 b0 a1 b1 c0 a2] :as hg]
            [hashgraph.schemas :as hgs]
            [hashgraph.utils.core :refer [hash= xor vec-difference vec-union map-vals] :refer-macros [defn* memoizing l letl2 t]]
            [malli.core :as m]
            [clojure.set :as set]
            [clojure.walk :refer [postwalk]]
            [clojure.test :refer [deftest testing is are]]))


(hgs/register!
 ::event-taped
 [:and
  [:map
   [:event/self-parent {:optional true} [:ref ::event-taped]]
   [:event/other-parent {:optional true} [:ref ::event-taped]]]
  [:fn {:error/message "event-taped does not have :tape/idx in meta "}
   (fn [event] (-> event meta :tape/idx idx?))]
  [:fn {:error/message "event-taped's :tape/idx is expected to be greater than its self-parent's"}
   (fn [event]
     (or (nil? (hg/self-parent event))
         (> (-> event meta :tape/idx)
            (-> event hg/self-parent :tape/idx))))]
  [:fn {:error/message "event-taped's :tape/idx is expected to be right after its other-parent's"}
   (fn [event]
     (or (nil? (hg/other-parent event))
         (= 1 (- (-> event meta :tape/idx)
                 (-> event hg/other-parent :tape/idx)))))]]
 :event-taped ::event-taped

 :tape [:vector [:ref ::event-taped]])


(hgs/register!
 ::tip-taped
 [:and
  [:merge
   [:ref ::hg/event]
   [:map
    [:event/self-parent {:optional true} [:ref ::tip-taped]]]]
  [:fn {:error/message ":tip/tape vec is not found in meta on tip-taped"}
   (fn [tip-taped] (-> tip-taped meta :tip/tape vector?))]
  [:fn {:error/message ":tip/tape vec in meta on tip-taped does not contain tip-taped as the last element"}
   (fn [tip-taped]
     (let [tape (-> tip-taped meta :tip/tape)]
       (hash= tip-taped (-> tape reverse first))))]
  [:fn {:error/message ":tip/novel-events vec in meta on tip-taped is not found"}
   (fn [tip-taped] (-> tip-taped meta :tip/novel-events vector?))]
  [:fn {:error/message ":tip/novel-events vec in meta on tip-taped does not contain tip-taped as the last element"}
   (fn [tip-taped]
     (let [novel-events (-> tip-taped meta :tip/novel-events)]
       (hash= tip-taped (-> novel-events reverse first))))]]
 :tip-taped ::tip-taped)

(m/=> tip+novel-events->tip-taped [:=> [:cat :event [:sequential :event]] :tip-taped])
(defn tip+novel-events->tip-taped [tip novel-events]
  (let [prev-tape (or (some-> (hg/self-parent tip) meta :tip/tape) [])
        new-tape  (vec (concat prev-tape novel-events))]
    (with-meta tip {:tip/tape         new-tape
                    :tip/novel-events novel-events})))

#_
(hgs/check [:ref ::tip-taped]
           (tip+novel-events->tip-taped a0 [a0]))


(def *tapes (atom []))
(defn new-tape [tape-idx events]
  (->> events
       (into [] (map (fn [event]
                       (->> event
                            (postwalk (fn [el]
                                        (if-not (map? el)
                                          el
                                          (with-meta el {:tape/idx     tape-idx
                                                         :tape/evt-idx (-indexOf events el)}))))))))))

(defn- new-tape! [events]
  (swap! *tapes (fn [tapes] (conj tapes (new-tape (count tapes) events)))))

(def a2t (new-tape! [a0 b0 a1 b1 c0 a2]))


;; Vectors are not sparse and assoc-n where n > size+1 throws out-of-bound
;; arrays in js may be objects underneath.. as a hash map
;; leave tape optimization for the Rust impl
#_#_#_#_
(def eventt->depth-costruct (transient []))
(m/=> eventt->depth [:=> [:cat EventTaped] Idx])
(defn eventt->depth [event]
  (let [tape-idx (:tape/idx (meta event))]
    (or (get eventt->depth-costruct tape-idx)
        (let [res (-> (max (some-> (hg/self-parent event) eventt->depth)
                           (some-> (hg/other-parent event) eventt->depth))
                      (or 0)
                      (inc))]
          (assoc! eventt->depth-costruct tape-idx res)
          res))))

(deftest eventt->depth-test
  (doseq [event-taped a2t]
    (let [expected-depth (get example-event->depth event-taped)]
      (is (= expected-depth (eventt->depth event-taped))))))




#_#_#_#_
(def example-tip+creator->descendants
  {[a0 b] #{a0}
   [a0 c] #{a0}
   [b0 a] #{b0}
   [b0 c] #{b0 a0}
   [b1 a] #{b0 b1}
   [b1 c] #{a0 b0 b1}
   [a1 b] #{a1}
   [a1 c] #{a0 b0 a1}
   [c0 a] #{b0 b1 c0}
   [c0 b] #{c0}
   [a2 b] #{a1 c0 a2}
   [a2 c] #{a1 a2}})

(defn tip+event->descendands [tip event]
  (-> (hg/events tip)
      (set/difference (hg/ancestors event))
      (disj event)
      #_(->> (sort-by hg/index <))))

(defn tip+creator->descendants [tip creator]
  (let [creator->tip (event->creator->tip tip)
        creator-tip  (get creator->tip creator)
        descendant   (tip+event->descendands tip creator-tip)]
    descendant))

(deftest tip+creator->descendants-test
  (doseq [[[tip creator] descendants] example-tip+creator->descendants]
    (is (= descendants (tip+creator->descendants tip creator)))))



#_#_#_
;; gives stable idxes for creators - first seen first, self-parent depth-first
(def example-event->creators
  {a0 [a]
   b0 [b]
   b1 [b a]
   a1 [a b]
   b2 [b a]
   c0 [b a c]
   a2 [a b c]})

(defn event->creators [event] ;; not performant impl
  (let [?sp-creators (some-> (hg/self-parent event) event->creators)
        ?op-creators (some-> (hg/other-parent event) event->creators)]
    (vec (distinct (concat ?sp-creators ?op-creators (list (hg/creator event)))))))

(deftest event->creators-test
  (doseq [[event creators] example-event->creators]
    (is (= (event->creators event) creators))))


#_#_
;; TODO add tests
(m/=> topic+?event->creators [:=> [:cat Topic [:maybe Event]] [:vector {:min 1} Member]])
(defn topic+?event->creators [topic ?event]
  (if (or (nil? ?event)
          (and (nil? (hg/self-parent ?event))
               (nil? (hg/other-parent ?event))))
    (:member-init-keys topic)
    (let [?sp-creators (some->> (hg/self-parent ?event) (topic+?event->creators topic))
          ?op-creators (some->> (hg/other-parent ?event) (topic+?event->creators topic))]
      (vec (distinct (concat ?sp-creators ?op-creators (list (hg/creator ?event))))))))

(defn ?event->member-init-keys-log [?event]
  (if (nil? ?event)
    []
    (-> ?event hg/evt->db :member-init-keys-log)))

#_#_
(m/=> ?event->creators [:=> [:cat [:maybe :event]] [:vector :public-key]])
(defn* ^:memoizing ?event->creators [?event]
  (cond (nil? ?event)        []
        (root-event? ?event) (-> ?event :event/topic :stake-map keys)
        :else
        (let [?sp-creators (some-> (hg/self-parent ?event) (?event->creators))
              ?op-creators (some-> (hg/other-parent ?event) (?event->creators))]
          (vec (distinct (concat ?sp-creators ?op-creators (list (hg/creator ?event))))))))

#_#_#_#_
(def example-tip+creator->scions
  (array-map
   [a0 b] [a0]
   [a0 c] [a0]
   [b0 a] [b0]
   [b0 c] [a0 b0]
   [b1 a] [b0 b1]
   [b1 c] [a0 b0 b1]
   [a1 b] [a1]
   [a1 c] [a0 b0 a1]
   [c0 a] [b0 b1 c0]
   [c0 b] [c0]
   [a2 b] [a1 c0 a2]
   [a2 c] [a1 a2]))

;; damm slow
(defn tip+stem->scions [tip stem]
  (when-not (or (= tip stem) (hg/ancestor? stem tip))
    (vec (distinct (concat (some-> (hg/self-parent tip) (tip+stem->scions stem))
                           (some-> (hg/other-parent tip) (tip+stem->scions stem))
                           (list tip))))))

(defn tip+creator->scions [tip creator]
  (let [stem   (-> tip (event->creator->tip) (get creator))
        scions (tip+stem->scions tip stem)]
    scions))

(deftest tip+creator->scions-test
  (doseq [[[tip creator] scions] example-tip+creator->scions]
    (is (= scions (tip+creator->scions tip creator)))))


;; would not be needed were self-parent chain be a vec
(m/=> subvec-events [:=> [:cat :event :idx] [:sequential :event]])
(defn subvec-events [event start]
  (->> (iterate hg/self-parent event)
       (take-while some?)
       (take-while (fn [event] (>= (hg/event->index event) start)))
       reverse
       (vec)))

(def example-tip+graftee->scion-creator->scions
  (array-map
   [a0 b] {a [a0]}
   [a0 c] {a [a0]}
   [b0 a] {b [b0]}
   [b0 c] {a [a0]
           b [b0]}
   [b1 a] {b [b0 b1]}
   [b1 c] {a [a0]
           b [b0 b1]}
   [a1 b] {a [a1]}
   [a1 c] {a [a0 a1]
           b [b0]}
   [c0 a] {b [b0 b1]
           c [c0]}
   [c0 b] {c [c0]}
   [a2 b] {a [a1 a2]
           c [c0]}
   [a2 c] {a [a1 a2]}))

(m/=> tip+graftee->scion-creator->scions [:=> [:cat :event :public-key] [:map-of :public-key [:sequential :event]]])
(defn tip+graftee->scion-creator->scions [tip graftee]
  ;; will it work with unique-tips?
  (let [scion-creator->unique-tip (-> tip hg/event->creator->unique-tip)
        ?stem                     (-> graftee scion-creator->unique-tip)
        stem-creator->stem-tip    (or (some-> ?stem hg/event->creator->unique-tip)
                                      (hash-map))
        scion-creator->scions     (->> scion-creator->unique-tip
                                       (into (hash-map) (comp (map (fn [[scion-creator scion-tip]]
                                                                     (let [?stem-tip (-> scion-creator stem-creator->stem-tip)]
                                                                       [scion-creator (subvec-events scion-tip (or (some-> ?stem-tip hg/event->index inc)
                                                                                                                   0))])))
                                                              (remove (fn [[_ scions]] (empty? scions))))))]
    scion-creator->scions))

(deftest tip+graftee->scion-creator->scions-test
  (doseq [[[tip graftee] scion-creator->scions] example-tip+graftee->scion-creator->scions]
    (is (= scion-creator->scions (tip+graftee->scion-creator->scions tip graftee)))))

;; S$
{a {a0 a0
    a1 a0}
 b {b0 b0}}
{0 {0 a0
    1 a0}
 1 {1 b0}}
{0 [a0
    a1]
 1 [b0]}
{0 [{;; :s$/creator$       0 ;; known from prev idx, we're saving plenty from that context for multiple events from the same creator
     :s$/creation-time 0}
    {:s$/creation-time 2
     ;; :s$/self-parent [0 0] ;; creator$ known from prev idx, self-parent-idx is implied
     :s$/other-parent$ [1 0]}]
 1 [{:s$/creation-time 1
     :s$/other-parent$ [0 0]}]}
(def s$-creation-time "ct")
(def s$-topic "topic")
(def s$-other-parent "op")
(def s$-tx "tx")
{0 [{"ct" 0}
    {"ct" 2
     "op" [1 0]}]
 1 [{"ct" 1
     "op" [0 0]}]}

(hgs/register!
 ::e$ [:tuple :idx :idx]
 :e$ ::e$
 ::s$
 [:map
  [s$-creation-time int?]
  [s$-topic {:optional true} :topic]
  [s$-other-parent {:optional true} [:ref ::e$]]
  [s$-tx {:optional true} :txes]]
 :s$ ::s$)


(def g$-topic 1)
(def g$-topic-hash 2)
(def g$-grafter 3)
(def g$-graftee 4)
(def g$-graftee-member-init-key 5)
(def g$-stem 6)
(def g$-scion 7)
(def g$-novel-creators 8)
(def g$-c$->ss$ 9)
;; TODO: check that novel scion-creator-tip's signatures match

;; creator's idx
(hgs/register!
 ::c$ :idx
 :c$ ::c$

 ::g$
 [:map
  [g$-topic-hash int?]
  [g$-graftee :public-key] ;; TODO idx can be used
  [g$-grafter :public-key] ;; TODO idx can be used
  [g$-stem {:optional true} :idx]
  [g$-scion :idx] ;; could be derived, but that's some hussle
  [g$-novel-creators {:optional true} [:vector {:min 1} :public-key]]
  [g$-c$->ss$ [:map-of {:min 1}
               :c$ [:vector {:min 1} :s$]]]]
 :g$ ::g$)

(def example-grafter-tip+graftee->g$+?graftee-tip->novel-events
  (array-map
   [a0 b] [{g$-topic-hash     (hash abc-topic)
            g$-graftee        b
            g$-grafter        a
            g$-scion          0
            g$-novel-creators [a b c]
            g$-c$->ss$        {0 [{s$-creation-time 0
                                   s$-topic         abc-topic}]}}
           {nil                          [a0]
            b0                           nil
            {hg/creator       b
             hg/creation-time 0
             :event/topic     abc-topic} [a0]}]
   [b0 a] [{g$-topic-hash (hash abc-topic)
            g$-graftee    a
            g$-grafter    b
            g$-stem       0
            g$-scion      0
            g$-c$->ss$    {1 [{s$-creation-time 1
                               s$-other-parent  [0 0]}]}}
           {a0 [b0]
            a1 nil
            a2 nil}]
   [b0 c] [{g$-topic-hash     (hash abc-topic)
            g$-graftee        c
            g$-grafter        b
            g$-scion          0
            g$-novel-creators [a b c]
            g$-c$->ss$        {0 [{s$-creation-time 0
                                   s$-topic         abc-topic}]
                               1 [{s$-creation-time 1
                                   s$-other-parent  [0 0]}]}}
           {nil [a0 b0]
            {hg/creator       c
             hg/creation-time 1
             hg/other-parent  a0} [b0]}]
   [b1 a] [{g$-topic-hash (hash abc-topic)
            g$-graftee    a
            g$-grafter    b
            g$-scion      1
            g$-stem       0
            g$-c$->ss$    {1 [{s$-creation-time 1
                               s$-other-parent [0 0]}
                              {s$-creation-time 2}]}}
           {a0 [b0 b1]
            a2 nil}]
   [b1 c] [{g$-topic-hash     (hash abc-topic)
            g$-graftee        c
            g$-grafter        b
            g$-scion          1
            g$-novel-creators [a b c]
            g$-c$->ss$        {0 [{s$-creation-time 0
                                   s$-topic         abc-topic}]
                               1 [{s$-creation-time 1
                                   s$-other-parent  [0 0]}
                                  {s$-creation-time 2}]}}
           {nil [a0 b0 b1]}]
   [a1 b] [{g$-topic-hash (hash abc-topic)
            g$-graftee    b
            g$-grafter    a
            g$-scion      1
            g$-stem       0
            g$-c$->ss$    {0 [{s$-creation-time 2
                               s$-other-parent  [1 0]}]}}
           {b0 [a1]}]
   [a1 c] [{g$-topic-hash     (hash abc-topic)
            g$-graftee        c
            g$-grafter        a
            g$-scion          1
            g$-novel-creators [a b c]
            g$-c$->ss$        {0 [{s$-creation-time 0
                                   s$-topic         abc-topic}
                                  {s$-creation-time 2
                                   s$-other-parent  [1 0]}]
                               1 [{s$-creation-time 1
                                   s$-other-parent  [0 0]}]}}
           {nil [a0 b0 a1]
            c0  [a1]}]

   [c0 a] [{g$-topic-hash (hash abc-topic)
            g$-graftee    a
            g$-grafter    c
            g$-stem       0
            g$-scion      0
            g$-c$->ss$    {1 [{s$-creation-time 1
                               s$-other-parent  [0 0]}
                              {s$-creation-time 2}]
                           2 [{s$-creation-time 3
                               s$-other-parent  [1 1]
                               s$-tx            [:text-message {:text-message/content "Heyo!"}]}]}}
           {a0 [b0 b1 c0]
            a1 [b1 c0]
            {hg/creator a
             hg/creation-time 3
             hg/self-parent   a1
             hg/other-parent b1} [c0]}]

   [c0 b] [{g$-topic-hash (hash abc-topic)
            g$-graftee    b
            g$-grafter    c
            g$-stem       1
            g$-scion      0
            g$-c$->ss$    {2 [{s$-creation-time 3
                               s$-other-parent  [1 1]
                               s$-tx            [:text-message {:text-message/content "Heyo!"}]}]}}
           {b1 [c0]}]

   [a2 b] [{g$-topic-hash (hash abc-topic)
            g$-graftee    b
            g$-grafter    a
            g$-stem       1
            g$-scion      2
            g$-c$->ss$    {0 [{s$-creation-time 2
                               s$-other-parent  [1 0]}
                              {s$-creation-time 4
                               s$-other-parent  [2 0]}]
                           2 [{s$-creation-time 3
                               s$-other-parent  [1 1]
                               s$-tx            [:text-message {:text-message/content "Heyo!"}]}]}}
           {b1 [a1 c0 a2]}]

   [a2 c] [{g$-topic-hash (hash abc-topic)
            g$-graftee    c
            g$-grafter    a
            g$-stem       0
            g$-scion      2
            g$-c$->ss$    {0 [{s$-creation-time 2
                               s$-other-parent  [1 0]}
                              {s$-creation-time 4
                               s$-other-parent  [2 0]}]}}
           {c0 [a1 a2]}]))

;; grafter is kinda the one who grafts, which is the receiver
;; sender-tip, receiver-tip?
(m/=> topic-hash+grafter-tip+graftee->g$ [:=> [:cat :hash :event :idx] :g$])
(defn topic-hash+grafter-tip+graftee->g$ [topic-hash >tip <creator]
  ;; will it work with unique-tips? it will not, as we do need to pass on forks as well
  (let [>creator->scion-tip (-> >tip hg/event->creator->unique-tip)
        ?<tip               (-> <creator >creator->scion-tip)
        >creator->scions    (not-empty (tip+graftee->scion-creator->scions >tip <creator))

        #_#_>creators      (-> >tip ?event->member-init-keys-log)
        #_#_<creators      (-> ?<tip ?event->member-init-keys-log)
        #_#_novel-creators (vec-difference >creators <creators)
        #_#_all-creators   (vec (concat <creators novel-creators))

        c$->ss$ (->> >creator->scions
                     (reduce (fn [c$->ss$-acc [scion-creator scions]]
                               (let [ss$ (->> scions
                                              (mapv (fn [{:event/keys [creation-time other-parent topic tx]}]
                                                      (cond-> {s$-creation-time creation-time}
                                                        other-parent (assoc s$-other-parent [(hg/creator other-parent) (hg/event->index other-parent)])
                                                        topic        (assoc s$-topic topic)
                                                        tx           (assoc s$-tx tx)))))]
                                 (assoc c$->ss$-acc scion-creator ss$)))
                             (hash-map)))]
    (cond-> {g$-topic-hash              topic-hash
             g$-graftee                 <creator
             g$-graftee-member-init-key (nth (-> >tip hg/evt->db :member-init-keys-log) <creator)
             g$-grafter                 (hg/creator >tip)
             g$-c$->ss$                 c$->ss$
             g$-scion                   (hg/event->index >tip)}
      ?<tip (assoc g$-stem (hg/event->index ?<tip))
      #_#_  (not-empty novel-creators) (assoc g$-novel-creators novel-creators))))

(deftest topic-hash+grafter-tip+graftee->g$-test
  (doseq [[[grafter-tip graftee] [g$]] example-grafter-tip+graftee->g$+?graftee-tip->novel-events]
    (is (= g$ (topic-hash+grafter-tip+graftee->g$ (hash abc-topic) grafter-tip graftee)))))

(defn ?<<tip+g$->?novel-events [?<<tip g$]
  (l [::?<<tip+g$->?novel-events ?<<tip g$])
  (letl2 [>creator             (g$ g$-grafter)
          <<creator->tip       (or (some-> ?<<tip hg/event->creator->unique-tip) (hash-map))
          ?<tip-idx            (get g$ g$-stem)
          ?<tip                (some->> ?<tip-idx (hg/nth-sp ?<<tip))
          <creator->tip        (or (some-> ?<tip hg/event->creator->unique-tip) (hash-map))
          <creator->scion-event-idx-relative-offset (->> <creator->tip (map-vals (comp inc hg/event->index)))

          #_#_<creators      (-> ?<tip ?event->creators)
          #_#_novel-creators (get g$ g$-novel-creators [])
          #_#_all-creators   (vec (concat <creators novel-creators))
          c$->ss$        (get g$ g$-c$->ss$)

          ;; memoizing by hashes of these args may easily result in hash conflicts, e.g. (= (hash [1 32]) (hash [2 1]))
          ->adoptables
          (memoizing {:args->mem-k (fn [[arg1 arg2]] (str arg1 "_" arg2))}
                     (fn ->adoptables* [scion-creator-idx scion-event-idx]
                       (letl2 [scion-creator scion-creator-idx #_ (nth all-creators scion-creator-idx)]
                         (if-let [known-event (some-> (<<creator->tip scion-creator) (hg/nth-sp scion-event-idx))]
                           (l [known-event])
                           (letl2 [scion-event-idx-relative                              (- scion-event-idx (<creator->scion-event-idx-relative-offset scion-creator))
                                   s$                                                    (or (get-in c$->ss$ [scion-creator-idx scion-event-idx-relative]) (throw (ex-info "can't find s$" {:scion-creator-idx scion-creator-idx :scion-event-idx scion-event-idx :all-creators all-creators :<<creator->tip <<creator->tip})))
                                   [?sp-event ?sp-novel-events #_?sp-novel-creator->tip] (when (> scion-event-idx 0)
                                                                                           (->adoptables* scion-creator-idx (dec scion-event-idx)))
                                   ?s$-op                                                (get s$ s$-other-parent)
                                   [?op-event ?op-novel-events #_?op-novel-creator->tip] (when-let [[op-scion-creator-idx op-scion-event-idx] ?s$-op]
                                                                                           (->adoptables* op-scion-creator-idx op-scion-event-idx))

                                   ?s$-topic           (get s$ s$-topic)
                                   ?s$-tx              (get s$ s$-tx)
                                   event               (cond-> (transient {hg/creator       scion-creator
                                                                           hg/creation-time (get s$ s$-creation-time)})
                                                         ?sp-event (assoc! hg/self-parent ?sp-event)
                                                         ?op-event (assoc! hg/other-parent ?op-event)
                                                         ?s$-tx    (assoc! hg/tx ?s$-tx)
                                                         ?s$-topic (assoc! hg/topic ?s$-topic)
                                                         :always   (persistent!))
                                   ?novel-events       (-> (or (xor ?sp-novel-events ?op-novel-events)
                                                               (and ?sp-novel-events ?op-novel-events
                                                                    (vec-union ?sp-novel-events ?op-novel-events))
                                                               [])
                                                           (conj event))
                                   #_#_
                                   ?novel-creator->tip (-> (or (xor ?sp-novel-creator->tip
                                                                    ?op-novel-creator->tip)
                                                               (merge-with hg/max-sp
                                                                           ?sp-novel-creator->tip
                                                                           ?op-novel-creator->tip))
                                                           (assoc scion-creator event))]
                                  [event ?novel-events #_?novel-creator->tip])))))

          >creator$ >creator #_(-indexOf all-creators >creator)
          >tip-idx  (get g$ g$-scion)

          [event ?novel-events #__novel-creator->tip] (->adoptables >creator$ >tip-idx)]
         [event ?novel-events] ;; TODO return only ?novel-events when done testing
         ))

(deftest ?<<tip+g$->?novel-events-test
  (doseq [[[grafter-tip graftee] [g$ ?grafter-tip->novel-events]] example-grafter-tip+graftee->g$+?graftee-tip->novel-events]
    (doseq [[graftee-tip novel-events] ?grafter-tip->novel-events]
      (let [[event ?novel-events] (?<<tip+g$->?novel-events graftee-tip g$)]
        (is (= grafter-tip event))
        (is (= novel-events ?novel-events))))))

(m/=> g$->?topic [:=> [:cat :g$] [:maybe :topic]])
(defn g$->?topic [g$]
  (->> (g$ g$-c$->ss$)
       (some (fn [[c$ ss$]]
               (->> ss$ (some (fn [s$] (s$ s$-topic))))))))

(deftest g$->?topic-test
  (doseq [[_ [g$]] example-grafter-tip+graftee->g$+?graftee-tip->novel-events]
    (let [expected-?topic (if (g$ g$-stem) nil abc-topic)]
      (is (= expected-?topic (g$->?topic g$))))))



;; ---- old -----
#_
(def Graft
  [:map
   [:graft/scions [:and
                   [:vector {:min 1} Event]
                   [:fn {:error/message "graft's scions are not distinct"}
                    (fn [scions] (distinct? scions))]]]
   [:graft/stem {:optional true} Event]])

;; update with :graft/topic
;; I want ->concluded-round to be pure fn of event, so topic needs to be on event
;; + events need to be globally distinct, so root events need to come with topic (do they though?)
;; more like it's nice of them to be fully self-descriptive

;; abc-topic is implied
#_
(def example-grafter-tip+graftee->graft+graft$+?graftee-tip->novel-events
  (array-map
   [a0 b] [{:graft/topic  abc-topic
            :graft/scions [a0]}
           {:graft$/topic          abc-topic ;; TODO topic$, mayb reconsider share-stake-log, why have it?
            :graft$/scions$        [{:scion$/creator$      0
                                     :scion$/creation-time 0}]
            :graft$/novel-creators [a b c]}
           {nil [a0]}]
   [a0 c] [{:graft/topic  abc-topic
            :graft/scions [a0]}
           {:graft$/topic          abc-topic
            :graft$/scions$        [{:scion$/creator$      0
                                     :scion$/creation-time 0}]
            :graft$/novel-creators [a b c]}
           {nil [a0]}]
   [b0 a] [{:graft/topic  abc-topic
            :graft/stem   a0
            :graft/scions [b0]}
           {:graft$/topic-hash (hash abc-topic)
            :graft$/stem$      0
            :graft$/scions$    [{:scion$/creator$      1
                                 :scion$/creation-time 1
                                 :scion$/other-parent$ [0 0]}]}
           {a0 [b0]}]
   [b0 c] [{:graft/topic  abc-topic
            :graft/scions [a0 b0]}
           {:graft$/topic          abc-topic
            :graft$/scions$        [{:scion$/creator$      0
                                     :scion$/creation-time 0}
                                    {:scion$/creator$      1
                                     :scion$/creation-time 1
                                     :scion$/other-parent$ [0 0]}]
            :graft$/novel-creators [a b c]}
           {nil [a0 b0]}]
   [b1 a] [{:graft/topic  abc-topic
            :graft/stem   a0
            :graft/scions [b0 b1]}
           {:graft$/topic-hash (hash abc-topic)
            :graft$/stem$      0
            :graft$/scions$    [{:scion$/creator$      1
                                 :scion$/creation-time 1
                                 :scion$/other-parent$ [0 0]}
                                {:scion$/creation-time 2
                                 :scion$/self-parent$  [1 0] ;; maybe switch to creator$ & self-parent-evt$
                                 }]}
           {a0 [b0 b1]}]
   [b1 c] [{:graft/topic  abc-topic
            :graft/scions [a0 b0 b1]}
           {:graft$/topic          abc-topic
            :graft$/scions$        [{:scion$/creator$      0
                                     :scion$/creation-time 0}
                                    {:scion$/creator$      1
                                     :scion$/creation-time 1
                                     :scion$/other-parent$ [0 0]}
                                    {:scion$/creation-time 2
                                     :scion$/self-parent$  [1 0]}]
            :graft$/novel-creators [a b c]}
           {nil [a0 b0 b1]}]
   [a1 b] [{:graft/topic  abc-topic
            :graft/stem   b0
            :graft/scions [a1]}
           {:graft$/topic-hash (hash abc-topic)
            :graft$/stem$      0
            :graft$/scions$    [{:scion$/creation-time 2
                                 :scion$/self-parent$  [0 0]
                                 :scion$/other-parent$ [1 0]}]}
           {b1 [a1]}]
   [a1 c] [{:graft/topic  abc-topic
            :graft/scions [a0 b0 a1]}
           {:graft$/topic          abc-topic
            :graft$/scions$        [{:scion$/creator$      0
                                     :scion$/creation-time 0}
                                    {:scion$/creator$      1
                                     :scion$/creation-time 1
                                     :scion$/other-parent$ [0 0]}
                                    {:scion$/creation-time 2
                                     :scion$/self-parent$  [0 0]
                                     :scion$/other-parent$ [1 0]}]
            :graft$/novel-creators [a b c]}
           {nil [a0 b0 a1]}]

   [c0 a] [{:graft/topic  abc-topic
            :graft/stem   a0
            :graft/scions [b0 b1 c0]}
           {:graft$/topic-hash (hash abc-topic)
            :graft$/stem$      0
            :graft$/scions$    [{:scion$/creator$      1
                                 :scion$/creation-time 1
                                 :scion$/other-parent$ [0 0]}
                                {:scion$/creation-time 2
                                 :scion$/self-parent$  [1 0]}
                                {:scion$/creator$      2
                                 :scion$/creation-time 3
                                 :scion$/other-parent$ [1 1]
                                 :scion$/tx            [:text-message {:text-message/content "Heyo!"}]}]}
           {a0 [b0 b1 c0]}]
   [c0 b] [{:graft/topic  abc-topic
            :graft/stem   b1
            :graft/scions [c0]}
           {:graft$/topic-hash (hash abc-topic)
            :graft$/stem$      1
            :graft$/scions$    [{:scion$/creator$      2
                                 :scion$/creation-time 3
                                 :scion$/other-parent$ [1 1]
                                 :scion$/tx            [:text-message {:text-message/content "Heyo!"}]}]}
           {b1 [c0]}]
   [a2 b] [{:graft/topic  abc-topic
            :graft/stem   b1
            :graft/scions [a1 c0 a2]}
           {:graft$/topic-hash (hash abc-topic)
            :graft$/stem$      1
            :graft$/scions$    [{:scion$/creation-time 2
                                 :scion$/self-parent$  [0 0]
                                 :scion$/other-parent$ [1 0]}
                                {:scion$/creation-time 3
                                 :scion$/creator$      2
                                 :scion$/other-parent$ [1 1]
                                 :scion$/tx            [:text-message {:text-message/content "Heyo!"}]}
                                {:scion$/creation-time 4
                                 :scion$/self-parent$  [0 1]
                                 :scion$/other-parent$ [2 0]}]}
           {b1 [a1 c0 a2]}]
   [a2 c] [{:graft/topic  abc-topic
            :graft/stem   c0
            :graft/scions [a1 a2]}
           {:graft$/topic-hash (hash abc-topic)
            :graft$/stem$      0
            :graft$/scions$    [{:scion$/creation-time 2
                                 :scion$/self-parent$  [0 0]
                                 :scion$/other-parent$ [1 0]}
                                {:scion$/creation-time 4
                                 :scion$/self-parent$  [0 1]
                                 :scion$/other-parent$ [2 0]}]}
           {c0 [a1 a2]}]))

#_#_
(m/=> topic+tip+creator->graft [:=> [:cat Topic Event string?] Graft])
(defn topic+tip+creator->graft [topic tip creator]
  (let [?stem  (-> tip (event->creator->tip) (get creator))
        scions (tip+creator->scions tip creator)]
    (cond-> {:graft/scions scions
             :graft/topic  topic}
      ?stem (assoc :graft/stem ?stem))))

#_
(deftest topic+tip+creator->graft-test
  (doseq [[[grafter-tip graftee] [graft]] example-grafter-tip+graftee->graft+graft$+?graftee-tip->novel-events]
    (is (= graft (topic+tip+creator->graft abc-topic grafter-tip graftee)))))


;; graft$ is a compact graft
#_
(def Scion$
  [:and
   [:map
    [:scion$/creator$ {:optional true} Idx]
    [:scion$/creation-time nat-int?]
    [:scion$/self-parent$ {:optional true} [:tuple Idx Idx]]
    [:scion$/other-parent$ {:optional true} [:tuple Idx Idx]]
    [:scion$/tx {:optional true} Txes]]
   [:fn {:error/message "scion$ has both creator$ and self-parent$, at most one is allowed"}
    (fn [{:scion$/keys [creator$ self-parent$]}] (xor creator$ self-parent$))]])

#_
(def Graft$
  [:and
   [:map
    [:graft$/topic {:optional true} Topic]
    [:graft$/topic-hash {:optional true} number?]
    [:graft$/stem$ {:optional true} Idx]
    [:graft$/scions$ [:and
                      [:vector {:min 1} Scion$]
                      [:fn {:error/message "graft$'s scions$ are not distinct"}
                       (fn [scions$] (distinct? scions$))]]]
    [:graft$/novel-creators {:optional true} [:vector {:min 1} Member]]]
   [:fn {:error/message "only one of topic or topic-hash is expected, but both are present"}
    (fn [{:graft$/keys [topic topic-hash]}]
      (xor topic topic-hash))]
   [:fn {:error/message "graft$ has stem$, so creator knows topic, topic-hash is expected but is missing"}
    (fn [{:graft$/keys [stem$ topic-hash]}]
      (or (nil? stem$)
          topic-hash))]
   [:fn {:error/message "graft$ doesn't have stem$, creator may not know topic, but topic is missing"}
    (fn [{:graft$/keys [stem$ topic]}]
      (or stem$
          topic))]])


#_#_
(m/=> graft->graft$ [:=> [:cat Graft] Graft$])
(defn graft->graft$ [{:graft/keys  [scions topic]
                      ?stem        :graft/stem}]
  (let [tip            (-> scions reverse last)
        tip-creators   (-> tip (?event->creators))
        stem-creators  (-> ?stem (?event->creators))
        novel-creators (->> tip-creators
                            (reduce
                             (fn [novel-creators-acc scion-creator]
                               (if (and (= -1 (-indexOf stem-creators scion-creator))
                                        (= -1 (-indexOf novel-creators-acc scion-creator)))
                                 (conj novel-creators-acc scion-creator)
                                 novel-creators-acc))
                             []))
        all-creators   (vec (concat stem-creators novel-creators))
        scions$        (->> scions
                            (reduce (fn [scions$-acc {:event/keys [creator creation-time self-parent other-parent payload]}]
                                      (let [creator$    (-indexOf all-creators creator)
                                            op-creator  (hg/creator other-parent)
                                            op-creator$ (-indexOf all-creators op-creator)
                                            scion$      (cond-> {:scion$/creation-time creation-time}
                                                          (not self-parent) (assoc :scion$/creator$ creator$)
                                                          self-parent       (assoc :scion$/self-parent$ [creator$ (hg/event->index self-parent)])
                                                          other-parent      (assoc :scion$/other-parent$ [op-creator$ (hg/event->index other-parent)])
                                                          payload           (assoc :scion$/payload payload))]
                                        (conj scions$-acc scion$)))
                                    []))
        graft$         (cond-> {:graft$/scions$ scions$}
                         (not-empty novel-creators) (assoc :graft$/novel-creators novel-creators)
                         ?stem                      (-> (assoc :graft$/stem$ (hg/event->index ?stem))
                                                        (assoc :graft$/topic-hash (hash topic)))
                         (not ?stem)                (assoc :graft$/topic topic))]
    graft$))

#_
(deftest graft->graft$-test
  (doseq [[grafter-tip+graftee [graft graft$]] example-grafter-tip+graftee->graft+graft$+?graftee-tip->novel-events]
    (l grafter-tip+graftee)
    (is (= graft$ (graft->graft$ graft)))))

#_#_
(m/=> ?tip+graft$->?novel-events [:=> [:cat [:maybe Event] Graft$] [:maybe [:vector Event]]])
(defn ?tip+graft$->?novel-events [?tip {:graft$/keys [topic novel-creators stem$ scions$] :as graft$}]
  (let [?stem         (when stem$ (or (nth-sp ?tip stem$) (throw (ex-info "did not find stem for stem$" {:stem$ stem$ :tip ?tip}))))
        stem-creators (?event->creators ?stem)
        _             (when-let [duplicate-creator (->> novel-creators (some (fn [new-creator] (not= -1 (-indexOf stem-creators new-creator)))))]
                        (throw (ex-info "Duplicate novel-creators of creators found" {:duplicate-creator duplicate-creator :stem-creators stem-creators :novel-creators novel-creators})))
        all-creators  (vec (concat stem-creators novel-creators))
        creator->tip  (or (some-> ?tip (event->creator->tip)) {})
        ->adoptables  (fn ->adoptables* [{:scion$/keys [creator$ creation-time self-parent$ other-parent$] :as scion$}]
                        (let [creator$      (or (l creator$) (first (l self-parent$)) (throw (ex-info "cannot determine creator$ of scion$" {:stem-creators stem-creators :novel-creators novel-creators :?tip ?tip :scion$ scion$ :graft$ graft$ })))
                              scion-creator (or (nth all-creators creator$)   (throw (ex-info "did not find creator for creator$" {:scion$ scion$ :creator$ creator$ :stem-creators stem-creators :?tip ?tip :graft$ graft$})))
                              scion-idx     (or (inc (second self-parent$)) 0)]
                          (if-let [known-event (some-> (creator->tip scion-creator) (nth-sp scion-idx nil))] ;; in case of possible duplicity, also check that scion and known event are the same
                            [known-event]
                            (let [[?sp-event ?sp-novel-events ?sp-creator->new-tip] (when self-parent$ (or (some-> (nth all-creators (first self-parent$) nil) (creator->tip) vector)
                                                                                                           (let [sp-s$ (some (fn [s$] (let [s$$ (if (:scion$/self-parent$ s$)
                                                                                                                                                  [(first (:scion$/self-parent$ s$))
                                                                                                                                                   (inc (second (:scion$/self-parent$ s$)))]
                                                                                                                                                  [(:scion$/creator$ s$)
                                                                                                                                                   0])]
                                                                                                                                        (when (= self-parent$ s$$)
                                                                                                                                          s$)))
                                                                                                                             scions$)]
                                                                                                             (-> (l sp-s$) ->adoptables* (or (throw (ex-info "did not find self-parent$ to adopt" {:self-parent$ self-parent$ :scion$ scion$ :creator$ creator$ :stem-creators stem-creators :?tip ?tip :graft$ graft$})))))))
                                  [?op-event ?op-novel-events ?op-creator->new-tip] (when other-parent$ (or (some-> (nth all-creators (first other-parent$) nil) (creator->tip) vector)
                                                                                                            (let [op-s$ (some (fn [s$] (let [s$$ (if (:scion$/self-parent$ s$)
                                                                                                                                                   [(first (:scion$/self-parent$ s$))
                                                                                                                                                    (inc (second (:scion$/self-parent$ s$)))]
                                                                                                                                                   [(:scion$/creator$ s$)
                                                                                                                                                    0])]
                                                                                                                                         (when (= other-parent$ s$$)
                                                                                                                                           s$)))
                                                                                                                              scions$)]
                                                                                                              (-> (l op-s$) ->adoptables* (or (throw (ex-info "did not find other-parent$ to adopt" {:self-parent$ self-parent$ :scion$ scion$ :creator$ creator$ :stem-creators stem-creators :?tip ?tip :graft$ graft$})))))))

                                  novel-event      (cond-> {hg/creator       scion-creator
                                                            hg/creation-time creation-time}
                                                     ?sp-event              (assoc hg/self-parent ?sp-event)
                                                     ?op-event              (assoc hg/other-parent ?op-event)
                                                     (and (nil? ?sp-event)
                                                          (nil? ?op-event)) (assoc :event/topic topic))
                                  novel-events     (-> (vec (concat ?sp-novel-events ?op-novel-events))
                                                       (conj novel-event))
                                  creator->new-tip (-> (or (xor ?op-creator->new-tip ?sp-creator->new-tip)
                                                           (merge-with (fn [sp-tip op-tip] (if (> (hg/event->index sp-tip) (hg/event->index op-tip)) sp-tip op-tip))
                                                                       ?sp-creator->new-tip
                                                                       ?op-creator->new-tip))
                                                       (assoc scion-creator novel-event))]
                              [novel-event novel-events creator->new-tip]))))
        [_ ?novel-events] (l (->adoptables (-> scions$ reverse first)))]
    ?novel-events))

#_
(deftest ?tip+graft$->novel-events-test
  (doseq [[grafter-tip+graftee [_ graft$ ?graftee-tip->novel-events]] example-grafter-tip+graftee->graft+graft$+?graftee-tip->novel-events]
    (doseq [[?graftee-tip novel-events] ?graftee-tip->novel-events]
      (l grafter-tip+graftee)
      (is (= novel-events (?tip+graft$->?novel-events ?graftee-tip graft$))))))

#_#_
(m/=> ?tip+graft$->graft [:=> [:cat [:maybe Event] Graft$] Graft])
(defn ?tip+graft$->graft [?tip {:graft$/keys [topic novel-creators stem$ scions$]}]
  (when (and stem$ (not ?tip)) (throw (ex-info "graft$ has stem$, but no tip is found" {:stem$ stem$ :?tip ?tip})))
  (let [creator->tip  (or (some-> ?tip (event->creator->tip)) {})
        ?stem         (when stem$ (or (nth-sp ?tip stem$) (throw (ex-info "did not find stem for stem$" {:stem$ stem$ :tip ?tip}))))
        stem-creators (?event->creators ?stem)
        _             (when-let [duplicate-creators (->> novel-creators
                                                         (filter (fn [new-creator] (not= -1 (-indexOf stem-creators new-creator))))
                                                         not-empty)]
                        (throw (ex-info "Duplicate novel-creators of creators found" {:duplicate-creators duplicate-creators})))
        all-creators  (vec (concat stem-creators novel-creators))

        scions (->> scions$
                    (reduce (fn [[creator->tip-acc scions-acc] {:scion$/keys [creator$ creation-time self-parent$ other-parent$ payload] :as scion$}]
                              (let [?self-parent  (when-let [[sp-creator$ sp$] self-parent$]
                                                    (let [sp-creator (nth all-creators sp-creator$)
                                                          sp-tip     (get creator->tip-acc sp-creator)
                                                          sp         (nth-sp sp-tip sp$)]
                                                      sp))
                                    ?other-parent (when-let [[op-creator$ op$] other-parent$]
                                                    (let [op-creator (nth all-creators op-creator$)
                                                          op-tip     (get creator->tip-acc op-creator)
                                                          op         (nth-sp op-tip op$)]
                                                      op))
                                    creator       (or (some->> creator$ (nth all-creators)) (hg/creator ?self-parent) (throw (ex-info "cannot determine creator of scion$" {:scion$ scion$})))
                                    scion         (cond-> {hg/creator creator
                                                           hg/creation-time creation-time}
                                                    ?self-parent  (assoc hg/self-parent ?self-parent)
                                                    ?other-parent (assoc hg/other-parent ?other-parent)
                                                    payload       (assoc :event/payload payload)
                                                    (and (nil? ?self-parent)
                                                         (nil? ?other-parent)) (assoc :event/topic topic))

                                    new-creator->tip-acc (assoc creator->tip-acc creator scion)
                                    new-scions-acc       (conj scions-acc scion)]
                                [new-creator->tip-acc new-scions-acc]))
                            [creator->tip []])
                    (last))
        graft (cond-> {:graft/scions scions}
                ?stem (assoc :graft/stem ?stem))]
    graft))

#_
(deftest ?tip+graft$->graft-test
  (doseq [[grafter-tip+graftee [graft graft$]] example-grafter-tip+graftee->graft+graft$+?graftee-tip->novel-events]
    (l grafter-tip+graftee)
    (let [?tip (:graft/stem graft)]
      (is (= graft (?tip+graft$->graft ?tip graft$))))))
