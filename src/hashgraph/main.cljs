(ns hashgraph.main
  (:require [cljs.math :refer [floor ceil]]
            [hashgraph.members :as hg-members]
            [taoensso.timbre :refer-macros [spy]]))

(defn parent [x] (:parent x))
(defn self-parent [x] (:self-parent x))

(def parents
  "Set of events that are parents of x"
  (memoize
   (fn [x]
     (cond-> #{}
       (parent x) (conj (parent x))
       (self-parent x) (conj (self-parent x))))))

(def ancestor?
  (memoize
   (fn [x y]
     (or ((parents x) y)
         (some #(ancestor? % y) (parents x))))))

(def self-ancestor?
  (memoize
   (fn [x y]
     (or (= (self-parent x) y)
         (some-> (self-parent x) (self-ancestor? y))))))

(def many-creators?
  (memoize
   (fn [events]
     (-> events
         (->> (map :creator))
         distinct
         count
         (> hg-members/many)))))

(def events
  "Returns a set of events known to x."
  (memoize
   (fn [x]
     (cond-> #{}
       (:self-parent x) (-> (conj (:self-parent x))
                            (into (events (:self-parent x))))
       (:parent x)      (-> (conj (:parent x))
                            (into (events (:parent x))))))))

(def tips "Latest events by members"
  (memoize (fn [evts]
             (->> evts (into #{} (remove (fn [x] (some (fn [y] (= (:self-parent y) x)) evts))))))))

(def index
  "Index position of event x, as max of index position of self-parent and parent"
  (memoize
   (fn [x]
     (if (and (nil? (self-parent x))
              (nil? (parent x)))
       0
       (apply max (cond-> []
                    (parent x)      (conj (inc (index (parent x))))
                    (self-parent x) (conj (inc (index (self-parent x))))))))))

(def self-ancestors
  (memoize
   (fn [x]
     (if (nil? (self-parent x))
       []
       (into [(self-parent x)] (self-ancestors (self-parent x)))))))

(def self-descendants
  "Returns a set of self-descendants of y, as known by x."
  (memoize
   (fn [x y]
     (-> x
         events
         (->> (filter (fn [z] (-> z :self-parent (= y)))))))))

(def in-fork?
  "Whether y is in a fork, as known to x."
  (memoize
   (fn [x y]
     (or (some-> y
                 self-parent
                 (->> (self-descendants x))
                 count
                 (> 1))
         (some->> y self-parent (in-fork? x))))))


(def see?
  (memoize
   (fn [x y]
     (and (ancestor? x y)
          #_(not (in-fork? x y))))))

(def see-many-see?
  (memoize
   (fn [x y] ;; strongly-see
     (-> x
         events
         (->> (filter (fn [z] (see? z y))))
         (many-creators?)))))

(def round ;; see-many-see-many-see ;; see-many-strongly-see
  "Returns round number of x.
   It's either 1 if x has no parents,
   or a max round of events strongly seen by many +1."
  (memoize
   (fn [x]
     (-> x
         events
         (->> (filter (fn [y] (see-many-see? x y)))
              (group-by round)
              (filter (comp many-creators? val))
              (map first))
         (some->> not-empty (apply max) inc)
         (or 1)))))

(defn witness? [x]
  (or (nil? (:self-parent x))
      (< (round (:self-parent x))
         (round x))))

(defn rounds-diff [x y]
  (- (round x)
     (round y)))

(def d 1) ;; round in which voting starts
(def c 10) ;; each c's round is a coin flip round
(defn voting-round? [x y] (> (rounds-diff x y) d))

(defn voting-coin-flip-round? [x y]
  (and (voting-round? x y)
       (->> (rounds-diff x y) (mod c) (= 0))))

(defn signature [evt] (:signature evt))
(defn middle-bit [sig] 1)

(defn self-witness [x]
  (cond (nil? (:self-parent x))     nil
        (witness? (:self-parent x)) (:self-parent x)
        :else                       (self-witness (:self-parent x))))

(defn vote-see? [x y] (= (rounds-diff x y) 1))
(defn vote-see  [x y] (see? x y))

(declare voting-concluded?)
(declare vote)
(defn vote-copy? [x y]
  (or (not (witness? x))
      (some-> (self-parent x) (voting-concluded? y))))
(defn vote-copy [x y] (vote (self-parent x) y))

(declare votes-fract-true)
(defn vote-coin-flip? [x y]
  (and (voting-coin-flip-round? x y)
       (<= (-> (votes-fract-true x y) (/ 3) floor))
       (>= (-> (votes-fract-true x y) (* 2) (/ 3) floor))))

(defn vote-coin-flip [x] (= 1 (middle-bit (signature x))))

(defn vote-for-majority [x y] (>= (votes-fract-true x y) (/ 1 2)))

(defn vote
  "Vote of x about fame of y."
  [x y]
  (cond (vote-see? x y)       (vote-see x y)
        (vote-copy? x y)      (vote-copy x y)
        (vote-coin-flip? x y) (vote-coin-flip x)
        :vote-for-majority    (vote-for-majority x y)))

(defn votes
  "Votes on fame of y from witnesses seen by many in the round before x, as known to x."
  [x y]
  (-> x
      events
      (->> (filter (fn [z] (and (= (rounds-diff x z) 1)
                                (witness? z)
                                (see-many-see? x z))))
           spy
           (map (fn [z] (vote z y)))
           spy)))

(defn votes-fract-true [x y]
  (/ (count (filter true? (votes x y)))
     (max 1 (count (votes x y)))))

(defn voting-concluded?
  "Whether fame of y is concluded, as known to x.
   Fame is concluded when in a regular round there are many same votes."
  [x y]
  (or (some-> (self-witness x) (voting-concluded? y))
      (and (witness? x)
           (witness? y)
           (voting-round? x y)
           (not (voting-coin-flip-round? x y))
           (or (> (count (filter true? (votes x y))) hg-members/many)
               (> (count (filter false? (votes x y))) hg-members/many)))))

(defn round-concluded?
  "Whether the round has fame of all it's witnesses concluded,
   and the previous round, if there is one, been concluded, as known to x."
  [x r]
  (and (-> x
           events
           (->> (filter (fn [y] (and (-> y round (= r))
                                     (witness? y))))
                (every? (fn [yr] (voting-concluded? x yr)))))
       (let [previous-round (dec r)]
         (or (zero? previous-round)
             (round-concluded? x previous-round)))))

(defn round-received
  "The first round in which all unique famous witnesess see it,
   given fame of all witnesses in previous rounds been decided (round been decided)."
  [x y]
  )
