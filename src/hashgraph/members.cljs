(ns hashgraph.members
  (:require [cljs.math :refer [floor ceil]]
            [clojure.set :as set]
            [taoensso.timbre :refer-macros [spy]]
            [garden.color :refer [rgb rgb->hsl rotate-hue hsl->rgb]
             :as gc]
            [hashgraph.utils.core :as utils]))

(def male-names
  ["Arnold"
   "Bob"
   "Charlie"
   "Dean"
   "Elon"])

(def female-names
  ["Alice"
   "Britney"
   "Carol"])

;; "Dean" "Elon" "Frank"
;; "Gregory" "Henry" "Ivan" "Jamie" "Kate" "Lesly"
;; "Mark" "Nora" "Omar" "Pavel" "Quinn" "Rob"

(def names ["Alice" "Bob" "Charlie" "Dean" "Elon" ;; "Frank"
            ;; "Gregory" "Henry" ;; "Ivan" "Jamie" "Kate" "Lesly"
            ;; "Mark" "Nora" "Omar" "Pavel" "Quinn" "Rob"
            ])
(def names-count (count names))

(not= -1 (-indexOf male-names "Charlie"))

[1 [2 3 4 5 6 7 8 9 10] 11]

(def colors
  [[255 0 0]
   [0 255 0]
   [0 0 255]
   [255 255 0]
   [0 255 255]
   [255 125 0]
   [0 125 255]
   [255 0 125]
   [125 0 255]
   [255 125 125]
   [125 255 125]
   [125 125 255]
   [255 255 125]
   [125 255 255]
   [255 125 255]
   [125 0 0]
   [0 125 0]
   [0 0 125]])

(def palette1
  [[38, 70, 83]
   [42, 157, 143]
   [233, 196, 106]
   [244, 162, 97]
   [231, 111, 81]

   [38, 70, 83]
   [42, 157, 143]
   [233, 196, 106]
   [244, 162, 97]
   [231, 111, 81]

   [38, 70, 83]
   [42, 157, 143]
   [233, 196, 106]
   [244, 162, 97]
   [231, 111, 81]

   [38, 70, 83]
   [42, 157, 143]
   [233, 196, 106]
   [244, 162, 97]
   [231, 111, 81]])

#_
(def colors
  [[160 0 0]
   [160 80 0]
   [160 160 0]
   [160 255 0]
   [160 255 80]
   [160 255 160]
   [160 160 160]
   [160 80 160]
   [160 0 160]
   [80 0 160]
   [80 0 80]
   [160 0 80]
   [160 80 80]
   ])

;; initial members count
(def initial-members-count 3 #_(count names))
(def people
  (->> names
       (into [] (map-indexed
                 (fn [idx n]
                   {:member/idx  idx
                    :member/name n
                    :member/gender (cond (not= -1 (-indexOf male-names n)) :male
                                         (not= -1 (-indexOf female-names n)) :female
                                         :else :unknown)
                    :member/color-rgb (get palette1 idx)
                    #_(-> (rgb->hsl (rgb 255 0 0))
                        (rotate-hue (-> 360
                                        (/ (count names))
                                        (* idx)))

                        #_(gc/scale-saturation 100)
                        #_(gc/saturate 100)
                        #_(gc/scale-alpha -100)
                        #_(gc/scale-lightness -10)
                        (gc/darken 7)
                        (hsl->rgb)
                        ((juxt :red :green :blue)))
                    #_(get colors idx)
                    #_(let [color-position (-> (+ 255 255 255)
                                             (/ (+ 2 (count names)))
                                             (* idx))
                          red            (min 255 color-position)
                          green          (-> (min (* 255 2) color-position)
                                             (- 255))
                          blue           (-> (min (* 255 3) color-position)
                                             (- 255 255))]
                      [red green blue])})))))

(def member-name->person
  (->> people
       (mapcat (fn [person] [(:member/name person) person]))
       (apply array-map)))

;; initial members
(def initial-member-names
  [(first names) (nth names 2) (last names)]
  #_[(nth people 0)
     (nth people 5)
     (nth people 10)])
(def hardly-reachable-members-count 1 #_(floor (/ (count names) 3)))
(def hardly-reachable-member-names (utils/random-nths hardly-reachable-members-count (set/difference (set names)
                                                                                                     (set initial-member-names))))
(def supermajority (-> initial-member-names (* 2) (/ 3) ceil)) ;; been used for main algorithm when there was no dynamic stake
(def many supermajority)
