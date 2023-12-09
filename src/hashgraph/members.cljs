(ns hashgraph.members
  (:require [cljs.math :refer [floor ceil]]
            [taoensso.timbre :refer-macros [spy]]))

(def names ["Alice" "Bob" "Charlie" "Dean" "Elon" "Frank"
            "Gregory" "Henry" ;; "Ivan" "Jamie" "Kenny" "Lesly"
            ;; "Mark" "Nora" "Omar" "Pavel" "Quinn" "Rob"
            ])
(def people
  (->> names
       (map-indexed
        (fn [idx n]
          [n {:member/idx  idx
              :member/name name
              :member/color-rgb
              (let [color-position (-> (+ 255 255 255)
                                       (/ (count names))
                                       (* idx))
                    red            (min 255 color-position)
                    green          (-> (min (* 255 2) color-position)
                                       (- 255))
                    blue           (-> (min (* 255 3) color-position)
                                       (- 255 255))]
                [red green blue])}]))
       (sequence cat)
       (apply array-map)))

(spy people)
(def members-count 6) ;; should be derived from the hashgraph
(def members (into (array-map) (take members-count) people))
(map key members) (type people)
(spy members)
(def supermajority (-> members-count (* 2) (/ 3) ceil))
(def many supermajority)
