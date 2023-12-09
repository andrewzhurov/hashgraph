(ns hashgraph.event
  (:require [malli.core :as m]
            [malli.experimental.lite :as l]
            [malli.registry :as mr]
            [malli.util :as mu]))

(def registry
  [:schema {:registry}]
  (merge
   (m/default-schemas)
   (mu/schemas)

   {:name [:and
           {:gen/elements ["Alice" "Bob" "Charlie" "Dean" "Elon" "Frank" "Gefry" "Henry" "Ivan" "Karl" "Mark" "Noah" "Opra"]}
           string?]

    ::creator                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       :name
    ;; ::parent :hashgraph/event
    ::self-parent                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   [:ref :hashgraph/event]
    ::signature                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     string?
    :hashgraph/event                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                [:map [::creator
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           [::parent {:optional true} [:ref :hashgraph/event]]]] #_ [:and [:map
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              #_#_#_#_[[:ref ::creator]]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           [[:ref ::parent] {:optional true}]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           [[:ref ::self-parent] {:optional true}]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              [[:ref ::signature]]]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     #_[:fn (fn [{::keys [creator self-parent]}]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (or (nil? self-parent)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (-> self-parent ::creator (= creator))))]]
    }))

(m/validate :hashgraph/event
            {::creator "Alice"}
            {:registry registry})


;; (def A1 {:creator "Alice"})
;; (def A2 {:creator     "Alice"
;;          :self-parent A1})
;; (def B1 {:creator "Bob"
;;          :parent  A2})
;; (def B2 {:creator     "Bob"
;;          :self-parent B1})
;; (def A3 {:creator "Alice"
;;          :self-parent A2
;;          :parent B2})

;; A3
;; | \
;; |  B2
;; |  |
;; |  B1
;; | /
;; A2
;; |
;; A1
