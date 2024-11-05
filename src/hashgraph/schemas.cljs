(ns hashgraph.schemas
  (:require
   ;; [hashgraph.utils.core :refer-macros [l]]
   [malli.core :as m]
   [malli.util :as mu]
   [malli.error :as me]
   [malli.registry :as mr]
   [clojure.pprint :refer [pprint]]))

(defonce *registry
  (atom (merge (m/default-schemas)
               (mu/schemas))))

(defn register!
  ([type ?schema & rest]
   (or (even? (count rest)) (throw (ex-info "odd count of arguments passed to register!" {:type type :?schema ?schema :rest rest})))
   (register! type ?schema)
   (doseq [[next-type next-?schema] (partition 2 rest)]
     (register! next-type next-?schema))
   @*registry)
  ([type ?schema]
   ;; (l [:registering type ?schema])
   (swap! *registry assoc type ?schema)))

(mr/set-default-registry!
 (mr/mutable-registry *registry))


(defn check
  ([schema-id value] (check schema-id value false))
  ([schema-id value not-valid-out]
   (or (m/validate schema-id value)
       (do (pprint value)
           (pprint (me/humanize (m/explain schema-id value)))
           (if (= not-valid-out ::throw)
             (throw (ex-info "valued does not conform to schema" {:value value :schema schema-id}))
             not-valid-out)))))

(defn checks [schema-id value]
  (check schema-id value ::throw)
  value)
