(ns hashgraph.app.core
  "Entry ns for the browser app"
  {:dev/always true}
  (:require [rum.core :as rum]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
            [hashgraph.app.log :as hga-log]
            [hashgraph.app.inspector :as hga-inspector]
            [hashgraph.app.page :as page]))

(defn start []
  ;; start is called after code's been reloaded
  ;; this is configured in :after-load in the shadow-cljs.edn
  (when-let [node (.getElementById js/document "root")]
    (rum/mount (page/view) node))
  (js/console.log "started"))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is configured in :before load in the shadow-cljs.edn
  (js/console.log "stopped"))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (hga-log/init)
  (tufte/add-basic-println-handler!
   {}
   #_{:format-pstats-opts {:columns      [:n :p50 :mean :clock :sum]
                           :format-id-fn name}})
  (hga-inspector/register-keydowns-lister)
  (start))
