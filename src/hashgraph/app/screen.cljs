(ns hashgraph.app.screen
  "Entry ns for the browser app"
  {:dev/always true}
  (:require-macros [cljs-thread.core :refer [spawn]])
  (:require [cljs-thread.core :as thread]
            [cljs-thread.state :as thread-s]
            [cljs-thread.spawn :as thread-sp]
            [cljs-thread.root :as thread-r]
            [cljs-thread.util :as thread-u]
            [rum.core :as rum]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
            [hashgraph.app.log :as hga-log]
            [hashgraph.app.inspector :as hga-inspector]
            [hashgraph.app.page :as page]
            [hashgraph.utils :refer-macros [l]]
            [hashgraph.utils-test]))


#_#_
(defn in-safari? []
  (and (thread-u/in-browser? "Safari")
       (not (thread-u/in-chrome?))))

(defn cljs-thread-init! [& [config-map]]
  (when config-map
    (swap! thread-s/conf merge config-map))
  (let [config @thread-s/conf]
    (assert (:sw-connect-string config))
    (do (thread-sp/spawn-sw #(spawn {:id :root :no-globals? true}
                             (thread-r/init-root! config)))
        (when-not (u/in-safari?)
          (sp/on-sw-registration-reload)))
    (if-not (:sw-connect-string config)
      (spawn {:id :root :no-globals? true}
             (r/init-root! config))
      )))

(defn start []
  ;; start is called after code's been reloaded
  ;; this is configured in :after-load in the shadow-cljs.edn
  (set! hashgraph.utils/log! hashgraph.app.inspector/log!)
  (hashgraph.utils-test/test)
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
  #_
  (with-redefs [cljs-thread.util/in-safari? (fn [] true)]
    (thread/init!
     {:sw-connect-string   "/js/sw.js"
      ;; :repl-connect-string "/js/repl.js"
      :core-connect-string "/js/core.js"
      }))
  (set! hashgraph.utils/log! hashgraph.app.inspector/log!)
  (hga-log/init)
  (tufte/add-basic-println-handler!
   {}
   #_{:format-pstats-opts {:columns      [:n :p50 :mean :clock :sum]
                           :format-id-fn name}})
  (hga-inspector/register-keydowns-lister)
  (start))
