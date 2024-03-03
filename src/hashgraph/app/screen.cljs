(ns hashgraph.app.screen
  "Entry ns for the browser app"
  {:dev/always true}
  (:require-macros [cljs-thread.core :refer [spawn]]
                   [cljs.test :refer [run-tests run-all-tests]])
  (:require [cljs-thread.core :as thread]
            [cljs-thread.state :as thread-s]
            [cljs-thread.spawn :as thread-sp]
            [cljs-thread.root :as thread-r]
            [cljs-thread.util :as thread-u]
            [rum.core :as rum]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
            [hashgraph.app.log :as hga-log]
            [hashgraph.app.keyboard :as hga-keyboard]
            [hashgraph.app.inspector :as hga-inspector]
            [hashgraph.app.page :as page]
            [hashgraph.utils.core :refer [log-flush!] :refer-macros [l]]
            [hashgraph.utils-test]
            [hashgraph.utils.js-map-test]
            [hashgraph.app.events :as hga-events]
            [hashgraph.app.playback :as hga-playback]
            [hashgraph.app.tutorial :as hga-tutorial]))

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

#_
(defn run-tests* []
  (run-tests ~env
             ~@(map
                (fn [ns] `(quote ~ns))
                (cond->> (ana-api/all-ns)
                  re (filter #(re-matches re (name %)))))))

(defn start []
  ;; start is called after code's been reloaded
  ;; this is configured in :after-load in the shadow-cljs.edn
  #_(js/setTimeout 100 #(run-all-tests #".*" #_#"hashgraph\..*test.*"))

  (when-let [node (.getElementById js/document "root")]
    (rum/mount (page/view) node))
  (js/console.log "started"))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is configured in :before load in the shadow-cljs.edn
  (log-flush!)
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
  (hga-log/init)
  (tufte/add-basic-println-handler!
   {}
   #_{:format-pstats-opts {:columns      [:n :p50 :mean :clock :sum]
                           :format-id-fn name}})

  (let [events< (hga-events/initial-events<->events< hga-tutorial/tutorial-events)]
    (hga-playback/init-playback! events<)
    (js/setTimeout #(hga-tutorial/create-tutors! events<)))

  (hga-keyboard/register-keydowns-lister!)

  (start))
