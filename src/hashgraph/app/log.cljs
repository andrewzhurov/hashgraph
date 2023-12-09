(ns hashgraph.app.log
  "Sets up portal with logs from timbre
   Following this guide https://github.com/djblue/portal/tree/master/examples/timbre"
  (:require [cljs.reader]
            [clojure.datafy :as d]
            #_[clojure.instant :as i]
            [portal.web :as p]
            [taoensso.timbre :refer-macros [spy] :as timbre]
            ))

(defonce *portal-logs (atom '()))

(defn log->portal-log [{:keys [spying? vargs level ?err msg_ timestamp_ ?ns-str ?file context ?line] :as all}]
  #_(js/console.log all)
  (cond spying?
        (let [[expr _ result] vargs]
          {;; :runtime :cljs
           :expr expr
           :result result})

        :else
        (merge
         (when ?err
           {:error (d/datafy ?err)})
         (when-let [ts (force timestamp_)]
           {:time ts #_ (i/read-instant-date ts)})
         {:level   level
          :ns      (symbol (or ?ns-str ?file "?"))
          :line    (or ?line 1)
          :column  1
          :result  (force msg_)
          :runtime :clj}
         context)))

(defn portal-log!
  "Accumulate a rolling log of 100 entries."
  [log]
  (swap! *portal-logs
         (fn [logs]
           (take 100 (conj logs (log->portal-log log))))))

(defn setup-timbre->portal []
  (timbre/merge-config!
   {:appenders
    {:memory {:enabled? true :fn portal-log!}}}))

;; Did not succeed in enabling data reader for #spy
;; Shadow-cljs complains that it's not found,
;; tried dynamic registration, as seen below,
;; and data_readers.cljc and data_readers.clj, akin to how it's setup in hashp,
;; with no avail.
;; Will use (spy (+ 1 1)) for now.
#_(defn setup-spy-data-reader []
  (cljs.reader/register-tag-parser! 'spy js/console.log))

(defn open-portal []
  (p/open {:window-title "Logs Viewer" :value *portal-logs}))

#_(log/with-context+
  {:runtime :cljs}
  (log/info "my cljs log"))

(defn init []
  (setup-timbre->portal)
  (p/set-defaults! {:theme :portal.colors/gruvbox})
  #_(add-tap p/submit)
  (open-portal))
