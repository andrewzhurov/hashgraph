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

(defn log->portal-log [{:keys [spying? id key vargs level ?err msg_ timestamp_ ?ns-str ?file context ?line] :as all}]
  #_(js/console.log all)
  (cond spying?
        (let [[expr _ result] vargs]
          (if (map? result)
            (apply array-map
                   :expr expr
                   (apply concat result))
            {:expr   expr
             :result result}))

        :else
        (let [k (and (= 2 (count vargs)) (:key (first vargs)))
              id (and (= 2 (count vargs)) (:id (first vargs)))
              result (last vargs)]
          (if id
            (cond-> {}
              id (assoc :id id)
              k (assoc k result)
              (and (nil? k) (map? result)) (merge result)
              (and (nil? k) (not (map? result))) (assoc :result result))))))

(defn portal-log!
  "Accumulate a rolling log of 100 entries."
  [log]
  (if (:spying? log)
    (let [[expr delim value] (:vargs log)]
      (js/console.log {:expr expr :value value}))
    (js/console.log (first (:vargs log))))
  #_(swap! *portal-logs
         (fn [logs]
           (let [id (and (some-> log :vargs second)
                         (some-> log :vargs first :id))
                 logged-log (and id
                                 (some #(and (= id (:id %))
                                             %)
                                       logs))]
             (take 100 (conj logs (cond->> (log->portal-log log)
                                    logged-log (merge logged-log))))))))

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
  #_(open-portal))
