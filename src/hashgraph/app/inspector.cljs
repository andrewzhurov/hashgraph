(ns hashgraph.app.inspector
  (:require
   [cljs.reader]
   [garden.core :refer [css]]
   [goog.object]
   #_[garden.def :refer-macros [defkeyframes]]
   [garden.color :as gc]
   [garden.stylesheet :refer [at-keyframes]]
   [hashgraph.app.icons :as hga-icons]
   [hashgraph.app.view :refer [t] :as hga-view]
   [hashgraph.app.keyboard :refer [kb-key? kb-keys?] :as hga-keyboard]
   [hashgraph.app.transitions :refer [tt] :as hga-transitions]
   [hashgraph.utils.core
    :refer-macros [defn* l]
    :refer [*parent-log-path* *parent-log-path-logging?* *log-path* *log-path-logging?*
            debounce lazy-derived-atom atomic? conjs merge-attr-maps]
    :as utils]
   [hashgraph.utils2 :refer-macros [td time3]]

   [rum.core :as rum]
   [taoensso.timbre :refer-macros [spy info]]
   [taoensso.tufte :as tufte :refer [defnp p profiled profile]]

   [clojure.set]
   [cljs.math :refer [floor ceil]]
   [clojure.set :as set]))

#_(type
   (let [tm (transient (hash-map))]
     (loop [tm* tm
            nth 2]
       (if (pos? nth)
         (let [new-tm* (assoc! tm* nth nth)]
           (recur new-tm* (dec nth)))
         tm*))))

(def *compact-names? (atom false))

;; (def peeked-init #{})
;; (def *peeked (atom peeked-init))
;; (defn peek-flush! [] (reset! *peeked peeked-init))
;; (defn peek-set! [value] (reset! *peeked #{value}) value)
;; (defn peek! [value] (peek-set! value) #_(swap! *peeked conj value) value)
;; (defn unpeek! [value] (swap! *peeked disj value) value)
(def inspected-init [])
(def *inspected (atom inspected-init))
(defn inspected-flush! [] (reset! *inspected inspected-init))

(defn inspect   [inspected value] (if (= -1 (-indexOf inspected value))
                                    (conj inspected value)
                                    inspected))
(defn uninspect [inspected value] (vec (remove #(= % value) inspected)))

(defn inspect!   [value] (swap! *inspected inspect   value) value)
(defn uninspect! [value] (swap! *inspected uninspect value) value)

(defn toggle-inspect! [value]
  (swap! *inspected (fn [inspected]
                      (if (= -1 (-indexOf inspected value))
                        (inspect inspected value)
                        (uninspect inspected value))))
  value)

(def ->in?
  (fn [coll el]
    (->> coll (some (fn [coll-el]
                      (or ;; (identical? coll-el el) ;; :inspected/at-time meta messes up identical?
                       (= (hash coll-el) (hash el))
                       (and (or (vector? coll-el)
                                (seq? coll-el)
                                (set? coll-el))
                            (->in? coll-el el))))))))

(def active-inspectable-init {})
(def *active-inspectable (atom active-inspectable-init))
(defn active-inspectable! [path value] (reset! *active-inspectable {:path path :value value}))
(defn inactive-inspectable! [path value] (swap! *active-inspectable (fn [active-inspectable]
                                                                      (if (and (some? active-inspectable)
                                                                               (identical? path (:path active-inspectable))
                                                                               (identical? value (:value active-inspectable)))
                                                                        active-inspectable-init
                                                                        active-inspectable))))
(defn subpath? [sub-path path]
  (boolean (reduce (fn [path-left sub-path-el]
                     (if (or (empty? path-left)
                             (not (identical? (first path-left) sub-path-el)))
                       (reduced false)
                       (rest path-left)))
                   path sub-path)))
(assert (true?  (subpath? [1 2]     [1 2 3])))
(assert (false? (subpath? [1 2 3 4] [1 2 3])))

(declare peek-key)
(def *peeked (rum/derived-atom [*active-inspectable hga-keyboard/*kb-keys] ::*peeked
                               (fn [active-inspectable kb-keys]
                                 (when (kb-key? peek-key kb-keys)
                                   (:value active-inspectable)))))

(def ->peeked?
  (fn [el]
    (let [peeked (rum/react *peeked)]
      (and (some? el)
           (identical? peeked el)))))

(def ->inspected?
  (fn [el]
    (when-let [inspected (not-empty (rum/react *inspected))]
      (and (some? el)
           (not= -1 (-indexOf inspected el))))))

(def ->in-inspected?
  (fn [el]
    (let [inspected (rum/react *inspected)]
      (->in? inspected el))))

(def ->in-peeked?
  (fn [el]
    (when-let [peeked (rum/react *peeked)]
      (or (= (hash el) (hash peeked))
          (and (or (vector? peeked)
                   (set? peeked)
                   (seq? peeked))
               (->in? peeked el))))))

(def ->analysis?
  (fn []
    (or (not (empty? (rum/react *inspected)))
        (not (nil? (rum/react *peeked))))))

(def icon-style
  [:& {:cursor          "pointer"
       :width           "20px"
       :height          "20px"
       :display         :flex
       :align-items     :center
       :justify-content :center
       :color           "maroon"
       :font-weight     :bold
       :opacity         0.5
       :transition      "0.2s"}
   [:&.active
    {:opacity 1}]])

(def inspector-styles
  [[:.inspector {:display       :flex
                 :padding-right "10px"
                 :width         :fit-content
                 :height        :fit-content}
    [:table {:border   "1px solid gray"
             :padding  "5px"
             :position :relative ;; for sticky keys
             :height   :fit-content
             :width    :fit-content
             ;; :border-collapse :collapse
             }
     [:.table-key {:position         :sticky
                   :background-color "rgba(255,255,255,0.9)" ;; for table keys to show overflown vals semi-transparent, before table key inspector hides them completely
                   }
      [:.inspector {:background-color "white" ;; for table keys to hide overflown vals
                    }]]
     [:&.horizontal
      [:.table-key {:left "0px"}]]
     [:&.vertical
      [:.table-key {:top "0px"}]]

     [:tbody {:vertical-align :top}]
     [:tr {:border-bottom "1px solid gray"
           :transition    "0.2s"}

      [:td {:vertical-align :top}]]]

    [:.dt-sym {:padding-left "5px" :padding-right "5px"}]
    [:.collapsed-content-sym {}]

    [:.map {:display     :flex
            :white-space :nowrap
            :align-items :top}
     [:.kv {:display     :flex
            :white-space :nowrap
            :align-items :top}
      [:.k {:margin-right "10px"}]
      [:.v {}]]]

    [:.object {:display     :flex
               :white-space :nowrap
               :align-items :top}]

    [:.seqable {:display     :flex
                :white-space :nowrap
                :align-items :top}]

    [:.kw {:display     :flex
           :white-space :nowrap}
     [:.delim {:color "black"}]
     [:.ns {:color "darkcyan"}]
     [:.name {:color "mediumblue"}]]

    [:.bool
     [:&.true {:color "green"}]
     [:&.false {:color "red"}]]

    [:.atom {:display :flex}]

    [:.trace
     [:.trace-info-wrapper {:background-color "orange"
                            :border           "1px solid white"}
      [:.trace-info {:width          :fit-content
                     :max-width      "100%"
                     :position       :sticky
                     :left           0
                     :overflow-x     :hidden
                     :display        :flex
                     :align-items    :top
                     :padding-top    "10px"
                     :padding-bottom "10px"}
       [:.fn-name {:padding-left "10px"}]
       [:.fn-args {}]
       [:.result  {}]]]
     [:.child-traces {:display :flex}
      [:.child-trace {}]]]]

   [:.inspectable {:cursor              :pointer
                   :unselectable        "on"
                   :user-select         :none ;; /* CSS3 (little to no support) */
                   :-ms-user-select     :none ;; /* IE 10+ */
                   :-moz-user-select    :none ;; /* Gecko (Firefox) */
                   :-webkit-user-select :none ;; /
                   :transition          "background-color 0.4s"}
    [:&.inspected
     {:background-color "rgba(0,0,0,0.05)"}]
    [:&.peeked
     {:background-color "rgba(0,0,0,0.1)"}]]])



(def peek-key :ctrl)
(def inspect-key :ctrl)
(def flip-key :alt)
(def enhance-key :shift)
(def ^:dynamic **inspector-in-view?* (atom true))

(defn inspectable [el & [{:keys [in-view? path]
                          :or   {in-view? true}}]]
  (when in-view?
    (let [analysis? (->analysis?)]
      {#_:on-mouse-over
       :on-mouse-move  #(do (.stopPropagation %)
                            (active-inspectable! path el) #_(reset! *inspectable-under-mouse el))
       :on-mouse-leave #(do (inactive-inspectable! path el) #_(reset! *inspectable-under-mouse nil))
       :on-click       #(when (kb-key? inspect-key)
                          (.preventDefault %)
                          (.stopPropagation %)
                          (toggle-inspect! el))
       :class          (cond-> ["inspectable"]
                         analysis? (into ["analysis"
                                          (when (->in-inspected? el) "in-inspected")
                                          (when (->in-peeked? el) "in-peeked")
                                          (when (->inspected? el) "inspected")
                                          (when (->peeked? el) "peeked")]))})))


(def ms-keys-orders
  [[:trace/fn-name :trace/fn-args :trace/result :trace/time :trace/traces]
   [:fn-profile/fn-name :fn-profile/min-time :fn-profile/max-time :fn-profile/medium-time :fn-profile/max-time :fn-profile/total-time :fn-profile/max-time-traces :fn-profile/traces]])

(def m-key->idx
  (->> ms-keys-orders
       (map (fn [ms-keys-order] (->> ms-keys-order (into {} (map-indexed (fn [idx ms-key-order] [ms-key-order idx]))))))
       (apply merge)))

(def m-keys-comparator
  (memoize
   (fn [k1 k2]
     (< (m-key->idx k1)
        (m-key->idx k2)))))

(def order-ms-keys
  (memoize
   (fn [ms-keys]
     (sort m-keys-comparator ms-keys))))

(def ms-keys->horizontal?
  (fn [ms-keys]
    (cond (contains? ms-keys :mem-atoms) true)))

(def ->m-type
  (fn [value]
    (or (cond (and (map? value) (-> value keys first keyword?))
              (-> value keys first namespace keyword))
        :plain)))

(def *a (atom {}))
(let [m1 (range 100000)]
  (time (= m1 (with-meta m1 {:k :y}))))
(add-watch *a :key (fn [& args] (println args)))
(swap! *a (fn [v] v))


(declare inspector)
(rum/defcs table-view < rum/static rum/reactive (rum/local true ::*horizontal?) (rum/local nil ::*sort-by-key->sort-direction)
  [{::keys [*horizontal? *sort-by-key->sort-direction]} ms {:keys [table-view] :as opts}]
  (let [ms-keys (reduce into #{} (map keys ms))
        horizontal? @*horizontal? #_(if (some? @*horizontal?)
                      @*horizontal?
                      (if-let [default-mode (get table-view :default-mode)]
                        (= :horizontal default-mode)
                        (ms-keys->horizontal? ms-keys)))
        flip! #(when (kb-key? flip-key)
                 (.stopPropagation %)
                 (reset! *horizontal? (not horizontal?)))

        ms-type (->m-type (first ms))

        ordered-ms-keys (order-ms-keys ms-keys)

        default-sort-by-key->sort-direction
        (case ms-type
          :trace
          [:trace/time :asc]

          nil)

        [sort-by-key sort-direction] (or @*sort-by-key->sort-direction
                                         default-sort-by-key->sort-direction)
        sort-by!                     (fn [k] (swap! *sort-by-key->sort-direction
                                (fn [sort-by-key->sort-direction]
                                  (case sort-by-key->sort-direction
                                    [k :desc]
                                    [k :asc]

                                    [k :asc]
                                    [nil nil]

                                    [k :desc]))))

        ms (cond->> ms
             sort-by-key (sort-by sort-by-key)
             (some-> sort-direction (= :asc)) reverse)
        opts (assoc opts :hide-ns? true)]

    (if horizontal?
      [:table.horizontal {:on-click flip!}
       [:thead
        [:th.table-key (inspector ms-type)]
        (->> ms
             (map-indexed
              (fn [idx m]
                (let [id (hash m)]
                  [:th (merge {:key id}
                              (inspectable m))
                   (inc idx)]))))]

       [:tbody
        (for [ms-key ordered-ms-keys]
          (let [id       (hash ms-key)
                new-opts (update opts :path conj ms-key)
                ms-vals  (map #(get % ms-key) ms)]
            [:tr {:key id}
             [:td.table-key (merge {:key id}
                                   (inspectable ms-vals new-opts))
              (inspector ms-key new-opts)]
             (->> ms
                  (map-indexed
                   (fn [idx m]
                     (let [id       (hash m)
                           new-opts (update new-opts :path conj idx)
                           m-val    (get m ms-key utils/lookup-sentinel)]
                       [:td {:key id}
                        (when (not (identical? m-val utils/lookup-sentinel))
                          (inspector m-val new-opts))]))))]))]]

      [:table.vertical {:on-click flip!}
       [:thead
        [:tr
         [:th.table-key
          (inspector ms-type)]
         (for [ms-key ordered-ms-keys]
           (let [id          (hash ms-key)
                 new-opts    (update opts :path conj id)
                 ms-key-vals (->> ms
                                  (map ms-key)
                                  (filter some?))]
             [:th.table-key (merge-attr-maps {:key      id
                                              :on-click #(do (.stopPropagation %)
                                                             (sort-by! ms-key))}
                                             (inspectable ms-key-vals new-opts))
              (inspector ms-key new-opts)]))]]

       [:tbody
        (->> ms
             (map-indexed
              (fn [idx m]
                (let [new-opts (update opts :path conj idx)]
                  [:tr {:key (hash m)}
                   [:td (inspectable m new-opts)
                    (inc idx)]
                   (->> ordered-ms-keys
                        (map-indexed
                         (fn [idx ms-key]
                           (let [new-opts (update opts :path conj idx)
                                 m-val    (get m ms-key)]
                             [:td {:key idx}
                              (when (contains? m ms-key)
                                (inspector m-val new-opts))]))))]))))]])))


(rum/defc inspector-view-trace-flame-el < rum/static rum/reactive
  [{:trace/keys [fn-name fn-args result time-start time time-end traces] :as trace} {:keys [trace-scale path inspectable-in-path?] :as opts}]
  (let [width (* time trace-scale)]
    (when (> width 1)
      (let [new-inspectable-in-path? (if (false? inspectable-in-path?) false
                                         (subpath? path (:path (rum/react *active-inspectable))))
            opts                     (assoc opts :inspectable-in-path? new-inspectable-in-path?)]
        #_(l [path (:path (rum/react *active-inspectable))])
        [:div.trace (merge {:style {:width     (* time trace-scale)
                                    :max-width (* time trace-scale)}}
                           (inspectable trace opts))
         [:div.trace-info-wrapper
          [:div.trace-info
           [:div.fn-name (inspector fn-name opts)]
           (when new-inspectable-in-path?
             [:<>
              [:div.fn-args (inspector fn-args (update opts :path conj :fn-args))]
              [:div.result  (inspector result (update opts :path conj :result))]])]]
         [:div.child-traces
          (->> traces
               (map-indexed
                (fn [idx child-trace]
                  (let [opts (update opts :path conj idx)]
                    [:div.child-trace {:key   idx
                                       ;; indent child trace relative to parent trace by it's start-time
                                       :style {:margin-left
                                               (let [?prev-child-trace (get traces (dec idx) nil)
                                                     delta-time        (- (:trace/time-start child-trace)
                                                                          (or (:trace/time-end ?prev-child-trace)
                                                                              time-start))]
                                                 (* delta-time trace-scale))}}
                     (inspector-view-trace-flame-el child-trace opts)]))))]]))))

(rum/defcs inspector-view-trace-flame < rum/static rum/reactive (rum/local 1 ::*trace-scale) (rum/local false ::*input-key)
  [{::keys [*trace-scale *input-key]} trace opts]
  (let [trace-scale  @*trace-scale
        trace-scale! #(do (.stopPropagation %)
                          (swap! *trace-scale
                                 (fn [scale]
                                   (cond (kb-key? enhance-key) (inc scale)
                                         (> scale 1)            1
                                         :else                  2)))
                          (swap! *input-key not))
        opts         (assoc opts :trace-scale trace-scale)]
    [:div {:on-click trace-scale!}
     [:input {:key           @*input-key
              :type          :range
              :min           1
              :max           (max 100 trace-scale)
              :default-value trace-scale
              :on-click      #(.stopPropagation %)
              :on-change     (debounce 16 #(do (.stopPropagation %)
                                               (reset! *trace-scale (int (-> % .-target .-value)))))}]
     (inspector-view-trace-flame-el trace opts)]))

(rum/defc inspector-view-map < rum/static rum/reactive
  [m {:keys [path expanded-depth plain?] :as opts}]
  [:div.map (inspectable m opts)
   [:div.dt-sym "{"]
   (or (and (>= (count path) expanded-depth)
            (or (and (not-empty m)
                     [:div.collapsed-content-sym "..."])
                ""))
       (and (not plain?)
            (= :trace (->m-type m))
            (inspector-view-trace-flame m opts))
       (table-view [m] opts))
   [:div.dt-sym "}"]])

(rum/defc inspector-view-object < rum/static rum/reactive
  [obj opts]
  [:div.object
   [:div.dt-sym "js"]
   (inspector (js->clj obj :keywordize-keys true) opts)])

#_
(rum/defc inspector-view-cr
  < rum/static
  [cr]
  [:div.crs
   (for [cr (take-while some? (iterate :concluded-round/prev-concluded-round cr))]
     (inspector-view-map cr))])

(defn coll->groups [coll]
  (->> coll
       (reduce (fn [[?last-group & rest-groups :as groups] el]
                 (let [?el-ns (when (-> el map?)
                                (or (and (some-> el keys first keyword?)
                                         (some-> el keys first namespace))
                                    :no-ns))]
                   (cond (nil? ?el-ns)                      (conj groups {:group-els [el]})
                         (= (:group-ns ?last-group) ?el-ns) (conj rest-groups (update ?last-group :group-els conj el))
                         :else
                         (conj groups {:group-ns  ?el-ns
                                       :group-els [el]}))))
               '())
       reverse))

(rum/defcs inspector-view-seqable < rum/static rum/reactive (rum/local true ::*table-view?)
  [{::keys [*table-view?]} s {:keys [path expanded-depth] :as opts}]
  (let [[bracket-start bracket-end]
        (cond (seq? s)    ["(" ")"]
              (vector? s) ["[" "]"]
              (set? s)    ["#{" "}"])

        show-dt-sym? (not (::hide-dt-sym? (meta s)))

        resolved-s (->> s (mapv (fn [s-el] (if (atomic? s-el)
                                             (rum/react s-el)  ;; losing @ view sign here
                                             s-el))))
        groups     (coll->groups resolved-s)]
    [:div.seqable (merge-attr-maps (inspectable s opts)
                                   {:on-click #(when (kb-key? :alt)
                                                 (.stopPropagation %)
                                                 (swap! *table-view? not))})
     (when show-dt-sym?
       [:div.dt-sym bracket-start])
     (cond (empty? resolved-s)
           ""

           (and (>= (count path) expanded-depth))
           [:div.collapsed-content-sym "..."]

           :else ;; show elements
           (for [{:keys [group-ns group-els]} groups]
             (if group-ns
               (table-view group-els opts)
               (->> group-els
                    (map-indexed
                     (fn [idx s-el]
                       (let [opts (-> opts
                                      (update :path conj idx)
                                      (update :expanded-depth inc))]
                         (rum/with-key
                           (inspector s-el opts)
                           idx))))))))

     (when show-dt-sym?
       [:div.dt-sym bracket-end])]))

(rum/defc inspector-view-keyword < rum/static
  [kw opts]
  [:div.kw
   (when-not (:hide-ns? opts)
     [:<>
      [:div.delim ":"]
      (when-let [namespace (namespace kw)]
        [:<>
         [:div.ns namespace]
         [:div.delim "/"]])])
   [:div.name (name kw)]])

(rum/defc inspector-view-boolean < rum/static
  [bool]
  [:div.bool {:class (str bool)} (str bool)])

(rum/defc inspector-view-atom < rum/static rum/reactive
  [*a {:keys [expanded-depth path] :as opts}]
  [:div.atom [:div.dt-sym "@"] (if (>= (count path) expanded-depth)
                                 [:div.collapsed-content-sym "..."]
                                 (inspector (rum/react *a) opts))])

(rum/defc inspector-view-number < rum/static rum/reactive
  [num opts]
  [:div.number (merge {:title (str num)}
                      (inspectable num))
   (if-not (int? num)
     (.toFixed num 1)
     num)])

(rum/defc inspector-view-symbol < rum/static rum/reactive
  [sym opts]
  [:div.symbol {:title (str sym)} (cond-> (str sym)
                                    (rum/react *compact-names?) utils/name->compact-name)])

(rum/defc inspector-view-string < rum/static
  [str opts]
  [:div.string
   str])

(rum/defc inspector-view-default < rum/static
  [any]
  (pr-str any))

(def *inspector-wrapper-view-bounds (atom nil))
(defn set-inspector-wrapper-view-bounds!
  [dom-node]
  (swap! *inspector-wrapper-view-bounds
         (fn [[_ _ signal]]
           [(.-clientWidth dom-node) (.-clientHeight dom-node) (not signal)])))

(defn ->in-view? [dom-node]
  true
  #_
  (when-let [[view-width view-height _signal] (rum/react *inspector-wrapper-view-bounds)]
    (let [bounding-rect (.getBoundingClientRect dom-node) ;; returns relative to scrolled viewport coords https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect
          rel-x         (.-x bounding-rect)
          rel-y         (.-y bounding-rect)
          rel-x-end     (+ rel-x (.-width bounding-rect))
          rel-y-end     (+ rel-y (.-height bounding-rect))]
      ;; in-view?
      (not
       ;; not-in-view?
       (or

        ;;
        ;; [ el ] [ view ]
        ;;
        (neg? rel-x-end)

        ;;
        ;;        [ view ] [ el ]
        ;;
        (> rel-x view-width)

        ;;         [ el ]
        ;;        [ view ]
        ;;
        (neg? rel-y-end)

        ;;
        ;;        [ view ]
        ;;         [ el ]
        (> rel-y view-height))))))

(extend-type js/Event
  IEncodeClojure
  (-js->clj [x options]
    (persistent!
     (reduce (fn [r k] (assoc! r (keyword k) (js->clj (goog.object/get x k) options)))
             (transient {}) (js-keys x)))))

(rum/defcs inspector < rum/static rum/reactive (rum/local nil ::*local-expanded-depth) (rum/local nil ::*dom-node)
  {:did-mount (fn [state] (reset! (::*dom-node state) (rum/dom-node state)) state)}
  [{::keys [*local-expanded-depth *dom-node] :as state} value {:keys [expanded-depth path in-view?] :as opts}]
  (let [current-expanded-depth (or @*local-expanded-depth
                                   expanded-depth)
        in-view?               (cond
                                 (= in-view? false) false
                                 (nil? @*dom-node)  false
                                 :else              (->in-view? @*dom-node))
        new-opts               (-> opts
                     (assoc :expanded-depth current-expanded-depth
                            :in-view? in-view?))]
    [:div.inspector (if (or (seqable? value)
                            (atomic? value))
                      {:on-click #(do (.stopPropagation %)
                                      (swap! *local-expanded-depth (fn [local-expanded-depth]
                                                                     (cond (kb-key? enhance-key)
                                                                           (inc current-expanded-depth)

                                                                           (or (nil? local-expanded-depth)
                                                                               (= local-expanded-depth (count path)))
                                                                           (inc (count path))

                                                                           :else
                                                                           (count path)))))}
                      {})
     (cond #_#_ (and (map? value) (:concluded-round/r value))
           (inspector-view-cr value)

           (string? value)
           (inspector-view-string value new-opts)

           (keyword? value)
           (inspector-view-keyword value new-opts)

           (symbol? value)
           (inspector-view-symbol value new-opts)

           (number? value)
           (inspector-view-number value new-opts)

           (boolean? value)
           (inspector-view-boolean value new-opts)

           (atomic? value)
           (inspector-view-atom value new-opts)

           (map? value)
           (inspector-view-map value new-opts)

           (or (vector? value) (seq? value) (set? value))
           (inspector-view-seqable value new-opts)

           (not= value (js->clj value))
           (inspector-view-object value new-opts)

           :else (inspector-view-default value new-opts))]))

(defn* ^{:memoizing {:recur-by   (fn [[_ & rest-traces]] [rest-traces]) ;; warmup cache on cold start to escape max call stack exceeded due to deep uncached recursion
                     :recur-stop empty?
                     :only-last? true}
         :tracing   {:enabled? false}}
  traces->fn-profiles*
  [[{:trace/keys [fn-name time] :as trace} & rest-traces]]
  (if (nil? trace)
    {}
    (let [prev-fn-profiles (traces->fn-profiles* rest-traces)]
      (-> prev-fn-profiles
          (update fn-name (fn [{:fn-profile/keys [_fn-name times total-time max-time min-time medium-time medium-time-delta-total medium-time-delta-times] :as fn-profile}]
                            (let [new-total-time                 (+ total-time time)
                                  new-times                      (inc times)
                                  new-medium-time                (/ new-total-time new-times)
                                  ?new-medium-time-delta         (when medium-time
                                                                  (- medium-time new-medium-time))
                                  new-medium-time-delta-total    (+ medium-time-delta-total ?new-medium-time-delta)
                                  new-medium-delta-times         (cond-> medium-time-delta-times
                                                                   ?new-medium-time-delta inc)
                                  ?new-medium-time-deltas-medium (when (> new-medium-delta-times 0)
                                                                   (/ new-medium-time-delta-total
                                                                      new-medium-delta-times))]
                              (-> fn-profile
                                  (assoc :fn-profile/times new-times)
                                  (assoc :fn-profile/total-time new-total-time)
                                  (assoc :fn-profile/medium-time new-medium-time)
                                  (assoc :fn-profile/medium-delta-times new-medium-delta-times)
                                  (assoc :fn-profile/medium-time-delta-total new-medium-time-delta-total)
                                  (update :fn-profile/traces conj trace)
                                  (cond->
                                      (or (nil? min-time)
                                          (< time min-time))       (assoc :fn-profile/min-time time)
                                      (> time max-time)              (-> (assoc  :fn-profile/max-time time)
                                                                        (assoc  :fn-profile/max-time-trace trace))
                                      ?new-medium-time-delta         (assoc :fn-profile/medium-time-delta ?new-medium-time-delta)
                                      ?new-medium-time-deltas-medium (assoc :fn-profile/medium-time-deltas-medium ?new-medium-time-deltas-medium))))))))))

(def *fn-profiles (lazy-derived-atom [utils/*traces] ::*fn-profiles
                                     (fn [traces]
                                       (->> traces
                                            traces->fn-profiles*
                                            (map (fn [[fn-name fn-profile]]
                                                   (assoc fn-profile :fn-profile/fn-name fn-name)))))))

(def *max-time-trace (lazy-derived-atom [*fn-profiles] ::*max-time-trace
                                        (fn [fn-profiles]
                                          (-> fn-profiles
                                              (->> (sort-by :fn-profile/max-time))
                                              last
                                              :fn-profile/max-time-trace))))

(def m-types
  [:event
   :tx
   :round
   :vote
   :concluded-voting
   :concluded-round
   :received-event
   :plain])

(defn m-type->order [m-type]
  (let [idx (-indexOf m-types m-type)]
    (if (not= -1 idx)
      idx
      (do (js/console.warn "m-type order is unset for" m-type)
          100))))

(def bin-size (/ hga-view/window-size (count m-types)))

(def bins-view-styles
  [[:.bins-view
    [:.bins {:width          "100%"
             :display        :inline-flex
             :flex-direction :row}
     [:.bin {:width      "0px" ;; so flex items are of the same size, with no regard to their content
             :max-height "100%"
             :flex       "1 1 0px"
             :padding    "5px"
             :opacity    1
             ;; :transition (t :opacity (/ tt 2)) ;; looks half-baked without transition on un-peek
             }
      [:&.empty {:opacity 0}]
      [:.inspector {:padding "0px"}]
      [:.dt-sym {:padding "0px"}]
      [:>.inspector {:width               :fit-content
                     :max-width           "100%"
                     :max-height          "100%"
                     :margin              "0px"
                     :padding             "0px"
                     :box-sizing          :border-box
                     :border              "1px solid gray"
                     :border-radius       hga-view/border-radius
                     :overflow-x          :auto
                     :overflow-y          :auto
                     :overscroll-behavior :contain
                     :scrollbar-width     :thin}
       [:>.seqable {:width     "100%"
                    :max-width "100%"}
        [:>table {:padding "5px"
                  :margin  "0px"
                  :border  :none}]]]]]]])

(rum/defc bins-view < rum/static rum/reactive
  []
  [:div.bins-view
   [:style (css bins-view-styles)]
   [:style (css inspector-styles)]
   [:div.bins
    (let [inspected          (rum/react *inspected)
          ?peeked            (rum/react *peeked)
          inspectable+peeked (if (nil? ?peeked)
                               inspected
                               (inspect inspected ?peeked))
          flattened-distinct (-> inspectable+peeked
                                 utils/flatten-all
                                 distinct)]
      (when (not-empty flattened-distinct)
        (let [bins (->> flattened-distinct
                        (group-by ->m-type))]
          (for [m-type m-types]
            (let [bin-items (get bins m-type)]
              [:div.bin {:class [(when (empty? bin-items) "empty")]}
               (when (not-empty bin-items)
                 [:<>
                  #_
                  [:button.clean {:on-click inspected-flush!
                                  :title    "Clean"}
                   (hga-icons/icon :solid :trash)]
                  (inspector (with-meta bin-items {::hide-dt-sym? true}) {:expanded-depth 1
                                                                          :in-view?       true})])])))))]])

(def debug-inspector-view-styles
  [[:.debug-inspector
    {:width          "50vw"
     :display        :flex
     :flex-direction :row
     :position       :fixed
     :left           "0px"
     :top            "0px"
     :bottom         "0px"
     :overflow-y     :auto
     :overflow-x     :auto
     :background     "rgba(255,255,255, 0.9)"}
    [:button.clean {:width           "18px"
                    :height          "18px"
                    :min-width       "18px"
                    :min-height      "18px"
                    :display         :inline-flex
                    :justify-content :center
                    :align-items     :center
                    :margin-left     "5px"}]]])

(rum/defcs debug-view < rum/static rum/reactive (rum/local 0 ::*open-depth)
  {:did-mount (fn [state]
                (let [dom-node (rum/dom-node state)]
                  (set-inspector-wrapper-view-bounds! dom-node)
                  (assoc state ::on-scroll-end #(set-inspector-wrapper-view-bounds! dom-node))))}
  [{::keys [*open-depth on-scroll-end] :as state}]
  (let [opts {:expanded-depth @*open-depth :path []}]
    [:div.debug-inspector {:on-scroll (debounce 100 on-scroll-end) ;; TODO only use in browsers that do not support :on-scroll-end event
                           }
     [:style (css debug-inspector-view-styles)]
     [:style (css inspector-styles)]
     [:div.logged "Logged"]
     [:div.controls
      [:button {:on-click #(utils/log-flush!)} "flush"]
      [:input {:type          :range
               :min           0
               :max           10
               :default-value @*open-depth
               :on-change     (debounce 16 #(reset! *open-depth (-> % .-target .-value)))}]]
     (inspector (rum/react utils/*log) opts)

     [:div.fn-profiles "Fn profiles"]
     (inspector *fn-profiles opts)

     [:div.max-time-trace "Max time trace"]
     (inspector *max-time-trace opts)]))
