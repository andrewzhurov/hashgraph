(ns hashgraph.app.inspector
  (:require
   [cljs.reader]
   [garden.core :refer [css]]
   #_[garden.def :refer-macros [defkeyframes]]
   [garden.color :as gc]
   [garden.stylesheet :refer [at-keyframes]]
   [hashgraph.utils
    :refer-macros [defn* l]
    :refer [*parent-log-path* *parent-log-path-logging?* *log-path* *log-path-logging?*
            debounce lazy-derived-atom atomic? conjs merge-attr-maps]]
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

(def key->key-mod
  {"Shift"         :shift
   "GroupPrevious" :shift
   "Alt"           :alt
   "Meta"          :meta
   "Control"       :ctrl})

(defn e->?key-mod [keyboard-event] (key->key-mod (.-key keyboard-event)))

(def *key-mods (atom #{}))

(defn key-mod! [k] (swap! *key-mods conj k))
(defn unkey-mod! [k] (swap! *key-mods disj k))

(defn key-mods? [ks] (assert (set? ks)) (set/subset? ks (if rum.core/*reactions*
                                                          (rum/react *key-mods)
                                                          @*key-mods)))
(defn key-mod? [k] (key-mods? #{k}))

(defn register-keydowns-lister
  []
  (.addEventListener js/document "keydown" (fn [e]
                                             (when-let [key-mod (e->?key-mod e)]
                                               (key-mod! key-mod))))
  (.addEventListener js/document "keyup"   (fn [e]
                                             (when-let [key-mod (e->?key-mod e)]
                                               (unkey-mod! key-mod)))))


;; (def peeked-init #{})
;; (def *peeked (atom peeked-init))
;; (defn peek-flush! [] (reset! *peeked peeked-init))
;; (defn peek-set! [value] (reset! *peeked #{value}) value)
;; (defn peek! [value] (peek-set! value) #_(swap! *peeked conj value) value)
;; (defn unpeek! [value] (swap! *peeked disj value) value)


(def inspected-init #{})
(def *inspected (atom inspected-init))
(defn inspected-flush! [] (reset! *inspected inspected-init))
(defn inspect! [value] (swap! *inspected conj value) value)
(defn uninspect! [value] (swap! *inspected disj value) value)
(defn toggle-inspect! [value]
  (swap! *inspected (fn [inspected] (if (contains? inspected value)
                                                             (disj inspected value)
                                                             (conj inspected value))))
  value)


(def init-log {})
(def ^:dynamic *log (atom init-log))
(defn log-flush! [] (reset! *log init-log))

(def default-log-path [:scratch])
(defn ->log-path [?relative-path path]
  (let [full-path (-> []
                      (into ?relative-path)
                      (into path))]
    (if (not-empty full-path)
      full-path
      default-log-path)))

(defn log!-with [with path value]
  (swap! *log (fn [log] (cond-> log
                          ;; add only when *tracing?*
                          (and *parent-log-path-logging?* *parent-log-path*) (update-in (->log-path (into *parent-log-path* *log-path*) path) with value)
                          *log-path-logging?* (update-in (->log-path *log-path* path) with value)))))

(defn log!
  ([value] (log! [] value))
  ([path value]
   (log!-with conjs path value)
   value))

(defn unlog!
  ([value] (unlog! [] value))
  ([path value]
   (log!-with disj path value)
   nil))

(defn log-set!
  ([value] (log-set! [] value))
  ([path value]
   (log!-with (fn [_old new] new) path value)
   value))

(def ->in?
  (fn [coll el]
    (->> coll (some (fn [coll-el]
                      (or (identical? coll-el el)
                          (and (or (vector? coll-el)
                                   (seq? coll-el)
                                   (set? coll-el))
                               (->in? coll-el el))))))))

(def ->inspected?
  (fn [el]
    (let [inspected (rum/react *inspected)]
      (contains? inspected el))))

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
(def *peeked (rum/derived-atom [*active-inspectable *key-mods] ::*peeked
                               (fn [active-inspectable key-mods]
                                 (when (key-mods peek-key)
                                   (:value active-inspectable)))))

(def ->peeked?
  (fn [el]
    (let [peeked (rum/react *peeked)]
      (and (some? el)
           (identical? peeked el)))))

(def ->in-inspected?
  (fn [el]
    (let [inspected (rum/react *inspected)]
      (->in? inspected el))))

(def ->in-peeked?
  (fn [el]
    (let [peeked (rum/react *peeked)]
      (or (identical? el peeked)
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
  [[:.inspector-wrapper
    {:width          "75vw"
     :height         "100vh"
     :bottom         "0px"
     :right          "0px"
     :display        :flex
     :flex-direction :column
     :overflow-y     :scroll
     :overflow-x     :scroll
     :background     "rgba(255,255,255, 0.9)"}]
   [:.inspector {:display :flex
                 :padding-right "10px"
                 :width :fit-content}
    [:.inspect
     icon-style
     [:img {:width "100%"
            :height "100%"}]]
    [:.expand
     icon-style]

    [:table {:border          "1px solid gray"
             :padding "5px"
             ;; :border-collapse :collapse
             }

     #_#_#_
     [:tr {:display :block
           :float   :left}]
     [:th {:display :block}]
     [:td {:display :block}]

     [:tbody {:vertical-align :top}]
     [:tr {:border-bottom "1px solid gray"
           :transition    "0.2s"}

      [:td {:vertical-align :top}]]]

    [:.dt-sym {:padding-left "5px" :padding-right "5px"}]
    [:.collapsed-content-sym {}]

    [:.map {:display     :flex
            :white-space :nowrap
            :align-items :top}
     [:.kv {:display :flex
            :white-space :nowrap
            :align-items :top}
      [:.k {:margin-right "10px"}]
      [:.v {}]]]

    [:.seqable {:display :flex
                :white-space :nowrap
                :align-items :top}
     ]

    [:.kw {:display :flex
           :white-space :nowrap}
     [:.delim {:color "black"}]
     [:.ns {:color "darkcyan" :display :none}]
     [:.name {:color "mediumblue"}]]

    [:.bool
     [:&.true {:color "green"}]
     [:&.false {:color "red"}]]

    [:.atom {:display :flex}]

    [:.trace
     [:.trace-info-wrapper {:background-color "orange"
                            :border           "1px solid white"}
      [:.trace-info {:width       :fit-content
                     :max-width   "100%"
                     :position    :sticky
                     :left        0
                     :overflow-x  :hidden
                     :display     :flex
                     :align-items :top
                     :padding-top "10px"
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
                   :transition "0.4s"}
    [:&.inspected
     {:background-color "rgba(0,0,0,0.05)"}]
    [:&.peeked
     {:background-color "rgba(0,0,0,0.1)"
      #_#_:scale        "1.5"}]]])

#_
(rum/defc inspect < rum/static rum/reactive
  [value]
  (let [being-inspected? (contains? (rum/react *inspected) value)
        being-peeked?    (contains? (rum/react *peeked) value)]
    [:div.inspect
     {:class [(when (or being-inspected? being-peeked?) "active")]
      :on-click #(do (.stopPropagation %)
                     (swap! *inspected (fn [inspected] (if (contains? inspected value)
                                                         (disj inspected value)
                                                         (conj inspected value)))))
      :on-mouse-enter #(do (.stopPropagation %)
                           (swap! *peeked conj value))
      :on-mouse-leave #(do (.stopPropagation %)
                           (swap! *peeked disj value))}
     (if being-inspected?
       [:img {:src "https://www.svgrepo.com/show/508054/eye-off.svg"}]
       [:img {:src "https://www.svgrepo.com/show/508052/eye.svg"}])]))

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
       :on-click       #(when (key-mod? inspect-key)
                          (.preventDefault %)
                          (.stopPropagation %)
                          (toggle-inspect! el))
       :class          ["inspectable"
                        (when (and analysis?
                                   (not (->in-inspected? el))
                                   (not (->in-peeked? el)))
                          "dimm")
                        (when (and analysis? (->inspected? el)) "inspected")
                        (when (and analysis? (->peeked? el)) "peeked")]})))


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
    (cond (and (map? value) (-> value keys first keyword?))
          (-> value keys first namespace keyword))))

(declare inspector)
(rum/defcs table-view < rum/static rum/reactive (rum/local nil ::*horizontal?) (rum/local nil ::*sort-by-key->sort-direction)
  [{::keys [*horizontal? *sort-by-key->sort-direction]} ms opts]
  (let [ms-keys (reduce into #{} (map keys ms))
        horizontal? (if (some? @*horizontal?)
                      @*horizontal?
                      (ms-keys->horizontal? ms-keys))
        flip! #(when (key-mod? flip-key)
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
        sort-by! (fn [k] (swap! *sort-by-key->sort-direction
                                (fn [sort-by-key->sort-direction]
                                  (case sort-by-key->sort-direction
                                    [k :desc]
                                    [k :asc]

                                    [k :asc]
                                    [nil nil]

                                    [k :desc]))))

        ms (cond->> ms
             sort-by-key (sort-by sort-by-key)
             (some-> sort-direction (= :asc)) reverse)]

    (if horizontal?
      [:table {:on-click flip!}
       (->> ordered-ms-keys
            (map-indexed
             (fn [idx ms-key]
               (let [new-opts (update opts :path conj idx)
                     ms-vals (map #(get % ms-key) ms)]
                 [:tr {:key idx}
                  [:td (inspectable ms-vals new-opts)
                   (inspector ms-key new-opts)]
                  (->> ms
                       (map-indexed (fn [idx m]
                                      (let [new-opts (update new-opts :path conj idx)]
                                        (if (not (contains? m ms-key))
                                          [:td {:key idx}]
                                          (let [ms-val (get m ms-key)]
                                            [:td {:key idx}
                                             (inspector ms-val new-opts)]))))))]))))]
      [:table {:on-click flip!}
       [:thead
        [:tr
         (->> ordered-ms-keys
              (map-indexed (fn [idx ms-key]
                             (let [new-opts (update opts :path conj idx)
                                   ms-key-vals (->> ms
                                                    (map ms-key)
                                                    (filter some?))]
                               [:th (merge-attr-maps {:key      idx
                                                      :on-click #(do (.stopPropagation %)
                                                                     (sort-by! ms-key))}
                                                     (inspectable ms-key-vals new-opts))
                                (inspector ms-key new-opts)]))))]]

       [:tbody
        (->> ms
             (map-indexed
              (fn [idx m]
                (let [new-opts (update opts :path conj idx)]
                  [:tr (merge {:key idx} (inspectable m new-opts))
                   (->> ordered-ms-keys
                        (map-indexed
                         (fn [idx ms-key]
                           (let [new-opts (update opts :path conj idx)
                                 m-val (get m ms-key)]
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
  (let [trace-scale @*trace-scale
        trace-scale! #(do (.stopPropagation %)
                          (swap! *trace-scale
                                 (fn [scale]
                                   (cond (key-mod? enhance-key) (inc scale)
                                         (> scale 1)            1
                                         :else                  2)))
                          (swap! *input-key not))
        opts (assoc opts :trace-scale trace-scale)]
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

#_
(rum/defc inspector-view-cr
  < rum/static
  [cr]
  [:div.crs
   (for [cr (take-while some? (iterate :concluded-round/prev-concluded-round cr))]
     (inspector-view-map cr))])


(rum/defcs inspector-view-seqable < rum/static rum/reactive (rum/local true ::*table-view?)
  [{::keys [*table-view?]} s {:keys [path expanded-depth] :as opts}]
  (let [[bracket-start bracket-end]
         (cond (seq? s)    ["(" ")"]
               (vector? s) ["[" "]"]
               (set? s)    ["#{" "}"])

        resolved-s (->> s (mapv (fn [s-el] (if (atomic? s-el)
                                             (rum/react s-el)  ;; losing @ view sign here
                                             s-el))))]
    [:div.seqable (merge-attr-maps (inspectable s opts)
                                   {:on-click #(when (key-mod? :alt)
                                                 (.stopPropagation %)
                                                 (swap! *table-view? not))})
     [:div.dt-sym bracket-start]

     (or (and (>= (count path) expanded-depth)
              (or (and (not-empty resolved-s)
                       [:div.collapsed-content-sym "..."])
                  ""))
         (and (->> resolved-s (every? map?))
              @*table-view?
              ;; some keys intersect
              #_(->> resolved-s (map (comp set keys)) (apply set/intersection) (not-empty))
              (table-view resolved-s opts))
         (->> resolved-s (map-indexed (fn [idx s-el]
                                        ;; conjing path leads to toggle between unfolded table and folded elements, if key intersection enabled ^
                                        (let [opts (update opts :path conj idx)]
                                          (rum/with-key
                                            (inspector s-el opts)
                                            idx))))))

     [:div.dt-sym bracket-end]]))

(rum/defc inspector-view-keyword < rum/static
  [kw]
  [:div.kw
   [:div.delim ":"]
   (when-let [namespace (namespace kw)]
     [:<>
      [:div.ns namespace]
      [:div.delim "/"]])
   [:div.name (name kw)]])

(rum/defc inspector-view-boolean < rum/static
  [bool]
  [:div.bool {:class (str bool)} (str bool)])

(rum/defc inspector-view-atom < rum/static rum/reactive
  [*a {:keys [expanded-depth path] :as opts}]
  [:div.atom [:div.dt-sym "@"] (if (>= (count path) expanded-depth)
                                 [:div.collapsed-content-sym "..."]
                                 (inspector (rum/react *a) opts))])

(rum/defc inspector-view-number < rum/static
  [num opts]
  [:div.number (if-not (int? num)
                 (.toFixed num 1)
                 num)])

(def name->compact-name
  (memoize
   (fn [string-name]
     (-> string-name
         (clojure.string/split #"\-\>")
         (->> (map (fn [part->]
                     (-> part->
                         (clojure.string/split #"\+")
                         (->> (map (fn [part-plus]
                                     (-> part-plus
                                         (clojure.string/split #"\-")
                                         (->> (map (fn [part-dash]
                                                     (if (= (first part-dash) \?)
                                                       (apply str (take 2 part-dash))
                                                       (first part-dash))))
                                              (apply str)))))
                              (interpose "+")
                              (apply str)))))
              (interpose "->")
              (apply str))))))

(assert (= (name->compact-name "concluded-round->event-to-receive->learned-member->learned-event")
           "cr->etr->lm->le"))

(assert (= (name->compact-name "concluded-round+event->event-to-receive->learned-member->learned-event")
           "cr+e->etr->lm->le"))

(assert (= (name->compact-name "?concluded-round->stake-map")
           "?cr->sm"))


(rum/defc inspector-view-symbol < rum/static
  [sym opts]
  [:div.symbol {:title (str sym)} (name->compact-name (str sym))])

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
  (when-let [[view-width view-height _signal] (rum/react *inspector-wrapper-view-bounds)]
    (let [bounding-rect (.getBoundingClientRect dom-node) ;; returns relative to scrolled viewport coords https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect
          rel-x     (.-x bounding-rect)
          rel-y     (.-y bounding-rect)
          rel-x-end (+ rel-x (.-width bounding-rect))
          rel-y-end (+ rel-y (.-height bounding-rect))]
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

(rum/defcs inspector < rum/static rum/reactive (rum/local nil ::*local-expanded-depth) (rum/local nil ::*dom-node)
  {:did-mount (fn [state] (reset! (::*dom-node state) (rum/dom-node state)) state)}
  [{::keys [*local-expanded-depth *dom-node] :as state} value {:keys [expanded-depth path in-view?] :as opts}]
  (let [current-expanded-depth (or @*local-expanded-depth
                                   expanded-depth)
        in-view? (cond
                   (= in-view? false) false
                   (nil? @*dom-node)  false
                   :else (->in-view? @*dom-node))
        new-opts (-> opts
                     (assoc :expanded-depth current-expanded-depth
                            :in-view? in-view?))]
    [:div.inspector (if (or (seqable? value)
                            (atomic? value))
                      {:on-click #(do (.stopPropagation %)
                                      (swap! *local-expanded-depth (fn [local-expanded-depth]
                                                                     (cond (key-mod? enhance-key)
                                                                           (inc current-expanded-depth)

                                                                           (nil? local-expanded-depth)
                                                                           (inc (count path))

                                                                           :else
                                                                           nil))))}
                      {})
     (cond #_#_ (and (map? value) (:concluded-round/r value))
           (inspector-view-cr value)

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

           :else (inspector-view-default value new-opts))]))

(def *traces (rum/cursor-in *log [:traces]))



(defn* ^{:memoizing {:recur-by   (fn [[_ & rest-traces]] [rest-traces]) ;; warmup cache on cold start to escape max call stack exceeded due to deep uncached recursion
                     :recur-stop empty?
                     :only-last? true}
         :tracing {:enabled? false}}
  traces->fn-profiles*
  [[{:trace/keys [fn-name time] :as trace} & rest-traces]]
  (if (nil? trace)
    {}
    (let [prev-fn-profiles (traces->fn-profiles* rest-traces)]
      (-> prev-fn-profiles
          (update fn-name (fn [{:fn-profile/keys [_fn-name times total-time max-time min-time medium-time medium-time-delta-total medium-time-delta-times] :as fn-profile}]
                            (let [new-total-time                (+ total-time time)
                                  new-times                     (inc times)
                                  new-medium-time               (/ new-total-time new-times)
                                  ?new-medium-time-delta        (when medium-time
                                                                  (- medium-time new-medium-time))
                                  new-medium-time-delta-total   (+ medium-time-delta-total ?new-medium-time-delta)
                                  new-medium-delta-times        (cond-> medium-time-delta-times
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
                                      (> time max-time)             (-> (assoc  :fn-profile/max-time time)
                                                                        (assoc  :fn-profile/max-time-trace trace))
                                      ?new-medium-time-delta (assoc :fn-profile/medium-time-delta ?new-medium-time-delta)
                                      ?new-medium-time-deltas-medium (assoc :fn-profile/medium-time-deltas-medium ?new-medium-time-deltas-medium))))))))))

(def *fn-profiles (lazy-derived-atom [*traces] ::*fn-profiles (fn [traces]
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

(rum/defcs inspector-wrapper < rum/static rum/reactive (rum/local 0 ::*open-depth)
  {:did-mount (fn [state]
                (let [dom-node (rum/dom-node state)]
                  (set-inspector-wrapper-view-bounds! dom-node)
                  (assoc state ::on-scroll-end #(set-inspector-wrapper-view-bounds! dom-node))))}
  [{::keys [*open-depth on-scroll-end] :as state}]
  (let [opts {:expanded-depth @*open-depth :path []}]
    [:div.inspector-wrapper {:on-scroll (debounce 100 on-scroll-end) ;; TODO only use in browsers that do not support :on-scroll-end event
                             }
     [:div.logged "Logged"]
     [:div.controls
      [:button {:on-click #(log-flush!)} "flush"]
      [:input {:type          :range
               :min           0
               :max           10
               :default-value @*open-depth
               :on-change     (debounce 16 #(reset! *open-depth (-> % .-target .-value)))}]]

     (inspector (rum/react *log) opts)

     [:div.inspected "Inspected"]
     [:div [:button {:on-click inspected-flush!} "flush"]]
     (inspector (rum/react *inspected) opts)

     [:div.fn-profiles "Fn profiles"]
     (inspector *fn-profiles opts)

     [:div.max-time-trace "Max time trace"]
     (inspector *max-time-trace opts)

     #_#_
     [:div.peeked "Peeked"]
     (inspector (rum/react *peeked) opts)]))


(rum/defc view < rum/static rum/reactive
  []
  [:<>
   [:style (css inspector-styles)]
   (inspector-wrapper)])
