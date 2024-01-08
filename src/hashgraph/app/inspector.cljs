(ns hashgraph.app.inspector
  (:require-macros hashgraph.app.inspector)
  (:require
   [cljs.reader]
   [garden.core :refer [css]]
   #_[garden.def :refer-macros [defkeyframes]]
   [garden.color :as gc]
   [garden.stylesheet :refer [at-keyframes]]
   [hashgraph.utils2 :refer-macros [td time3]]

   [rum.core :as rum]
   [taoensso.timbre :refer-macros [spy info]]
   [taoensso.tufte :as tufte :refer [defnp p profiled profile]]

   [clojure.set]
   [cljs.math :refer [floor ceil]]
   [clojure.set :as set]))

(defn l [value] (js/console.log value) value)
(defn merge-attr-maps [& attr-maps]
  (apply merge-with (fn [& vals]
                      (cond
                        (every? fn? vals)
                        (fn [& args] (doall (map #(apply % args) vals)))

                        (every? map? vals)
                        (apply merge vals)

                        (every? #(or (seq? %) (vector? %) (set? %)) vals)
                        (reduce into [] vals)

                        :else
                        (do (js/console.warn "Don't know how to merge, taking the last of vals: " (pr-str vals))
                            (last vals))))
         attr-maps))

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


(def *inspectable-under-mouse (atom nil))
(declare peek-key)
(def *peeked (rum/derived-atom [*inspectable-under-mouse *key-mods] ::*peeked
               (fn [inspectable-under-mouse key-mods]
                 (and (key-mods peek-key)
                      inspectable-under-mouse))))

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
(def *log (atom init-log))
(defn log-flush! [] (reset! *log init-log))
(def ^:private ^:dynamic *log-path* nil)
(def default-log-path [:scratch])
(defn ->log-path [path]
  (cond *log-path*    (into *log-path* path)
        (empty? path) default-log-path
        :else         path))
(defn log!
  ([value] (log! [] value))
  ([path value]
   (swap! *log update-in (->log-path path) conj value)
   value))
(defn unlog!
  ([value] (unlog! [] value))
  ([path value]
   (swap! *log update-in (->log-path path) (fn [path-coll] (->> path-coll (remove (fn [path-coll-el] (= path-coll-el value))))))
   value))
(defn log-set!
  ([value] (log-set! [] value))
  ([path value]
   (swap! *log assoc-in (->log-path path) value)
   value))

(def ->in?
  (memoize
   (fn [coll el]
     (->> coll (some (fn [coll-el]
                       (or (= coll-el el)
                           (and (or (vector? coll-el)
                                    (seq? coll-el)
                                    (set? coll-el))
                                (->in? coll-el el)))))))))

(def ->inspected?
  (fn [el]
    (let [inspected (rum/react *inspected)]
      (contains? inspected el))))

(def ->peeked?
  (fn [el]
    (let [peeked (rum/react *peeked)]
      (and (some? el)
           (= peeked el)))))

(def ->in-inspected?
  (fn [el]
    (let [inspected (rum/react *inspected)]
      (->in? inspected el))))

(def ->in-peeked?
  (fn [el]
    (let [peeked (rum/react *peeked)]
      (or (= el peeked)
          (and (or (vector? peeked)
                   (set? peeked)
                   (seq? peeked))
               (->in? peeked el))))))

(def ->analysis?
  (fn []
    (or (not (empty? (rum/react *inspected)))
        (not (empty? (rum/react *peeked))))))

#_
[:div.debug {:style {:width       "50vw"
                     :height      "90vh"
                     :white-space "pre-wrap"}}
 (when hovered-evt
   (with-out-str (cljs.pprint/pprint hovered-evt)))]

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
                 :padding-right "10px"}
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

    ]

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

(def peek-key :shift)
(def inspect-key :shift)
(defn inspectable [el]
  {#_:on-mouse-over
   :on-mouse-move #(do (.stopPropagation %)
                       (reset! *inspectable-under-mouse el))
   :on-mouse-leave #(do (reset! *inspectable-under-mouse nil))
   :on-click       #(when (key-mod? inspect-key)
                      (.preventDefault %)
                      (.stopPropagation %)
                      (toggle-inspect! el))
   :class ["inspectable"
           (when (and (->analysis?)
                      (not (->in-inspected? el))
                      (not (->in-peeked? el)))
             "dimm")
           (when (->inspected? el) "inspected")
           (when (->peeked? el) "peeked")]})

(declare inspector)
(rum/defcs table-view < rum/static rum/reactive (rum/local nil ::*horizontal?)
  [{::keys [*horizontal?]} ms opts]
  (let [horizontal? (if (some? @*horizontal?)
                      @*horizontal?
                      (-> ms count (= 1)))
        ms-keys (reduce into #{} (map keys ms))
        flip! #(when (key-mod? :ctrl)
                 (.stopPropagation %)
                 (reset! *horizontal? (not horizontal?)))]
    (if horizontal?
      [:table {:on-click flip!}
       (->> ms-keys
            (map-indexed
             (fn [idx ms-key]
               [:tr {:key idx}
                (let [ms-vals (map #(get % ms-key) ms)]
                  [:<>
                   [:td (inspectable ms-vals)
                    (inspector ms-key (update opts :path conj ms-key))]
                   (->> ms (map-indexed (fn [idx m]
                                          (if (not (contains? m ms-key))
                                            [:td]
                                            (let [ms-val (get m ms-key)]
                                              [:td (merge {:key idx} (inspectable ms-val))
                                               (inspector ms-val (update opts :path conj idx))])))))])])))]
      [:table {:on-click flip!}
       [:thead
        [:tr
         (for [ms-key ms-keys]
           (let [ms-key-vals (->> ms
                                  (map ms-key)
                                  (filter some?))]
             [:th (merge {:key (str ms-key)} (inspectable ms-key-vals))
              (inspector ms-key (update opts :path conj ms-key))]))]]

       [:tbody
        (->> ms (map-indexed
                 (fn [idx m]
                   [:tr (merge {:key idx} (inspectable m))
                    (->> ms-keys
                         (map-indexed
                          (fn [idx ms-key]
                            (let [m-val (get m ms-key)]
                              [:td {:key idx}
                               (when (contains? m ms-key)
                                 (inspector m-val (update opts :path conj ms-key)))]))))])))]])))

(rum/defcs inspector-view-map < rum/static rum/reactive (rum/local nil ::*expanded?)
  [{::keys [*expanded?]} m {:keys [path expanded-depth] :as opts}]
  (let [current-expanded-depth (if (some? @*expanded?)
                                 (if @*expanded?
                                   (-> path count (+ 2))
                                   (-> path count))
                                 expanded-depth)]
    [:div {:on-click #(do (.stopPropagation %)
                          (swap! *expanded? not))}
     [:div.map (inspectable m)
      "{"
      (or (and (>= (count path) current-expanded-depth)
               (or (and (not-empty m)
                        "...")
                   ""))
          (table-view [m] (assoc opts :expanded-depth current-expanded-depth)))
      "}"]]))

#_
(rum/defc inspector-view-cr
  < rum/static
  [cr]
  [:div.crs
   (for [cr (take-while some? (iterate :concluded-round/prev-concluded-round cr))]
     (inspector-view-map cr))])



(rum/defcs inspector-view-seqable < rum/static rum/reactive (rum/local nil ::*expanded?)
  [{::keys [*expanded?]} s {:keys [path expanded-depth] :as opts}]
  (let [[bracket-start bracket-end]
         (cond (seq? s)    ["(" ")"]
               (vector? s) ["[" "]"]
               (set? s)    ["#{" "}"])
        current-expanded-depth (if (some? @*expanded?)
                                 (if @*expanded?
                                   (-> path count (+ 2))
                                   (-> path count))
                                 expanded-depth)]
    [:div.seqable (merge-attr-maps (inspectable s)
                                   {:on-click #(do (.stopPropagation %)
                                                   (swap! *expanded? not))})
     bracket-start
     (or (and (>= (count path) current-expanded-depth)
              (or (and (not-empty s)
                       "...")
                  ""))
         (and (every? map? s)
              (table-view (vec s) (assoc opts :expanded-depth current-expanded-depth)))
         (->> s (map-indexed (fn [idx s-el]
                               (inspector s-el (-> opts
                                                   (update :path conj idx)
                                                   (assoc :expanded-depth current-expanded-depth)))))))
     bracket-end]))

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

(rum/defc inspector-view-default < rum/static
  [any]
  (pr-str any))

(rum/defc inspector < rum/static rum/reactive
  [value opts]
  [:div.inspector
   (cond #_#_ (and (map? value) (:concluded-round/r value))
         (inspector-view-cr value)

         (keyword? value)
         (inspector-view-keyword value opts)

         (map? value)
         (inspector-view-map value opts)

         (or (vector? value) (seq? value) (set? value))
         (inspector-view-seqable value opts)

         (boolean? value)
         (inspector-view-boolean value opts)

         :else (inspector-view-default value opts))])

(rum/defcs inspector-wrapper < rum/static rum/reactive (rum/local 2 ::*open-depth)
  [{::keys [*open-depth]}]
  (let [opts {:expanded-depth @*open-depth :path []}]
    [:div.inspector-wrapper
     [:div.logged "Logged"]
     [:div.controls
      [:button {:on-click #(log-flush!)} "flush"]
      [:input {:type      :range
               :min       0
               :max       10
               :value     @*open-depth
               :on-change #(reset! *open-depth (-> % .-target .-value))}]]
     #_(let [?a-top-witness        (hg/creator-hg-map->main-top-witness (rum/react hga-events/*creator-hg-map))
             ?last-concluded-round (some-> ?a-top-witness hg/->?concluded-round)]
         (when ?last-concluded-round
           (for [cr (take-while some? (iterate :concluded-round/prev-concluded-round ?last-concluded-round))]
             (inspector cr 1 1))))


     (inspector (rum/react *log) opts)

     [:div.inspected "Inspected"]
     [:div [:button {:on-click inspected-flush!} "flush"]]
     (inspector (rum/react *inspected) opts)

     #_#_
     [:div.peeked "Peeked"]
     (inspector (rum/react *peeked) opts)]))

(rum/defc view < rum/static rum/reactive
  []
  [:<>
   [:style (css inspector-styles)]
   (inspector-wrapper)])
