(ns hashgraph.utils.core
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.walk]
            [taoensso.tufte :as tufte]
            [rum.core :as rum]
            #?(:cljs [cljs.analyzer :as ana])))
#?(:clj (alias 'ana 'cljs.analyzer))

(def ^:dynamic *parent-log-path* nil)
(def ^:dynamic *parent-log-path-logging?* true)
(def ^:dynamic *log-path* nil)
(def ^:dynamic *log-path-logging?* true)

#?(:cljs (def lookup-sentinel (js-obj))
   :clj  (def lookup-sentinel nil))
#?(:cljs (def ^:private lookup-empty-args [(js-obj)]))
(def ^:dynamic *mem-atom* nil)
(def ^:dynamic *mem* nil)
(def ^:dynamic *from-mem* nil)

(def ^:dynamic default-tracing-enabled? false)
#?(:dev (set! default-tracing-enabled? false))

;; #?(:cljs (goog-define tracing false))
;; #?(:cljs (goog-define traces-store (atom (hash-map))))

(def ^:dynamic *time-start* nil)
(def ^:dynamic *->time* nil)
(def ^:dynamic *time* nil)

(def ^:dynamic *trace-atom* nil)
(def ^:dynamic *parent-trace-atom* nil)

(def init-log {})
(def ^:dynamic *log (atom init-log))
(defn log-flush! [] (reset! *log init-log))
(def *traces (rum/cursor-in *log [:traces]))


(def default-log-path [:scratch])
(defn ->log-path [?relative-path path]
  (let [full-path (-> []
                      (into ?relative-path)
                      (into path))]
    (if (not-empty full-path)
      full-path
      default-log-path)))



(def *id->logger (atom {}))
(defn add-logger! [id logger] (swap! *id->logger assoc id logger))
#?(:clj  (add-logger! :console (fn [path _with value] (println path value)))
   :cljs (do (add-logger! :*log (fn [path with value] (swap! *log update-in path with value)))
             (add-logger! :*console (fn [path with value] (js/console.log path value)))))

(defn log!* [path with value]
  (doseq [[_id logger] @*id->logger]
    (logger path with value)))

(defn log!-with [path with value]
  (when (and *parent-log-path-logging?* *parent-log-path*) (log!* (->log-path (into *parent-log-path* *log-path*) path) with value))
  (when *log-path-logging?*                                (log!* (->log-path                         *log-path*  path) with value)))

(defn ->path [path-or-key] (if (sequential? path-or-key) path-or-key (vector path-or-key)))
(defn log!
  ([value] (log! [] value))
  ([path-or-key value]
   (log!-with (->path path-or-key) conj value)
   value))

(defn unlog!
  ([value] (unlog! [] value))
  ([path-or-key value]
   (log!-with (->path path-or-key) disj value)
   nil))

(defn log-set!
  ([value] (log-set! [] value))
  ([path-or-key value]
   (log!-with (->path path-or-key) (fn [_old new] new) value)
   value))


(defmacro logged [& exprs]
  `(let [*log-acc# (atom (hash-map))
         result#   (binding [*log              *log-acc#
                             *parent-log-path-logging?* false
                             *log-path*       []]
                     ~@exprs)]
     [@*log-acc# result#]))

(defmacro l [expr]
  `(let [res# ~expr]
     (log! [(quote ~expr)] res#)
     res#))

;; is a letl macro and not a l-binds macro to make litner happy

#_(defmacro log-relative [path-or-key & exprs]
  `(binding [*parent-log-path* (let [prev    (or *parent-log-path*
                                                 [])
                                     current (cond-> prev
                                               (some? *log-path*) (into *log-path*))])
             *log-path* (->path ~path-or-key)]
     ~@exprs))

(defmacro log-under [path-or-key & exprs]
  `(binding [*log-path* (->path ~path-or-key)]
     ~@exprs))

(defmacro letl
  "Logs bindings from sym to value."
  [binds & exprs]
  `(let [[log# result#]
         (logged
          (let ~(->> binds
                     (partition 2)
                     (mapcat (fn [[to expr]] [to `(log-set! (quote ~to) ~expr)]))
                     vec)
            ~@exprs))]
     (log! ['letl (quote ~binds)] log#)
     result#))

(defmacro letp
  "Profiles bindings let bindings."
  [binds & exprs]
  `(let ~(->> binds
              (partition 2)
              (mapcat (fn [bind]
                        (if (-> bind first symbol? not)
                          bind
                          (let [[?sym expr] bind]
                            [?sym `(tufte/p ~(keyword ?sym) ~expr)]))))
              vec)
     ~@exprs))


(defmacro cl
  "Conditional log. Logs expr and result when (pred value) or pred holds true."
  [pred-or-bool expr]
  `(do
     (when (if (fn? ~pred-or-bool)
             (~pred-or-bool value) ;; broken
             ~pred-or-bool)
       (l ~expr))
     ~expr))

(defn random-nths
  "Returns randomly picked n elements from a coll."
  [n coll]
  (if (or (zero? n) (empty? coll))
    #{}
    (let [item (rand-nth (vec coll))]
      (conj (random-nths (dec n) (disj (set coll) item))
            item))))

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

(deftest name->compact-name-test
  (is (= (name->compact-name "concluded-round->event-to-receive->learned-member->learned-event")
         "cr->etr->lm->le"))

  (is (= (name->compact-name "concluded-round+event->event-to-receive->learned-member->learned-event")
         "cr+e->etr->lm->le"))

  (is (= (name->compact-name "?concluded-round->stake-map")
         "?cr->sm")))

(defn svs? [el] (or (seq? el) (vector? el) (set? el)))
(defn flattenable? [el] (svs? el))
(defn hash= [el1 el2] (= (hash el1) (hash el2)))

(defn flatten-all [maybe-flattenbale]
  (if-not (flattenable? maybe-flattenbale)
    maybe-flattenbale
    (let [flattenable maybe-flattenbale]
      (->> flattenable
           (map flatten-all)
           (reduce (fn [flattened-acc maybe-flattenbale-el]
                     (if-not (flattenable? maybe-flattenbale-el)
                       (conj flattened-acc maybe-flattenbale-el)
                       (into flattened-acc (flatten-all maybe-flattenbale-el))))
                   [])))))

(deftest flatten-all-test
  (is (-> (flatten-all ['(1 2) #{2 3} [3 #{3 4} [4] 4] 4])
          (sort)
          (= '(1 2 2 3 3 3 4 4 4 4)))))

(defn flat [maybe-flattenable]
  (if (flattenable? maybe-flattenable)
    (flatten-all maybe-flattenable)
    [maybe-flattenable]))

#?(:cljs
   (defn distinct-with [with coll]
     (reduce (fn [coll-acc el]
               (let [idx (-indexOf coll-acc el)]
                 (if (not= -1 idx)
                   (update coll-acc idx with el)
                   (conj coll-acc el))))
             []
             coll)))

#?(:cljs
   (deftest distinct-with-test
     (let [out (distinct-with (fn [el1 el2] (vary-meta el1 merge (meta el2))) [^:l {1 1}     {1 1}
                                                                                   {2 2} ^:r {2 2}
                                                                               ^:l {3 3} ^:r {3 3}])]
       (is (= out [{1 1} {2 2} {3 3}]))
       (is (= (map meta out) '({:l true} {:r true} {:l true :r true}))))))


(defn partition-at-with [with coll]
  (->> coll
       (reduce (fn [[last-part & rest-parts :as parts] el]
                 (if (with el)
                   (conj parts [el])
                   (conj rest-parts (conj last-part el))))
               '([]))
       reverse
       vec))

(deftest partition-at-with-test
  (is (= (partition-at-with keyword? [1 2 :k 3 4 :k 5 6])
         [[1 2] [:k 3 4] [:k 5 6]])))

(def range-map
  (memoize
   (fn
     ([end] (range-map 0 end))
     ([start end]
      (let [range-n (range start end)]
        (apply hash-map (interleave range-n range-n)))))))

(def nested-range-map
  (fn
    ([end] (nested-range-map 0 end))
    ([start end]
     (let [rm (range-map start end)]
       (apply hash-map (interleave (range start end) (repeat (- end start) rm)))))))


#?(:cljs (defn ->neighbours? [coll els]
           (when (and (or (vector? coll) (seq? coll) (set? coll))
                      (or (vector? els) (seq? els) (set? els)))
             (or (and (vector? coll)
                      (let [idx1 (-indexOf coll (first els))
                            idx2 (-indexOf coll (second els))]
                        (when (and (not= idx1 -1)
                                   (not= idx2 -1))
                          (= 1 (- (max idx1 idx2) (min idx1 idx2))))))
                 (->> coll (some #(when (sequential? %) (->neighbours? % els))))))))

#?(:cljs (deftest ->neighbours?-test
           (is (true? (->neighbours? [{} [1 2 3 4]] [2 3])))))


(defn merge! [m1 m2]
  (reduce (fn [acc [k v]] (assoc! acc k v)) (transient m1) m2))

#?(:cljs
   (defn safe-assoc!
     "When applied to a transient map, adds mapping of key(s) to
  val(s). When applied to a transient vector, sets the val at index.
  When applied to a nil, returns a transient hash-map with key(s) to val(s).
  Note - index must be <= (count vector). Returns coll."
     ([tcoll key val]
      (cljs.core/-assoc! (or tcoll (transient (hash-map))) key val))
     ([tcoll key val & kvs]
      (let [ntcoll (cljs.core/-assoc! (or tcoll (transient (hash-map))) key val)]
        (if kvs
          (recur ntcoll (first kvs) (second kvs) (nnext kvs))
          ntcoll)))))

#?(:cljs
   (do (defn safe-assoc-in!
         [m [k & ks] v]
         (if ks
           (safe-assoc! m k (safe-assoc-in! (get m k) ks v))
           (safe-assoc! m k v)))
       (assert (-> (safe-assoc-in! nil [:key1 :key2 :key3] :val)
                   :key1
                   :key2
                   :key3
                   (= :val)))))

#?(:cljs
   (do
     (defn safe-update!
       ([m k f]
        (safe-assoc! m k (f (get m k))))
       ([m k f x]
        (safe-assoc! m k (f (get m k) x)))
       ([m k f x y]
        (safe-assoc! m k (f (get m k) x y)))
       ([m k f x y z]
        (safe-assoc! m k (f (get m k) x y z)))
       ([m k f x y z & more]
        (safe-assoc! m k (apply f (get m k) x y z more))))

     (assert
      (-> (safe-update! nil :key1 safe-update! :key2 safe-assoc! :key3 :val)
          :key1
          :key2
          :key3
          (= :val)))))

#?(:cljs
   (do
     (defn safe-update-in!
       [m [k & ks] f & f-args]
       (if ks
         (safe-assoc! m k (apply safe-update-in! (get m k) ks f f-args))
         (safe-assoc! m k (apply f (get m k) f-args))))

     (assert
      (-> (safe-update-in! nil [:key1 :key2] safe-assoc! :key3 :val)
          :key1
          :key2
          :key3
          (= :val)))))

#?(:cljs
   (defn merge-with!
     "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
     [f & maps]
     (when (some identity maps)
       (let [merge-entry (fn [m-tr e]
                           (let [k (key e) v (val e)]
                             (if (contains? m-tr k)
                               (safe-assoc! m-tr k (f (get m-tr k) v))
                               (safe-assoc! m-tr k v))))
             merge2      (fn [m1-tr m2]
                      (reduce merge-entry m1-tr (seq m2)))]
         (reduce merge2 maps)))))

(defn merge-attr-maps
  "Merge that preserve functions under map keys as (fn outer [] (inner1) (inner2))."
  [& attr-maps]
  (apply merge-with (fn [& vals]
                      (cond
                        (every? fn? vals)
                        (fn [& args] (doall (map #(apply % args) vals)))

                        (every? map? vals)
                        (apply merge vals)

                        (every? #(or (seq? %) (vector? %) (set? %)) vals)
                        (reduce into [] vals)

                        :else
                        (do (log! [:warnings] {:message "Don't know how to merge, taking the last val."
                                               :vals    vals})
                            (last vals))))
         attr-maps))

#?(:clj
   (defn k->val-fn? [k] (clojure.string/starts-with? (name k) "on-")))

#?(:clj (require '[hashgraph.app.inspector :refer-macros [inspectable*]]))
(defmacro merge-attr-maps* [attr-map1 attr-map2 & rest-attr-maps]
  ;; works only on plain {} as args
  (let [merged1
        (reduce
         (fn [attr-map-acc [k v]]
           (if-not (contains? attr-map-acc k)
             (assoc attr-map-acc k v)
             (update attr-map-acc k (fn [acc-v]
                                      (cond (k->val-fn? k)
                                            `(fn [] ~acc-v ~v)
                                            (and (map? acc-v)
                                                 (map? v))
                                            (merge-attr-maps* acc-v v)
                                            (and (sequential? acc-v)
                                                 (sequential? v))
                                            (into acc-v v)
                                            :else
                                            (do (log! [:warnings] {:message "Don't know how to merge, taking the last val."
                                                                   :vals    [acc-v v]})
                                                v))))))
         attr-map1
         attr-map2)]
    merged1
    #_
    (if (empty? rest-attr-maps)
      merged1
      (apply merge-attr-maps* merged1 rest-attr-maps))))

(defn conjs [?coll el]
  (if (and (some? ?coll)
           (set? ?coll))
    (conj ?coll el)
    (conj (set ?coll) el)))

(defn rest-n [n coll]
  (if (zero? n)
    coll
    (recur (dec n) (rest coll))))

(defn fn->sorted-set-safe-comparator
  "Ensures set semantics of sorted-set are preserved."
  [f]
  (fn [x y]
    (let [r (f x y)]
      (if (number? r)
        (if (not= 0 r) ;; 0 will lead to not conjing a unique el, fallback to poor sort
          r
          -1)
        (if r
          -1
          (if (f y x) 1
              -1 ;; default to a poor sort
              ))))))

#?(:cljs
   (do
     (defn sorted-set-by*
       ([keyfn] (sorted-set-by* keyfn cljs.core/compare))
       ([keyfn comp] (let [safe-comp             (fn->sorted-set-safe-comparator comp)
                           safe-comp-after-keyfn (fn [x y] (if (= x y)
                                                             0 ;; do not replace existing el
                                                             (safe-comp (keyfn x) (keyfn y))))]
                       (cljs.core/sorted-set-by safe-comp-after-keyfn))))
     (let [ss (-> (sorted-set-by* meta)
                  (conj (with-meta {1 1} 1))
                  (conj (with-meta {2 2} 2)))]
       ;; sorts by cljs.core/comparator by default
       (assert (= ss #{{1 1} {2 2}})))

     (let [ss (-> (sorted-set-by* meta)
                  (conj (with-meta {1 1} 1))
                  (conj (with-meta {1 1} 2)))]
       ;; does not add duplicates
       (assert (= ss #{{1 1}}))
       (assert (= (-> ss first meta) 1)))

     (let [ss (-> (sorted-set-by* meta)
                  (conj (with-meta {1 1} 1))
                  (conj (with-meta {2 2} 1)))]
       ;; adds unique vals with the same comparation result
       (assert (= ss #{{1 1} {2 2}}))
       (assert (= (map meta ss) '(1 1))))

     (let [ss (-> (sorted-set-by* meta >=)
                  (conj (with-meta {2 2} 2))
                  (conj (with-meta {1 1} 1)))]
       ;; supports custom comparator
       (assert (= (vec ss) [{2 2} {1 1}])))))


#?(:cljs
   (defn debounce [ms f]
     (let [on-debounced (volatile! nil)]
       (fn [& args]
         (some-> on-debounced deref (js/clearTimeout))
         (vreset! on-debounced (js/setTimeout #(apply f args) ms))))))

#?(:cljs
   (defn silence
     "Subsequent calls to f within the timeframe from prev call to prev call + ms won't be executed."
     [ms f]
     (let [*prev-t (volatile! nil)]
       (fn [& args]
         (let [t (cljs.core/system-time)]
           (when (> t (+ @*prev-t ms))
             (vreset! *prev-t t)
             (apply f args)))))))

#?(:cljs
   (defn async-sequential
     "Async calls to this function will complete in the order they were made, one after the other."
     [f]
     (let [last-f-promise* (volatile! (js/Promise.resolve true))]
       (fn [& args]
         (let [last-f-promise @last-f-promise*]
           (vreset! last-f-promise* (new js/Promise (fn [resolve reject]
                                                      (-> last-f-promise
                                                          (.then (fn [] (apply f args) (resolve true))))))))))))

#?(:cljs
   (deftype LazyDerivedAtom [^:mutable state meta validator watches this-key refs deriving-f]
     Object
     (equiv [this other]
       (-equiv this other))

     IAtom

     IEquiv
     (-equiv [o other] (identical? o other))

     IDeref
     (-deref [o]
       (when (empty? watches)
         (set! state (apply deriving-f (map deref refs))))
       state)

     IMeta
     (-meta [_] meta)

     IWatchable
     (-notify-watches [this oldval newval]
       #_(l [:on-notify-watces this-key this oldval newval])
       (doseq [[key f] watches]
         (f key this oldval newval)))
     (-add-watch [this key f]
       #_(l [:on-add-watch this-key this key f])
       (when (empty? watches)
         (let [recalc! (async-sequential #(let [oldval state
                                                newval (apply deriving-f (map deref refs))]
                                            (set! state newval)
                                            (-notify-watches this oldval newval)))]
           (doseq [ref refs]
             (add-watch ref this-key recalc!))
           (set! (.-watches this) (assoc (.-watches this) key f))
           ;; recalc?
           ))
       this)
     (-remove-watch [this key]
       #_(l [:on-remove-watch this-key this key])
       (let [new-watches (dissoc watches key)]
         (when (empty? new-watches)
           (doseq [ref refs]
             (remove-watch ref this-key)))
         (set! (.-watches this) new-watches))
       this)

     IHash
     (-hash [this] (goog/getUid this))))

#?(:cljs
   (defn lazy-derived-atom [refs key f] (LazyDerivedAtom. nil nil nil nil key refs f)))

#?(:cljs
   (defn atomic? [obj] (or (identical? (type obj) cljs.core/Atom)
                           (identical? (type obj) LazyDerivedAtom))))

(def color-rgba-str
  (memoize
   (fn ([rgb-vec] (color-rgba-str rgb-vec 1))
       ([[red green blue] alpha] (str "rgba(" red "," green "," blue "," alpha  ")")))))


(defmacro timed
  [expr]
  `(cljs.core/let [start# (cljs.core/system-time)
                   ret#   ~expr
                   end#   (cljs.core/system-time)
                   took#  (cljs.core/- (cljs.core/system-time) start#)]
     [took# ret#]))

(defmacro timing
  "Wraps expr with binding for *time-start*, *->time* to fn that, when called, returns time passed as of call to the expression"
  [expr]
  `(binding [*time-start* (cljs.core/system-time)
             *->time* (fn [] (- (cljs.core/system-time) *time-start*))]
     ~expr))

;; def fn memoized
(defmacro defnm [fn-name & forms]
  (let [[fn-doc fn-inputs & fn-bodies] (if (string? (first forms))
                                         forms
                                         (cons "" forms))]
    (assert (string? fn-doc))
    (assert (every? symbol? fn-inputs) "defnml only supports symbol inputs atm")
    `(def ~fn-name (memoize (fn ~fn-inputs ~@fn-bodies)))))

;; def fn memoized logging (tracing?)
(defmacro defnml
  "Returns a memoized function, memos are aggregated under hashgraph.app.inspector/*log.
  Additionally, traces calls to nested defnmls, storing them alongside this fn's result."
  [f-name & forms]
  (assert (symbol? f-name))
  (let [[?doc rest-forms] (if (string? (first forms))
                                 [(first forms) (rest forms)]
                                 [nil forms])
        m (cond-> {}
            ?doc (assoc :doc ?doc))
        fn-fs (if (and (some-> rest-forms first list?)
                       (some-> rest-forms first first vector?))
                rest-forms
                (list rest-forms))]

    (doall
     (for [[f-inputs & _f-bodies] fn-fs]
       (assert (vector? f-inputs) (str "fn inputs must be a vector, got " (pr-str f-inputs)))))

    `(def ~(with-meta f-name m)
       (let [*mem# (atom {})
             f# ~(concat `(fn)
                         (map (fn [[f-inputs & f-bodies]]
                                `(~f-inputs
                                  ~@f-bodies))
                              fn-fs))]
         (fn [& args#]
           (let [mem# @*mem#
                 v#   (get mem# args# lookup-sentinel)]
             (if (identical? v# lookup-sentinel)
               (let [v# (binding [*mem*      mem#
                                  *from-mem* (fn [& args2#] (get mem# args2#))]
                          (apply f# args#))]
                 (swap! *mem# assoc args# v#)
                 v#)
               v#)))))))

(defn warmup-cold-inputs [f cold-inputs]
  (loop [[cold-input & rest-cold-inputs] cold-inputs]
    (when cold-input
      (apply f cold-input)
      (recur rest-cold-inputs))))

(defmacro memoizing
    "Returns memoized function over the supplied function, optionally binding mem state to mem symbol from opts."
    [{{:keys [mem ->from-mem ->in-mem?]
       :or   {mem        (gensym "mem")
              ->from-mem (gensym "->from-mem")
              ->in-mem?  (gensym "->in-mem")}} :bind

      :keys [only-last?
             recur-by recur-stop] ;; be careful to not run with warmup async, will misbehave
      }
     fn-form]

    (assert (or (and (nil? recur-by) (nil? recur-stop))
                (and (some? recur-by) (some? recur-stop))))

    (let [args-sym          (gensym "args")
          v-sym             (gensym "v")
          from-mem-args-sym (gensym "from-mem-args")
          skip-warmup?-sym  (gensym "skip-warmup?")
          mem-fn-sym        (gensym "mem-fn")
          args-hash-sym     (gensym "args-hash")]
      `(let [~mem ~(if only-last?
                     `(volatile! nil)
                     `(transient (hash-map))) ;; in clojure it's an array may, so bashing in place no good
             ~->from-mem (fn [~from-mem-args-sym] (get ~(if only-last? `(deref ~mem) mem)
                                                       (cljs.core/-hash ~from-mem-args-sym) lookup-sentinel))
             ~->in-mem?  (fn [in-mem-args#] (not (identical? (~->from-mem in-mem-args#) lookup-sentinel)))
             f#          ~fn-form
             ~skip-warmup?-sym (volatile! false)]

         (fn ~mem-fn-sym [& ~args-sym]
           (let [#_#_
                 ~args-sym
                 (if (empty? ~args-sym) ;; adds a bit of cost
                   lookup-empty-args
                   ~args-sym)
                 ~args-hash-sym (cljs.core/-hash ~args-sym)
                 mem-v# (get ~(if only-last? `(deref ~mem) mem)
                             ~args-hash-sym lookup-sentinel)
                 #_
                 (~->from-mem ~args-sym)
                 #_ (get-in ~mem-sym ~args-sym lookup-sentinel)]
             #_(l [:mem ~mem])
             #_(l [:args ~args-sym])
             #_(log! [:input (count (first ~args-sym))])
             (if-not (identical? mem-v# lookup-sentinel)
               mem-v#
               (do ~(when recur-by
                      ;; ensure mem is warmed up with recursion results, before calling it
                      `(when-not @~skip-warmup?-sym
                         #_(l [:ensuring-warmed-up ~args-sym])
                         #_(l [:mem ~mem])
                         (let [cold-inputs#
                               ((fn [cold-inputs-acc# prev-input#]
                                  (if (or (apply ~recur-stop prev-input#)
                                          (~->in-mem? prev-input#))
                                    cold-inputs-acc#
                                    (recur (conj cold-inputs-acc# prev-input#)
                                           (apply ~recur-by prev-input#))))
                                '()
                                (apply ~recur-by ~args-sym))]
                           (when (not (empty? cold-inputs#))
                             #_(l :warming-up)
                             (vreset! ~skip-warmup?-sym true)
                             (warmup-cold-inputs ~mem-fn-sym cold-inputs#)
                             (vreset! ~skip-warmup?-sym false)
                             #_(l :warmed-up)))))
                   #_(log! [:evaling ~args-sym (cljs.core/-hash ~args-sym) ~mem])
                   (let [~v-sym (apply f# ~args-sym)
                         #_#_
                         butlast-arg-mem#
                         (reduce (fn [prev-mem# arg#]
                                   (let [?arg-mem# (get prev-mem# arg# lookup-sentinel)
                                         arg-mem#  (if (identical? ?arg-mem# lookup-sentinel)
                                                     (transient (hash-map))
                                                     ?arg-mem#)]
                                     (assoc! prev-mem# arg# arg-mem#)
                                     arg-mem#))
                                 ~mem
                                 (butlast ~args-sym))]
                     ~(if only-last?
                        `(vreset! ~mem {~args-hash-sym ~v-sym})
                        `(assoc! ~mem ~args-hash-sym ~v-sym))
                     #_(assoc! butlast-arg-mem# (last ~args-sym) v#)
                     ~v-sym))))))))

(defmacro tracing-fn-form
  [fn-form f-name]
  `(fn [& args#]
     (binding [*parent-trace-atom* *trace-atom*
               *trace-atom*        (transient (hash-map
                                               :trace/fn-name (quote ~f-name)
                                               :trace/fn-args args#
                                               :trace/traces  (transient [])))]
       (timing (let [result#   (apply ~fn-form args#)
                     time#     (*->time*)
                     time-end# (+ *time-start* time#)
                     trace#    (persistent! (-> *trace-atom*
                                                (assoc! :trace/time-start *time-start*
                                                        :trace/time time#
                                                        :trace/time-end time-end#
                                                        :trace/result result#)
                                                (safe-update! :trace/traces persistent!)))]
                 (swap! *traces conj trace#)
                 (when *parent-trace-atom* (safe-update! *parent-trace-atom* :trace/traces conj! trace#))
                 result#)))))

(defmacro defn*
  "Returns a defn. Fn name is parameterizable via meta with any of:
  :timing will wrap fn bodies with hashgraph.utils.core/timing.
  :profiling will add profiling via taoensso.tufte.
  :memoizing will wrap with  memoization and bind *mem* and *from-mem*.
  :tracing will bind *parent-log-path* and *log-path*, and will log any captured info under fn call path in hashgraph.app.inspector/*log, under :traces key.

  All the captured info is glued together in hashgraph.app.inspector to display a glimpse of what happens at runtime."
  [f-name & forms]
  (assert (symbol? f-name))
  (let [[?doc [fn-inputs & fn-bodies]] (if (string? (first forms))
                                         [(first forms) (rest forms)]
                                         [nil forms])
        m (cond-> {}
            ?doc (assoc :doc ?doc))

        timing?        (-> f-name meta (contains? :timing))
        memoizing?     (-> f-name meta (contains? :memoizing))
        memoizing-opts (-> f-name meta :memoizing)
        profiling?     (-> f-name meta (contains? :profiling))
        tracing?       (if (-> f-name meta (contains? :tracing))
                         (-> f-name meta :tracing :enabled? false? not)
                         default-tracing-enabled?)

        fn-form `(fn ~fn-inputs ~@(if timing?
                                    `((timing ~@fn-bodies))
                                    fn-bodies))

        fn-form (if tracing?
                  (macroexpand `(tracing-fn-form ~fn-form ~f-name))
                  fn-form)

        fn-form (if memoizing?
                  (macroexpand `(memoizing ~memoizing-opts ~fn-form))
                  fn-form)
        ]

    `(def ~(with-meta f-name m)
       ~fn-form)))

(defmacro assert-warn [pred-expr & [?explanation]]
  `(when-not ~pred-expr (if ~?explanation
                          (js/console.warn "assert failed, " ~?explanation ", " ~(pr-str pred-expr))
                          (js/console.warn "assert failed, " ~(pr-str pred-expr)))))

(defmacro macroexpand-names
  "Recursively macroexpands all macros from the ns."
  [names expr]
  (let [names (set names)]
    `(quote ~(clojure.walk/prewalk (fn expand-form [form]
                                     (if-not (seq? form)
                                       form
                                       (if (or (-> form first symbol? not)
                                               (-> form first name (names) not))
                                         form
                                         (let [expanded (ana/macroexpand-1 &env form)]
                                           (if (= expanded form)
                                             form
                                             (expand-form expanded))))))
                                   expr))))
