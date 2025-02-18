(ns hashgraph.utils.lazy-derived-atom
  (:require [hashgraph.utils.core :refer [hash= logging-enabled?] :refer-macros [timed nl]]
            [clojure.test :refer [deftest testing is are run-tests]]
            :reload-all))

#?(:cljs
   (do (defonce **lda->watches (transient (hash-map)))

       (defprotocol IDerived
         (-derive              [this new-args] #_ [^:mutable state args new-args deriving-f])
         (-ensure-derived      [this]          #_ [^:mutable state args refs     deriving-f])
         (-ensure-derived-with [this with-ref with-ref-val])
         )

       (deftype LazyDerivedAtom [^:mutable **args ^:mutable **state *meta validator watches refs sym deriving-f]
         Object
         (equiv [this other]
           (-equiv this other))

         IAtom

         IEquiv
         (-equiv [o other] (identical? o other))

         IDerived
         (-ensure-derived      [o]
           (let [new-args (map deref refs)]
             (-derive o new-args)))
         (-ensure-derived-with [o with-ref with-ref-val]
           ;; (l [:lda-ensure-derived])
           (let [new-args (->> refs
                               (reduce (fn [new-args-acc ref]
                                         (if (identical? ref with-ref)
                                           (conj! new-args-acc with-ref-val)
                                           (conj! new-args-acc @ref)))
                                       (transient []))
                               (persistent!))]
             (-derive o new-args)))
         (-derive [o new-args]
           (when (not (hash= **args new-args))
             (set! **args new-args)
             (let [old-state **state
                   [t new-state] (timed (apply deriving-f new-args))]

               (when (not (hash= old-state new-state))
                 (set! **state new-state)
                 (when (some-> (meta sym) :log)
                   (nl :<= (into {} (map (fn [ref-sym new-arg] [ref-sym new-arg]) (-> sym meta :ref-syms) new-args)))
                   (nl :took t)
                   (nl :=> {(with-meta sym nil) new-state}))
                 (-notify-watches o old-state new-state)))))

         IDeref
         (-deref [o]
           ;; (l [:lda-deref])
           #_(when (empty? watches)
               (-ensure-derived o))
           (-ensure-derived o)
           **state)

         IMeta
         (-meta [_] meta)

         IWatchable
         (-notify-watches [this oldval newval]
           ;; (l [:lda-notify-watches oldval newval])
           (doseq [[key f] watches]
             (f key this oldval newval)))
         (-add-watch [this key f]
           #_
           (assoc! **lda->watches this (assoc (**lda->watches this) key f))
           (when (-> sym meta :lazy?)
             (when (empty? watches)
               (doseq [ref refs]
                 (add-watch ref this (fn [_ _ _ new-ref-val] (-ensure-derived this) #_(-ensure-derived-with this ref new-ref-val)))))) ;; as lazy-derived-atom is self-cleaning (removes watches when not watched) there's no need for "key", as its purpose is to override
           (set! (.-watches this) (assoc (.-watches this) key f))
           this)
         (-remove-watch [this key]
           #_(let [new-lda-watches (dissoc (**lda->watches this) key)]
             (if (empty? new-lda-watches)
               (dissoc! **lda->watches this)
               (assoc! **lda->watches this new-lda-watches)))

           (let [new-watches (dissoc watches key)]
             (when (-> sym meta :lazy?)
               (when (empty? new-watches)
                 (doseq [ref refs]
                   (remove-watch ref this))))
             (set! (.-watches this) new-watches))
           this)

         IHash
         (-hash [this] (goog/getUid this)))

       (defn lazy-derived-atom
         ([refs f] (lazy-derived-atom refs 'unnamed-lazy-derived-atom f))
         ([refs sym f]
          (LazyDerivedAtom. nil nil nil nil (hash-map) refs sym f)))

       (defn atom? [obj] (identical? (type obj) cljs.core/Atom))
       (defn lazy-derived-atom? [obj] (identical? (type obj) LazyDerivedAtom))
       (defn atomic? [obj] (or (atom? obj)
                               (lazy-derived-atom? obj)))



       (defn logging-under [*log ^js lda]
         (let [deriving-f (.-deriving_f lda)]
           (set! (.-deriving_f lda) (fn [& new-args]
                                      (let [new-val (apply deriving-f new-args)]
                                        (swap! *log conj [(.-this-key lda) new-args new-val])
                                        new-val)))
           lda))

       (deftest lazy-derived-atom-test
         (testing "won't eval on create, will on deref, won't on subsequent deref"
           (let [*counter (atom 0)
                 *lda     (lazy-derived-atom []
                              (fn [] (swap! *counter inc)))]

             (is (zero? @*counter))
             (is (= 1 @*lda))
             (is (= 1 @*counter))
             (is (= 1 @*lda))
             (is (= 1 @*counter))))

         (let [lda->results-init {::lda-a  []
                                  ::lda-ab []}
               *lda->results     (atom lda->results-init)
               *counter          (atom 0)
               *lda-a            (lazy-derived-atom [*counter]
                                     (fn [counter]
                                       (swap! *lda->results update ::lda-a conj counter)
                                       counter))
               *lda-ab           (lazy-derived-atom [*lda-a]
                                     (fn [lda-a]
                                       (swap! *lda->results update ::lda-ab conj lda-a)
                                       lda-a))]
           (testing "won't eval on create"
             (is (= @*lda->results lda->results-init)))

           (testing "won't eval on refs change, if not watched"
             (is (= (swap! *counter inc) 1))
             (is (= @*lda->results lda->results-init)))

           (testing "will eval on deref, a dependent lazy atom does'n get evaled"
             (is (= @*lda-a 1))
             (is (= @*lda->results {::lda-a  [1]
                                    ::lda-ab []})))

           (testing "won't re-eval when args don't change"
             (is (= @*lda-ab 1))
             (is (= @*lda->results {::lda-a  [1]
                                    ::lda-ab [1]})))

           (testing "stays lazy even when been derefed"
             (is (= (swap! *counter inc) 2))
             (is (= @*lda->results {::lda-a  [1]
                                    ::lda-ab [1]})))

           (testing "will eagerly re-derive if watched"
             (add-watch *lda-a ::lda-a-watch (fn []))
             (is (= (swap! *counter inc) 3))
             (is (= @*lda->results {::lda-a  [1 3]
                                    ::lda-ab [1]})))

           (testing "won't unneceserraly reeval on deref from dependent"
             (is (= @*lda-ab 3))
             (is (= @*lda->results {::lda-a  [1 3]
                                    ::lda-ab [1 3]})))

           (remove-watch *lda-a ::lda-a-watch)

           (testing "will eagerly re-derive if watched, from bottom"
             (add-watch *lda-ab ::lda-ab-watch (fn []))
             (is (= (swap! *counter inc) 4))
             (is (= @*lda->results {::lda-a  [1 3 4]
                                    ::lda-ab [1 3 4]}))
             (remove-watch *lda-ab ::lda-ab-watch))

           (testing "won't eagerly re-derive if not watched (watches been removed)"
             (is (= (swap! *counter inc) 5))
             (is (= @*lda->results {::lda-a  [1 3 4]
                                    ::lda-ab [1 3 4]})))

           (testing "and on deref does eval, from bottom"
             (is (= @*lda-ab 5))
             (is (= @*lda->results {::lda-a  [1 3 4 5]
                                    ::lda-ab [1 3 4 5]})))

           (testing "diamond graph eval sibling nodes only once"
             ;;   a
             ;;  / \
             ;; b  c ;; <- siblings
             ;; \ /
             ;;  d
             (let [*log    (atom [])
                   logging (partial logging-under *log)
                   *a      (atom 0)
                   *b      (logging (lazy-derived-atom [*a] inc))
                   *c      (logging (lazy-derived-atom [*a] inc))
                   *d      (logging (lazy-derived-atom [*b *c] +))]
               (add-watch *d ::watch-d (fn []))
               (swap! *a inc)
               (is (= @*log [[:a->b '(1) 2]
                             [:a->c '(1) 2]
                             [:b+c->d '(2 2) 4]]))))))))




(def default-lda-logging? logging-enabled? #_false #_true)

(defmacro deflda [sym refs body]
  `(def ~sym
     (lazy-derived-atom
         ~refs
         ~(if default-lda-logging?
            `(with-meta (quote ~sym) {:lazy?    true
                                      :log      true
                                      :ref-syms (quote ~refs)})
            `(with-meta (quote ~sym) {:lazy? true}))
       ~body)))

(defmacro defda [sym refs body]
  `(do (def ~sym
         (lazy-derived-atom
             ~refs
             ~(if default-lda-logging?
                `(with-meta (quote ~sym) {:lazy?    false
                                          :log      true
                                          :ref-syms (quote ~refs)})
                `(with-meta (quote ~sym) {:lazy? false}))
           ~body))
       (doseq [ref# ~refs]
         (add-watch ref# ~sym  #(-ensure-derived ~sym)))))

#_
(deftest def<--test
  (testing "def<- defines an lda"
    (testing ", works on empty deps"
      (let [empty-out (macroexpand '(def<- *b nil))]
        (is (= empty-out
               `(def *b (hashgraph.utils.lazy-derived-atom/lazy-derived-atom []
                            (fn [])))))))

    (testing ", works on a dep deref"
      (let [out (macroexpand '(def<- *b (inc @*a)))]
        (is (= out
               `(def *b (hashgraph.utils.lazy-derived-atom/lazy-derived-atom [*a]
                            (fn [a] (inc a))))))))

    (testing ", deduplicates deps deref"
      (let [out (macroexpand '(def<- *b (+ @*a @*a)))]
        (is (= out
               `(def *b (hashgraph.utils.lazy-derived-atom/lazy-derived-atom [*a]
                            (fn [a] (+ a a))))))))

    (testing ", works on duplicated nested derefs"
      (let [out (macroexpand '(def<- *b
                                (let [a    @*a
                                      ->a2 (fn [a-arg] (+ a-arg @*a1))
                                      ;; macro-extraction of derefs will not correctly bind run-time refs
                                      ;; perhaps instead do as rum.core/reactive does - collect refs at run-time,
                                      ;; Pros:
                                      ;; - correct refs
                                      ;; - only reached by run-time refs will be dependent upon
                                      ;; Cons:
                                      ;; - more compute costly
                                      ;; - need to make sure @*a @*a won't be costly
                                      ;; - still not a pure function
                                      ->a3 (fn [*a4-runtime-bind] (+ (->a2 a) @*a4-runtime-bind))]
                                  (->a3 *a4))))]
        (is (= out
               `(def *b (hashgraph.utils.lazy-derived-atom/lazy-derived-atom [*a]
                            (fn [a] (+ a a))))))))))
