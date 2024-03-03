(ns hashgraph.utils.lazy-derived-atom
  (:require [clojure.test :refer [deftest testing is are run-tests]]
            [hashgraph.utils.core :refer [l hash= hashes=]]))

(defprotocol IDerived
  (-ensure-derived [this]          #_ [^:mutable state args refs     deriving-f])
  (-derive         [this new-args] #_ [^:mutable state args new-args deriving-f]))

(deftype LazyDerivedAtom [^:mutable state ^:mutable args meta validator watches this-key refs deriving-f]
  Object
  (equiv [this other]
    (-equiv this other))

  IAtom

  IEquiv
  (-equiv [o other] (identical? o other))

  IDerived
  (-ensure-derived [o]
    (let [new-args (map deref refs)]
      (when (not= new-args args)
        (-derive o new-args))))
  (-derive [o new-args]
    (set! args new-args)
    (let [old-state state
          new-state (apply deriving-f new-args)]
      (when (not= old-state new-state)
        (set! state new-state)
        (-notify-watches o old-state new-state))))

  IDeref
  (-deref [o]
    (-ensure-derived o)
    state)

  IMeta
  (-meta [_] meta)

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (when (empty? watches)
      (let [ensure-derived! #(-ensure-derived this)]
        (doseq [ref refs]
          (add-watch ref this-key ensure-derived!))))
    (set! (.-watches this) (assoc (.-watches this) key f))
    this)
  (-remove-watch [this key]
    (let [new-watches (dissoc watches key)]
      (when (empty? new-watches)
        (doseq [ref refs]
          (remove-watch ref this-key)))
      (set! (.-watches this) new-watches))
    this)

  IHash
  (-hash [this] (goog/getUid this)))

(defn lazy-derived-atom [refs key f] (LazyDerivedAtom. nil nil nil nil nil key refs f))

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
          *lda     (lazy-derived-atom [] ::lda
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
        *lda-a            (lazy-derived-atom [*counter] ::lda-a
                                    (fn [counter]
                                      (swap! *lda->results update ::lda-a conj counter)
                                      counter))
        *lda-ab           (lazy-derived-atom [*lda-a] ::lda-ab
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
            *b      (logging (lazy-derived-atom [*a]    :a->b   inc))
            *c      (logging (lazy-derived-atom [*a]    :a->c   inc))
            *d      (logging (lazy-derived-atom [*b *c] :b+c->d +))]
        (add-watch *d ::watch-d (fn []))
        (swap! *a inc)
        (is (= @*log [[:a->b '(1) 2]
                      [:a->c '(1) 2]
                      [:b+c->d '(2 2) 4]]))))))
