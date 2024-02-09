(ns hashgraph.utils.js-map-test
  (:require-macros [hashgraph.utils.js-map :refer [js-map] :as js-map]
                   [hashgraph.utils.core :as utils])
  (:require [clojure.test :refer [deftest testing is are run-tests]]
            [hashgraph.utils.js-map]))

(deftest js-map-test
  (testing "js-map"
    (is (= js/Map (type (js-map))))
    (is (= js/Map (type (js-map :a :b)))))

  (testing "js-map/js-map?"
    ;; wrap with true? since 'is' would expect a function, not a macro
    (is (true? (js-map/js-map? (js-map)))
        (false? (js-map/js-map? {}))))

  (testing "js-map/js-map->cljs-map"
    (let [m (js-map :a (js-map :aa (js-map :aaa :aaa-val
                                           :aab :aab-val
                                           :aac :aac-val)
                               :ab (js-map :aba :aba-val))
                    :b (js-map :ba :ba-val
                               :bb :bb-val
                               :bc :bc-val))]
      (is (-> m
              (js-map/js-map->clj-map)
              (= {:a {:aa {:aaa :aaa-val
                           :aab :aab-val
                           :aac :aac-val}
                      :ab {:aba :aba-val}}
                  :b {:ba :ba-val
                      :bb :bb-val
                      :bc :bc-val}})))))

  (testing "js-map/get"
    (let [m (js-map :a :b)]
      (is (= (js-map/get m :a) :b))
      (is (every? number? (js-map/keys-impl m))))
    (is (-> (js-map/get nil :a)
            (= nil))))

  (testing "js-map/get-in"
    (let [m (js-map)]
      (is (-> (js-map/get-in nil [:a :b])
              (nil?)))
      (is (true? (js-map/empty? m))))

    (let [m (js-map :a (js-map :b :c))]
      (is (-> (js-map/get-in m [:a :b])
              (= :c)))))

  (testing "js-map/assoc!"
    (let [m (js-map)]
      (is (-> (js-map/assoc! m :a :b)
              (js-map/get :a)
              (= :b)))
      (is (= 1 (count (js-map/keys-impl m))))
      (is (every? number? (js-map/keys-impl m)))
      (is (-> (js-map/assoc! nil :a :b)
              (js-map/get :a)
              (= :b))))

    (let [m (js-map)]
      (js-map/assoc! m :a :a-val :b :b-val)
      (is (-> (js-map/get m :a) (= :a-val)))
      (is (-> (js-map/get m :b) (= :b-val))))

    (let [m (js-map/assoc! nil
                           :a (js-map/assoc! nil :aa :aa-val)
                           :b (js-map/assoc! nil :ab :ab-val))]
      (is (-> (js-map/get-in m [:a :aa]) (= :aa-val)))
      (is (-> (js-map/get-in m [:b :ab]) (= :ab-val)))))

  (testing "js-map/assoc-in!"
    (let [m (js-map)]
      (is (-> (js-map/assoc-in! m [:a :b] :c)
              (js-map/get :a)
              (js-map/js-map?)
              true?))
      (is (-> m
              (js-map/get :a)
              (js-map/get :b)
              (= :c)))
      (is (-> (js-map/assoc-in! nil [:a :b] :c)
              (js-map/get :a)
              (js-map/get :b)
              (= :c)))))

  (testing "js-map/dissoc!"
    (let [m (js-map :a :b)]
      (is (-> m
              (js-map/dissoc! :a)
              (js-map/get :a)
              (= nil)))
      (is (-> m
              (js-map/assoc! :a :b)
              (js-map/assoc! :c :d)
              (js-map/dissoc! :a)
              (js-map/get :c)
              (= :d)))))

  (testing "js-map/update!"
    (let [m (js-map :a (js-map :b :c))]
      (is (-> m
              (js-map/update! :a js-map/dissoc! :b)
              (js-map/get :a)
              (js-map/empty?)))
      (is (-> m
              (js-map/assoc! :counter 0)
              (js-map/update! :counter inc)
              (js-map/get :counter)
              (= 1)))
      (is (-> nil
              (js-map/update! :a js-map/assoc! :b :c)
              (js-map/get :a)
              (js-map/get :b)
              (= :c)))))

  (testing "js-map/update-in!"
    (is (-> (js-map/update-in! nil [:a :b] js-map/assoc! :c :d)
            (js-map/get-in [:a :b :c])
            (= :d))))

  (testing "js-map/empty?"
    (let [m (js-map)]
      (is (true? (js-map/empty? m)))
      (js-map/assoc! m :a :b)
      (is (false? (js-map/empty? m)))
      (js-map/dissoc! m :a)
      (is (true? (js-map/empty? m)))))

  (testing "js-map/contains?"
    (let [m (js-map :a :b)]
      (is (true? (js-map/contains? m :a)))
      (is (false? (js-map/contains? (js-map/dissoc! m :a) :a)))))

  #_(cljs.pprint/pprint
   (utils/macroexpand-names
    ["js-map"]
    (js-map/js-map)))

  #_
  (testing "js-map/get!!"
    (let [m  (js-map/js-map)
          ma (js-map/get!! m :a)]
      (is (true? (js-map/js-map? m)))
      (is (identical? (js-map/get m :a) ma))
      (is (true? (js-map/js-map? ma)))))

  #_
  (testing "js-map/get-in!!"
    (let [m   (js-map)
          mab (js-map/get-in!! m [:a :b])
          ma  (js-map/get m :a)]
      (is (true? (js-map/js-map? m)))
      (is (true? (js-map/js-map? ma)))
      (is (true? (js-map/js-map? mab)))
      (is (identical? (js-map/get m :a) ma))
      (is (identical? (js-map/get ma :b) mab))))

  #_
  (testing "js-map/assoc!!"
    (let [m (js-map/assoc!! nil :a :b)]
      (is (true? (js-map/js-map? m)))
      (is (= (js-map/get m :a) :b))))

  #_
  (testing "js-map/dissoc!!"
    (let [m (js-map/dissoc!! nil :a)]
      (is (true? (js-map/js-map? m)))
      (is (true? (js-map/empty? m)))))

  #_
  (testing "js-map/update!!"
    (let [m  (js-map/update!! nil :a js-map/assoc!! :b :c)
          ma (js-map/get m :a)]
      (is (true? (js-map/js-map? m)))
      (is (true? (js-map/js-map? ma)))
      (is (= (js-map/get ma :b) :c))))

  #_
  (testing "js-map/update-in!!"
    (let [m (js-map/update-in!! nil [:a :b] js-map/assoc!! :c :d)
          ma (js-map/get m :a)
          mab (js-map/get ma :b)]
      (is (js-map/js-map? m))
      (is (js-map/js-map? ma))
      (is (js-map/js-map? mab))
      (is (= (js-map/get mab :c) :d)))))

(run-tests)
