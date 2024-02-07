(ns hashgraph.utils.js-map-test
  (:require-macros [hashgraph.utils.js-map :refer [js-map] :as js-map])
  (:require [clojure.test :refer [deftest testing is are run-tests]]))

(deftest js-map-test
  (testing "js-map"
    (is (= js/Map (type (js-map))))
    (is (= js/Map (type (js-map :a :b)))))

  (testing "js-map/js-map?"
    (is (true? (js-map/js-map? (js-map)))
        (false? (js-map/js-map? {}))))

  (testing "js-map/get"
    (let [m (js-map :a :b)]
      (is (= (js-map/get m :a) :b))
      (is (every? number? (js-map/keys m)))))

  (testing "js-map/assoc!"
    (let [m (js-map)]
      (is (-> (js-map/assoc! m :a :b)
              (js-map/get :a)
              (= :b)))
      (is (= 1 (count (js-map/keys m))))
      (is (every? number? (js-map/keys m)))))

  (testing "js-map/dissoc!"
    (let [m (js-map :a :b)]
      (is (= nil (-> m
                     (js-map/dissoc! :a)
                     (js-map/get :a))))
      (is (= :d (-> m
                    (js-map/assoc! :a :b)
                    (js-map/assoc! :c :d)
                    (js-map/dissoc! :a)
                    (js-map/get :c))))))

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
      (is (false? (js-map/contains? (js-map/dissoc! m :a) :a))))))
