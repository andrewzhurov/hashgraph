(ns hashgraph.utils.js-map-test
  (:require-macros [hashgraph.utils.js-map :refer [js-map] :as js-map]
                   [hashgraph.utils.core :as utils]
                   :reload-all)
  (:require [clojure.test :refer [deftest testing is are run-tests]]
            [hashgraph.utils.js-map]))

(deftest js-map-test
  (testing "js-map"
    (is (= js/Map (type (js-map))))
    (is (= js/Map (type (js-map :a :b)))))

  (testing "js-map/js-map?"
    ;; wrap with true? since 'is' would expect a function, not a macro
    (is (= true (js-map/js-map? (js-map)))
        (= false (js-map/js-map? {}))))

  (testing "js-map/js-map->cljs-map"
    (let [m (js-map :a (js-map :aa (js-map :aaa :aaa-val
                                           :aab :aab-val
                                           :aac :aac-val)
                               :ab (js-map :aba :aba-val))
                    :b (js-map :ba :ba-val
                               :bb :bb-val
                               :bc :bc-val))]
      (is (= {:a {:aa {:aaa :aaa-val
                       :aab :aab-val
                       :aac :aac-val}
                  :ab {:aba :aba-val}}
              :b {:ba :ba-val
                  :bb :bb-val
                  :bc :bc-val}}
             (js-map/js-map->clj-map m)))))

  (testing "js-map/get"
    (let [m (js-map :a :b)]
      (is (= :b (js-map/get m :a)))
      (is (every? number? (js-map/keys-impl m))))
    (is (= nil (js-map/get nil :a))))

  (testing "js-map/get!"
    (let [m (js-map)]
      (is (= :b (js-map/get! m :a :b)))
      (is (= {:a :b} (js-map/js-map->clj-map m)))))

  (testing "js-map/get-in"
    (let [m (js-map)]
      (is (= nil (js-map/get-in nil [:a :b])))
      (is (= true (js-map/empty? m))))

    (let [m (js-map :a (js-map :b :c))]
      (is (= :c (js-map/get-in m [:a :b])))))

  (testing "js-map/get-in!"
    (let [m (js-map)]
      (is (= :b (js-map/get-in! m [:a] :b)))
      (is (= {:a :b} (js-map/js-map->clj-map m))))

    (let [m (js-map)]
      (is (= :c (js-map/get-in! m [:a :b] :c)))
      (is (= {:a {:b :c}} (js-map/js-map->clj-map m))))

    (let [m (js-map :a (js-map :b (js-map :c :d)))]
      (is (= {:c :d} (-> m
                         (js-map/get-in! [:a :b] :c)
                         js-map/js-map->clj-map)))
      (is (= {:a {:b {:c :d}}} (js-map/js-map->clj-map m)))))

  (testing "js-map/assoc!"
    (let [m (js-map)]
      (is (= :b (-> (js-map/assoc! m :a :b)
                    (js-map/get :a))))
      (is (= 1 (count (js-map/keys-impl m))))
      (is (every? number? (js-map/keys-impl m)))
      (is (= :b (-> (js-map/assoc! nil :a :b)
                    (js-map/get :a)))))

    (let [m (js-map)]
      (js-map/assoc! m :a :a-val :b :b-val)
      (is (= :a-val (js-map/get m :a)))
      (is (= :b-val (js-map/get m :b))))

    (let [m (js-map/assoc! nil
                           :a (js-map/assoc! nil :aa :aa-val)
                           :b (js-map/assoc! nil :ab :ab-val))]
      (is (= :aa-val (js-map/get-in m [:a :aa])))
      (is (= :ab-val (js-map/get-in m [:b :ab])))))

  (testing "js-map/assoc-in!"
    (let [m (js-map)]
      (is (= true (-> (js-map/assoc-in! m [:a :b] :c)
                      (js-map/get :a)
                      (js-map/js-map?))))
      (is (= :c (-> m
                    (js-map/get :a)
                    (js-map/get :b))))
      (is (= :c (-> (js-map/assoc-in! nil [:a :b] :c)
                    (js-map/get :a)
                    (js-map/get :b))))))

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

  (testing "js-map/update!"
    (let [m (js-map :a (js-map :b :c))]
      (is (= true (-> m
                      (js-map/update! :a js-map/dissoc! :b)
                      (js-map/get :a)
                      (js-map/empty?))))
      (is (= 1 (-> m
                   (js-map/assoc! :counter 0)
                   (js-map/update! :counter inc)
                   (js-map/get :counter))))
      (is (= :c (-> nil
                    (js-map/update! :a js-map/assoc! :b :c)
                    (js-map/get :a)
                    (js-map/get :b))))))

  (testing "js-map/update-in!"
    (is (= :d (-> (js-map/update-in! nil [:a :b] js-map/assoc! :c :d)
                  (js-map/get-in [:a :b :c])))))

  (testing "js-map/empty?"
    (let [m (js-map)]
      (is (= true (js-map/empty? m)))
      (js-map/assoc! m :a :b)
      (is (= false (js-map/empty? m)))
      (js-map/dissoc! m :a)
      (is (= true (js-map/empty? m)))))

  (testing "js-map/contains?"
    (let [m (js-map :a :b)]
      (is (= true (js-map/contains? m :a)))
      (is (= false (js-map/contains? (js-map/dissoc! m :a) :a)))))

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

#_
(run-tests)
