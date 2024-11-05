(ns hashgraph.utils.core-test
  (:require [clojure.test :refer [deftest testing is are run-tests]]
            [hashgraph.utils.core :refer-macros [when-let*]]
            :reload-all
            ))

(deftest when-let*-test
  (is (= :reached (when-let* [a true]
                    :reached)))
  (is (= :reached (when-let* [a true
                              b true]
                    :reached)))
  (is (= :reached (when-let* [a true
                              b :reached]
                    b)))
  (is (= :reached (when-let* [a :reached
                              b a]
                    b)))
  (is (= :reached (when-let* [a {:key :reached}
                              b (:key a)]
                    b)))
  (is (= nil (when-let* [a false
                         b :not-reached]
               b)))
  (is (= nil (when-let* [a true
                         b false]
               :not-reached))))
