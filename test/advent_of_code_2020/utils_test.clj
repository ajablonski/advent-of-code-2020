(ns advent-of-code-2020.utils-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2020.utils :refer :all]))

(deftest map-vals-test
  (testing "Should apply f to all values in map"
    (is (= {:a 1 :b 1}
           (map-vals inc {:a 0 :b 0})))))

(deftest find-first-test
  (testing "Should find the first item matching predicate"
    (is (= (find-first even? '(1 2 3 4 5))
           2)))
  (testing "Should return nil when nothing matches predicate"
    (is (= (find-first even? '(1 3 5))
           nil))))
