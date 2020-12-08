(ns advent-of-code-2020.utils-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2020.utils :refer :all]))

(deftest map-vals-test
  (testing "Should apply f to all values in map"
    (is (= {:a 1 :b 1}
           (map-vals inc {:a 0 :b 0})))))
