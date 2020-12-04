(ns advent-of-code-2020.day14_test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day14 :refer :all]))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "\n"
           (with-out-str (main-2))))))
