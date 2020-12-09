(ns advent-of-code-2020.day9_test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day9 :refer :all]))

(deftest find-numbers-summing-to-test
  (testing "Finds numbers in set that sum to number"
    (is (true?
           (has-numbers-summing-to? 40 #{35
                                         20
                                         15
                                         25
                                         47})))
    (is (false?
           (has-numbers-summing-to? 127 #{95
                                          102
                                          117
                                          150
                                          182})))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "127N\n"
           (with-out-str (main-1 5))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "62N\n"
           (with-out-str (main-2 5))))))
