(ns advent-of-code-2020.day9_test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day9 :refer :all]))

(deftest find-numbers-summing-to-test
  (testing "Finds numbers in set that sum to number"
    (is (false?
          (has-no-numbers-summing-to? #{35 20 15 25 47} 40)))
    (is (true?
          (has-no-numbers-summing-to? #{95 102 117 150 182} 127)))))

(deftest find-invalid-entry-test
  (testing "Finds the first number in the ordered list
  which is not a sum of the previous preamble-entry numbers"
    (is (= (find-invalid-entry [1 2 3 6 9 15] 2)
           6))))

(deftest find-contiguous-entries-summing-to-test
  (testing "Finds contiguous numbers that sum to a given number"
    (is (= (find-contiguous-entries-summing-to [1 2 3 6 9 15] 6)
           [1 2 3]))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "127N\n"
           (with-out-str (main-1 5))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "62N\n"
           (with-out-str (main-2 5))))))
