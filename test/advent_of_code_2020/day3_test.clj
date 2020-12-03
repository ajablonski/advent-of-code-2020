(ns advent-of-code-2020.day3_test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day3 :refer :all])
  (:import (advent_of_code_2020.day3 Position)))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "7\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "336\n"
           (with-out-str (main-2))))))

(deftest get-col-test
  (testing "Move next position"
    (is (= (get-col 0 10 3)
           0))
    (is (= (get-col 3 10 3)
           9))
    (is (= (get-col 4 10 3)
           2))))

(deftest get-tree-count-test
  (testing "Tree not present"
    (is (= (get-tree-count 2 2 [".##" "..." "..."])
           0))
    (is (= (get-tree-count 0 2 [".##" "..." "..."])
           1))))
