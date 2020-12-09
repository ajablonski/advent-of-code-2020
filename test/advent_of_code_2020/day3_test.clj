(ns advent-of-code-2020.day3-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day3 :refer :all]))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "7\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "336\n"
           (with-out-str (main-2))))))

(deftest get-col-test
  (testing "Get column associated with row after filtering out skipped rows"
    (is (= (get-col :row-num 0 :grid-width 10 :step-size 3)
           0))
    (is (= (get-col :row-num 3 :grid-width 10 :step-size 3)
           9))
    (is (= (get-col :row-num 4 :grid-width 10 :step-size 3)
           2))))

(deftest get-tree-count-test
  (testing "Tree not present"
    (is (= (get-tree-count 2 "##.")
           0)))
  (testing "Tree present"
    (is (= (get-tree-count 0 "#..")
           1))))

(deftest get-trees-in-path-test
  (testing "Get count of trees in 1-1 path"
    (is (= (get-trees-in-path 1 1 [
                                   "#.."
                                   ".#."
                                   "..#"])
           3)))
  (testing "Get trees when col-step is greater than 1"
    (is (= (get-trees-in-path 2 1 [
                                   "#...."
                                   "..#.."
                                   "....#"])
           3)))
  (testing "Get trees when row-step is greater than 1"
    (is (= (get-trees-in-path 1 2 [
                                   "#...."
                                   "#####"
                                   ".#..."])
           2)))
  (testing "Get trees when wraps around"
    (is (= (get-trees-in-path 2 1 [
                                   "#.."
                                   "..#"
                                   ".#."])
           3))))
