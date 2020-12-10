(ns advent-of-code-2020.day10-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day10 :refer :all]))

(deftest get-joltage-differences-test
  (testing "Should calculate differences"
    (is (= {1 2 2 1 3 3}
           (get-joltage-differences #{1 2 5 7 10})))))

(deftest get-arrangements-test
  (testing "Should get all possible arrangements"
    (is (= 1
           (get-arrangements-wrapper (list 2))
           ))
    (is (= 2
           (get-arrangements-wrapper (list 1 2))))
    (is (= 8
           (get-arrangements-wrapper (list 16 10 15 5 1 11 7 19 6 12 4))))
    (is (= 19208
           (get-arrangements-wrapper (list 28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3))))))

(deftest get-subgraphs-test
  (testing "Separates input into parts different by 3"
    (is (= (list (list 0 1 2 3 4) (list 7) (list 10) (list 13) (list 16) (list 19))
           (get-subgraphs-wrapper (list 1 2 3 4 7 10 13 16))))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "35\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "8\n"
           (with-out-str (main-2))))))
