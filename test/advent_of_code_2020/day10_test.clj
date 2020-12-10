(ns advent-of-code-2020.day10-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day10 :refer :all]))

(deftest get-joltage-differences-counts-test
  (testing "Should calculate differences"
    (is (= {1 2,
            2 1,
            3 3}
           (get-joltage-differences-counts #{1 2 5 7 10})))))

(deftest get-arrangements-test
  (testing "Should get all possible arrangements"
    (is (= #{
             (list 0 2 5)}
           (set (get-arrangements 0 (list 2 5) '())
                )))
    (is (= #{
             (list 0 1 2 5)
             (list 0 2 5)}
           (set (get-arrangements 0 (list 1 2 5) '())
                )))
    (is (= #{
             (list 0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22)
             (list 0, 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, 22)
             (list 0, 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, 22)
             (list 0, 1, 4, 5, 7, 10, 12, 15, 16, 19, 22)
             (list 0, 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, 22)
             (list 0, 1, 4, 6, 7, 10, 12, 15, 16, 19, 22)
             (list 0, 1, 4, 7, 10, 11, 12, 15, 16, 19, 22)
             (list 0, 1, 4, 7, 10, 12, 15, 16, 19, 22)
             }
           (set (get-arrangements 0 (list 16 10 15 5 1 11 7 19 6 12 4 22) '()))))))

(deftest get-arrangements-wrapper-test
  (testing "Should get all possible arrangements"
    (is (= 1
           (get-total-arrangements-count (list 2))
           ))
    (is (= 2
           (get-total-arrangements-count (list 1 2))))
    (is (= 8
           (get-total-arrangements-count (list 16 10 15 5 1 11 7 19 6 12 4))))
    (is (= 19208
           (get-total-arrangements-count (list 28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3))))))

(deftest get-subgraphs-test
  (testing "Separates input into parts different by 3"
    (is (= (list (list 0 1 2 3 4) (list 7) (list 10) (list 13) (list 16) (list 19))
           (reverse (get-subgraphs (list 0 1 2 3 4 7 10 13 16 19)))))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "35\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "8\n"
           (with-out-str (main-2))))))
