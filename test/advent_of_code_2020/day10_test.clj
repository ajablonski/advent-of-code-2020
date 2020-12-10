(ns advent-of-code-2020.day10-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day10 :refer :all]))

(deftest get-joltage-differences-test
  (testing "Should calculate differences"
    (is (= {1 2 2 1 3 3}
           (get-joltage-differences #{1 2 5 7 10})))))

(deftest get-arrangements-test
  (testing "Should get all possible arrangements"
    (is (= #{
             (list 0 2 5)}
           (set (get-arrangements-wrapper (list 2))
           )))
    (is (= #{
             (list 0 1 2 5)
             (list 0 2 5)}
           (set (get-arrangements-wrapper (list 1 2))
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
           (set (get-arrangements-wrapper (list 16 10 15 5 1 11 7 19 6 12 4)))))

    ))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "35\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "8\n"
           (with-out-str (main-2))))))
