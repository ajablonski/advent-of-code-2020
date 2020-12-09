(ns advent-of-code-2020.day5-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day5 :refer :all]))

(deftest get-seat-id-test
  (testing "Seat IDs"
    (is (= (get-seat-id "BFFFBBFRRR")
           567))
    (is (= (get-seat-id "FFFBBBFRRR")
           119))
    (is (= (get-seat-id "BBFFBBFRLL")
           820))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "6\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "5\n"
           (with-out-str (main-2))))))
