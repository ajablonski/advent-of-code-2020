(ns advent-of-code-2020.day1-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day1 :refer :all]))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "514579\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "241861950\n"
           (with-out-str (main-2))))))

(deftest parse-lines-test
  (testing "parse-lines"
    (is (= [1 2 3]
           (parse-lines "1\n2\n3\n")))))

(deftest get-pair-summing-to-test
  (testing "get-pair-summing-to")
  (is (= (find-pair-summing-to '(1 2 3 4) 3)
         #{1 2})))

