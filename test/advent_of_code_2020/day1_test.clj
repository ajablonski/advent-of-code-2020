(ns advent-of-code-2020.day1_test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day1 :as day1]))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "514579\n"
           (with-out-str (day1/main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "241861950\n"
           (with-out-str (day1/main-2))))))

(deftest parse-lines-test
  (testing "day1/parse-lines"
    (is (= [1 2 3]
           (day1/parse-lines "1\n2\n3\n")))))

(deftest get-pair-test
  (testing "day1/get-pair"
    (is (= (day1/get-pair 1) 2019))
    (is (= (day1/get-pair 0) 2020))
    (is (= (day1/get-pair -200) 2220))))
