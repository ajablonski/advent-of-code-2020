(ns advent-of-code-2020.day19-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day19 :refer :all]))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "3\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "12\n"
           (with-out-str (main-2))))))
