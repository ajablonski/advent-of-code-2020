(ns advent-of-code-2020.day13-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day13 :refer :all]
            [advent-of-code-2020.utils :refer (with-minimal-output)]))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "295\n"
           (with-out-str (with-minimal-output (main-1)))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "1068781N\n"
           (with-out-str (with-minimal-output (main-2)))))))
