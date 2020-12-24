(ns advent-of-code-2020.day23-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day23 :refer :all]
            [advent-of-code-2020.utils :refer [with-minimal-output]]))

(deftest parse-input-to-maps-test
  (testing "Should parse the input to a map"
    (is (= '({0 {:val 7 :next 1}
              1 {:val 2 :next 2}
              2 {:val 5 :next 3}
              3 {:val 3 :next 0}}
             {7 0
              2 1
              5 2
              3 3})
           (convert-vector-to-maps [7 2 5 3])))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "67384529\n"
           (with-out-str (with-minimal-output (main-1)))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "149245887792\n"
           (with-out-str (with-minimal-output (main-2)))))))
