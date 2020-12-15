(ns advent-of-code-2020.day15-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day15 :refer :all]))

(deftest expand-entries-test
  (testing "Expand entries when last is new"
    (is (= {:last-entry-map
              {0 1
               1 2
               2 3} :last-turn 4 :last-val 0}
           (expand {0 1
                    1 2} 3 2 1))))
  (testing "Expand entries when last is not new"
    (is (= {':last-entry-map
           {0 4
            1 2
            2 3} :last-turn 5 :last-val 3}
           (expand {0 1
                    1 2
                    2 3} 4 0 1))))
)

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "436\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "175594\n"
           (with-out-str (main-2))))))
