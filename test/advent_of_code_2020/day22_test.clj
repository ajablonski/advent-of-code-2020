(ns advent-of-code-2020.day22-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day22 :refer :all]
            [advent-of-code-2020.utils :refer [with-minimal-output]]))

(deftest parse-input-test
  (testing "Should parse input"
    (is (= (list '(9 2 6 3 1) '(5 8 4 7 10))
           (parse-input day-22-input)))))

(deftest take-turn-test
  (testing "Should take a turn, putting winning cards at end of winner's hand with winning card first"
    (is (= (list '(2 6 3 1 9 5) '(8 4 7 10))
           (take-normal-turn (list (list 9 2 6 3 1) (list 5 8 4 7 10)))))))

(deftest play-game-test
  (testing "Should play a full game, resulting in p2 winning"
    (is (= (list nil '(3 2 10 6 8 5 9 4 7 1))
           (play-normal-game (list '(9 2 6 3 1) '(5 8 4 7 10)))))))

(deftest calculate-score-test
  (testing "Should calculate the score using decreasing point count"
    (is (= 306
           (calculate-score '(3 2 10 6 8 5 9 4 7 1))))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "306\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "291\n"
           (with-out-str (with-minimal-output (main-2)))))))
