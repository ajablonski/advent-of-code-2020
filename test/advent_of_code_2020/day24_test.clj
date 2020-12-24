(ns advent-of-code-2020.day24-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day24 :refer :all]
            [advent-of-code-2020.utils :refer [with-minimal-output]]))

(deftest parse-input-line-test
  (testing "Should parse e and w as individual tokens"
    (is (= '(:e :w :w :e :e :w)
           (parse-input-line "ewweew"))))
  (testing "Should parse se, sw, ne, and nw as separate tokens"
    (is (= '(:se :sw :ne :nw :nw :sw :ne :se)
           (parse-input-line "seswnenwnwswnese"))))
  (testing "Should parse a complex string"
    (is (= '(:ne :e :e :ne :se :nw :nw :w :sw :ne :ne :w :nw :w :se :w :ne :nw :se :sw :e :sw)
           (parse-input-line "neeenesenwnwwswnenewnwwsewnenwseswesw")))))

(deftest get-coordinates-test
  (testing "Should calculate coordinates"
    (is (= '(-1 0) (get-coordinate '(:w))))
    (is (= '(1 0) (get-coordinate '(:e))))
    (is (= '(0 1) (get-coordinate '(:nw))))
    (is (= '(0 -1) (get-coordinate '(:se))))
    (is (= '(1 1) (get-coordinate '(:ne))))
    (is (= '(-1 -1) (get-coordinate '(:sw))))
    (is (= '(0 0) (get-coordinate '(:nw :w :sw :e :e))))))

(deftest expand-grid-test
  (testing "Should expand a state grid by +- 1 in x and y"
    (is (= {-1 {-1 :white 0 :white 1 :white}
            0 {-1 :white 0 :black 1 :white}
            1 {-1 :white 0 :white 1 :white}}
           (expand-grid {0 {0 :black}})))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "10\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "2208\n"
           (with-out-str (with-minimal-output (main-2)))))))
