(ns advent-of-code-2020.day8-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day8 :refer :all]))

(deftest switch-instr-test
  (testing "Replace instruction correctly"
    (is (= "nop +5"
           (switch-instr "jmp +5")))
    (is (= "jmp +5"
           (switch-instr "nop +5")))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "5\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "8\n"
           (with-out-str (main-2))))))
