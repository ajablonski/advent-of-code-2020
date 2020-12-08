(ns advent-of-code-2020.day8-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day8 :refer :all]))

(deftest parse-instruction-test
  (testing "Parse and apply acc instruction"
    (is (= '(12 1)
           ((parse-instruction "acc +10") '(2 0))))
    (is (= '(-8 1)
           ((parse-instruction "acc -10") '(2 0)))))
  (testing "Parse and apply jump instruction"
    (is (= '(2 30)
           ((parse-instruction "jmp +10") '(2 20))))
    (is (= '(2 10)
           ((parse-instruction "jmp -10") '(2 20)))))
  (testing "Parse and apply nop instruction"
    (is (= '(2 1)
           ((parse-instruction "nop +10") '(2 0))))
    (is (= '(2 1)
           ((parse-instruction "nop -10") '(2 0))))))

(deftest parse-program-test
  (testing "Should parse full program using parse-instruction on each line"
    (with-redefs [parse-instruction (fn [_] "MOCK")]
      (is (= ["MOCK" "MOCK" "MOCK"]
             (parse-program ["jmp +10" "jmp+10" "jmp+10"]))))))

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
