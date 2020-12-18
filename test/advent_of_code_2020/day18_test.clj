(ns advent-of-code-2020.day18-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day18 :refer :all]))


(deftest evaluate-expression-test
  (testing "Should perform addition"
    (is (= 6
           (evaluate-expression-equal-precedence (parse-expression-equal-precedence "1 + 2 + 3")))))
  (testing "Should perform multiplication"
    (is (= 24
           (evaluate-expression-equal-precedence (parse-expression-equal-precedence "1 * 2 * 3 * 4")))))
  (testing "Should perform mix addition and multiplication"
    (is (= 71
           (evaluate-expression-equal-precedence (parse-expression-equal-precedence "1 + 2 * 3 + 4 * 5 + 6")))))
  (testing "Should handle operation in parentheses"
    (is (= 3
           (evaluate-expression-equal-precedence (parse-expression-equal-precedence "(1 + 2)")))))
  (testing "Should handle nested parentheses"
    (is (= 12240
           (evaluate-expression-equal-precedence (parse-expression-equal-precedence "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))))
    (is (= 13632
           (evaluate-expression-equal-precedence (parse-expression-equal-precedence "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))
    )
  )


(deftest evaluate-expression-addition-precedence-test
  (testing "Should perform addition"
    (is (= 6
           (evaluate-expression-addition-precedence (parse-expression-addition-precedence "1 + 2 + 3")))))
  (testing "Should perform multiplication"
    (is (= 24
           (evaluate-expression-addition-precedence (parse-expression-addition-precedence "1 * 2 * 3 * 4")))))
  (testing "Should perform mix addition and multiplication"
    (is (= 231
           (evaluate-expression-addition-precedence (parse-expression-addition-precedence "1 + 2 * 3 + 4 * 5 + 6")))))
  (testing "Should handle operation in parentheses"
    (is (= 3
           (evaluate-expression-addition-precedence (parse-expression-addition-precedence "(1 + 2)")))))
  (testing "Should handle nested parentheses"
    (is (= 669060
           (evaluate-expression-addition-precedence (parse-expression-addition-precedence "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))))
    (is (= 23340
           (evaluate-expression-addition-precedence (parse-expression-addition-precedence "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))
    )
  )

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "26406\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "694122\n"
           (with-out-str (main-2))))))
