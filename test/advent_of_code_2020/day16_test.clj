(ns advent-of-code-2020.day16-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day16 :refer :all]
            [advent-of-code-2020.utils :refer (with-minimal-output)])
  (:import (advent_of_code_2020.day16 Rule)))

(deftest parse-rules-section-test
  (testing "Should return list of rules"
    (is (= (list (Rule. "class" [1 3] [5 7])
                 (Rule. "row" [6 11] [33 44])
                 (Rule. "seat" [13 40] [45 50]))
           (parse-rules-section "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50")))))

(deftest parse-my-ticket-section-test
  (testing "Should parse my ticket"
    (is (= (list 7, 1, 14)
           (parse-my-ticket-section "your ticket:\n7,1,14")))))

(deftest parse-other-ticket-section-test
  (testing "Should return list of tickets"
    (is (= (list '(7, 3, 47)
                 '(40, 4, 50)
                 '(55, 2, 20)
                 '(38, 6, 12))
           (parse-other-ticket-section "nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")))))

(deftest rule-test
  (let [rule (Rule. "name" [1 5] [9 12])]
    (testing "Should match correctly"
      (testing "Should match in first range"
        (is (true? (matches? rule 1)))
        (is (true? (matches? rule 3)))
        (is (true? (matches? rule 5))))
      (testing "Should match in second range"
        (is (true? (matches? rule 9)))
        (is (true? (matches? rule 11)))
        (is (true? (matches? rule 12))))
      (testing "Should not match below first range"
        (is (false? (matches? rule 0))))
      (testing "Should not match between ranges"
        (is (false? (matches? rule 6)))
        (is (false? (matches? rule 7)))
        (is (false? (matches? rule 8))))
      (testing "Should not match above second range"
        (is (false? (matches? rule 13)))))))

(deftest parse-sections-test
  (testing "Should parse the separate sections"
    (with-redefs
      [parse-rules-section (fn [body] (when (= body "A") :rules-section))
       parse-my-ticket-section (fn [body] (when (= body "B") :my-ticket-section))
       parse-other-ticket-section (fn [body] (when (= body "C") :other-ticket-section))]
      (is (= {:rules         :rules-section
              :my-ticket     :my-ticket-section
              :other-tickets :other-ticket-section}
             (parse-sections "A\n\nB\n\nC"))))))

(deftest get-valid-tickets-test
  (testing "Should only return valid tickets"
    (is (= (list '(7 3 47))
           (get-valid-tickets (list '(7, 3, 47)
                                    '(40, 4, 50)
                                    '(55, 2, 20)
                                    '(38, 6, 12))
                              (list (Rule. "class" [1 3] [5 7])
                                    (Rule. "row" [6 11] [33 44])
                                    (Rule. "seat" [13 40] [45 50])))))))

(deftest get-possible-rules-test
  (let [class-rule (Rule. "class" [1 3] [5 7])
        row-rule (Rule. "row" [6 11] [33 44])
        seat-rule (Rule. "seat" [13 40] [45 50])]
    (testing "Should return set of possible rules for each field in the ticket"
      (is (= (list
               #{class-rule row-rule}
               #{class-rule}
               #{seat-rule}
               )
             (get-possible-rules-per-ticket-index '(7 3 47) (list class-rule
                                                                  row-rule
                                                                  seat-rule)))))))

(deftest assign-fields-to-ticket-test
  (testing "Should assign fields to a ticket based on known indices of rules"
    (is (= {"class" 1
            "row"   7
            "seat"  14}
           (assign-fields-to-ticket '(7 1 14)
                                    {0 "row"
                                     1 "class"
                                     2 "seat"})))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "71\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "21\n"
           (with-out-str (with-minimal-output (main-2)))))))
