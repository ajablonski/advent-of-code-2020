(ns advent-of-code-2020.day2_test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day2 :refer :all])
  (:import (advent_of_code_2020.day2 Rule RuleAndPassword)))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "2\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "1\n"
           (with-out-str (main-2))))))

(deftest pw-rule-test
  (testing "Fails when password has too few of letter"
    (is (false? (.pw-matches? (Rule. 1 2 \a) "bbb"))))
  (testing "Fails when password has too many of letter"
    (is (false? (.pw-matches? (Rule. 1 2 \a) "aaa"))))
  (testing "True when password has just allowed number of letter"
    (is (true? (.pw-matches? (Rule. 1 3 \a) "a")))
    (is (true? (.pw-matches? (Rule. 1 3 \a) "aa")))
    (is (true? (.pw-matches? (Rule. 1 3 \a) "aaa")))
    (is (true? (.pw-matches? (Rule. 1 3 \a) "babab")))))

(deftest pw-rule-2-test
  (testing "Fails when password does not contain letter at either index"
    (is (false? (.pw-matches-rule-2? (Rule. 1 2 \a) "bb"))))
  (testing "Fails when password contains letter at both indices"
    (is (false? (.pw-matches-rule-2? (Rule. 1 4 \a) "aaaa"))))
  (testing "True when password has letter at just one index"
    (is (true? (.pw-matches-rule-2? (Rule. 1 3 \a) "abb")))
    (is (true? (.pw-matches-rule-2? (Rule. 1 3 \a) "bba")))))

(deftest parse-line-test
  (testing "Can parse a line"
    (is (= (RuleAndPassword. (Rule. 6 7 \z) "dqzzzjbzz")
           (parse-line "6-7 z: dqzzzjbzz")))))

(deftest parse-lines-test
  (testing "Can parse multiple lines"
    (is (=
          (parse-lines "6-7 z: dqzzzjbzz\n1-5 a: sososos")
          [
           (RuleAndPassword. (Rule. 6 7 \z) "dqzzzjbzz")
           (RuleAndPassword. (Rule. 1 5 \a) "sososos")
           ]))))

(run-all-tests)
