(ns advent-of-code-2020.day6-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day6 :refer :all]))


(deftest parse-answers-test
  (testing "Splits out answers into sets"
    (is (= (parse-answers-string "abc\ndef\naef")
           '(#{\a \b \c} #{\d \e \f} #{\a \e \f})))))

(deftest parse-groups-string-test
  (testing "Spits out groups' answers given raw input string"
    (is (= (parse-groups-string "abc\ndef\n\na\nb")
           '((#{\a \b \c} #{\d \e \f}) (#{\a} #{\b}))))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "11\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "6\n"
           (with-out-str (main-2))))))
