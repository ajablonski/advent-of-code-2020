(ns advent-of-code-2020.day11-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day11 :refer :all]))

(deftest parse-seat-grid-test
  (testing "Should parse seat grid"
    (is (= [[\L \. \.] [\# \# \#] [\. \L \.]]
           (parse-seat-grid "L..\n###\n.L.\n")))))

(deftest step-test
  (testing "Should step forward one state when only rule 1 applies"
    (is (= [[\. \. \.] [\. \# \.] [\. \. \.]]
           (step ["..." ".L." "..."])))
    (is (= [[\. \. \.] [\. \# \.] [\. \. \.]]
           (step (step ["..." ".L." "..."]))))
    (is (= [[\. \# \.] [\# \# \#] [\. \. \.]]
           (step [".L." "LLL" "..."])))
    (is (= [[\. \# \.] [\# \# \#] [\. \# \.]]
           (step [".L." "LLL" ".L."])))
    (is (= [[\. \# \.] [\# \L \#] [\. \# \.]]
           (step [[\. \# \.] [\# \# \#] [\. \# \.]])))
    (is (= [[\. \# \.] [\# \L \#] [\. \# \.]]
           (step [[\. \# \.] [\# \L \#] [\. \# \.]])))
    ))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "37\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "\n"
           (with-out-str (main-2))))))
