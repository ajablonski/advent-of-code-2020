(ns advent-of-code-2020.day11-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day11 :refer :all]
            [advent-of-code-2020.utils :refer (with-minimal-output)]))

(deftest parse-seat-grid-test
  (testing "Should parse seat grid"
    (is (= [[\L \. \.] [\# \# \#] [\. \L \.]]
           (parse-seat-grid "L..\n###\n.L.\n")))))

(deftest step-test
  (testing "Should step forward one state when only rule 1 applies"
    (is (= [[\. \. \.] [\. \# \.] [\. \. \.]]
           (step-1 ["..." ".L." "..."])))
    (is (= [[\. \. \.] [\. \# \.] [\. \. \.]]
           (step-1 (step-1 ["..." ".L." "..."]))))
    (is (= [[\. \# \.] [\# \# \#] [\. \. \.]]
           (step-1 [".L." "LLL" "..."])))
    (is (= [[\. \# \.] [\# \# \#] [\. \# \.]]
           (step-1 [".L." "LLL" ".L."])))
    (is (= [[\. \# \.] [\# \L \#] [\. \# \.]]
           (step-1 [[\. \# \.] [\# \# \#] [\. \# \.]])))
    (is (= [[\. \# \.] [\# \L \#] [\. \# \.]]
           (step-1 [[\. \# \.] [\# \L \#] [\. \# \.]])))
    ))

(deftest step-2-test
  (testing "second step function"
    (is (= [[\# \# \L]
            [\# \L \L]
            [\L \L \#]]
           (step-2 [[\# \# \L]
                    [\# \L \L]
                    [\L \L \L]])))
    (let [start [[\# \. \# \# \. \# \# \. \# \#]
                 [\# \# \# \# \# \# \# \. \# \#]
                 [\# \. \# \. \# \. \. \# \. \.]
                 [\# \# \# \# \. \# \# \. \# \#]
                 [\# \. \# \# \. \# \# \. \# \#]
                 [\# \. \# \# \# \# \# \. \# \#]
                 [\. \. \# \. \# \. \. \. \. \.]
                 [\# \# \# \# \# \# \# \# \# \#]
                 [\# \. \# \# \# \# \# \# \. \#]
                 [\# \. \# \# \# \# \# \. \# \#]]]
      (is (= [
              [\# \. \L \L \. \L \L \. \L \#]
              [\# \L \L \L \L \L \L \. \L \L]
              [\L \. \L \. \L \. \. \L \. \.]
              [\L \L \L \L \. \L \L \. \L \L]
              [\L \. \L \L \. \L \L \. \L \L]
              [\L \. \L \L \L \L \L \. \L \L]
              [\. \. \L \. \L \. \. \. \. \.]
              [\L \L \L \L \L \L \L \L \L \#]
              [\# \. \L \L \L \L \L \L \. \L]
              [\# \. \L \L \L \L \L \. \L \#]
              ]
             (step-2 start))))
    (is (= (parse-seat-grid "#.L#.##.L#\n#L#####.LL\nL.#.#..#..\n##L#.##.##\n#.##.#L.##\n#.#####.#L\n..#.#.....\nLLL####LL#\n#.L#####.L\n#.L####.L#")
           (step-2 (parse-seat-grid "#.LL.LL.L#\n#LLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLL#\n#.LLLLLL.L\n#.LLLLL.L#"))))

    ))

(deftest get-first-visible-in-line-test
  (testing "Should return the first visible seat type"
    (is (= \.
           (get-first-visible-in-line [\. \. \.])))
    (is (= \L
           (get-first-visible-in-line [\. \L \#])))
    (is (= \#
           (get-first-visible-in-line [\. \. \#])))
    (is (= \.
           (get-first-visible-in-line [])))
    ))

(deftest get-diagonal-visible-seats-test
  (testing "Should get all 4 diagonals starting with upper right"
    (is (= (list \# \L \. \#)
           (get-diagonal-visible-seats
             [[\. \. \. \# \L]
              [\# \. \. \. \L]
              [\. \. \. \. \L]
              [\. \. \L \. \L]
              [\. \. \. \# \L]]
             2 1)))
    (is (= (list \. \. \. \L)
           (get-diagonal-visible-seats
             [[\# \# \L]
              [\# \L \L]
              [\L \L \L]]
             2 2)))
    ))

(deftest get-horizontal-visible-seats-test
  (testing "Should get all 4 diagonals starting with upper right"
    (is (= (list \# \L)
           (get-horizontal-visible-seats
             [[\. \. \. \. \L]
              [\# \. \. \. \L]
              [\. \. \. \. \L]
              [\. \. \. \. \L]
              [\. \. \. \L \L]]
             1 2)))
    ))

(deftest get-vertical-visible-seats-test
  (testing "Should get all 4 diagonals starting with upper right"
    (is (= (list \L \#)
           (get-vertical-visible-seats
             [[\. \. \L \. \L]
              [\# \. \. \. \L]
              [\. \. \. \. \L]
              [\. \. \. \. \L]
              [\. \. \# \L \L]]
             2 2)))
    ))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "37\n"
           (with-out-str (with-minimal-output (main-1)))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "26\n"
           (with-out-str (with-minimal-output (main-2)))))))
