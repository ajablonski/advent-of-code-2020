(ns advent-of-code-2020.day17-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day17 :refer :all]))

(deftest parse-initial-state-test
  (testing "Parse initial state string"
    (is (= {0 {0 {0 \.} 1 {0 \.} 2 {0 \#}}
            1 {0 {0 \#} 1 {0 \.} 2 {0 \#}}
            2 {0 {0 \.} 1 {0 \#} 2 {0 \#}}}
           (parse-initial-state ".#.\n..#\n###\n")))))

(deftest expand-state-test
  (testing "Expand state grid to next level"
    (is (= {-1 {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \. 1 \.} 1 {-1 \. 0 \. 1 \.} 2 {-1 \. 0 \. 1 \.} 3 {-1 \. 0 \. 1 \.}}
            0  {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \. 1 \.} 1 {-1 \. 0 \. 1 \.} 2 {-1 \. 0 \# 1 \.} 3 {-1 \. 0 \. 1 \.}}
            1  {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \# 1 \.} 1 {-1 \. 0 \. 1 \.} 2 {-1 \. 0 \# 1 \.} 3 {-1 \. 0 \. 1 \.}}
            2  {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \. 1 \.} 1 {-1 \. 0 \# 1 \.} 2 {-1 \. 0 \# 1 \.} 3 {-1 \. 0 \. 1 \.}}
            3  {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \. 1 \.} 1 {-1 \. 0 \. 1 \.} 2 {-1 \. 0 \. 1 \.} 3 {-1 \. 0 \. 1 \.}}}
           (expand-state {0 {0 {0 \.} 1 {0 \.} 2 {0 \#}}
                          1 {0 {0 \#} 1 {0 \.} 2 {0 \#}}
                          2 {0 {0 \.} 1 {0 \#} 2 {0 \#}}})))))


(deftest count-active-neighbors-test
  (testing "Should could active neighbors of coordinate"
    (let [state (expand-state (parse-initial-state ".#.\n..#\n###\n"))]
      (is (= 1
             (count-active-neighbors state (get-neighbor-coordinates [1 0 0]))))
      (is (= 3
             (count-active-neighbors state (get-neighbor-coordinates [1 3 0])))))))


(deftest update-state-test
  (testing "Updating state should apply the applicable rules"
    (is (= "Z =  -1\n.....\n.....\n.#...\n...#.\n..#..
Z =  0\n.....\n.....\n.#.#.\n..##.\n..#..
Z =  1\n.....\n.....\n.#...\n...#.\n..#..\n"
           (with-out-str (print-grid
             (update-state
               {-1 {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \. 1 \.} 1 {-1 \. 0 \. 1 \.} 2 {-1 \. 0 \. 1 \.} 3 {-1 \. 0 \. 1 \.}}
                0  {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \. 1 \.} 1 {-1 \. 0 \. 1 \.} 2 {-1 \. 0 \# 1 \.} 3 {-1 \. 0 \. 1 \.}}
                1  {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \# 1 \.} 1 {-1 \. 0 \. 1 \.} 2 {-1 \. 0 \# 1 \.} 3 {-1 \. 0 \. 1 \.}}
                2  {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \. 1 \.} 1 {-1 \. 0 \# 1 \.} 2 {-1 \. 0 \# 1 \.} 3 {-1 \. 0 \. 1 \.}}
                3  {-1 {-1 \. 0 \. 1 \.} 0 {-1 \. 0 \. 1 \.} 1 {-1 \. 0 \. 1 \.} 2 {-1 \. 0 \. 1 \.} 3 {-1 \. 0 \. 1 \.}}})))))))

(deftest get-neighbor-coordinates-test
  (testing "Get coordinates of all neighbors"
    (is (= (list
             '(-1 0 1) '(-1 0 2) '(-1 0 3)
             '(-1 1 1) '(-1 1 2) '(-1 1 3)
             '(-1 2 1) '(-1 2 2) '(-1 2 3)
             '(0 0 1) '(0 0 2) '(0 0 3)
             '(0 1 1) '(0 1 3)
             '(0 2 1) '(0 2 2) '(0 2 3)
             '(1 0 1) '(1 0 2) '(1 0 3)
             '(1 1 1) '(1 1 2) '(1 1 3)
             '(1 2 1) '(1 2 2) '(1 2 3))
           (get-neighbor-coordinates [0 1 2])))))

(deftest count-occupied-test
  (testing "Should count occupied entries"))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "112\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "848\n"
           (with-out-str (main-2))))))
