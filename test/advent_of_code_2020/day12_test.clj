(ns advent-of-code-2020.day12-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day12 :refer :all]))

(deftest move-n-test
  (testing "Should move to the N"
    (is (= ({:x 0 :y 1 :direction :W}
            (move-n {:x 0 :y 0 :direction :W} 1))))))

(deftest move-e-test
  (testing "Should move to the E"
    (is (= ({:x 1 :y 0 :direction :W}
            (move-e {:x 0 :y 0 :direction :W} 1))))))

(deftest move-w-test
  (testing "Should move to the W"
    (is (= ({:x -1 :y 0 :direction :E}
            (move-w {:x 0 :y 0 :direction :E} 1))))))

(deftest move-s-test
  (testing "Should move to the S"
    (is (= ({:x 0 :y -1 :direction :E}
            (move-s {:x 0 :y 0 :direction :E} 1))))))

(deftest move-f-test
  (testing "Should move forward in current direction"
    (is (= ({:x 0 :y 1 :direction :W}
            (move-forward {:x 0 :y 0 :direction :N} 1))))
    (is (= ({:x 1 :y 0 :direction :W}
            (move-forward {:x 0 :y 0 :direction :E} 1))))
    (is (= ({:x 0 :y -1 :direction :W}
            (move-forward {:x 0 :y 0 :direction :S} 1))))
    (is (= ({:x -1 :y 0 :direction :W}
            (move-forward {:x 0 :y 0 :direction :W} 1))))))

(deftest turn-l-test
  (testing "Should turn left"
    (is (= ({:x 0 :y 0 :direction :W}
            (turn-left {:x 0 :y 0 :direction :N} 90))))
    (is (= ({:x 0 :y 0 :direction :N}
            (turn-left {:x 0 :y 0 :direction :E} 90))))
    (is (= ({:x 0 :y 0 :direction :E}
            (turn-left {:x 0 :y 0 :direction :S} 90))))
    (is (= ({:x 0 :y 0 :direction :S}
            (turn-left {:x 0 :y 0 :direction :W} 90))))
    (is (= ({:x 0 :y 0 :direction :S}
            (turn-left {:x 0 :y 0 :direction :N} 180))))
    (is (= ({:x 0 :y 0 :direction :W}
            (turn-left {:x 0 :y 0 :direction :E} 180))))
    (is (= ({:x 0 :y 0 :direction :N}
            (turn-left {:x 0 :y 0 :direction :S} 180))))
    (is (= ({:x 0 :y 0 :direction :E}
            (turn-left {:x 0 :y 0 :direction :W} 180))))))

(deftest turn-r-test
  (testing "Should turn right"
    (is (= ({:x 0 :y 0 :direction :E}
            (turn-right {:x 0 :y 0 :direction :N} 90))))
    (is (= ({:x 0 :y 0 :direction :S}
            (turn-right {:x 0 :y 0 :direction :E} 90))))
    (is (= ({:x 0 :y 0 :direction :W}
            (turn-right {:x 0 :y 0 :direction :S} 90))))
    (is (= ({:x 0 :y 0 :direction :N}
            (turn-right {:x 0 :y 0 :direction :W} 90))))
    (is (= ({:x 0 :y 0 :direction :E}
            (turn-right {:x 0 :y 0 :direction :W} 180))))
    (is (= ({:x 0 :y 0 :direction :S}
            (turn-right {:x 0 :y 0 :direction :N} 180))))
    (is (= ({:x 0 :y 0 :direction :W}
            (turn-right {:x 0 :y 0 :direction :E} 180))))
    (is (= ({:x 0 :y 0 :direction :N}
            (turn-right {:x 0 :y 0 :direction :S} 180))))))

(deftest move-waypoint-n-test
  (testing "Should move waypoint to the N"
    (is (= ({:x 0 :y 0 :waypoint-x 0 :waypoint-y 1}
            (move-waypoint-n {:x 0 :y 0 :waypoint-x 0 :waypoint-y 0} 1))))))

(deftest move-waypoint-e-test
  (testing "Should move waypoint to the E"
    (is (= ({:x 0 :y 0 :waypoint-x 1 :waypoint-y 0}
            (move-waypoint-e {:x 0 :y 0 :waypoint-x 0 :waypoint-y 0} 1))))))

(deftest move-waypoint-w-test
  (testing "Should move waypoint to the W"
    (is (= ({:x 0 :y 0 :waypoint-x -1 :waypoint-y 0}
            (move-waypoint-w {:x 0 :y 0 :waypoint-x 0 :waypoint-y 0} 1))))))

(deftest move-waypoint-s-test
  (testing "Should move waypoint to the S"
    (is (= ({:x 0 :y 0 :waypoint-x 0 :waypoint-y -1}
            (move-waypoint-s {:x 0 :y 0 :waypoint-x 0 :waypoint-y 0} 1))))))

(deftest move-towards-waypoint-test
  (testing "Should move forward towards waypoint"
    (is (= ({:x 2 :y 1 :waypoint-x 2 :waypoint-y 1}
            (move-towards-waypoint {:x 0 :y 0 :waypoint-x 2 :waypoint-y 1} 1))))
    (is (= ({:x 6 :y 3 :waypoint-x 2 :waypoint-y 1}
            (move-towards-waypoint {:x 0 :y 0 :waypoint-x 2 :waypoint-y 1} 3))))))

(deftest rotate-waypoint-l-test
  (testing "Should rotate waypoint left"
    (is (= {:x 0 :y 0 :waypoint-x -4 :waypoint-y 10}
           (rotate-waypoint-l {:x 0 :y 0 :waypoint-x 10 :waypoint-y 4} 90)))
    (is (= {:x 0 :y 0 :waypoint-x -10 :waypoint-y -4}
           (rotate-waypoint-l {:x 0 :y 0 :waypoint-x -4 :waypoint-y 10} 90)))
    (is (= {:x 0 :y 0 :waypoint-x 4 :waypoint-y -10}
           (rotate-waypoint-l {:x 0 :y 0 :waypoint-x -10 :waypoint-y -4} 90)))
    (is (= {:x 0 :y 0 :waypoint-x 10 :waypoint-y 4}
           (rotate-waypoint-l {:x 0 :y 0 :waypoint-x 4 :waypoint-y -10} 90)))
    (is (= {:x 0 :y 0 :waypoint-x -10 :waypoint-y -4}
           (rotate-waypoint-l {:x 0 :y 0 :waypoint-x 10 :waypoint-y 4} 180)))
    (is (= {:x 0 :y 0 :waypoint-x 4 :waypoint-y -10}
           (rotate-waypoint-l {:x 0 :y 0 :waypoint-x -4 :waypoint-y 10} 180)))
    (is (= {:x 0 :y 0 :waypoint-x 10 :waypoint-y 4}
           (rotate-waypoint-l {:x 0 :y 0 :waypoint-x -10 :waypoint-y -4} 180)))
    (is (= {:x 0 :y 0 :waypoint-x -4 :waypoint-y 10}
           (rotate-waypoint-l {:x 0 :y 0 :waypoint-x 4 :waypoint-y -10} 180)))))

(deftest rotate-waypoint-r-test
  (testing "Should rotate waypoint right"
    (is (= {:x 0 :y 0 :waypoint-x 4 :waypoint-y -10}
           (rotate-waypoint-r {:x 0 :y 0 :waypoint-x 10 :waypoint-y 4} 90)))
    (is (= {:x 0 :y 0 :waypoint-x 10 :waypoint-y 4}
           (rotate-waypoint-r {:x 0 :y 0 :waypoint-x -4 :waypoint-y 10} 90)))
    (is (= {:x 0 :y 0 :waypoint-x -4 :waypoint-y 10}
           (rotate-waypoint-r {:x 0 :y 0 :waypoint-x -10 :waypoint-y -4} 90)))
    (is (= {:x 0 :y 0 :waypoint-x -10 :waypoint-y -4}
           (rotate-waypoint-r {:x 0 :y 0 :waypoint-x 4 :waypoint-y -10} 90)))
    (is (= {:x 0 :y 0 :waypoint-x -10 :waypoint-y -4}
           (rotate-waypoint-r {:x 0 :y 0 :waypoint-x 10 :waypoint-y 4} 180)))
    (is (= {:x 0 :y 0 :waypoint-x 4 :waypoint-y -10}
           (rotate-waypoint-r {:x 0 :y 0 :waypoint-x -4 :waypoint-y 10} 180)))
    (is (= {:x 0 :y 0 :waypoint-x 10 :waypoint-y 4}
           (rotate-waypoint-r {:x 0 :y 0 :waypoint-x -10 :waypoint-y -4} 180)))
    (is (= {:x 0 :y 0 :waypoint-x -4 :waypoint-y 10}
           (rotate-waypoint-r {:x 0 :y 0 :waypoint-x 4 :waypoint-y -10} 180)))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "25\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "286\n"
           (with-out-str (main-2))))))
