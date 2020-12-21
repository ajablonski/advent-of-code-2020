(ns advent-of-code-2020.day20-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day20 :refer :all]
            [clojure.java.io :as io]
            [advent-of-code-2020.utils :refer [with-minimal-output]]))


; Edges
;      0         0
;     ___       ___
;    |   |     |   |
;  3 |   | 1 3 |   | 1
;    |___|     |___|
;      2         2

;
; All combinations
; Original
; Rotate 90
; Rotate 180
; Rotate 270
; Rotate 360, Flip
; Rotate 90
; Rotate 180
; Rotate 270

; 3 sets:
; old
; active
; unplaced

; at start of each pass through:
;
; for each in "active"
; loop through all unplaced
; if place-able, add to new-active and remove from unplaced

; at end of pass through, add all "active" to "old"
; and set active to new-active

; repeat until unplaced is empty


(deftest parse-tile-test
  (testing "Should parse tile input"
    (is (= (->Tile 2311 '("..##.#..#." "##..#....." "#...##..#." "####.#...#" "##.##.###." "##...#.###" ".#.#.#..##" "..#....#.." "###...#.#." "..###..###") nil)
           (parse-tile "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###")))))


(deftest Tile-test
  (let [initial-tile (parse-tile "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###")]
    (testing "should rotate"
      (let [rotate-90 (rotate-cc initial-tile)
            rotate-180 (rotate-cc rotate-90)
            rotate-270 (rotate-cc rotate-180)
            rotate-360 (rotate-cc rotate-270)]
        (is (= initial-tile rotate-360))
        (is (= rotate-90 (->Tile 2311 '("...#.##..#" "#.#.###.##" "....##.#.#" "....#...#." "#.##.##..." ".##.#....#" "#..##.#..#" "#..#...###" ".#.####.#." ".#####..#.") nil)))
        (is (= rotate-180 (->Tile 2311 '("###..###.." ".#.#...###" "..#....#.." "##..#.#.#." "###.#...##" ".###.##.##" "#...#.####" ".#..##...#" ".....#..##" ".#..#.##..") nil)))
        (is (= rotate-270 (->Tile 2311 '(".#..#####." ".#.####.#." "###...#..#" "#..#.##..#" "#....#.##." "...##.##.#" ".#...#...." "#.#.##...." "##.###.#.#" "#..##.#...") nil)))))
    (testing "should flip on y axis"
      (let [flip-once (flip-y initial-tile)
            flip-twice (flip-y flip-once)]
        (is (= flip-twice initial-tile)
            (= flip-once (->Tile 2311 nil nil)))))
    (testing "should match with another tile"
      (let [initial-tile (->Tile 1427 '("..##.#..#." "..#..###.#" ".#.####.#." "...#.#####" "...##..##." "....#...##" "#.#.#.##.#" ".#.##.#..#" ".#..#.##.." "###.##.#..") nil)
            left-tile (->Tile 2729 '("#.##...##." "##..#.##.." "##.####..." "####.#.#.." ".#.####..." ".##..##.#." "....#..#.#" "..#.#....." "####.#...." "...#.#.#.#") nil)
            right-tile (->Tile 2473 '("..#.###..." "##.##....#" "..#.###..#" "###.#..###" ".######.##" "#.#.#.#..." "#.###.###." "#.###.##.." ".######..." ".##...####") nil)
            bottom-tile (->Tile 1489 '("###.##.#.." "..##.##.##" "##.#...##." "...#.#.#.." "#..#.#.#.#" "#####...#." "..#...#..." ".##..##..." "..##...#.." "##.#.#....") nil)
            top-tile (->Tile 2311 '("..###..###" "###...#.#." "..#....#.." ".#.#.#..##" "##...#.###" "##.##.###." "####.#...#" "#...##..#." "##..#....." "..##.#..#.") nil)]
        (is (= (match-or-nil initial-tile left-tile)
               '(-1 0)))
        (is (= (match-or-nil initial-tile right-tile)
               '(1 0)))
        (is (= (match-or-nil initial-tile bottom-tile)
               '(0 -1)))
        (is (= (match-or-nil initial-tile top-tile)
               '(0 1)))
        (is (nil? (match-or-nil initial-tile initial-tile)))))
    (testing "should strip border"
      (is (= (->Tile 2311 '("#..#...." "...##..#" "###.#..." "#.##.###" "#...#.##" "#.#.#..#" ".#....#." "##...#.#") nil)
             (strip-border initial-tile))))
    (testing "should append other tile to right"
      (is (= (->Tile 2311 '("#..#............" "...##..#........" "###.#..........." "#.##.###........" "#...#.##........" "#.#.#..#........" ".#....#........." "##...#.#########") '(nil 1))
             (combine-tile (->Tile 2311 '("#..#...." "...##..#" "###.#..." "#.##.###" "#...#.##" "#.#.#..#" ".#....#." "##...#.#") '(2 1))
                           (->Tile 1 '("........" "........" "........" "........" "........" "........" "........" "########") '(3 1))))))))

;           ..###..###
;           ###...#.#.
;           ..#....#..
;           .#.#.#..##
;           ##...#.###
;           ##.##.###.
;           ####.#...#
;           #...##..#.
;           ##..#.....
;           ..##.#..#.

;#.##...##. ..##.#..#. ..#.###...
;##..#.##.. ..#..###.# ##.##....#
;##.####... .#.####.#. ..#.###..#
;####.#.#.. ...#.##### ###.#..###
;.#.####... ...##..##. .######.##
;.##..##.#. ....#...## #.#.#.#...
;....#..#.# #.#.#.##.# #.###.###.
;..#.#..... .#.##.#..# #.###.##..
;####.#.... .#..#.##.. .######...
;...#.#.#.# ###.##.#.. .##...####

;           ###.##.#..
;           ..##.##.##
;           ##.#...##.
;           ...#.#.#..
;           #..#.#.#.#
;           #####...#.
;           ..#...#...
;           .##..##...
;           ..##...#..
;           ##.#.#....

(deftest try-tile-possibilities-test
  (testing "Should try to place other tile with respect to first tile"
    (let [tiles (parse-tiles day-20-input)
          center-tile (assoc (rotate-cc (rotate-cc (flip-y (nth tiles 3)))) :coordinates '(0 0))]
      (is (= (try-place-tile center-tile (first tiles))
             (->Tile 2311 '("..###..###" "###...#.#." "..#....#.." ".#.#.#..##" "##...#.###" "##.##.###." "####.#...#" "#...##..#." "##..#....." "..##.#..#.") '(0 1)))))))

(deftest join-tiles-test
  (testing "Should join coordinates that already have coordinates"
    (let [joined-tile (join-tiles (find-tile-positions (parse-tiles day-20-input)))]
      (is (= joined-tile
             (->Tile 0 '("...###...##...#...#..###" ".#.###..##..##..####.##." "#.##..#..#...#..####...#" "#####..#####...###....##" "#..####...#.#.#.###.###." "..#.#..#..#.#.#.####.###" ".####.###.#...###.#..#.#" ".#.#.###.##.##.#..#.##.." "###.#...#..#.##.######.." ".#.#....#.##.#...###.##." "...#..#..#.#.##..###.###" "##..##.#...#...#.#.#.#.." "#.####....##..########.#" "###.#.#...#.######.#..##" "#.####..#.####.#.#.###.." "#..#.##..#..###.#.##...." ".####...#..#.....#......" "....#..#...##..#.#.###.." "...########.#....#####.#" "##.#....#.##.####...#.##" "###.#####...#.#####.#..#" "##.##.###.#.#..######..." "###....#.#....#..#......" ".#.#..#.##...#.##..#####") '(0 0)))))))

(deftest count-monsters-test
  (testing "Should count the monster locations"
    (is (= 1
          (count-monsters (->Tile 0 '(".####...#####..#...###.." "#####..#..#.#.####..#.#." ".#.#...#.###...#.##.##.." "#.#.##.###.#.##.##.#####" "..##.###.####..#.####.##" "...#.#..##.##...#..#..##") '(0 0)))))))

(deftest integration-test-main-1
  (testing "Integration test part 1"
    (is (= "20899048083289\n"
           (with-out-str (main-1))))))

(deftest integration-test-main-2
  (testing "Integration test part 2"
    (is (= "273\n"
           (with-out-str (with-minimal-output (main-2)))))))
