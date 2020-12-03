(ns advent-of-code-2020.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day-3-input (io/resource "day3/input1.txt"))

(defrecord Position [x y])

(defn get-col
  [row grid-width step-size]
  (mod (* row step-size) grid-width))

(defn get-tree-count
  [row col grid]
  (if (= (.charAt ^String (get grid row) col) \#) 1 0))

(defn get-total-tree-count
  [col-step row-step grid]
  (let [adjusted-grid (vec (keep-indexed #(if (= (mod %1 row-step) 0) %2) grid))
        grid-width (count (get grid 0))
        tree-counts (map-indexed
                      (fn [index _]
                        (get-tree-count index (get-col index grid-width col-step) adjusted-grid))
                      adjusted-grid)]
    (reduce + tree-counts)))

(defn main-1
  []
  (println
    (get-total-tree-count 3 1 (str/split-lines (slurp day-3-input)))))

(defn main-2
  []
  (let [grid (str/split-lines (slurp day-3-input))
        slope-1 (get-total-tree-count 1 1 grid)
        slope-2 (get-total-tree-count 3 1 grid)
        slope-3 (get-total-tree-count 5 1 grid)
        slope-4 (get-total-tree-count 7 1 grid)
        slope-5 (get-total-tree-count 1 2 grid)]
    (println
      (* slope-1 slope-2 slope-3 slope-4 slope-5))))
