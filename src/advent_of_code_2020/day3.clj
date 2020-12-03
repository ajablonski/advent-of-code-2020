(ns advent-of-code-2020.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day-3-input (io/resource "day3/input1.txt"))

(defn get-col
  [& {:keys [row-num grid-width step-size]}]
  (mod (* row-num step-size) grid-width))

(defn get-tree-count
  [col row-string]
  (if (= (.charAt ^String row-string col) \#) 1 0))

(defn get-total-tree-count
  [col-step row-step grid]
  (let [adjusted-grid (vec (keep-indexed #(if (= (mod %1 row-step) 0) %2) grid))]
    (reduce-kv
      (fn [total row-num row]
        (+ total
           (get-tree-count
             (get-col :row-num row-num :grid-width (count (get grid 0)) :step-size col-step)
             row)))
      0
      adjusted-grid)))

(defn main-1
  []
  (println
    (get-total-tree-count 3 1 (str/split-lines (slurp day-3-input)))))

(defn main-2
  []
  (let [grid (str/split-lines (slurp day-3-input))]
    (println
      (*
        (get-total-tree-count 1 1 grid)
        (get-total-tree-count 3 1 grid)
        (get-total-tree-count 5 1 grid)
        (get-total-tree-count 7 1 grid)
        (get-total-tree-count 1 2 grid)))))
