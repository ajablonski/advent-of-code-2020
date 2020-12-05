(ns advent-of-code-2020.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day-3-input (io/resource "day3.txt"))

(defn get-col
  [& {:keys [row-num grid-width step-size]}]
  (mod (* row-num step-size) grid-width))

(defn get-tree-count
  [col row-string]
  (if (= (.charAt ^String row-string col) \#) 1 0))

(defn get-trees-in-path
  [col-step row-step grid]
  (let [adjusted-grid (vec (keep-indexed #(if (= (mod %1 row-step) 0) %2) grid))]
    (reduce-kv
      (fn [total row-num row]
        (+
          total
          (let [col-num (get-col :row-num row-num :grid-width (count (get grid 0)) :step-size col-step)]
            (get-tree-count col-num row))))
      0
      adjusted-grid)))

(defn main-1
  []
  (println
    (get-trees-in-path 3 1 (str/split-lines (slurp day-3-input)))))

(defn main-2
  []
  (let [col-and-row-steps ['(1 1) '(3 1) '(5 1) '(7 1) '(1 2)]]
    (println
      (reduce
        (fn [product [col-step row-step]]
          (*
            product
            (get-trees-in-path col-step row-step (str/split-lines (slurp day-3-input)))))
        1
        col-and-row-steps))))
