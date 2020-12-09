(ns advent-of-code-2020.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day-9-input (str/split-lines (slurp (io/resource "day9.txt"))))

(defn has-numbers-summing-to?
  [result number-set]
  (not
    (empty?
      (filter
        (fn [n]
          (contains? (disj number-set n) (- result n)))
        number-set))))

(defn find-invalid-entry
  [number-vector preamble-size]
  (first
    (filter
      (fn [[previous-entry-set item]]
        (not
          (has-numbers-summing-to?
            item
            previous-entry-set)))
      (map #(list
              (set (subvec number-vector %1 (+ %1 preamble-size)))
              %2)
           (range)
           (subvec number-vector preamble-size)))))

(defn find-contiguous-entries-summing-to
  [number-vector target-number]
  (loop [start-idx 0
         end-idx 0]
    (let [subvector (subvec number-vector start-idx end-idx)
          sum-in-range (reduce + subvector)]
      (cond (< sum-in-range target-number) (recur start-idx (+ end-idx 1))
            (> sum-in-range target-number) (recur (+ start-idx 1) end-idx)
            :else subvector))))

(defn main-1
  ([] (main-1 25))
  ([preamble-size]
   (let
     [numbers (vec (map bigint day-9-input))]
     (println
       (second (find-invalid-entry numbers preamble-size))))))

(defn main-2
  ([] (main-2 25))
  ([preamble-size]
   (let
     [numbers (vec (map bigint day-9-input))
      [_ invalid-entry] (find-invalid-entry numbers preamble-size)
      range-nums (find-contiguous-entries-summing-to numbers invalid-entry)]
     (println
       (+ (reduce min range-nums) (reduce max range-nums))))))
