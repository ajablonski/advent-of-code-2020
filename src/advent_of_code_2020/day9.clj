(ns advent-of-code-2020.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-9-input (str/split-lines (slurp (io/resource "day9.txt"))))

(defn has-no-numbers-summing-to?
  [number-set result]
  (empty?
    (filter
      #(contains? (disj number-set %) (- result %))
      number-set)))

(defn find-invalid-entry
  [number-vector preamble-size]
  (->> (subvec number-vector preamble-size)
       (map (fn [start-idx num]
              (list
                (set (subvec number-vector start-idx (+ start-idx preamble-size)))
                num))
            (range))
       (find-first #(apply has-no-numbers-summing-to? %))
       second))

(defn find-contiguous-entries-summing-to
  [number-vector target-number]
  (loop [start-idx 0
         end-idx 1]
    (let [values-to-sum (subvec number-vector start-idx end-idx)
          sum-of-values (reduce + values-to-sum)]
      (cond (< sum-of-values target-number) (recur start-idx (+ end-idx 1))
            (> sum-of-values target-number) (recur (+ start-idx 1) end-idx)
            :else values-to-sum))))

(defn main-1
  ([] (main-1 25))
  ([preamble-size]
   (println
     (find-invalid-entry
       (vec (map bigint day-9-input))
       preamble-size))))

(defn main-2
  ([] (main-2 25))
  ([preamble-size]
   (let
     [numbers (vec (map bigint day-9-input))
      invalid-entry (find-invalid-entry numbers preamble-size)
      range-nums (find-contiguous-entries-summing-to numbers invalid-entry)]
     (println
       (+ (reduce min range-nums) (reduce max range-nums))))))
