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
  (first (keep-indexed
           (fn [idx item]
             (if (and (>= idx preamble-size)
                      (let [subvector (subvec number-vector (- idx preamble-size) idx)]
                        (not
                          (has-numbers-summing-to?
                            item
                            (set
                              subvector)))))
               item))
           number-vector)))

(defn main-1
  ([] (main-1 25))
  ([preamble-size]
   (let
     [numbers (vec (map bigint day-9-input))]
     (println
       (find-invalid-entry numbers preamble-size)))))

(defn main-2
  ([] (main-2 25))
  ([preamble-size]
   (let
     [numbers (vec (map bigint day-9-input))
      invalid-entry (find-invalid-entry numbers preamble-size)
      range-nums (loop [start-idx 0
                   end-idx 0]
              (let [subvector (subvec numbers start-idx end-idx)
                    sum-in-range (reduce + subvector)]
                (cond (< sum-in-range invalid-entry) (recur start-idx (+ end-idx 1))
                      (> sum-in-range invalid-entry) (recur (+ start-idx 1) end-idx)
                      :else subvector)))]
     (println
       (+ (reduce min range-nums) (reduce max range-nums))))))
