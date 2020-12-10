(ns advent-of-code-2020.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-10-input (slurp (io/resource "day10.txt")))

(defn get-joltage-differences
  [input-adapters]
  (let [differences (map (fn [input output] (- output input))
                         (sort (conj input-adapters 0))
                         (sort (conj input-adapters (+ (apply max input-adapters) 3))))]
    (map-vals count (group-by identity differences))))

(defn get-arrangements
  [starting-item remaining-items result-so-far]
  (let [next-item-possibilities (filter #(<= % (+ starting-item 3)) remaining-items)]
    (if (empty? remaining-items)
      (list (reverse (cons starting-item result-so-far)))
      (mapcat (fn [next-item]
             (get-arrangements next-item
                               (filter #(> % next-item) remaining-items)
                               (cons starting-item result-so-far)))
           next-item-possibilities))))

(defn get-arrangements-wrapper
  [input-adapters]
  (let [sorted-adapters (sort
                          (conj
                            (conj input-adapters 0)
                            (+ (apply max input-adapters) 3)))
        ]
    (get-arrangements (first sorted-adapters) (rest sorted-adapters) '())))



(defn main-1
  []
  (let [ints (map #(Integer/parseInt %) (str/split-lines day-10-input))
        diffs (get-joltage-differences (set ints))]
    (println (* (get diffs 3) (get diffs 1)))))

(defn main-2
  []
  (let [ints (map #(Integer/parseInt %) (str/split-lines day-10-input))]
    (println (count (get-arrangements-wrapper ints)))))
