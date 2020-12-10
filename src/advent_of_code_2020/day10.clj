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
  [starting-item remaining-items]
  (let [next-item-possibilities (filter #(<= % (+ starting-item 3)) remaining-items)]
    (if (empty? remaining-items)
      1
      (let [result (reduce
                     +
                     (map
                       (fn [next-item]
                         (get-arrangements next-item
                                           (filter #(> % next-item) remaining-items)))
                       next-item-possibilities))]
        result))))

(defn get-subgraphs
  [adapters]
  (loop
    [prev-item (first adapters)
     curr-item (first (rest adapters))
     remaining-items (rest (rest adapters))
     subgraphs (list (list (first adapters)))]
    (cond (empty? remaining-items)
          (cons (list curr-item) (cons (reverse (first subgraphs)) (rest subgraphs)))
          (< curr-item (+ prev-item 3))
          (recur curr-item
                 (first remaining-items)
                 (rest remaining-items)
                 (cons (cons curr-item (first subgraphs)) (rest subgraphs)))
          (= curr-item (+ prev-item 3))
          (recur curr-item
                 (first remaining-items)
                 (rest remaining-items)
                 (cons (list curr-item) (cons (reverse (first subgraphs)) (rest subgraphs)))))))

(defn get-subgraphs-wrapper
  [adapters]
  (reverse (get-subgraphs
             (sort
               (conj
                 (conj adapters 0)
                 (+ (apply max adapters) 3))))))

(defn get-arrangements-wrapper
  [input-adapters]
  (let [subgraphs (get-subgraphs-wrapper input-adapters)
        subgraph-possibilities (map
                                 (fn [subgraph] (get-arrangements (first subgraph) (rest subgraph)))
                                 subgraphs)]
    (apply
      *
      subgraph-possibilities)))


(defn main-1
  []
  (let [ints (map #(Integer/parseInt %) (str/split-lines day-10-input))
        diffs (get-joltage-differences (set ints))]
    (println (* (get diffs 3) (get diffs 1)))))

(defn main-2
  []
  (let [ints (map #(Integer/parseInt %) (str/split-lines day-10-input))]
    (println (get-arrangements-wrapper ints))))
