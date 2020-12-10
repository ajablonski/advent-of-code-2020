(ns advent-of-code-2020.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-10-input (slurp (io/resource "day10.txt")))

(defn get-joltage-differences-counts
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

(defn get-total-arrangements-count
  [input-adapters]
  (let [sorted-adapters-with-start-and-end
        (sort (conj input-adapters 0 (+ (apply max input-adapters) 3)))
        subgraphs
        (get-subgraphs sorted-adapters-with-start-and-end)
        subgraph-possibilities
        (map
          (fn [subgraph] (count (get-arrangements (first subgraph) (rest subgraph) '())))
          subgraphs)]
    (apply
      *
      subgraph-possibilities)))

(defn main-1
  []
  (let [adapters (map #(Integer/parseInt %) (str/split-lines day-10-input))
        diffs (get-joltage-differences-counts (set adapters))]
    (println (* (get diffs 3) (get diffs 1)))))

(defn main-2
  []
  (let [adapters (map #(Integer/parseInt %) (str/split-lines day-10-input))]
    (println (get-total-arrangements-count adapters))))
