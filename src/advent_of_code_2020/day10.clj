(ns advent-of-code-2020.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-10-input (slurp (io/resource "day10.txt")))

(def max-jump-size 3)

(defn get-joltage-differences-counts
  [input-adapters]
  (let [differences (map (fn [input output] (- output input))
                         (sort (conj input-adapters 0))
                         (sort (conj input-adapters (+ (apply max input-adapters) max-jump-size))))]
    (map-vals count (group-by identity differences))))

(defn get-arrangements
  ([sorted-items]
   (get-arrangements (first sorted-items) (rest sorted-items) '()))
  ([starting-item remaining-items result-so-far]
   (let [next-item-possibilities
         (filter #(<= % (+ starting-item max-jump-size)) remaining-items)]
     (if (empty? remaining-items)
       (list (reverse (cons starting-item result-so-far)))
       (mapcat (fn [next-item]
                 (get-arrangements next-item
                                   (filter #(> % next-item) remaining-items)
                                   (cons starting-item result-so-far)))
               next-item-possibilities)))))

(defn get-subgraphs
  [[starting-joltage adapter-1 & other-adapters]]
  (loop
    [prev-item starting-joltage
     curr-item adapter-1
     remaining-items other-adapters
     [active-subgraph & other-subgraphs] (list (list starting-joltage))]
    (let [jump-size (- curr-item prev-item)
          new-subgraphs
          (cond (< jump-size max-jump-size)
                (cons (cons curr-item active-subgraph) other-subgraphs)
                (= jump-size max-jump-size)
                (cons (list curr-item) (cons (reverse active-subgraph) other-subgraphs)))]
      (if (empty? remaining-items)
        new-subgraphs
        (recur curr-item
               (first remaining-items)
               (rest remaining-items)
               new-subgraphs)))))

(defn get-total-arrangements-count
  [input-adapters]
  (let [sorted-adapters-with-start-and-end
        (sort (conj input-adapters 0 (+ (apply max input-adapters) max-jump-size)))
        subgraph-possibility-counts
        (map
          #(count (get-arrangements %))
          (get-subgraphs sorted-adapters-with-start-and-end))]
    (apply * subgraph-possibility-counts)))

(defn main-1
  []
  (let [adapters (map #(Integer/parseInt %) (str/split-lines day-10-input))
        diffs (get-joltage-differences-counts (set adapters))]
    (println (* (get diffs 3) (get diffs 1)))))

(defn main-2
  []
  (let [adapters (map #(Integer/parseInt %) (str/split-lines day-10-input))]
    (println (get-total-arrangements-count adapters))))
