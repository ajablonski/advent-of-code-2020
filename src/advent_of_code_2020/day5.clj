(ns advent-of-code-2020.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def day-5-input (io/resource "day5.txt"))

(let [digit-map (hash-map \B 1, \R 1, \F 0, \L 0)]
  (defn get-seat-id
    [seat-string]
    (reduce
      (fn [id-so-far char] (+ (* id-so-far 2) (get digit-map char)))
      0
      seat-string)))

(defn main-1
  []
  (println
    (reduce (fn [max-so-far seat-string]
              (max max-so-far (get-seat-id seat-string)))
            0
            (str/split-lines (slurp day-5-input)))))

(defn main-2
  []
  (let [seat-ids (map get-seat-id (str/split-lines (slurp day-5-input)))
        all-seats (set (range (apply min seat-ids) (+ (apply max seat-ids) 1)))]
    (println
      (apply min (set/difference all-seats (set seat-ids))))))
