(ns advent-of-code-2020.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-11-input (slurp (io/resource "day11.txt")))

(defn parse-seat-grid
  [seat-grid-string]
  (vec (map #(vec (seq (char-array %))) (str/split-lines seat-grid-string))))

(defn too-busy?
  [grid row-num col-num]
  (let [grid-vec (vec (map vec grid))
        entries (list
                  (get-in grid-vec [(- row-num 1) (- col-num 1)])
                  (get-in grid-vec [(- row-num 1) col-num])
                  (get-in grid-vec [(- row-num 1) (+ col-num 1)])
                  (get-in grid-vec [row-num (- col-num 1)])
                  (get-in grid-vec [row-num (+ col-num 1)])
                  (get-in grid-vec [(+ row-num 1) (- col-num 1)])
                  (get-in grid-vec [(+ row-num 1) col-num])
                  (get-in grid-vec [(+ row-num 1) (+ col-num 1)])
                  )]
    (>= (count (filter #(= % \#) entries))
        4)
    ))

(defn others-empty?
  [grid row-num col-num]
  (let [grid-vec (vec (map vec grid))
        entries (list
                  (get-in grid-vec [(- row-num 1) (- col-num 1)])
                  (get-in grid-vec [(- row-num 1) col-num])
                  (get-in grid-vec [(- row-num 1) (+ col-num 1)])
                  (get-in grid-vec [row-num (- col-num 1)])
                  (get-in grid-vec [row-num (+ col-num 1)])
                  (get-in grid-vec [(+ row-num 1) (- col-num 1)])
                  (get-in grid-vec [(+ row-num 1) col-num])
                  (get-in grid-vec [(+ row-num 1) (+ col-num 1)])
                  )]
    (= (count (filter #(= % \#) entries))
       0)
    ))

(defn step
  [seat-grid]
  (vec
    (map-indexed
    (fn [row-num row]
      (vec (map-indexed
        (fn [col-num letter] (cond (and (= letter \#)
                                        (too-busy? seat-grid row-num col-num))
                                   \L
                                   (and (= letter \L)
                                        (others-empty? seat-grid row-num col-num))
                                   \#
                                   :else letter))
        row)))
    seat-grid)))



(defn count-occupied
  [grid]
  (reduce
    +
    (map
      (fn [row] (count (filter #(= % \#) row)))
      grid))
  )

(defn stabilize
  [seat-grid]
  (let [next-grid (step seat-grid)]
    (if (= next-grid seat-grid)
      seat-grid
      (recur next-grid))))


(defn main-1
  []
  (println
    (reduce
      +
      (map
        (fn [row] (count (filter #(= % \#) row)))
        (stabilize (parse-seat-grid day-11-input))))))

(defn main-2
  []
  (println day-11-input))

      