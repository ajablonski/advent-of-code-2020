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



(defn step-1
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


(defn get-first-visible-in-line
  [seats]
  (reduce
    (fn [vis seat] (if (= vis \.) seat vis))
    \.
    seats))

(defn get-rows
  [grid anchor-row anchor-col]
  (list
    (vec (reverse (subvec (get grid anchor-row) 0 (max 0 anchor-col))))
    (subvec (get grid anchor-row) (+ anchor-col 1))))

(defn get-cols
  [grid anchor-row anchor-col]
  (list
    (vec (reverse (map #(get % anchor-col) (subvec grid 0 (max 0 anchor-row)))))
    (map #(get % anchor-col) (subvec grid (+ anchor-row 1)))))


(defn get-diagonals
  [grid anchor-row anchor-col]
  (let [max-col (- (count (first grid)) 1)
        max-row (- (count grid) 1)]
    (list
      (loop
        [items '()
         curr-row (- anchor-row 1)
         curr-col (+ anchor-col 1)]
        (if (or (< curr-row 0)
                (> curr-col max-col))
          (reverse items)
          (recur
            (cons
              (get-in grid [curr-row curr-col]) items)
            (- curr-row 1)
            (+ curr-col 1)))
        )
      (loop
        [items '()
         curr-row (+ anchor-row 1)
         curr-col (+ anchor-col 1)]
        (if (or (> curr-row max-row)
                (> curr-col max-col))
          (reverse items)
          (recur
            (cons
              (get-in grid [curr-row curr-col]) items)
            (+ curr-row 1)
            (+ curr-col 1)))
        )
      (loop
        [items '()
         curr-row (+ anchor-row 1)
         curr-col (- anchor-col 1)]
        (if (or (> curr-row max-row)
                (< curr-col 0))
          (reverse items)
          (recur
            (cons
              (get-in grid [curr-row curr-col]) items)
            (+ curr-row 1)
            (- curr-col 1)))
        )
      (loop
        [items '()
         curr-row (- anchor-row 1)
         curr-col (- anchor-col 1)]
        (if (or (< curr-row 0)
                (< curr-col 0))
          (reverse items)
          (recur
            (cons
              (get-in grid [curr-row curr-col]) items)
            (- curr-row 1)
            (- curr-col 1)))
        )
      )
    ))

(defn too-busy-2?
  [grid row-num col-num]
  (let [entries (concat (get-rows grid row-num col-num)
                        (get-cols grid row-num col-num)
                        (get-diagonals grid row-num col-num))]
    (>= (count (filter #(= % \#) (map get-first-visible-in-line entries)))
        5)
    ))

(defn others-empty-2?
  [grid row-num col-num]
  (let [entries (concat (get-rows grid row-num col-num)
                        (get-cols grid row-num col-num)
                        (get-diagonals grid row-num col-num))]
    (= (count (filter #(= % \#) (map get-first-visible-in-line entries)))
       0)
    ))

(defn step-2
  [seat-grid]
  (vec
    (map-indexed
      (fn [row-num row]
        (vec (map-indexed
               (fn [col-num letter]
                 (cond (and (= letter \#)
                            (too-busy-2? seat-grid row-num col-num))
                       \L
                       (and (= letter \L)
                            (others-empty-2? seat-grid row-num col-num))
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

(defn print-grid
  [grid]
  (println "GRID")
  (doseq [row grid] (println (apply str row))))

(defn stabilize
  [seat-grid step-fn]
  (let [next-grid (step-fn seat-grid)]
    (if (= next-grid seat-grid)
      seat-grid
      (recur next-grid step-fn))))


(defn main-1
  []
  (println
    (count-occupied (stabilize (parse-seat-grid day-11-input) step-1))))


(defn main-2
  []
  (println
    (count-occupied (stabilize (parse-seat-grid day-11-input) step-2))))

      