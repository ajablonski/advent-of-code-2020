(ns advent-of-code-2020.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-11-input (slurp (io/resource "day11.txt")))

(def ^:dynamic *verbose* true)

(defmacro printlnv
  [& args]
  `(when *verbose*
     (println ~@args)))

(defmacro with-minimal-output
  [& body]
  `(binding [*verbose* false] ~@body))

(defn parse-seat-grid
  [seat-grid-string]
  (vec (map #(vec (seq (char-array %))) (str/split-lines seat-grid-string))))

(defn- neighbors-meet-condition?
  [grid row-num col-num p]
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
    (p entries)))

(defn too-many-neighbors?
  [grid row-num col-num]
  (neighbors-meet-condition?
    grid
    row-num
    col-num
    (fn [neighbors]
      (>= (count (filter #(= % \#) neighbors))
          4))))

(defn all-neighbors-empty?
  [grid row-num col-num]
  (neighbors-meet-condition?
    grid
    row-num
    col-num
    (fn [neighbors]
      (= (count (filter #(= % \#) neighbors))
         0))))

(defn step-1
  [seat-grid]
  (vec
    (map-indexed
      (fn [row-num row]
        (vec (map-indexed
               (fn [col-num letter]
                 (cond (and (= letter \#)
                            (too-many-neighbors? seat-grid row-num col-num))
                       \L
                       (and (= letter \L)
                            (all-neighbors-empty? seat-grid row-num col-num))
                       \#
                       :else letter))
               row)))
      seat-grid)))


(defn get-first-visible-in-line
  [seats]
  (or (some (fn [seat] (when (not (= seat \.)) seat)) seats)
      \.))

(defn get-horizontal-visible-seats
  [grid anchor-row anchor-col]
  (map get-first-visible-in-line
       (list
         (reverse (subvec (get grid anchor-row) 0 (max 0 anchor-col)))
         (subvec (get grid anchor-row) (+ anchor-col 1)))))

(defn get-vertical-visible-seats
  [grid anchor-row anchor-col]
  (map get-first-visible-in-line
       (list
         (reverse (map #(get % anchor-col) (subvec grid 0 (max 0 anchor-row))))
         (map #(get % anchor-col) (subvec grid (+ anchor-row 1))))))

(defn- get-diagonal-first-visible
  [grid start-row start-col row-change-fn col-change-fn]
  (let [max-col (- (count (first grid)) 1)
        max-row (- (count grid) 1)]
    (loop
      [visible-item \.
       curr-row start-row
       curr-col start-col]
      (if (and (<= 0 curr-row max-row)
               (<= 0 curr-col max-col)
               (= visible-item \.))
        (recur
          (get-in grid [curr-row curr-col])
          (row-change-fn curr-row)
          (col-change-fn curr-col))
        visible-item))))

(defn get-diagonal-visible-seats
  [grid anchor-row anchor-col]
  (list
    (get-diagonal-first-visible grid (- anchor-row 1) (+ anchor-col 1) dec inc)
    (get-diagonal-first-visible grid (+ anchor-row 1) (+ anchor-col 1) inc inc)
    (get-diagonal-first-visible grid (+ anchor-row 1) (- anchor-col 1) inc dec)
    (get-diagonal-first-visible grid (- anchor-row 1) (- anchor-col 1) dec dec)))

(defn- visibles-meet-condition?
  [grid row-num col-num p]
  (let [visibles (concat (get-vertical-visible-seats grid row-num col-num)
                         (get-horizontal-visible-seats grid row-num col-num)
                         (get-diagonal-visible-seats grid row-num col-num))]
    (p visibles)))

(defn too-many-visible-occupied?
  [grid row-num col-num]
  (visibles-meet-condition?
    grid
    row-num
    col-num
    (fn [entries]
      (>= (count (filter #(= % \#) entries))
          5))))

(defn all-visible-unoccupied?
  [grid row-num col-num]
  (visibles-meet-condition?
    grid
    row-num
    col-num
    (fn [entries]
      (= (count (filter #(= % \#) entries))
         0))))

(defn step-2
  [seat-grid]
  (vec
    (map-indexed
      (fn [row-num row]
        (vec (map-indexed
               (fn [col-num letter]
                 (cond (and (= letter \#)
                            (too-many-visible-occupied? seat-grid row-num col-num))
                       \L
                       (and (= letter \L)
                            (all-visible-unoccupied? seat-grid row-num col-num))
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
      grid)))

(defn print-grid
  [grid]
  (printlnv "\n\n~~~~~GRID~~~~~")
  (doseq [row grid] (printlnv (apply str row))))

(defn stabilize
  [seat-grid step-fn]
  (print-grid seat-grid)
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

      