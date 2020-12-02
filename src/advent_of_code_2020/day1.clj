(ns advent-of-code-2020.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set]))

(def day-1-input (io/resource "day1/input1.txt"))

(defn parse-lines
  [s]
  (map #(Integer/parseInt %) (str/split s #"\n")))

(defn find-pair-summing-to
  [nums sum]
  (let [s (set nums)]
    (set (filter (fn [n] (contains? s (- sum n))) nums))))

(defn main-1
  []
  (let [nums (parse-lines (slurp day-1-input))
        paired (find-pair-summing-to nums 2020)]
    (println (apply * paired))))

(defn main-2
  []
  (let [nums (set (parse-lines (slurp day-1-input)))
        trios (mapcat
                (fn [n]
                  (let [pair
                        (find-pair-summing-to
                          (clojure.set/difference nums #{n})
                          (- 2020 n))]
                    (if (empty? pair) nil (cons n pair))))
                nums)]
    (println (apply * (set trios)))))