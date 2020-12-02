(ns advent-of-code-2020.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set]))

(defn parse-lines
  [s]
  (map #(Integer/parseInt %) (str/split s #"\n")))

(defn get-pair
  [n & {:keys [sum] :or {sum 2020}}]
  (- sum n))

(defn get-pair-summing-to
  [s sum]
  (filter (fn [n] (contains? s (get-pair n :sum sum))) s))

(defn main-1
  []
  (let [nums (set (parse-lines
                    (slurp (io/resource "day1/input1.txt"))))
        paired (get-pair-summing-to nums 2020)]
    (println (apply * paired))))

(defn main-2
  []
  (let [nums (set (parse-lines
                    (slurp (io/resource "day1/input1.txt"))))
        remainders (map (fn [n] (list n (clojure.set/difference nums #{n}))) nums)
        trios (map
                (fn [n-and-set]
                  (let
                    [[n s] n-and-set]
                    (cons n (get-pair-summing-to s (- 2020 n)))))
                remainders)
        sets (set (mapcat (fn [trio] (if (= 3 (count trio)) trio nil)) trios))
        ]
    (println (apply * sets))))