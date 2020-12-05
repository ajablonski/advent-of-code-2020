(ns advent-of-code-2020.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day-1-input (io/resource "day1.txt"))

(defn parse-lines
  [s]
  (map #(Integer/parseInt %) (str/split s #"\n")))

(defn find-pair-summing-to
  [nums sum]
  (let [s (set nums)]
    (set (filter #(contains? s (- sum %)) nums))))

(defn main-1 []
  (let [nums (parse-lines (slurp day-1-input))
        paired (find-pair-summing-to nums 2020)]
    (println (apply * paired))))

(defn main-2 []
  (let [nums (set (parse-lines (slurp day-1-input)))
        trios (mapcat
                #(let [pair (find-pair-summing-to
                              (disj nums %)
                              (- 2020 %))]
                   (if (empty? pair) nil (cons % pair)))
                nums)]
    (println (apply * (set trios)))))