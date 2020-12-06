(ns advent-of-code-2020.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def day-6-input (io/resource "day6.txt"))

(defn main-1
  []
  (let [input (slurp day-6-input)
        groups (str/split input #"\n\n")
        groups-no-gaps (map #(count (set (str/replace % #"[^a-z]" ""))) groups)]
    (println
      (reduce + groups-no-gaps))))

(defn main-2
  []
  (let [input (slurp day-6-input)
        groups (str/split input #"\n\n")
        groups-as-sets (map #(count (reduce set/intersection (map set (str/split-lines %)))) groups)]
    (println
      (reduce + groups-as-sets))))
