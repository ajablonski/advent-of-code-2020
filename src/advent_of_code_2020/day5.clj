(ns advent-of-code-2020.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def day-5-input (io/resource "day5.txt"))

(defn get-seat-id
  [seat-string]
  (Integer/parseInt
    (str/replace
      (str/replace seat-string #"[BR]" "1")
      #"[FL]" "0")
    2))

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
