(ns advent-of-code-2020.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-15-input (slurp (io/resource "day15.txt")))

(defn expand
  [entries-so-far last-turn last-val additional-entries]
  (let [penultimate-spoken-index (get entries-so-far last-val)
        curr-val (if (nil? penultimate-spoken-index) 0 (- last-turn penultimate-spoken-index))
        ]
    (println-debug "Turn " last-turn " Value " last-val)
    (if (= additional-entries 0)
      {:last-entry-map entries-so-far :last-turn last-turn :last-val last-val}
      (recur
        (assoc entries-so-far last-val last-turn)
        (inc last-turn)
        curr-val
        (dec additional-entries)))))

(defn get-value-on-turn
  [starting-entries turn-number]
  (let [vals-and-turns
        (map list
             starting-entries
             (range 1 ##Inf))
        [bw-last-val & bw-rest-entries] (reverse vals-and-turns)
        [last-val last-turn] bw-last-val
        penultimate-map (apply hash-map (flatten (reverse bw-rest-entries)))]
    (println-debug "Initial map " penultimate-map)
    (println-debug "Starting with turn " last-turn " having val" last-val)
    (:last-val (expand penultimate-map last-turn last-val (- turn-number (count vals-and-turns))))))

(defn main-1
  []
  (let [initial-vals (map #(Integer/parseInt (str/trim-newline %)) (str/split day-15-input #","))]
    (println
      (get-value-on-turn
        initial-vals
        2020))))

(defn main-2
  []
  (let [initial-vals (map #(Integer/parseInt (str/trim-newline %)) (str/split day-15-input #","))]
    (println
      (get-value-on-turn
        initial-vals
        30000000))))
