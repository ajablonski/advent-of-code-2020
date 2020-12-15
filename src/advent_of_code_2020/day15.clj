(ns advent-of-code-2020.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-15-input (slurp (io/resource "day15.txt")))

(defn parse-input
  [input-str]
  (map #(Integer/parseInt (str/trim-newline %)) (str/split input-str #",")))

(defmacro maybe-log-turn
  [turn val]
  `(if (= 0 (mod ~turn 100000))
     (println-info "Turn " ~turn " Value " ~val)
     (println-debug "Turn " ~turn " Value " ~val)))

(defn expand
  [entries-so-far last-turn last-val target-turn]
  (loop
    [last-turn last-turn
     last-val last-val
     entries-so-far (transient entries-so-far)]
    (maybe-log-turn last-turn last-val)
    (if (= target-turn last-turn)
      {:last-entry-map (persistent! entries-so-far) :last-turn last-turn :last-val last-val}
      (recur
        (inc last-turn)
        (- last-turn (get entries-so-far last-val last-turn))
        (assoc! entries-so-far last-val last-turn)))))

(defn get-value-on-turn
  [starting-entries target-turn]
  (let [vals-and-turns
        (map list
             starting-entries
             (range 1 ##Inf))
        [bw-last-val & bw-rest-entries] (reverse vals-and-turns)
        [last-val last-turn] bw-last-val
        penultimate-map (apply hash-map (flatten (reverse bw-rest-entries)))]
    (println-debug "Initial map " penultimate-map)
    (println-debug "Starting with turn " last-turn " having val" last-val)
    (:last-val (expand penultimate-map last-turn last-val target-turn))))

(defn main-1
  []
  (let [initial-vals (parse-input day-15-input)]
    (println
      (get-value-on-turn
        initial-vals
        2020))))

(defn main-2
  []
  (let [initial-vals (parse-input day-15-input)]
    (println
      (get-value-on-turn
        initial-vals
        30000000))))
