(ns advent-of-code-2020.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-23-input (slurp (io/resource "day23.txt")))

(defn parse-input
  [input]
  (vec (map #(Integer/parseInt %) (str/split input #""))))

(defn get-next-cup-to-try
  [cup max-cup]
  (let [ret (mod (- cup 1) max-cup)]
    (if (= 0 ret) max-cup ret)))

(defn get-destination-cup
  [current-cup invalid-cups max-cup]
  (let [invalid-cup-set (set invalid-cups)]
    (loop
      [cup-to-try (get-next-cup-to-try current-cup max-cup)]
      (if (contains? invalid-cup-set cup-to-try)
        (recur (get-next-cup-to-try cup-to-try max-cup))
        cup-to-try))))

(defn move-items
  [cup-vec curr-index dest-cup array-size]
  (let [orig-value-1 (get cup-vec (mod (+ curr-index 1) array-size))
        orig-value-2 (get cup-vec (mod (+ curr-index 2) array-size))
        orig-value-3 (get cup-vec (mod (+ curr-index 3) array-size))]
    (reduce
      (fn [cup-vec i]
        (let [index (mod i array-size)
              from-index (mod (+ i 3) array-size)
              curr-val (get cup-vec from-index)
              new-vec (assoc! cup-vec index curr-val)]
          (if (= curr-val dest-cup)
            (reduced
              (assoc! new-vec
                      (mod (+ index 1) array-size)
                      orig-value-1
                      (mod (+ index 2) array-size)
                      orig-value-2
                      (mod (+ index 3) array-size)
                      orig-value-3))
            new-vec)))
      cup-vec
      (range (inc curr-index) ##Inf))))

(defn move
  [cups curr-index cup-count]
  (let [curr-cup (get cups curr-index)
        next-cup-indices (map #(mod (+ % curr-index) cup-count) '(1 2 3))
        next-cups (map #(get cups %) next-cup-indices)
        dest-cup (get-destination-cup curr-cup next-cups cup-count)]
    (move-items cups curr-index dest-cup cup-count)))

(defn play-turns
  [cups n-turns]
  (let [cup-count (count cups)]
    (loop
      [cups (transient cups)
       turns-left n-turns
       curr-index 0]
      (when (zero? (mod turns-left 10)) (println-info "Turns left" turns-left))
      (if (= turns-left 0)
        (persistent! cups)
        (recur (move cups curr-index cup-count) (dec turns-left) (mod (inc curr-index) cup-count))))))

(defn wrap-to-1
  [cups]
  (if (= (first cups) 1)
    (rest cups)
    (recur (concat (rest cups) (list (first cups))))))

(defn main-1
  []
  (let [cups (parse-input day-23-input)
        result-state (play-turns cups 100)]
    (println (str/join (wrap-to-1 result-state)))))

(defn main-2
  []
  (let [input-cups (parse-input day-23-input)
        cups (into input-cups (range (+ (apply max input-cups) 1) 1000001))
        result-state (play-turns cups 10000000)]
    (println (take 3 (wrap-to-1 result-state)))))


      