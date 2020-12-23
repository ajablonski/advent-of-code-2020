(ns advent-of-code-2020.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-23-input (slurp (io/resource "day23.txt")))

(defn parse-input
  [input]
  (map #(Integer/parseInt %) (str/split input #"")))

(defn get-next-cup-to-try
  [cup max-cup]
  (let [ret (mod (- cup 1) max-cup)]
    (if (= 0 ret) max-cup ret)))

(defn find-destination-cup
  [current-cup remaining-cups max-cup]
  (let [cup-set (set remaining-cups)]
    (loop
      [cup-to-try (get-next-cup-to-try current-cup max-cup)]
      (if (contains? cup-set cup-to-try)
        cup-to-try
        (recur (get-next-cup-to-try cup-to-try max-cup))))))

(defn insert-into
  [all-cups dest-cup cups-to-insert]
  (mapcat
    (fn [c] (if (= dest-cup c)
              (cons c cups-to-insert)
              (list c)))
    all-cups))

(defn move
  [cups]
  (println-info "Starting state:" cups)
  (let [curr-cup (first cups)
        next-cups (take 3 (rest cups))
        remaining-cups (drop 4 cups)
        max-cup (apply max cups)
        dest-cup (find-destination-cup curr-cup remaining-cups max-cup)]
    (concat (insert-into remaining-cups dest-cup next-cups) (list curr-cup))))

(defn play-turns
  [cups n-turns]
  (if (= n-turns 0)
    cups
    (recur (move cups) (dec n-turns))))

(defn wrap-to-1
  [cups]
  (if (= (first cups) 1)
    (rest cups)
    (recur (concat (rest cups) (list (first cups))))))

(defn main-1
  []
  (let [result-state (play-turns (parse-input day-23-input) 100)]
    (println (str/join (wrap-to-1 result-state)))))

(defn main-2
  []
  (println day-23-input))

      