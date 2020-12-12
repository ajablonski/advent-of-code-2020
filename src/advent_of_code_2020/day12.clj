(ns advent-of-code-2020.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-12-input (slurp (io/resource "day12.txt")))

(defn move-n
  [current-state amount]
  (update current-state :y #(+ % amount)))

(defn move-s
  [current-state amount]
  (update current-state :y #(- % amount)))

(defn move-e
  [current-state amount]
  (update current-state :x #(+ % amount)))

(defn move-w
  [current-state amount]
  (update current-state :x #(- % amount)))

(let [left-turns {:N :W,
                  :W :S,
                  :S :E,
                  :E :N}]
  (defn turn-left
    [current-state amount]
    (if (= amount 0)
      current-state
      (recur
        (update current-state :direction left-turns)
        (- amount 90)))))

(let [right-turns {:N :E,
                   :E :S,
                   :S :W,
                   :W :N}]
  (defn turn-right
    [current-state amount]
    (if (= amount 0)
      current-state
      (recur
        (update current-state :direction right-turns)
        (- amount 90)))))

(defn move-forward
  [current-state amount]
  (let [direction (current-state :direction)]
    (cond (= direction :N) (move-n current-state amount)
          (= direction :W) (move-w current-state amount)
          (= direction :E) (move-e current-state amount)
          (= direction :S) (move-s current-state amount)
          )))


(defn move-waypoint-n
  [current-state amount]
  (update current-state :waypoint-y #(+ % amount)))

(defn move-waypoint-s
  [current-state amount]
  (update current-state :waypoint-y #(- % amount)))

(defn move-waypoint-e
  [current-state amount]
  (update current-state :waypoint-x #(+ % amount)))

(defn move-waypoint-w
  [current-state amount]
  (update current-state :waypoint-x #(- % amount)))

(defn rotate-waypoint-l
  [current-state amount]
  (if (= amount 0)
    current-state
    (recur
      (assoc current-state :waypoint-x (- (:waypoint-y current-state))
                           :waypoint-y (:waypoint-x current-state))
      (- amount 90))))

(defn rotate-waypoint-r
  [current-state amount]
  (if (= amount 0)
    current-state
    (recur
      (assoc current-state :waypoint-x (:waypoint-y current-state)
                           :waypoint-y (- (:waypoint-x current-state)))
      (- amount 90))))

(defn move-towards-waypoint
  [current-state amount]
  (update
    (update current-state
            :x #(+ % (* (:waypoint-x current-state) amount)))
    :y #(+ % (* (:waypoint-y current-state) amount))))

(defn parse-input-line
  [line parser-map]
  (let [[_ command amount] (re-find #"([A-Z])([0-9]+)" line)]
    (fn [state]
      (
       (parser-map command)
       state
       (Integer/parseInt amount)))))

(defn parse-input-lines
  [input parser-map]
  (map #(parse-input-line % parser-map) (str/split-lines input)))


(defn get-manhattan-distance
  [state]
  (+ (Math/abs (:x state))
     (Math/abs (:y state))))

(defn main-1
  []
  (let [fn-map {"L" turn-left,
                "R" turn-right,
                "F" move-forward,
                "N" move-n,
                "S" move-s,
                "E" move-e,
                "W" move-w}
        steps (parse-input-lines day-12-input fn-map)
        final-pos (reduce (fn [state step] (step state)) {:x 0 :y 0 :direction :E} steps)]
    (println (get-manhattan-distance final-pos))))

(defn main-2
  []
  (let [fn-map {"L" rotate-waypoint-l,
                "R" rotate-waypoint-r,
                "F" move-towards-waypoint,
                "N" move-waypoint-n,
                "S" move-waypoint-s,
                "E" move-waypoint-e,
                "W" move-waypoint-w}
        steps (parse-input-lines day-12-input fn-map)
        final-pos (reduce (fn [state step] (step state)) {:x 0 :y 0 :waypoint-x 10 :waypoint-y 1} steps)]
    (println (get-manhattan-distance final-pos))))
