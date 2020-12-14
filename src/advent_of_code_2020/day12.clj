(ns advent-of-code-2020.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-12-input (slurp (io/resource "day12.txt")))

(defn parse-input-line
  [line instruction-map]
  (let [[_ command amount] (re-find #"([A-Z])([0-9]+)" line)]
    (fn [state]
      ((instruction-map command)
       state
       (Integer/parseInt amount)))))

(defn parse-input-lines
  [input instruction-map]
  (map #(parse-input-line % instruction-map) (str/split-lines input)))

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

(defn- turn
  [turn-map]
  (fn [current-state amount]
    (if (= amount 0)
      current-state
      (recur
        (update current-state :direction turn-map)
        (- amount 90)))))

(def turn-left
  (turn {:N :W, :W :S, :S :E, :E :N}))

(def turn-right
  (turn {:N :E, :E :S, :S :W, :W :N}))

(defn move-forward
  [current-state amount]
  (({:N move-n, :W move-w, :E move-e, :S move-s}
    (:direction current-state))
   current-state
   amount))

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

(defn- rotate-waypoint
  [x-update-fn y-update-fn]
  (fn [current-state amount]
    (if (= amount 0)
      current-state
      (recur
        (assoc current-state :waypoint-x (x-update-fn current-state)
                             :waypoint-y (y-update-fn current-state))
        (- amount 90)))))

(def rotate-waypoint-l
  (rotate-waypoint #(- (:waypoint-y %)) #(:waypoint-x %)))

(def rotate-waypoint-r
  (rotate-waypoint #(:waypoint-y %) #(- (:waypoint-x %))))

(defn move-towards-waypoint
  [current-state amount]
  (update
    (update current-state
            :x #(+ % (* (:waypoint-x current-state) amount)))
    :y #(+ % (* (:waypoint-y current-state) amount))))

(defn get-manhattan-distance
  [state]
  (+ (Math/abs ^int (:x state))
     (Math/abs ^int (:y state))))

(defn- do-main
  [instruction-map initial-state]
  (->> (parse-input-lines day-12-input instruction-map)
       (reduce (fn [state step] (step state)) initial-state)
       get-manhattan-distance))

(defn main-1
  []
  (println
    (do-main
      {"L" turn-left,
       "R" turn-right,
       "F" move-forward,
       "N" move-n,
       "S" move-s,
       "E" move-e,
       "W" move-w}
      {:x 0 :y 0 :direction :E})))

(defn main-2
  []
  (println
    (do-main
      {"L" rotate-waypoint-l,
       "R" rotate-waypoint-r,
       "F" move-towards-waypoint,
       "N" move-waypoint-n,
       "S" move-waypoint-s,
       "E" move-waypoint-e,
       "W" move-waypoint-w}
      {:x 0 :y 0 :waypoint-x 10 :waypoint-y 1})))
