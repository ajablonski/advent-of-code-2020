(ns advent-of-code-2020.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-17-input (slurp (io/resource "day17.txt")))

(defn parse-initial-state
  [state-string]
  (reduce
    (fn [ret-map [composite-key value]]
      (assoc-in ret-map composite-key value))
    {}
    (mapcat identity
            (map-indexed
              (fn [y row]
                (map-indexed
                  (fn [x value] (list [x y 0] value))
                  (.toCharArray row)))
              (str/split-lines state-string)))))

(defn expand-state
  [state]
  (let [min-x (apply min (keys state))
        max-x (apply max (keys state))
        min-y (apply min (keys (get state 0)))
        max-y (apply max (keys (get state 0)))
        min-z (apply min (keys (get-in state [0 0])))
        max-z (apply max (keys (get-in state [0 0])))]
    (reduce
      (fn [new-state coordinates] (update-in new-state coordinates (fn [curr-val] (if (nil? curr-val) \. curr-val))))
      state
      (mapcat
        (fn [x]
          (mapcat
            (fn [y]
              (map
                (fn [z] [x y z])
                (range (- min-z 1) (+ max-z 2))))
            (range (- min-y 1) (+ max-y 2))))
        (range (- min-x 1) (+ max-x 2))))))

(defn get-neighbor-coordinates
  [[x y z :as original-coordinates]]
  (filter #(not (= % original-coordinates))
          (mapcat
            (fn [x]
              (mapcat
                (fn [y]
                  (map
                    (fn [z] [x y z])
                    (range (- z 1) (+ z 2))))
                (range (- y 1) (+ y 2))))
            (range (- x 1) (+ x 2)))))

(defn print-grid
  [state]
  (let [min-x (apply min (keys state))
        max-x (apply max (keys state))
        min-y (apply min (keys (get state 0)))
        max-y (apply max (keys (get state 0)))
        min-z (apply min (keys (get-in state [0 0])))
        max-z (apply max (keys (get-in state [0 0])))]
    (mapcat
      (fn [z]
        (println "Z = " z)
        (mapcat
          (fn [y]
            (println (apply str (map
                                  (fn [x] (get-in state [x y z]))
                                  (range min-x (+ max-x 1))))))
          (range min-y (+ max-y 1))))
      (range min-z (+ max-z 1)))))

(defn count-active-neighbors
  [state neighbors]
  (reduce (fn [total neighbor] (+ total (if (= \# (get-in state neighbor)) 1 0)))
          0
          neighbors))

(defn update-state
  [state]
  (let [min-x (apply min (keys state))
        max-x (apply max (keys state))
        min-y (apply min (keys (get state 0)))
        max-y (apply max (keys (get state 0)))
        min-z (apply min (keys (get-in state [0 0])))
        max-z (apply max (keys (get-in state [0 0])))]
    (reduce
      (fn [new-state coordinates]
        (update-in
          new-state
          coordinates
          (fn [curr-val] (let [active-neighbors (count-active-neighbors state (get-neighbor-coordinates coordinates))]
                           (cond (= \# curr-val) (if (<= 2 active-neighbors 3) \# \.)
                                 (= \. curr-val) (if (= 3 active-neighbors) \# \.))))))
      state
      (mapcat
        (fn [x]
          (mapcat
            (fn [y]
              (map
                (fn [z] [x y z])
                (range min-z (inc max-z))))
            (range min-y (inc max-y))))
        (range min-x (inc max-x))))))

(defn step
  [state]
  (update-state (expand-state state)))

(defn count-occupied
  [state]
  (let [min-x (apply min (keys state))
        max-x (apply max (keys state))
        min-y (apply min (keys (get state 0)))
        max-y (apply max (keys (get state 0)))
        min-z (apply min (keys (get-in state [0 0])))
        max-z (apply max (keys (get-in state [0 0])))]
    (reduce
      (fn [total-occupied coordinates]
        (+ total-occupied (if (= \# (get-in state coordinates)) 1 0)))
      0
      (mapcat
        (fn [x]
          (mapcat
            (fn [y]
              (map
                (fn [z] [x y z])
                (range min-z (inc max-z))))
            (range min-y (inc max-y))))
        (range min-x (inc max-x))))))

(defn main-1
  []
  (let [initial-state (parse-initial-state day-17-input)]
    (println (count-occupied (->> initial-state
                                  step
                                  step
                                  step
                                  step
                                  step
                                  step)))))


(defn parse-initial-state-with-w
  [state-string]
  (reduce
    (fn [ret-map [composite-key value]]
      (assoc-in ret-map composite-key value))
    {}
    (mapcat identity
            (map-indexed
              (fn [y row]
                (map-indexed
                  (fn [x value] (list [x y 0 0] value))
                  (.toCharArray row)))
              (str/split-lines state-string)))))

(defn expand-state-with-w
  [state]
  (let [min-x (apply min (keys state))
        max-x (apply max (keys state))
        min-y (apply min (keys (get state 0)))
        max-y (apply max (keys (get state 0)))
        min-z (apply min (keys (get-in state [0 0])))
        max-z (apply max (keys (get-in state [0 0])))
        min-w (apply min (keys (get-in state [0 0 0])))
        max-w (apply max (keys (get-in state [0 0 0])))]
    (reduce
      (fn [new-state coordinates] (update-in new-state coordinates (fn [curr-val] (if (nil? curr-val) \. curr-val))))
      state
      (mapcat
        (fn [x]
          (mapcat
            (fn [y]
              (mapcat
                (fn [z]
                  (map
                    (fn [w] [x y z w])
                    (range (- min-w 1) (+ max-w 2))))
                (range (- min-z 1) (+ max-z 2))))
            (range (- min-y 1) (+ max-y 2))))
        (range (- min-x 1) (+ max-x 2))))))

(defn get-neighbor-coordinates-with-w
  [[x y z w :as original-coordinates]]
  (filter #(not (= % original-coordinates))
          (mapcat
            (fn [x]
              (mapcat
                (fn [y]
                  (mapcat
                    (fn [z]
                      (map
                        (fn [w] [x y z w])
                        (range (- w 1) (+ w 2))))
                    (range (- z 1) (+ z 2))))
                (range (- y 1) (+ y 2))))
            (range (- x 1) (+ x 2)))))


(defn update-state-w
  [state]
  (let [min-x (apply min (keys state))
        max-x (apply max (keys state))
        min-y (apply min (keys (get state 0)))
        max-y (apply max (keys (get state 0)))
        min-z (apply min (keys (get-in state [0 0])))
        max-z (apply max (keys (get-in state [0 0])))
        min-w (apply min (keys (get-in state [0 0 0])))
        max-w (apply max (keys (get-in state [0 0 0])))]
    (reduce
      (fn [new-state coordinates]
        (update-in
          new-state
          coordinates
          (fn [curr-val] (let [active-neighbors (count-active-neighbors state (get-neighbor-coordinates-with-w coordinates))]
                           (cond (= \# curr-val) (if (<= 2 active-neighbors 3) \# \.)
                                 (= \. curr-val) (if (= 3 active-neighbors) \# \.))))))
      state
      (mapcat
        (fn [x]
          (mapcat
            (fn [y]
              (mapcat
                (fn [z]
                  (map
                    (fn [w]
                      [x y z w])
                    (range min-w (inc max-w))))
                (range min-z (inc max-z))))
            (range min-y (inc max-y))))
        (range min-x (inc max-x))))))

(defn count-occupied-w
  [state]
  (let [min-x (apply min (keys state))
        max-x (apply max (keys state))
        min-y (apply min (keys (get state 0)))
        max-y (apply max (keys (get state 0)))
        min-z (apply min (keys (get-in state [0 0])))
        max-z (apply max (keys (get-in state [0 0])))
        min-w (apply min (keys (get-in state [0 0 0])))
        max-w (apply max (keys (get-in state [0 0 0])))
        ]
    (reduce
      (fn [total-occupied coordinates]
        (+ total-occupied (if (= \# (get-in state coordinates)) 1 0)))
      0
      (mapcat
        (fn [x]
          (mapcat
            (fn [y]
              (mapcat
                (fn [z]
                  (map
                    (fn [w] [x y z w])
                    (range min-w (inc max-w))))
                (range min-z (inc max-z))))
            (range min-y (inc max-y))))
        (range min-x (inc max-x))))))

(defn step-w
  [state]
  (update-state-w (expand-state-with-w state)))

(defn main-2
  []
  (let [initial-state (parse-initial-state-with-w day-17-input)]
    (println (count-occupied-w (->> initial-state
                                  step-w
                                  step-w
                                  step-w
                                  step-w
                                  step-w
                                  step-w)))))
