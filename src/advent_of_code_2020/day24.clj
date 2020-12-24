(ns advent-of-code-2020.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-24-input (slurp (io/resource "day24.txt")))

(defn parse-input-line
  [input-line]
  (loop [letters input-line
         result '()]
    (if (empty? letters)
      (reverse result)
      (let [token-start (first letters)
            token-end (if (contains? #{\e \w} token-start) nil (second letters))
            next-letters (if (contains? #{\e \w} token-start) (rest letters) (drop 2 letters))]
        (recur next-letters (cons (keyword (str token-start token-end)) result))))))

(let [coord-delta-dict {:w  [-1 0]
                        :e  [1 0]
                        :nw [0 1]
                        :se [0 -1]
                        :ne [1 1]
                        :sw [-1 -1]}]
  (defn get-coordinate
    [instruction-seq]
    (loop
      [from [0 0]
       instructions instruction-seq]
      (if (empty? instructions)
        from
        (recur
          (map + from (get coord-delta-dict (first instructions)))
          (rest instructions))))))

(defn parse-input-lines
  [input-lines]
  (map parse-input-line (str/split-lines input-lines)))

(defn get-tile-start-state
  [instructions]
  (reduce
    (fn [state-map instruction]
      (update-in
        state-map
        (get-coordinate instruction)
        (fn
          [old-val]
          (if (or (nil? old-val) (= old-val :white))
            :black
            :white))))
    {}
    instructions))

(defn count-black-tiles
  [state]
  (apply
    +
    (map
      (fn [[_ col]]
        (apply
          +
          (map
            (fn [val] (if (= val :black) 1 0))
            (vals col))))
      state)))

(defn main-1
  []
  (let [instructions (parse-input-lines day-24-input)]
    (println
      (count-black-tiles (get-tile-start-state instructions)))))

(defn expand-grid
  [state]
  (let [min-x (apply min (keys state))
        max-x (apply max (keys state))
        min-y (apply min (mapcat keys (vals state)))
        max-y (apply max (mapcat keys (vals state)))]
    (reduce
      (fn [state x]
        (reduce
          (fn [state y]
            (update-in state [x y] (fn [old-val] (if (nil? old-val) :white old-val))))
          state
          (range (dec min-y) (+ max-y 2)))
        )
      state
      (range (dec min-x) (+ max-x 2)))))

(defn count-black-neighbors
  [grid [x y]]
  (let [neighbors (list [x (inc y)]
                        [x (dec y)]
                        [(inc x) y]
                        [(dec x) y]
                        [(inc x) (inc y)]
                        [(dec x) (dec y)])]
    (count (filter #(= % :black) (map (fn [coords] (get-in grid coords)) neighbors)))))

(defn take-step
  [orig-state]
  (let [min-x (apply min (keys orig-state))
        max-x (apply max (keys orig-state))
        min-y (apply min (mapcat keys (vals orig-state)))
        max-y (apply max (mapcat keys (vals orig-state)))]
    (reduce
      (fn [state x]
        (reduce
          (fn [state y]
            (update-in
              state
              [x y]
              (fn [old-val]
                (let [black-neighbor-count
                      (count-black-neighbors orig-state [x y])]
                  (cond
                    (and (= old-val :white) (= black-neighbor-count 2)) :black
                    (and (= old-val :black) (or (= black-neighbor-count 0) (> black-neighbor-count 2))) :white
                    :else old-val)))))
          state
          (range min-y (inc max-y)))
        )
      orig-state
      (range min-x (inc max-x)))))

(defn take-steps
  [grid step-count]
  (println-info "Steps left" step-count)
  (if (= step-count 0)
    grid
    (recur (take-step (expand-grid grid)) (dec step-count))))

(defn main-2
  []
  (let [instructions (parse-input-lines day-24-input)
        init-state (get-tile-start-state instructions)
        final-state (take-steps init-state 100)]
    (println
      (count-black-tiles final-state))))
      