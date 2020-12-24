(ns advent-of-code-2020.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-23-input (slurp (io/resource "day23.txt")))

(defn parse-input
  [input]
  (vec (map #(Integer/parseInt %) (str/split input #""))))

(defn convert-vector-to-maps
  [input-vector]
  (let [index-to-vals (apply hash-map (mapcat (fn [i v] (list i {:val v :next (mod (inc i) (count input-vector))})) (range) input-vector))]
    (list
      index-to-vals
      (apply hash-map (mapcat (fn [[i {val :val}]] (list val i)) index-to-vals)))))

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

(defn move-with-map
  [cup-map cup-lookup-map curr-index]
  (let [this-cup (get cup-map curr-index)
        next-cup-1-index (:next this-cup)
        next-cup-2-index (:next (get cup-map next-cup-1-index))
        next-cup-3-index (:next (get cup-map next-cup-2-index))
        next-cups (vec (map #(get cup-map %) (list next-cup-1-index next-cup-2-index next-cup-3-index)))
        after-removed-index (:next (get cup-map next-cup-3-index))
        dest-cup-val (get-destination-cup
                       (:val this-cup)
                       (map :val next-cups)
                       (count cup-map))
        dest-cup-index (get cup-lookup-map dest-cup-val)
        dest-cup (get cup-map dest-cup-index)
        dest-cup-old-next (:next dest-cup)
        new-map (assoc!
                  cup-map
                  curr-index (assoc this-cup :next after-removed-index)
                  dest-cup-index (assoc dest-cup :next next-cup-1-index)
                  next-cup-3-index (assoc (get next-cups 2) :next dest-cup-old-next)
                  )]
    (list new-map after-removed-index)))

(defn play-turns
  [cups n-turns]
  (let [[cup-map cup-lookup-map] (convert-vector-to-maps cups)]
    (loop
      [cup-map (transient cup-map)
       turns-left n-turns
       curr-index 0]
      (when (zero? (mod turns-left 10000)) (println-info "Turns left" turns-left))
      (if (= turns-left 0)
        (list (persistent! cup-map) cup-lookup-map)
        (let [[new-map new-index] (move-with-map cup-map cup-lookup-map curr-index)]
          (recur new-map
                 (dec turns-left)
                 new-index))))))

(defn to-seq
  [cup-map val-map]
  (loop
    [curr-index (get val-map 1)
     visited #{}
     result '()]
    (if (contains? visited curr-index)
      (reverse result)
      (let [cup (get cup-map curr-index)]
        (recur (:next cup) (conj visited curr-index) (cons (:val cup) result))))))

(defn main-1
  []
  (let [cups (parse-input day-23-input)
        [cup-map val-map] (play-turns cups 100)]
    (println (str/join (rest (to-seq cup-map val-map))))))

(defn main-2
  []
  (let [input-cups (parse-input day-23-input)
        cups (into input-cups (range (+ (apply max input-cups) 1) 1000001))
        [cup-map val-map] (play-turns cups 10000000)]
    (println (apply * (take 3 (to-seq cup-map val-map))))))
