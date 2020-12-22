(ns advent-of-code-2020.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-22-input (slurp (io/resource "day22.txt")))

(defn- parse-player-input
  [player-input-string]
  (map #(Integer/parseInt %) (rest (str/split-lines player-input-string))))

(defn parse-input
  [input-string]
  (let [[p1-part p2-part] (str/split input-string #"\n\n")]
    (list (parse-player-input p1-part) (parse-player-input p2-part))))

(defn take-normal-turn
  [[[p1-next & p1-rest] [p2-next & p2-rest]]]
  (if (> p1-next p2-next)
    (list (concat p1-rest (list p1-next p2-next)) p2-rest)
    (list p1-rest (concat p2-rest (list p2-next p1-next)))))

(defn play-normal-game
  [[p1-cards p2-cards :as hands]]
  (if (or (empty? p1-cards) (empty? p2-cards))
    hands
    (recur (take-normal-turn hands))))

(defn calculate-score
  [hand]
  (apply
    +
    (map *
         hand
         (range (count hand) 0 -1))))

(defn main-1
  []
  (let [hands (play-normal-game (parse-input day-22-input))
        winning-hand (first (filter some? hands))]
    (println (calculate-score winning-hand))))

(defn get-winner
  [hands]
  (if (nil? (first hands)) 2 1))

(defn should-play-recursive?
  [[[p1-next & p1-rest] [p2-next & p2-rest]]]
  (and (>= (count p1-rest) p1-next)
       (>= (count p2-rest) p2-next)))

(declare play-recursive-game)

(defn take-recursive-turn
  [[[p1-next & p1-rest] [p2-next & p2-rest] :as hands] game]
  (if (should-play-recursive? hands)
    (if (=
          (get-winner
            (play-recursive-game
              (list
                (take p1-next p1-rest)
                (take p2-next p2-rest))
              (+ game 1)))
          1)
      (list (concat p1-rest (list p1-next p2-next)) p2-rest)
      (list p1-rest (concat p2-rest (list p2-next p1-next))))
    (take-normal-turn hands)))

(defn play-recursive-game
  [hands game]
  (println-info (apply str (repeat (dec game) " ")) "Game" game)
  (loop
    [turn-history #{}
     hands hands]
    (cond
      (or (empty? (first hands)) (empty? (second hands))) hands
      (contains? turn-history hands) (first hands)
      :else (recur (conj turn-history hands) (take-recursive-turn hands game)))))

(defn main-2
  []
  (let [hands (play-recursive-game (parse-input day-22-input) 1)
        winning-hand (first (filter some? hands))]
    (println (calculate-score winning-hand))))
