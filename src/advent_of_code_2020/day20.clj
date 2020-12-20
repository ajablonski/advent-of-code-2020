(ns advent-of-code-2020.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]
            [clojure.set :as set]))

(def day-20-input (slurp (io/resource "day20.txt")))

(defprotocol TileP
  (rotate-cc [_] "Rotate counterclockwise")
  (flip-y [_] "Flip across y axis")
  (match-or-nil [_ other] "Get match with another tile in terms of an x-y delta, or nil if no match")
  (print-debug [_] "Print a debugging version of the tile"))

(defrecord Tile [id lines coordinates] TileP
  (rotate-cc [_]
    (let [new-lines (map
                      (fn [col-num] (apply str (map (fn [line] (get line col-num)) lines)))
                      (range (dec (count (first lines))) -1 -1))]
      (->Tile id new-lines nil)))
  (flip-y [_] (let [new-lines (map str/reverse lines)]
                (->Tile id new-lines nil)))
  (match-or-nil [_ other]
    (let [other-lines (:lines other)
          left-edge (apply str (map first lines))
          other-left-edge (apply str (map first other-lines))
          right-edge (apply str (map last lines))
          other-right-edge (apply str (map last other-lines))]
      (cond (= (first lines) (last other-lines)) '(0 1)
            (= right-edge other-left-edge) '(1 0)
            (= (last lines) (first other-lines)) '(0 -1)
            (= left-edge other-right-edge) '(-1 0))))
  (print-debug [_]
    (println-info "ID:" id)
    (println-info "x,y:" coordinates)
    (doall (map (println-info) lines))))

(defn try-place-tile
  [start-tile other-tile]
  (let [possibilities (list other-tile
                            (rotate-cc other-tile)
                            (-> other-tile rotate-cc rotate-cc)
                            (-> other-tile rotate-cc rotate-cc rotate-cc)
                            (flip-y other-tile)
                            (-> other-tile flip-y rotate-cc)
                            (-> other-tile flip-y rotate-cc rotate-cc)
                            (-> other-tile flip-y rotate-cc rotate-cc rotate-cc))]
    (first (keep (fn [possibility]
                   (let [maybe-match (match-or-nil start-tile possibility)]
                     (if (some? maybe-match)
                       (assoc possibility :coordinates (map + (:coordinates start-tile) maybe-match)))))
                 possibilities))))

(defn parse-tile
  [tile-string]
  (let [[header & lines] (str/split-lines tile-string)
        [_ tile-id] (re-matches #"Tile (\d+):" header)]
    (->Tile (Integer/parseInt tile-id) lines nil)))

(defn parse-tiles
  [tiles-string]
  (map parse-tile (str/split tiles-string #"\n\n")))

(defn pass-through
  [active-tile-set unplaced-tile-set]
  (reduce
    (fn [[new-active unplaced] active-tile]
      (let [newly-placed-tiles (keep #(try-place-tile active-tile %) unplaced)
            newly-placed-tile-ids (set (map :id newly-placed-tiles))
            new-unplaced (remove (fn [t] (contains? newly-placed-tile-ids (:id t))) unplaced)]
        (list (set/union (set newly-placed-tiles) new-active) new-unplaced)))
    (list #{} unplaced-tile-set)
    active-tile-set))

(defn do-main
  [tiles]
  (loop
    [set-tiles #{}
     active-set #{(assoc (first tiles) :coordinates '(0 0))}
     unset-set (set (rest tiles))]
    (if (empty? unset-set)
      (set/union set-tiles active-set)
      (let [[new-active new-unset] (pass-through active-set unset-set)]
        (recur (set/union set-tiles active-set) new-active new-unset)))))

(defn main-1
  []
  (let [sorted (do-main (parse-tiles day-20-input))
        min-x (apply min (map (fn [t] (first (:coordinates t))) sorted))
        max-x (apply max (map (fn [t] (first (:coordinates t))) sorted))
        min-y (apply min (map (fn [t] (second (:coordinates t))) sorted))
        max-y (apply max (map (fn [t] (second (:coordinates t))) sorted))
        corners (filter
                  (fn [t]
                    (or (= (:coordinates t) (list min-x min-y))
                        (= (:coordinates t) (list max-x min-y))
                        (= (:coordinates t) (list min-x max-y))
                        (= (:coordinates t) (list max-x max-y))))
                  sorted)]
    (println (apply * (map :id corners)))))

(defn main-2
  []
  (println day-20-input))

      