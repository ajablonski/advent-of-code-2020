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
  (print-debug [_] "Print a debugging version of the tile (borders only)"))

(defrecord Tile [id borders coordinates] TileP
  (rotate-cc [_] (->Tile id (concat (rest borders) (list (first borders))) nil))
  (flip-y [_] (let [[old-b0 old-b1 old-b2 old-b3] borders]
                (->Tile id (map str/reverse (list old-b0 old-b3 old-b2 old-b1)) nil)))
  (match-or-nil [_ other]
    (let [[this-b0 this-b1 this-b2 this-b3] borders
          [other-b0 other-b1 other-b2 other-b3] (:borders other)]
      (cond (= this-b0 (str/reverse other-b2)) '(0 1)
            (= this-b1 (str/reverse other-b3)) '(1 0)
            (= this-b2 (str/reverse other-b0)) '(0 -1)
            (= this-b3 (str/reverse other-b1)) '(-1 0))))
  (print-debug [_]
    (let [[top right bottom left] borders
          left-side (subvec (vec (str/reverse left)) 1 (- (count left) 1))
          right-side (subvec (vec right) 1 (- (count left) 1))]
      (println-info "ID:" id)
      (println-info "x,y:" coordinates)
      (println-info top)
      (doall (map (fn [l r] (printf-info "%s        %s\n" l r)) left-side right-side))
      (println-info (str/reverse bottom)))))

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
        [_ tile-id] (re-matches #"Tile (\d+):" header)
        border-0 (first lines)
        border-1 (apply str (map last lines))
        border-2 (str/reverse (last lines))
        border-3 (apply str (reverse (map first lines)))]
    (->Tile (Integer/parseInt tile-id) (list border-0 border-1 border-2 border-3) nil)))

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

      