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
  (strip-border [_] "Remove the border from the tile")
  (print-debug [_] "Print a debugging version of the tile")
  (combine-tile [_ other] "Combine with a tile to the right"))

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
  (strip-border [_]
    (->Tile id
            (map (fn [row] (let [row-vector (vec row)]
                             (apply str (subvec row-vector 1 (dec (count row-vector))))))
                 (subvec (vec lines) 1 (dec (count lines))))
            coordinates))
  (combine-tile [_ other]
    (let [other-lines (:lines other)]
      (->Tile id (map str lines other-lines) (list nil (second coordinates)))))
  (print-debug [_]
    (println-info "ID:" id)
    (println-info "x,y:" coordinates)
    (doall (map #(println-info %) lines))))

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

(defn find-tile-positions
  [tiles]
  (loop
    [set-tiles #{}
     active-set #{(assoc (first tiles) :coordinates '(0 0))}
     unset-set (set (rest tiles))]
    (if (empty? unset-set)
      (set/union set-tiles active-set)
      (let [[new-active new-unset] (pass-through active-set unset-set)]
        (recur (set/union set-tiles active-set) new-active new-unset)))))

(defn arrange-and-strip-tiles
  "Arranges tiles into a map of a map of row id to map of column id to tile"
  [tile-set]
  (apply
    sorted-map
    (mapcat
      (fn [[k tiles]]
        (list k
              (apply sorted-map
                     (mapcat
                       (fn [[k ts]] (list k (strip-border (first ts))))
                       (group-by (fn [t] (first (:coordinates t))) tiles)))))
      (group-by
        (fn [t] (second (:coordinates t)))
        tile-set))))

(defn find-all-indexes
  [substring whole-string]
  (loop [start-index 0
         matches-so-far '()]
    (let [match (str/index-of whole-string substring start-index)]
      (if (nil? match) (reverse matches-so-far) (recur (inc match) (cons match matches-so-far))))))

(defn- head-matches?
  [tile [head-row head-col]]
  (let [row (nth (:lines tile) head-row)
        does-head-match (= (nth row head-col) \#)]
    (when (not does-head-match) (println-info "Ignoring" head-row head-col "as head does not match"))
    does-head-match))

(defn- base-matches?
  [tile [base-row base-start-col]]
  (let [row (nth (:lines tile) base-row)]
    (some? (re-matches #"#..#..#..#..#..#" (subs row base-start-col (+ base-start-col 16))))))

(defn count-monsters
  "Returns the total count of sea monsters pairs containing sea monsters"
  [tile]
  (let [body-matches (apply concat
                            (map-indexed
                              (fn [row-num line] (mapcat (fn [match] (map #(list (inc row-num) %) (find-all-indexes match line))) (re-seq #"#....##....##....###" line)))
                              (rest (:lines tile))))
        total-matches (filter (fn [[row-match col-match]]
                                (and (head-matches? tile [(dec row-match) (+ col-match 18)])
                                     (base-matches? tile [(inc row-match) (+ col-match 1)])))
                              body-matches)]
    (println-info "Found matches with body starting at " total-matches)
    (count total-matches)))

(defn has-sea-monster?
  [tile]
  (not (= 0 (count-monsters tile))))

(defn find-orientation-with-monster
  [large-tile]
  (let [possibilities (list large-tile
                            (rotate-cc large-tile)
                            (-> large-tile rotate-cc rotate-cc)
                            (-> large-tile rotate-cc rotate-cc rotate-cc)
                            (flip-y large-tile)
                            (-> large-tile flip-y rotate-cc)
                            (-> large-tile flip-y rotate-cc rotate-cc)
                            (-> large-tile flip-y rotate-cc rotate-cc rotate-cc))
        orientations-with-monsters (filter has-sea-monster? possibilities)
        tile-orientation-with-monster (first orientations-with-monsters)]
    (when (> (count orientations-with-monsters) 1) (println "ERROR: More configurations found with monsters than expected"))
    tile-orientation-with-monster))

(defn join-tiles
  [tiles-with-coordinates]
  (let [tile-map (arrange-and-strip-tiles tiles-with-coordinates)
        tile-rows (map
                    (fn [[_ tiles-by-cols]] (reduce (fn [combined-tile tile] (combine-tile combined-tile tile)) (map second tiles-by-cols)))
                    (reverse tile-map))]
    (->Tile 0 (mapcat :lines tile-rows) '(0 0))))

(defn main-1
  []
  (let [sorted (find-tile-positions (parse-tiles day-20-input))
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

(defn get-sea-choppiness
  [tiles]
  (let [sorted (find-tile-positions tiles)
        large-tile (join-tiles sorted)
        correct-orientation (-> large-tile find-orientation-with-monster)
        monster-count (count-monsters correct-orientation)
        total-#s (apply + (map (fn [row] (count (filter #(= % \#) row))) (:lines correct-orientation)))]
    (- total-#s (* monster-count 15))))

(defn main-2
  []
  (println (get-sea-choppiness (parse-tiles day-20-input))))

      