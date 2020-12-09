(ns advent-of-code-2020.utils)


(defn map-vals
  "Given a map m, applies f to all values in the map"
  [f m]
  (zipmap (keys m) (map f (vals m))))

(defn find-first
  "Given a predicate and a collection,
  finds and returns the first item matching the predicate,
  or nil if none is found"
  [p coll]
  (first (filter p coll)))
