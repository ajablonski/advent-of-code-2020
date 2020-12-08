(ns advent-of-code-2020.utils)


(defn map-vals
  "Given a map m, applies f to all values in the map"
  [f m]
  (zipmap (keys m) (map f (vals m))))
