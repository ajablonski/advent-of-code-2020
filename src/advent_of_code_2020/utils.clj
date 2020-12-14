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

(def ^:dynamic *debug* false)
(def ^:dynamic *info* true)


(defmacro println-info
  [& args]
  `(when *info*
     (println ~@args)))

(defmacro printf-info
  [& args]
  `(when *info*
     (printf ~@args)))

(defmacro println-debug
  [& args]
  `(when *debug*
     (println ~@args)))

(defmacro printf-debug
  [& args]
  `(when *debug*
     (printf ~@args)))

(defmacro with-minimal-output
  [& body]
  `(binding [*debug* false
             *info* false] ~@body))
