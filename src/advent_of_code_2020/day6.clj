(ns advent-of-code-2020.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def day-6-input (slurp (io/resource "day6.txt")))

(defn parse-answers-string
  [group-string]
  (map set (str/split-lines group-string)))

(defn parse-groups-string
  [groups-string]
  (map parse-answers-string (str/split groups-string #"\n\n")))

(defn count-yes-answers-with-joiner-fn
  [joiner]
  (fn [groups]
    (reduce
      #(+ %1 (count (reduce joiner %2)))
      0
      groups)))

(defn- do-main
  [joiner]
  (println
    ((count-yes-answers-with-joiner-fn joiner)
     (parse-groups-string day-6-input))))

(defn main-1
  []
  (do-main set/union))

(defn main-2
  []
  (do-main set/intersection))
