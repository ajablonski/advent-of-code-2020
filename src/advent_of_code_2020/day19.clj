(ns advent-of-code-2020.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]
            [instaparse.core :as insta]))

(def day-19-input (slurp (io/resource "day19.txt")))

(defn main-1
  []
  (let [[rules-part vals-part] (str/split day-19-input #"\n\n")
        parser (insta/parser (str "S: 0\n" rules-part))]
    (println (count (filter #(not (empty? %)) (map #(insta/parses parser %) (str/split-lines vals-part)))))))

(defn main-2
  []
  (let [[rules-part vals-part] (str/split day-19-input #"\n\n")
        parser (insta/parser
                 (str/replace
                   (str/replace
                     (str "S: 0\n" rules-part)
                     "8: 42"
                     "8: 42 | 42 8")
                   "11: 42 31"
                   "11: 42 31 | 42 11 31"))]
    (println (count (filter #(not (empty? %)) (map #(insta/parses parser %) (str/split-lines vals-part)))))))

      