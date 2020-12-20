(ns advent-of-code-2020.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]
            [instaparse.core :as insta])
  (:import (instaparse.gll Failure)))

(def day-19-input (slurp (io/resource "day19.txt")))

(defn parse-input
  ([input-string] (parse-input input-string '()))
  ([input-string replacements]
   (let [[rules-part vals-part] (str/split input-string #"\n\n")]
     (list
       (insta/parser
         (str "S: 0\n"
              (reduce
                (fn [result [replace with]] (str/replace result replace with))
                rules-part
                replacements)))
       (str/split-lines vals-part)))))

(defn parseable?
  [parser]
  (fn [input-str] (not (instance? Failure (insta/parse parser input-str)))))

(defn- do-main
  [parser inputs]
  (count
    (filter
      (parseable? parser)
      inputs)))

(defn main-1
  []
  (println (apply do-main (parse-input day-19-input))))

(defn main-2
  []
  (let [replacements (list '("8: 42" "8: 42 | 42 8") '("11: 42 31" "11: 42 31 | 42 11 31"))]
    (println (apply do-main (parse-input day-19-input replacements)))))

