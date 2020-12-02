(ns advent-of-code-2020.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day-2-input (io/resource "day2/input1.txt"))

(defprotocol PasswordMatcher
  (pw-matches-count-rule? [this pw])
  (pw-matches-index-rule? [this pw]))

(defrecord Rule [^Integer num-1 ^Integer num-2 ^Character letter]
  PasswordMatcher
  (pw-matches-count-rule? [_ pw]
    (let [letter-count (count (filter #(= % letter) pw))
          min num-1
          max num-2]
      (and (>= letter-count min)
           (<= letter-count max))))
  (pw-matches-index-rule? [_ pw]
    (let [index-1 (- num-1 1)
          index-2 (- num-2 1)
          letter-1 (.charAt ^String pw index-1)
          letter-2 (.charAt ^String pw index-2)]
      (and (or (= letter-1 letter) (= letter-2 letter))
           (not (and (= letter-1 letter) (= letter-2 letter)))))))

(defrecord RuleAndPassword [rule password])

(defn parse-line
  [line]
  (let [[_ num-1 num-2 character pw]
        (re-find #"(\d+)-(\d+) ([a-z]): ([a-z]+)" line)]
    (RuleAndPassword.
      (Rule.
        (Integer/parseInt num-1)
        (Integer/parseInt num-2)
        (.charAt ^String character 0))
      pw)))

(defn parse-lines
  [s]
  (map parse-line (str/split s #"\n")))

(defn count-matches
  [rules-and-pws matcher-fn]
  (reduce
    (fn [count rule-and-pw]
      (let [{rule :rule
             pw   :password} rule-and-pw]
        (if (matcher-fn rule pw)
          (+ 1 count)
          count)))
    0
    rules-and-pws))

(defn main-1
  []
  (println
    (count-matches
      (parse-lines (slurp day-2-input))
      pw-matches-count-rule?)))

(defn main-2
  []
  (println
    (count-matches
      (parse-lines (slurp day-2-input))
      pw-matches-index-rule?)))
