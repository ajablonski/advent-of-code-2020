(ns advent-of-code-2020.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day-2-input (io/resource "day2/input1.txt"))

(defprotocol PasswordMatcher
  (pw-matches? [this pw])
  (pw-matches-rule-2? [this pw]))

(defrecord Rule [min max letter]
  PasswordMatcher
  (pw-matches? [_ pw] (let [letter-count (count (filter #(= % letter) pw))]
                        (and (>= letter-count min)
                             (<= letter-count max))))
  (pw-matches-rule-2? [_ pw] (let [letter-1 (.charAt pw (- min 1))
                                   letter-2 (.charAt pw (- max 1))]
                        (and (or (= letter-1 letter) (= letter-2 letter))
                             (not (and (= letter-1 letter) (= letter-2 letter)))))))

(defrecord RuleAndPassword [rule password])

(defn parse-line
  [line]
  (let [[rule-part pw] (str/split line #": ")
        [range-part character] (str/split rule-part #" ")
        [min max] (str/split range-part #"-")]
    (RuleAndPassword.
      (Rule.
        (Integer/parseInt min)
        (Integer/parseInt max)
        (.charAt character 0))
      pw)))

(defn parse-lines
  [s]
  (map parse-line (str/split s #"\n")))

(defn main-1
  []
  (let [rules-and-pws (parse-lines (slurp day-2-input))]
    (println
      (reduce
        (fn [count rule-and-pw]
          (let [{rule :rule
                 pw   :password} rule-and-pw]
            (if (.pw-matches? rule pw)
              (+ 1 count)
              count)))
        0
        rules-and-pws))))

(defn main-2
  []
  (let [rules-and-pws (parse-lines (slurp day-2-input))]
    (println
      (reduce
        (fn [count rule-and-pw]
          (let [{rule :rule
                 pw   :password} rule-and-pw]
            (if (.pw-matches-rule-2? rule pw)
              (+ 1 count)
              count)))
        0
        rules-and-pws))))
