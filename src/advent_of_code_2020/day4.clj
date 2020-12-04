(ns advent-of-code-2020.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day-4-input (io/resource "day4/input1.txt"))

(defn to-document
  [s]
  (apply merge
         (map #(hash-map (keyword (get % 0)) (get % 1))
              (map #(str/split % #":")
                   (str/split s #"\s")))))

(let [required-keys [:byr :iyr :eyr :hgt :hcl :ecl :pid]]
  (defn valid-document?
    [doc]
    (reduce
      (fn [valid key] (and valid (contains? doc key)))
      true
      required-keys)))

(defn valid-height?
  [height]
  (or
    (and (str/ends-with? height "cm") (<= 150 (Integer/parseInt (str/replace height "cm" "")) 193))
    (and (str/ends-with? height "in") (<= 59 (Integer/parseInt (str/replace height "in" "")) 76))))

(defn valid-haircolor? [hair-color]
  (some? (re-matches #"#[0-9a-z]{6}" hair-color)))

(let [valid-eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}]
  (defn valid-eyecolor? [eye-color]
    (contains? valid-eye-colors eye-color)))

(defn valid-pid? [passport-id]
  (some? (re-matches #"[0-9]{9}" passport-id)))

(defn extra-valid-document?
  [doc]
  (and (valid-document? doc)
       (<= 1920 (Integer/parseInt (doc :byr)) 2002)
       (<= 2010 (Integer/parseInt (doc :iyr)) 2020)
       (<= 2020 (Integer/parseInt (doc :eyr)) 2030)
       (valid-height? (doc :hgt))
       (valid-haircolor? (doc :hcl))
       (valid-pid? (doc :pid))
       (valid-eyecolor? (doc :ecl))))

(defn main-1
  []
  (let [doc-strings (str/split (slurp day-4-input) #"\n\n")
        docs (map to-document doc-strings)]
    (println
      (count (filter valid-document? docs)))))

(defn main-2
  []
  (let [doc-strings (str/split (slurp day-4-input) #"\n\n")
        docs (map to-document doc-strings)]
    (println
      (count (filter extra-valid-document? docs)))))
