(ns advent-of-code-2020.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def day-4-input (io/resource "day4.txt"))

(defn to-document
  [s]
  (let [entries (str/split s #"\s")
        k-v-pairs (map #(str/split % #":") entries)]
    (reduce
      (fn [result [k v]] (assoc result (keyword k) v))
      {}
      k-v-pairs)))

(let [required-keys #{:byr :iyr :eyr :hgt :hcl :ecl :pid}]
  (defn valid-document?
    [doc]
    (set/subset? required-keys (set (keys doc)))))

(defn valid-height?
  [height]
  (or
    (and (str/ends-with? height "cm")
         (<= 150 (Integer/parseInt (str/replace height "cm" "")) 193))
    (and (str/ends-with? height "in")
         (<= 59 (Integer/parseInt (str/replace height "in" "")) 76))))

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

(defn parse-docs
  [all-docs-string]
  (map to-document (str/split all-docs-string #"\n\n")))

(defn count-valid-docs
  [docs validator]
  (count (filter validator docs)))

(defn main-1
  []
  (println
    (count-valid-docs
      (parse-docs (slurp day-4-input))
      valid-document?)))

(defn main-2
  []
  (println
    (count-valid-docs
      (parse-docs (slurp day-4-input))
      extra-valid-document?)))
