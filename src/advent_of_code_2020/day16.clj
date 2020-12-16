(ns advent-of-code-2020.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]
            [clojure.set :as set]))

(def day-16-input (slurp (io/resource "day16.txt")))

(defprotocol RuleP
  (matches? [_ value]
    "Returns true if a value is permitted by this rule"))

(defrecord Rule [name range-1 range-2]
  RuleP
  (matches? [_ value]
    (let [[min1 max1] range-1
          [min2 max2] range-2]
      (or (<= min1 value max1)
          (<= min2 value max2)))))

(defn parse-your-ticket-section
  [your-ticket-section]
  (let [[_ your-ticket] (str/split-lines your-ticket-section)]
    (map #(Integer/parseInt %) (str/split your-ticket #","))))

(defn parse-other-ticket-section
  [other-ticket-section]
  (let [[_ & rest] (str/split-lines other-ticket-section)]
    (map (fn [ticket-line] (map #(Integer/parseInt %) (str/split ticket-line #","))) rest)))

(defn parse-rules-section
  [rules-section]
  (map (fn
         [rule-line]
         (let [[_ name min-1 max-1 min-2 max-2] (re-find #"([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)" rule-line)]
           (Rule. name [(Integer/parseInt min-1) (Integer/parseInt max-1)]
                  [(Integer/parseInt min-2) (Integer/parseInt max-2)])))
       (str/split-lines rules-section)))

(defn parse-sections
  [input-string]
  (let [[rules-section your-ticket-section other-tickets-section] (str/split input-string #"\n\n")]
    {
     :rules         (parse-rules-section rules-section)
     :your-ticket   (parse-your-ticket-section your-ticket-section)
     :other-tickets (parse-other-ticket-section other-tickets-section)
     }))

(defn get-invalid-fields-for-ticket
  [ticket rules]
  (filter (fn [field]
            (not (some (fn [rule] (matches? rule field)) rules))) ticket))

(defn get-invalid-fields-for-tickets
  [tickets rules]
  (mapcat #(get-invalid-fields-for-ticket % rules) tickets))

(defn main-1
  []
  (let [parsed-data (parse-sections day-16-input)]
    (println (apply + (get-invalid-fields-for-tickets (:other-tickets parsed-data) (:rules parsed-data))))))

(defn ticket-valid?
  [ticket rules]
  (empty? (get-invalid-fields-for-ticket ticket rules)))

(defn get-valid-tickets
  [tickets rules]
  (filter #(ticket-valid? % rules) tickets))

(defn get-possible-rules-for-field
  [field rules]
  (set (filter #(matches? % field) rules)))

(defn get-possible-rules
  [ticket rules]
  (map #(get-possible-rules-for-field % rules) ticket))

(defn assign-fields-to-ticket
  [ticket field-rule-map]
  (apply
    hash-map
    (flatten
      (map-indexed
        (fn [idx item]
          (list
            (:name (get field-rule-map idx))
            item))
        ticket))))

(defn get-field-to-rule-assignments
  [tickets rules]
  (let [valid-tickets (get-valid-tickets tickets rules)
        rules-per-tickets (map #(get-possible-rules % rules) valid-tickets)
        rules-per-field (reduce
                          (fn
                            [aggregate rules-per-ticket]
                            (map set/intersection aggregate rules-per-ticket))
                          rules-per-tickets)
        rules-per-field-with-index (map #(hash-map
                                           :index %1
                                           :rule-set %2) (range) rules-per-field)
        rules-per-field-sorted (sort-by #(count (:rule-set %)) rules-per-field-with-index)]
    (:field-rule-map
      (reduce
        (fn [{field-rule-map :field-rule-map used-rules :used-rules}
             {index :index rule-set :rule-set}]
          (let
            [rule-for-index (first (set/difference rule-set used-rules))]
            {:field-rule-map (assoc field-rule-map index rule-for-index)
             :used-rules     (conj used-rules rule-for-index)}))
        {:field-rule-map {}
         :used-rules     #{}}
        rules-per-field-sorted))))

(defn main-2
  []
  (let [{rules         :rules
         other-tickets :other-tickets
         your-ticket   :your-ticket} (parse-sections day-16-input)
        rule-per-field (get-field-to-rule-assignments other-tickets rules)
        parsed-ticket (assign-fields-to-ticket your-ticket rule-per-field)]
    (println-info "Ticket details: " parsed-ticket)
    (println
      (apply
        *
        (keep
          (fn [[k v]] (when (str/starts-with? k "departure") v))
          parsed-ticket)))))
