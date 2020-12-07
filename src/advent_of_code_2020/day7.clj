(ns advent-of-code-2020.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def day-7-input (io/resource "day7.txt"))

(defn parse-rule [rule]
  (let [[bag-type contained-strings] (str/split rule #" bags contain ")
        initial-rule {bag-type []}]
    (if (= contained-strings "no other bags.")
      initial-rule
      (reduce
        (fn [result contained-string]
          (let [[_ quantity contained-color]
                (re-find #"([0-9]+) (.*) bag" contained-string)]
            (update result bag-type
                    #(conj
                       %
                       {:color    contained-color
                        :quantity (Integer/parseInt quantity)})
                    )))
        initial-rule
        (str/split contained-strings #", ")))))

(defn parse-rules [rules-string]
  (reduce merge (map parse-rule (str/split-lines rules-string))))

(defn get-can-contain
  [rules-map color]
  (let [direct-containers
        (map
          key
          (filter (fn [[_ v]] (some #(= (:color %) color) v)) rules-map))]
    (set/union (set direct-containers)
               (reduce
                 #(set/union
                    %1
                    (get-can-contain rules-map %2)) #{} direct-containers))))

(defn get-bags-contained
  [rules-map bag-type & {:keys [multiplier] :or {multiplier 1}}]
  (reduce
    (fn [bag-map {color :color quantity :quantity}]
      (merge-with
        +
        (update bag-map color
                (fn [old-value]
                  (if (nil? old-value)
                    (* quantity multiplier)
                    (+ old-value (* quantity multiplier)))))
        (get-bags-contained
          rules-map color :multiplier (* multiplier quantity))))
    {}
    (get rules-map bag-type))
  )

(defn main-1
  []
  (let [rules-map (parse-rules (slurp day-7-input))]
    (println
      (count (get-can-contain rules-map "shiny gold")))))

(defn main-2
  []
  (let [rules-map (parse-rules (slurp day-7-input))]
    (println
      (reduce
        (fn [sum [_ v]] (+ sum v))
        0
        (get-bags-contained rules-map "shiny gold")))))
