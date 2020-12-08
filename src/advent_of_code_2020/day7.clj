(ns advent-of-code-2020.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2020.utils :refer :all]))

(def day-7-input (io/resource "day7.txt"))

(defn parse-rule [rule]
  (let [[bag-type contained-strings] (str/split rule #" bags contain ")]
    {bag-type
     (if (= contained-strings "no other bags.")
       []
       (reduce
         (fn [result contained-string]
           (let [[_ quantity contained-color]
                 (re-find #"([0-9]+) (.*) bag" contained-string)]
             (conj
               result
               {:color    contained-color
                :quantity (Integer/parseInt quantity)})))
         []
         (str/split contained-strings #", ")))}))

(defn parse-rules [rules-string]
  (reduce merge (map parse-rule (str/split-lines rules-string))))

(defn get-bags-containing
  "Gets bags that can contain a certain bag color, directly or indirectly"
  [rules-map color]
  (let [direct-containers
        (keep
          (fn [[k v]] (if (some #(= (:color %) color) v) k))
          rules-map)]
    (set/union (set direct-containers)
               (reduce
                 #(set/union
                    %1
                    (get-bags-containing rules-map %2)) #{} direct-containers))))

(declare get-bags-contained-by)

(defn- add-other-bag's-bags-fn
  "Returns a function that merges in another bag's map of bags to quantities with an existing map of bags to quantities"
  [rules-map]
  (fn [bag-map {color :color quantity :quantity}]
    (merge-with
      +
      bag-map
      {color quantity}
      (map-vals #(* % quantity) (get-bags-contained-by rules-map color)))))

(defn get-bags-contained-by
  "Gets bags that a certain bag color contains, directly or indirectly"
  [rules-map bag-type]
  (reduce
    (add-other-bag's-bags-fn rules-map)
    {}
    (get rules-map bag-type)))

(defn main-1
  []
  (let [rules-map (parse-rules (slurp day-7-input))]
    (println
      (count (get-bags-containing rules-map "shiny gold")))))

(defn main-2
  []
  (let [rules-map (parse-rules (slurp day-7-input))]
    (println
      (reduce
        +
        (vals (get-bags-contained-by rules-map "shiny gold"))))))
