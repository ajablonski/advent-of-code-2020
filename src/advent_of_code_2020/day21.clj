(ns advent-of-code-2020.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]
            [clojure.set :as set]))

(def day-21-input (slurp (io/resource "day21.txt")))

(defn parse-food
  [food-string]
  (let [[_ food-part allergen-part] (re-matches #"(.*) \(contains (.*)\)" food-string)]
    {:ingredients (set (str/split food-part #" "))
     :allergens   (set (str/split allergen-part #", "))}))

(defn parse-foods
  [foods-string]
  (map parse-food (str/split-lines foods-string)))

(defn get-all-allergens
  [food-list]
  (apply set/union (map :allergens food-list)))

(defn get-possible-culprits-for-allergen
  [allergen food-list]
  (apply set/intersection
         (map :ingredients (filter #(contains? (:allergens %) allergen) food-list))))

(defn get-all-ingredients
  [food-list]
  (apply set/union (map :ingredients food-list)))

(defn get-allergen-potential-ingredients
  [food-list]
  (let [allergens (get-all-allergens food-list)]
    (apply set/union
           (map #(get-possible-culprits-for-allergen % food-list) allergens))))

(defn get-allergen-free-ingredients
  [food-list]
  (let [ingredients (get-all-ingredients food-list)]
    (set/difference ingredients (get-allergen-potential-ingredients food-list))))

(defn count-ingredient-appearances
  [food ingredients]
  (count (set/intersection (:ingredients food) ingredients)))

(defn main-1
  []
  (let [food-list (parse-foods day-21-input)
        allergen-free-ingredients (get-allergen-free-ingredients food-list)]
    (println
      (apply
        +
        (map
          #(count-ingredient-appearances % allergen-free-ingredients)
          food-list)))))

(defn get-allergen-causing-ingredients
  [food-list]
  (loop [allergen-ingredient-map (sorted-map)
         to-process (map
                      #(list
                         %
                         (get-possible-culprits-for-allergen
                           %
                           food-list))
                      (get-all-allergens food-list))]
    (println-info "End map" allergen-ingredient-map
                  "To process" to-process)
    (if (empty? to-process)
      allergen-ingredient-map
      (let [[[a is] & rest] (sort-by #(count (second %)) to-process)
            ingredient (first is)]
        (recur
          (assoc allergen-ingredient-map a ingredient)
          (map (fn [[a is]] (list a (disj is ingredient))) rest))))))

(defn main-2
  []
  (->>
    day-21-input
    parse-foods
    get-allergen-causing-ingredients
    (map second)
    (str/join ",")
    println))
