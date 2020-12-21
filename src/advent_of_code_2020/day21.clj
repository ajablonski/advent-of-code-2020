(ns advent-of-code-2020.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]
            [clojure.set :as set]))

(def day-21-input (slurp (io/resource "day21.txt")))

(defn parse-food
  [food-string]
  (let [[_ food-part allergen-part] (re-matches #"(.*) \(contains (.*)\)" food-string)]
    {:ingredients (set (str/split food-part #" ")) :allergens (set (str/split allergen-part #", "))}))

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
    (println (apply + (map #(count-ingredient-appearances % allergen-free-ingredients) food-list)))))

(defn main-2
  []
  (let [food-list (parse-foods day-21-input)
        allergen-free-ingredients (get-allergen-free-ingredients food-list)
        allergens (get-all-allergens food-list)
        foods-with-only-allergen-ingredients (map #(update % :ingredients (fn [old-ingredients] (set/difference old-ingredients allergen-free-ingredients))) food-list)
        allergens-and-ingredients (map (fn [allergen] (list allergen (get-possible-culprits-for-allergen allergen foods-with-only-allergen-ingredients))) allergens)
        allergen-ingredient-map (loop
                [end-map (sorted-map)
                 known-ingredients #{}
                 to-process allergens-and-ingredients
                 ]
                (println-info "End map" end-map "known ingredients" known-ingredients "to process" to-process)
                (if (empty? to-process)
                  end-map
                  (let [[[a is] & rest] (sort-by #(count (second %)) to-process)
                        ingredient (first is)]
                    (recur
                      (assoc end-map a ingredient)
                      (conj known-ingredients ingredient)
                      (map (fn [[a is]] (list a (disj is ingredient))) rest))
                    )))]
    (println (str/join "," (map second allergen-ingredient-map)))))
      