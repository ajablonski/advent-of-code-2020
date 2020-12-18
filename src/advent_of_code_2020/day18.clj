(ns advent-of-code-2020.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]
            [instaparse.core :as insta]))

(def day-18-input (slurp (io/resource "day18.txt")))

(def parse-expression-equal-precedence
  (insta/parser "
    expression = term { <' '> operation <' '> term }
    <term> = digit | <'('> expression <')'>
    digit = #'\\d'
    operation = '*' | '+'"))

(defn evaluate-expression-equal-precedence
  ([[expr-type & rest-expr]]
   (cond (= expr-type :expression) (first
                                     (reduce
                                       (fn [[total op] argument]
                                         (if (nil? op)
                                           (list total (evaluate-expression-equal-precedence argument))
                                           (list (op total (evaluate-expression-equal-precedence argument)) nil)))
                                       (list 0 +)
                                       rest-expr))
         (= expr-type :digit) (Integer/parseInt (first rest-expr))
         (= expr-type :operation) (resolve (symbol (first rest-expr)))
         :else (do (println "Fell through at " expr-type "\nwith rest: " rest-expr)))))

(defn main-1
  []
  (println
    (apply
      +
      (map
        #(evaluate-expression-equal-precedence (parse-expression-equal-precedence %))
        (str/split-lines day-18-input)))))

(def parse-expression-addition-precedence
  (insta/parser "
    expression = term { <' '> <'*'> <' '> term }
    term = added { <' '> <'+'> <' '> added }
    added = digit | <'('> expression <')'>
    digit = #'\\d'"))

(defn evaluate-expression-addition-precedence
  ([[expr-type & rest-expr]]
   (cond (= expr-type :expression) (apply * (map evaluate-expression-addition-precedence rest-expr))
         (= expr-type :term) (apply + (map evaluate-expression-addition-precedence rest-expr))
         (= expr-type :added) (apply evaluate-expression-addition-precedence rest-expr)
         (= expr-type :digit) (Integer/parseInt (first rest-expr))
         :else (do (println "Fell through at " expr-type "\nWith rest: " rest-expr)))))

(defn main-2
  []
  (println
    (apply
      +
      (map
        #(evaluate-expression-addition-precedence (parse-expression-addition-precedence %))
        (str/split-lines day-18-input)))))

      