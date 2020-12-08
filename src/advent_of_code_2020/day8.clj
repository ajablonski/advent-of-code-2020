(ns advent-of-code-2020.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day-8-input (str/split-lines (slurp (io/resource "day8.txt"))))

(defn acc
  [[accumulator index] argument]
  (list (+ accumulator argument) (+ index 1)))

(defn nop
  [[accumulator index] _]
  (list accumulator (+ index 1)))

(defn jump
  [[accumulator index] argument]
  (list accumulator (+ index argument)))

(defn parse-instruction
  [instruction-string]
  (let [[_ inst sign number]
        (re-find #"([a-z]{3}) ([+-])([0-9]+)" instruction-string)
        argument ((if (= sign "-") - +) (Integer/parseInt number))
        inst-fn (cond (= inst "acc") acc (= inst "jmp") jump :else nop)]
    (fn [state] (inst-fn state argument))))


(defn build-program
  [instruction-list]
  (fn [accumulator idx visited-idxs]
    (let [[new-acc new-idx]
          ((get instruction-list idx) (list accumulator idx))
          new-visited-idxs (conj visited-idxs idx)]
      (cond (contains? new-visited-idxs new-idx) (list new-acc :fail)
            (>= new-idx (count instruction-list)) (list new-acc :success)
            :else (recur new-acc new-idx new-visited-idxs)))))


(defn execute-program
  [program-fn]
  (program-fn 0 0 #{}))

(defn replace-instr
  [instr]
  (cond (str/starts-with? instr "jmp") (str/replace instr "jmp" "nop")
        (str/starts-with? instr "nop") (str/replace instr "nop" "jmp")))

(defn main-1
  []
  (let [instruction-fns (vec (map parse-instruction day-8-input))
        program (build-program instruction-fns)]
    (println (first (execute-program program)))))

(defn main-2
  []
  (let [replaceable-instr-idxs (keep-indexed
                                 (fn [idx item]
                                   (if (re-find #"nop|jmp" item) idx))
                                 day-8-input)
        programs (map
                   (fn [idx-to-replace]
                     (build-program
                       (vec
                         (map parse-instruction
                              (update day-8-input idx-to-replace replace-instr)))))
                   replaceable-instr-idxs)
        successful-program-results
        (filter #(= (second %) :success) (map execute-program programs))]
    (println
      (first (first successful-program-results)))))
