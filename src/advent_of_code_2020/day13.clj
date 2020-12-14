(ns advent-of-code-2020.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-13-input (slurp (io/resource "day13.txt")))

(defn main-1
  []
  (let [[min-time routes-string] (str/split-lines day-13-input)
        min-time-int (Integer/parseInt min-time)
        routes (map #(Integer/parseInt %) (filter #(not (= "x" %)) (str/split routes-string #",")))
        route-and-earliest-time-per-route
        (map (fn [route-num]
               (list route-num (first (filter #(>= % min-time-int)
                                              (map #(* % route-num) (range)))))) routes)
        [earliest-route earliest-time] (first (sort-by second route-and-earliest-time-per-route))]
    (println (* earliest-route (- earliest-time min-time-int)))))

(defn find-first-instance-of-combined-event
  "Given
    - a base event (base-name) that occurs every base-period time units, with first instance at base-offset (from 0)
    - another event (other-name) that occurs every other-period time units, other-offset units after base event

  Returns the period, offset (from 0), and name of the combined event (base occurring and other occurring other-offset units later)"
  [base-period base-offset base-name other-period other-offset other-name]
  (let [base-event-occurrences (map #(+ base-offset (* base-period %)) (range))
        combined-event-occurrences (filter
                                     #(= 0 (mod (+ % other-offset) other-period))
                                     base-event-occurrences)
        combined-event-offset (first combined-event-occurrences)
        combined-event-period (* base-period other-period)
        combined-event-name (str base-name " followed by " other-name)]
    (printf-info "%s happens every %s minutes, starting at %s\n" combined-event-name combined-event-period combined-event-offset)
    (list combined-event-period combined-event-offset combined-event-name)))

(defn main-2
  []
  (let [[_ routes-string] (str/split-lines day-13-input)
        routes (str/split routes-string #",")
        routes-and-offsets (map
                             (fn [[route offset]]
                               (list
                                 (bigint route)
                                 offset
                                 (str "Bus " route (if (= offset 0) (format " arriving %s minutes later" offset)))))
                             (filter
                               #(not (= (first %) "x"))
                               (map list routes (range))))]
    (println
      (second
        (reduce
          (fn [[period-a offset-a name-a] [freq-b offset-b name-b]]
            (find-first-instance-of-combined-event period-a offset-a name-a freq-b offset-b name-b))
          (first routes-and-offsets)
          (rest routes-and-offsets))))))
