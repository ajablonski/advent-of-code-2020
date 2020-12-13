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


(defn extended-gcd
  "Extended GCD algorithm from
  https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode

  Returns (GCD of a and b, s, t)
  Such that s * a + t * b = GCD"
  [a b]
  (loop [old-r a
         r b
         old-s 1
         s 0
         old-t 0
         t 1]
    (if (= 0 r)
      (do
        (list old-r old-s old-t))
      (let [quotient (quot old-r r)]
        (recur
          r
          (- old-r (* quotient r))
          s
          (- old-s (* quotient s))
          t
          (- old-t (* quotient t)))))))

(defn combine-phase-rotations
  "Phase combination from https://math.stackexchange.com/a/3864593.
  Returns a (period, offset) combination at which the combined event of A reaching its phase and B reaching its phase occurs.

  Bus A comes every first-period minutes, offset by first-phase
  Bus B comes every second-period minutes, offset by second-phase (in the scenario we're searching for)

  The combination has period (* first-period second-period) when prime -- it will recur this often
  The phase represents the (positive) delta from the reference point when this occurs.

  This gives us the frequency (period) of the combined event -
  Bus A showing up, then Bus B showing up offset minutes later. It happens every frequency,
  and happens at offset during that period, using the same reference point.
  "
  [first-period first-phase second-period second-phase]
  (let [[gcd s _] (extended-gcd first-period second-period) ; Get GCD of the two periods,
            ; and multiple of the first required to advance the phase by gcd
            ; (which will have a corresponding value for the second period)
        phase-diff (- first-phase second-phase) ; Total phase difference to be overcome
        pd-mult (quot phase-diff gcd) ; phase difference is will be overcome after pd-mult periods of the GCD
            ; for us, this will just be phase-diff, since GCD is 1 for all combinations of primes
        combined-period (* second-period (quot first-period gcd))
            ; for us, just the product of the two prime periods
        combined-phase (mod (- first-phase (* first-period s pd-mult)) combined-period)
            ; take initial phase,
            ; subtract the difference accumulated by overcoming the phase difference,
            ; and adjust to the new combined period
        ]
    (list combined-period combined-phase)))

(defn main-2
  []
  (let [[_ routes-string] (str/split-lines day-13-input)
        routes (str/split routes-string #",")
        routes-and-offsets (map
                             (fn [[route offset]]
                               (list (bigint route) offset)) (filter #(not (= (first %) "x")) (map (fn [route offset] (list route offset)) routes (range))))
        routes-times-and-offsets (map
                                   (fn [[route offset]]
                                     (list route
                                      offset))
                                   routes-and-offsets)
        ]
    (println (second (reduce
      (fn [[freq-a offset-a] [freq-b offset-b]]
        (combine-phase-rotations freq-a offset-a freq-b (mod (- offset-b) freq-b)))
      (first routes-times-and-offsets)
      (rest routes-times-and-offsets))))))
