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
  Returns a (period, offset) for a wave function of the combined event
  of A departing (with its offset) and B departing (with its offset)
                         this is the point we're looking for.
                           V
  A:   |____|____|____|____|____|____|____| period 5, offset 0
  B:   |______|______|______|______|______| period 7, offset 0

  We can form a combined frequency of this event by representing a phase-shifted B:

  A :  |____|____|____|____|____|____|____| period 5, offset 0
  B':  ______|______|______|______|______|_ period 7, offset -1/+6

  The phase shift here is -1, or +6 in mod 7\n

  Adding together, and counting only when doubled up, we end up with

  A+B: ____________________|_______________ period 7+5 = 35, phase shift 20

  The phase shift comes from extended GCD, where s * freq_a + t * freq_b = GCD.
  All our input is prime, so GCD is always one. s and t give one solution to
  the number of cycles of their respective frequencies needed to achieve a phase shift of GCD.

  For 5 and 7, s = 3 and t = -2
  Phase diff here is (- 0 6) = -6
  So we need to 'advance' by that many cycles relative to the phase of A.

  We know the combined period is 35.
  And we know we can achieve a difference of 1 every s periods of A.

  So we take s = 3, A period = 5 => 15, times -6 required phase diff => -90.

  Subtract from our first phase gives 0 - -90 = 90.

  Translate to the modulo of our new period: 90 mod 35 = 20

  So period is 35, offset (phase) by 20 from initial reference point.
  "
  [first-period first-phase first-event-name second-period second-phase second-event-name]
  (let [[gcd s _] (extended-gcd first-period second-period)
        phase-diff (- first-phase second-phase)
        pd-mult (quot phase-diff gcd)
        combined-period (* second-period (quot first-period gcd))
        combined-phase (mod (- first-phase (* first-period s pd-mult)) combined-period)
        combined-event-name (str first-event-name " followed by " second-event-name)]
    (printf-debug "GCD: %s S: %s\n" gcd s)
    (printf-debug "Phase diff: %s\n" phase-diff)
    (printf-debug "Period multiplier: %s\n" pd-mult)
    (printf-debug "Combined period: %s\n" combined-period)
    (printf-debug "Combined phase: %s (mod %s %s)\n" combined-phase (- first-phase (* first-period s pd-mult)) combined-period)
    (printf-info "%s happens every %s minutes, starting at %s\n" combined-event-name combined-period combined-phase)
    (list combined-period combined-phase combined-event-name)))

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
            (combine-phase-rotations period-a offset-a name-a freq-b (mod (- offset-b) freq-b) name-b))
          (first routes-and-offsets)
          (rest routes-and-offsets))))))
