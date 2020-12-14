(ns advent-of-code-2020.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-13-input (slurp (io/resource "day13.txt")))

(defprotocol EventP
  (get-occurrences [_])
  (get-first-departure-on-or-after [_ time])
  (get-description [_]))

(defrecord Event [period offset name]
  EventP
  (get-occurrences [_]
    (map #(+ offset (* period %)) (range)))
  (get-first-departure-on-or-after [this time]
    (first (filter #(>= % time) (get-occurrences this))))
  (get-description [_]
    (format "%s happens every %s minutes, starting at t = %s"
            name period offset)))

(defn parse-routes-with-initial-offsets
  [routes-string offsets]
  (map
    (fn [[route offset]]
      (Event.
        (bigint route)
        offset
        (format
          "Bus %s at %s"
          route
          (if (= offset 0) "t" (format "t + %s" offset)))))
    (filter
      #(not (= (first %) "x"))
      (map list (str/split routes-string #",") offsets))))

(defn main-1
  []
  (let [[min-time routes-string] (str/split-lines day-13-input)
        routes (parse-routes-with-initial-offsets routes-string (repeat 0))
        min-time-int (Integer/parseInt min-time)
        earliest-route (apply
                         min-key
                         #(get-first-departure-on-or-after % min-time-int)
                         routes)]
    (println
      (*
        (:period earliest-route)
        (- (get-first-departure-on-or-after earliest-route min-time-int)
           min-time-int)))))

(defn combine-events
  "Given
    - a base event (base-name) that occurs every base-period time units,
      with first instance at base-offset (from 0)
    - another event (other-name) that occurs every other-period time units,
      other-offset units after base event

  Returns a new event reflecting the combined event
    (base occurring and other occurring other-offset units later)"
  [base-event other-event]
  (let [combined-event-occurrences
        (filter
          #(= 0 (mod (+ % (:offset other-event)) (:period other-event)))
          (get-occurrences base-event))
        combined-event
        (Event. (* (:period base-event) (:period other-event))
                (first combined-event-occurrences)
                (str (:name base-event) " followed by " (:name other-event)))]
    (println-info (get-description combined-event))
    combined-event))

(defn main-2
  []
  (let [[_ routes-string] (str/split-lines day-13-input)]
    (println
      (:offset
        (reduce
          combine-events
          (parse-routes-with-initial-offsets routes-string (range)))))))
