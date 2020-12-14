(ns advent-of-code-2020.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2020.utils :refer :all]))

(def day-14-input (slurp (io/resource "day14.txt")))

(defn set-mask
  [new-mask]
  (fn [state]
    (println-debug "Setting mask to " new-mask)
    (assoc state :mask new-mask)))

(defn apply-mask
  [val mask]
  (bit-and
    (bit-or
      (Long/parseLong (str/replace mask "X" "0") 2)
      val)
    (Long/parseLong (str/replace mask "X" "1") 2)))

(defn set-memory
  [mem-address mem-val]
  (fn [state]
    (assoc-in state [:memory mem-address] (apply-mask mem-val (:mask state)))))

(defn parse-instruction
  [instruction memory-instr]
  (let [[mask-match mask-str] (re-find #"mask = ([1X0]{36})" instruction)
        [mem-match mem-addr-str mem-val-str] (re-find #"mem\[([0-9]+)\] = ([0-9]+)" instruction)]
    (println-debug "Parsing instruction " instruction)
    (cond (some? mask-match) (set-mask mask-str)
          (some? mem-match) (memory-instr (Long/parseLong mem-addr-str) (Long/parseLong mem-val-str)))))

(defn parse-instructions
  [instructions-string memory-instr]
  (map #(parse-instruction % memory-instr) (str/split-lines instructions-string)))

(defn main-1
  []
  (let [initial-state {:mask   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                       :memory {}}
        instructions (parse-instructions day-14-input set-memory)
        final-state (reduce
                      (fn [state instruction] (instruction state))
                      initial-state
                      instructions)]
    (println (apply + (vals (:memory final-state))))))

(defn resolve-float-bits
  ([address-with-floats index]
   (cond (= index 36) (list (Long/parseLong address-with-floats 2))
         (= (get address-with-floats index) \X)
         (concat
           (resolve-float-bits
             (apply str (assoc (vec (char-array address-with-floats)) index \0)) (+ index 1))
           (resolve-float-bits (apply str (assoc (vec (char-array address-with-floats)) index \1)) (+ index 1))
           )
         :else (resolve-float-bits address-with-floats (+ index 1))))
  ([address-with-floats]
   (resolve-float-bits address-with-floats 0)))

(defn apply-mask-to-mem
  [address-val mask]
  (let [address-val-str (Long/toBinaryString address-val)
        padding (apply str (repeat (- 36 (count address-val-str)) "0"))
        padded-address-val-string (str padding address-val-str)]
    (apply str
           (map
             (fn [a m] (if (= m \0) a m))
             padded-address-val-string
             mask))))

(defn set-memory-with-mask
  [mem-address mem-val]
  (fn [state]
    (let [addresses
          (resolve-float-bits
            (apply-mask-to-mem mem-address (:mask state)))]
      (println-debug "Setting " addresses " to " mem-val)
      (reduce
        (fn [intermediate-state address]
          (assoc-in intermediate-state [:memory address] mem-val))
        state
        addresses))))

(defn main-2
  []
  (let [initial-state {:mask   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                       :memory {}}
        instructions (parse-instructions day-14-input set-memory-with-mask)
        final-state (reduce
                      (fn [state instruction] (instruction state))
                      initial-state
                      instructions)]
    (println (apply + (vals (:memory final-state))))))
