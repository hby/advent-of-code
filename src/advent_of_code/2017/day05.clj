(ns advent-of-code.2017.day05
  (:require [clojure.string :as cs]))

(def puzzle
  (->> (slurp "resources/2017/day05.txt")
       (cs/split-lines)
       (mapv #(Integer/parseInt %))))

(defn exited?
  [[instructions position]]
  (or (neg? position)
      (> position (dec (count instructions)))))

(defn jump
  [[instructions position]]
  (let [offset (get instructions position)
        newpos (+ position offset)
        newins (assoc instructions position (inc offset))]
    [newins newpos]))

(defn one
  [puzzle]
  (let [start [puzzle 0]
        state-seq (iterate jump start)]
    (reduce (fn [i s]
              (if (exited? s)
                (reduced i)
                (inc i)))
            0
            state-seq)))

(comment
  (one puzzle))
  ; => 391540


(defn jump2
  [[instructions position]]
  (let [offset (get instructions position)
        newpos (+ position offset)
        newoff (if (>= offset 3) (dec offset) (inc offset))
        newins (assoc instructions position newoff)]
    [newins newpos]))

(defn two
  [puzzle]
  (let [start [puzzle 0]
        state-seq (iterate jump2 start)]
    (reduce (fn [i s]
              (if (exited? s)
                (reduced i)
                (inc i)))
            0
            state-seq)))

(comment
  (two puzzle))
  ; => 30513679

