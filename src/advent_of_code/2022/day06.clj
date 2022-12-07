(ns advent-of-code.2022.day06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "resources/2022/day06.txt"))

(defn part1
  [input]
  (->> input
       (partition 4 1)
       (map #(count (set %)))
       (take-while #(not= 4 %))
       (count)
       (+ 4)))

(comment
  (part1 input)
  ;; => 1356
  :end)

(defn part2
  [input]
  (->> input
       (partition 14 1)
       (map #(count (set %)))
       (take-while #(not= 14 %))
       (count)
       (+ 14)))

(comment
  (part2 input)
  ;; => 2564
  :end)

