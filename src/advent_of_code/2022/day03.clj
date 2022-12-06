(ns advent-of-code.2022.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (->> (slurp "resources/2022/day03.txt")
                (str/split-lines)))

(def item->priority
  (merge (zipmap "abcdefghijklmnopqrstuvwxyz" (range 1 27))
         (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 27 53))))

(defn part1
  [input]
  (->> input
       (map #(split-at (/ (count %) 2) %))
       (map #(map set %))
       (map #(apply set/intersection %))
       (map #(item->priority (first %)))
       (apply +)))

(comment
  (part1 input)
  ;; => 8185
  :end)

(defn part2
  [input]
  (->> input
       (map set)
       (partition-all 3)
       (map #(apply set/intersection %))
       (map #(item->priority (first %)))
       (apply +)
       ))

(comment
  (part2 input)
  ;; => 2817
  :end)
