(ns advent-of-code.2022.day04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def rx #"(\d+)-(\d+),(\d+)-(\d+)")
(def input (->> (slurp "resources/2022/day04.txt")
                (str/split-lines)
                (map (partial re-matches rx))
                (map (partial drop 1))
                (map #(map (fn [s] (Integer/parseInt s)) %))))


(defn contains
  [b1 e1 b2 e2]
  (or (and (<= b2 b1) (<= e1 e2))
      (and (<= b1 b2) (<= e2 e1))))

(defn part1
  [input]
  (->> input
       (map (partial apply contains))
       (filter true?)
       count))

(comment
  (part1 input)
  ;;=> 528
  :end)

(defn overlap
  [b1 e1 b2 e2]
  (not (or (< e1 b2)
           (< e2 b1))))

(defn part2
  [input]
  (->> input
       (map (partial apply overlap))
       (filter true?)
       count))

(comment
  (part2 input)
  ;;=> 881
  :end)
