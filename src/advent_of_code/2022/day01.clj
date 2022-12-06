(ns advent-of-code.2022.day01
  (:require [clojure.string :as str]))

(def input (->> (slurp "resources/2022/day01.txt")
                (str/split-lines)
                (partition-by str/blank?)
                (remove (partial = [""]))
                (map (fn [ns] (map #(Integer/parseInt %) ns)))))

(defn part1
  [input]
  (->> input
       (map (partial apply +))
       (apply max)))

(comment
  (part1 input)
  ;; => 72017
  :end)

(defn part2
  [input]
  (->> input
       (map (partial apply +))
       (sort #(compare %2 %1))
       (take 3)
       (apply +)))

(comment
  (part2 input)
  ;; => 212520
  :end)
