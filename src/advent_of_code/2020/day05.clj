(ns advent-of-code.2020.day05
  (:require [clojure.string :as str]))

(def input-file (slurp "resources/2020/day05.txt"))

(def input
  (->> (str/split input-file #"\n")
       (mapv #(vec (replace {\F 0 \B 1 \L 0 \R 1} %)))))

(defn bs->int
  [bs]
  (let [powers (iterate (partial * 2) 1)]
    (reduce + (map * powers (reverse bs)))))

(defn part1
  [input]
  (-> (sort input)
      last
      bs->int))

(comment

  (part1 input)
  ;; 883
  )

(defn part2
  [input]
  (->> (sort input)
       (mapv bs->int)
       (partition 2 1)
       (filter #(not= -1 (apply - %)))
       first
       first
       inc))

(comment

  (part2 input)
  ;; 532
  )
