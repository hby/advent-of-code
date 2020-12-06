(ns advent-of-code.2020.day06
  (:require [clojure.string :as str]
            [clojure.set :as cs]))

(def input-file (slurp "resources/2020/day06.txt"))

(def input
  (->> (str/split input-file #"\n\n")
       (mapv #(str/split % #"\n"))))


(defn part1
  [input]
  (->> input
       (map #(apply concat %))
       (map distinct)
       (map count)
       (reduce +)))

(comment

  (part1 input)
  ;; 6662
  )

(defn part2
  [input]
  (->> input
       (map #(map set %))
       (map #(apply cs/intersection %))
       (map count)
       (reduce +)))

(comment

  (part2 input)
  ;; 3382
  )
