(ns advent-of-code.2018.day03
  (:require [clojure.java.io :as io]))

(def pattern #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(def input-data
  (->> (io/reader "resources/2018/day03.txt")
       line-seq
       (map #(re-find pattern %))
       (map #(drop 1 %))
       (map (partial map #(Integer/parseInt %)))))

(defn mk-map
  [x y w h]
  (into {}
        (for [xk (range x (+ x w))
              yk (range y (+ y h))]
          [[xk yk] 1])))

(defn part1
  [input]
  (->> (reduce (fn [acc [n x y w h]]
                 (merge-with + acc (mk-map x y w h)))
               {}
               input)
       vals
       (filter #(< 1 %))
       count))

(comment
  (part1 input-data))
  ;; 110827)
