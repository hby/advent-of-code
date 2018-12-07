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

(defn overlap?
  [[n1 x1 y1 w1 h1] [n2 x2 y2 w2 h2]]
  (not
    (or
      (> x1 (dec (+ x2 w2)))
      (> x2 (dec (+ x1 w1)))
      (> y1 (dec (+ y2 h2)))
      (> y2 (dec (+ y1 h1))))))

(defn connected?
  [ss e]
  (some (partial overlap? e) ss))

(defn grow-connected-sets
  [cs e]
  (let [c (filter #(connected? % e) cs)
        nc (remove #(connected? % e) cs)]
    (conj nc (apply concat (conj c [e])))))

(defn connected-sets
  [input]
  (reduce (fn [cs e]
            (grow-connected-sets cs e))
          []
          input))

(defn part2
  [input]
  (->> (connected-sets input)
       (filter #(= 1 (count %)))))

(comment
  (part2 input-data))
  ;; (((116 864 693 21 17))) )
