(ns advent-of-code.day02
  (:require [clojure.string :as s]))

(def puzzle-text (slurp "resources/input/day02.txt"))

(def puzzle
  (mapv (fn [l] (vec (sort (map #(Integer/parseInt %)
                                (s/split l #"x")))))
        (s/split-lines puzzle-text)))

(defn box-wrapping-square-feet
  [[d1 d2 d3]]
  {:pre [(<= d1 d2 d3)]}
  (+ (* 2 d1 d2)
     (* 2 d1 d3)
     (* 2 d2 d3)
     (* d1 d2)))

(defn total-square-feet
  [puzzle]
  (reduce + (map box-wrapping-square-feet puzzle)))

(comment
  (total-square-feet puzzle))
;; 1588178

(defn ribbon-length
  [[d1 d2 d3]]
  {:pre [(<= d1 d2 d3)]}
  (+ (* 2 (+ d1 d2))
     (* d1 d2 d3)))

(defn total-ribbon-length
  [puzzle]
  (reduce + (map ribbon-length puzzle)))

(comment
  (total-ribbon-length puzzle))
;; 3783758
