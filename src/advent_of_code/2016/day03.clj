(ns advent-of-code.2016.day03
  (:require [clojure.string :as cs]))

(def input (slurp "resources/2016/day03.txt"))
(def puzzle (->> (cs/split input #"\s+")
                 (remove empty?)
                 (map #(Integer/parseInt %))
                 (partition 3)))

(defn valid?
  [[a b c]]
  (and
    (> (+ a b) c)
    (> (+ a c) b)
    (> (+ b c) a)))

(comment
  (->> puzzle
       (filter valid?)
       (count))
  ;; => 1050
  )

(comment
  (->> puzzle
       (apply map vector)
       (apply concat)
       (partition 3)
       (filter valid?)
       (count))
  ;; => 1921
  )
