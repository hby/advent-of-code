(ns advent-of-code.day15
  (:require [clojure.string :as s]))

(def puzzle-text (slurp "resources/input/day15.txt"))

(def pattern
  #"(\w+): capacity ([\-0-9]+), durability ([\-0-9]+), flavor ([\-0-9]+), texture ([\-0-9]+), calories ([\-0-9]+)")

(def puzzle-pieces
  (map (fn [l] (drop 1 (re-find pattern l)))
       (s/split-lines puzzle-text)))

(def puzzle
  (map (fn [p]
         (into [(first p)]
               (map #(Integer/parseInt %)
                    (rest p))))
       puzzle-pieces))

;; I suspect this could be better, I'll change it if I need to
(defn mixtures
  "ordered 4-tuples (vectors) where each sums to 100 and each element is between 0 and 100"
  []
  (distinct
    (for [f (range 0 101)
          c (range 0 101)
          b (range 0 101)]
      (let [frosting f
            candy (if (<= (+ frosting c) 100) c 0)
            butterscotch (if (<= (+ frosting candy b) 100) b 0)
            sugar (- 100 frosting candy butterscotch)]
        [frosting candy butterscotch sugar]))))

(defn score
  [properties mixture]
  {:pre [(= 100 (apply + mixture))]}
  (->> (map #(drop 1 %) properties)
       (map (fn [m p] (map #(* m %) p)) mixture)
       (apply map vector)
       (map #(apply + %))
       (map #(if (neg? %) 0 %))
       (split-at 4)
       ((fn [sc] [(apply * (first sc)) (first (second sc))]))
       ))

(comment
  (apply max-key first
    (map #(score puzzle %) (mixtures)))
  ;=> [18965440 554]

  (apply max-key first
         (->> (mixtures)
              (map #(score puzzle %))
              (filter #(= 500 (second %)))))
  ;=> [15862900 500]
  )

