(ns advent-of-code.day13
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(def puzzle-text (slurp "resources/input/day13.txt"))

(def pattern #"(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+)")

;; sequence of ((person1 lose|gain amount person2) ...)
(def puzzle-pieces
  (map (fn [l] (drop 1 (re-find pattern l)))
       (s/split-lines puzzle-text)))

(def happiness-calc
  (let [mapper (juxt
                 #(nth % 0)
                 #(if (= "gain" (nth % 1)) + -)
                 #(Integer/parseInt (nth % 2))
                 #(nth % 3))]
    (into {} (->> puzzle-pieces
                  (map mapper)
                  (map #(vector [(first %) (last %)] (fn [x] ((nth % 1) x (nth % 2)))))))))

(def guests ["Alice" "Bob" "Carol" "David" "Eric" "Frank" "George" "Mallory"])

(defn arrangements
  [guests]
  (->> (drop 1 guests)
       (combo/permutations)
       (map #(concat (take 1 guests) % (take 1 guests)))
       (map #(partition 2 1 %))))

(defn happiness
  [arrangement]
  (reduce #((get happiness-calc %2 identity) %) 0 arrangement))

(comment
  (->> (arrangements guests)
       (map (fn [a] (concat a (map (fn [p] (reverse p)) a))))
       (map happiness)
       (apply max))
  ;=> 664
  )

(def guests-and-me ["Alice" "Bob" "Carol" "David" "Eric" "Frank" "George" "Mallory" "Bret"])

(comment
  (->> (arrangements guests-and-me)
       (map (fn [a] (concat a (map (fn [p] (reverse p)) a))))
       (map happiness)
       (apply max))
  ;=> 640
  )
