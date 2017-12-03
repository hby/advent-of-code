(ns advent-of-code.2015.day16
  (:require [clojure.string :as s]))

(def puzzle-text (slurp "resources/2015/day16.txt"))

(def puzzle
  (map read-string (s/split-lines puzzle-text)))

(defn compare-if-key
  "returns value of comparison predicate c applied to
  value in map m under key k, if k is a key in m.
  returns true if k is not a key in m"
  [m v c k]
  (if (contains? m k)
    (c v (m k))
    true))

(comment
  (->> puzzle
       (filter #(compare-if-key % 3 = :children))
       (filter #(compare-if-key % 7 = :cats))
       (filter #(compare-if-key % 2 = :samoyeds))
       (filter #(compare-if-key % 3 = :pomeranians))
       (filter #(compare-if-key % 0 = :akitas))
       (filter #(compare-if-key % 0 = :vizslas))
       (filter #(compare-if-key % 5 = :goldfish))
       (filter #(compare-if-key % 3 = :trees))
       (filter #(compare-if-key % 2 = :cars))
       (filter #(compare-if-key % 1 = :perfumes)))
  ;=> ({:sue 213, :children 3, :goldfish 5, :vizslas 0})
  (->> puzzle
       (filter #(compare-if-key % 3 = :children))
       (filter #(compare-if-key % 7 < :cats))
       (filter #(compare-if-key % 2 = :samoyeds))
       (filter #(compare-if-key % 3 > :pomeranians))
       (filter #(compare-if-key % 0 = :akitas))
       (filter #(compare-if-key % 0 = :vizslas))
       (filter #(compare-if-key % 5 > :goldfish))
       (filter #(compare-if-key % 3 < :trees))
       (filter #(compare-if-key % 2 = :cars))
       (filter #(compare-if-key % 1 = :perfumes)))
  ;=> ({:sue 323, :perfumes 1, :trees 6, :goldfish 0})
  )
