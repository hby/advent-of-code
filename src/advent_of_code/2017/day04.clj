(ns advent-of-code.2017.day04
  (:require [clojure.string :as cs]
            [clojure.math.combinatorics :as comb]))

(def puzzle
  (->> (slurp "resources/2017/day04.txt")
       (cs/split-lines)
       (map #(cs/split % #"\s+"))))

(defn valid?
  [pp]
  (= (count pp)
     (count (distinct pp))))

(defn one
  [puzzle]
  (->> puzzle
       (map valid?)
       (filter identity)
       (count)))

(comment
  (one puzzle))
  ; => 386


(defn anagrams?
  [w1 w2]
  (= (sort w1) (sort w2)))

(defn pairs
  [pp]
  (comb/combinations pp 2))

(defn valid2?
  [pp]
  (not (some #(apply anagrams? %) (pairs pp))))

(defn two
  [puzzle]
  (->> puzzle
       (map valid2?)
       (filter identity)
       (count)))

(comment
  (two puzzle))
  ; => 208

