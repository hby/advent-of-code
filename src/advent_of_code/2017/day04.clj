(ns advent-of-code.2017.day04
  (:require [clojure.string :as cs]))

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
  (one puzzle)
  ; => 386
  )
