(ns advent-of-code.2021.day08
  (:require [clojure.string :as str]))

(def input (slurp "resources/2021/day08.txt"))

(defn lines
  [input]
  (->> (str/split-lines input)
       (map #(re-matches #"(.*)\|(.*)" %))
       (map (fn [[_ a b]] [(str/split (str/trim a) #" ")
                           (str/split (str/trim b) #" ")]))))
(defn part1
  [input]
  (->> (lines input)
       (mapcat second)
       (map count)
       (filter #{2 3 4 7})
       count))

(comment
  (part1 input)
  ;=> 383
  :end)

(defn count->ns
  [code]
  ({2 #{1}
    3 #{7}
    4 #{4}
    5 #{2 3 5}
    6 #{0 6 9}
    7 #{8}} (count code)))

(defn n->cononical-code
  [n]
  {0 "abcefg"
   1 "cf"
   2 "acdeg"
   3 "acdfg"
   4 "bcdf"
   5 "abdfg"
   6 "abdefg"
   7 "acf"
   8 "abcdefg"
   9 "abcdfg"} n)

(comment
  [["acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"]
   ["cdfeb" "fcadb" "cdfeb" "cdbaf"]]
  :end)
;; ab = 1
