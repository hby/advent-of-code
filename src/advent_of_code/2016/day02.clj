(ns advent-of-code.2016.day02
  (:require [clojure.string :as cs]))

(def input (slurp "resources/2016/day02.txt"))
(def puzzle (-> input
                (cs/split #"\n")))

(defn U
  "move U"
  [p]
  ({1 1 2 2 3 3
    4 1 5 2 6 3
    7 4 8 5 9 6} p))

(defn D
  "move D"
  [p]
  ({1 4 2 5 3 6
    4 7 5 8 6 9
    7 7 8 8 9 9} p))

(defn L
  "move L"
  [p]
  ({1 1 2 1 3 2
    4 4 5 4 6 5
    7 7 8 7 9 8} p))

(defn R
  "move L"
  [p]
  ({1 2 2 3 3 3
    4 5 5 6 6 6
    7 8 8 9 9 9} p))

(def m->f {\U U
           \D D
           \L L
           \R R})

(defn code
  [sp ms]
  (reduce (fn [p m] ((m->f m) p))
          sp
          ms))

(defn solve1
  []
  (reductions (fn [s ms] (code s ms))
              5
              puzzle))

(comment
  (solve1)
  ;;=> (5 8 4 4 5 2)
  ;; answer: 84452
  )

(defn U2
  "move U"
  [p]
  ({1 1
    2 2 3 1 4 4
    5 5 6 2 7 3 8 4 9 9
    10 6 11 7 12 8
    13 11} p))

(defn D2
  "move D"
  [p]
  ({1 3
    2 6 3 7 4 8
    5 5 6 10 7 11 8 12 9 9
    10 10 11 13 12 12
    13 13} p))

(defn L2
  "move L"
  [p]
  ({1 1
    2 2 3 2 4 3
    5 5 6 5 7 6 8 7 9 8
    10 10 11 10 12 11
    13 13} p))

(defn R2
  "move R"
  [p]
  ({1 1
    2 3 3 4 4 4
    5 6 6 7 7 8 8 9 9 9
    10 11 11 12 12 12
    13 13} p))

(def m->f2 {\U U2
            \D D2
            \L L2
            \R R2})

(defn code2
  [sp ms]
  (reduce (fn [p m] ((m->f2 m) p))
          sp
          ms))

(defn solve2
  []
  (reductions (fn [s ms] (code2 s ms))
              5
              puzzle))

(comment
  (solve2)
  ;; => (5 13 6 5 12 3)
  ;; answer: D65C3
  )
