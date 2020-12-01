(ns advent-of-code.2020.day01)

(def input (read-string (slurp "resources/2020/day01.edn")))

(defn pair
  [input]
  (first
    (for [a input
          b input
          :when (= 2020 (+ a b))]
     [a b])))

(defn part1
  [input]
  (apply * (pair input)))

(comment

  (part1 input)
  ; 494475
  )


(defn triple
  [input]
  (first
    (for [a input
          b input
          c input
          :when (= 2020 (+ a b c))]
      [a b c])))

(defn part2
  [input]
  (apply * (triple input)))

(comment

  (part2 input)
  ; 267520550
  )
