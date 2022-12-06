(ns advent-of-code.2021.day18
  (:require [clojure.zip :as z]))

(def input (read-string (slurp "resources/2021/day18.edn")))

(defn add
  [a b]
  [a b])

(defn magnitude
  [[a b]]
  (let [am (if (number? a) a (magnitude a))
        bm (if (number? b) b (magnitude b))]
    (+ (* 3 am) (* 2 bm))))

(first input)
; [[6 [[9 4] [5 1]]] [[[6 5] [9 4]] 2]]

(defn explode
  [loc])

(->> input
     first
     z/vector-zip
     z/down
     z/right)
