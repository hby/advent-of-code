(ns advent-of-code.2016.day06
  (:require [clojure.string :as cs]))

(def input (slurp "resources/2016/day06.txt"))
(def lines (cs/split input #"\n"))

(def columns (apply map vector lines))

(defn most-frequent
  [c]
  (->> (frequencies c)
       (sort-by second)
       (last)
       (first)))

(comment
  (apply str (map most-frequent columns))
  ;; => "usccerug"
  )

(defn least-frequent
  [c]
  (->> (frequencies c)
       (sort-by second)
       (first)
       (first)))

(comment
  (apply str (map least-frequent columns))
  ;;=> "cnvvtafc"
  )
