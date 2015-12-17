(ns advent-of-code.day17
  (:require [clojure.math.combinatorics :as combo]))

(def containers [33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42])

(def subsets (->> (combo/subsets (range 0 (count containers)))
                  (map (fn [s]
                         (map (fn [i] (nth containers i)) s))))
  )

(def filling-subsets
  (filter (fn [s] (= 150 (apply + s)) )
          subsets))

(comment
  (count filling-subsets)
  ;=> 1304
  )

(def smallest-subset-count
  (apply min (map count filling-subsets)))

(comment
  (count
    (filter (fn [s] (= (count s) smallest-subset-count))
            filling-subsets))
  ;=> 18
  )
