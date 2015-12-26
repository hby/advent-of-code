(ns advent-of-code.day18
  (:require [clojure.string :as s]))

(def on 1)
(def off 0)
(def size 100)

(def grid-text (slurp "resources/input/day18.txt"))

;; A grid is a vector of vectors of on's and off's
(def grid-start
  (mapv (fn [l]
          (mapv (fn [c] (if (= c \#) on off)) l))
        (s/split-lines grid-text)))

(defn grid-value
  "value of grid at position [x y]"
  [g [x y]]
  (if (and (< -1 x size)
           (< -1 y size))
    ((g y) x)
    off))

(defn neighbor-positions
  "positions of neighbors of [x y]"
  [[x y]]
  (into []
        (take 8
              (for [yd [-1 1 0]
                    xd [-1 1 0]]
                [(+ xd x) (+ yd y)]))))

(defn neighbor-values
  "vector of grid values for neighbors of p ([x y])"
  [g p]
  (mapv #(grid-value g %) (neighbor-positions p)))

(defn onrule
  "for lights on,
   return on if 2 or 3 neightbors are on,
   otherwise return off"
  [g p]
  {:pre [(= on (grid-value g p))]}
  (let [nvs (neighbor-values g p)
        n (apply + nvs)]
    (if (<= 2 n 3) on off)))

(defn offrule
  "for lights off
   return off if exactly 3 neightbors are on,
   otherwise return off"
  [g p]
  {:pre [(= off (grid-value g p))]}
  (let [nvs (neighbor-values g p)
        n (apply + nvs)]
    (if (= 3 n) on off)))

;; the rule to apply for each grid value
(def rulemap
  {on onrule
   off offrule})

(defn grid->grid
  "returns new grid based on one rule application
  for each position"
  [stuckps g]
  (into []
        (map vec
             (partition size
                        (for [y (range 0 size)
                              x (range 0 size)
                              :let [p [x y]]]
                          (if (not-any? #(= % p) stuckps)
                            ((rulemap (grid-value g p)) g p)
                            (grid-value g p)))))))

(comment
  ; part 1
  (reduce +
          (flatten
            (first (drop 100
                         (iterate (partial grid->grid []) grid-start)))))
  ;=> 768

  ;part 2
  (reduce +
          (flatten
            (first (drop 100
                         (iterate (partial grid->grid [[0 0] [0 99] [99 0] [99 99]]) grid-start)))))
  ;=> 781
  )

