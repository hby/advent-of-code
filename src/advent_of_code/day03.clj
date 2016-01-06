(ns advent-of-code.day03)

(def puzzle (slurp "resources/input/day03.txt"))

(defn move
  "return new position after moving [x y] in direction"
  [[x y] direction]
  (cond
    (= direction \^) [x (inc y)]
    (= direction \v) [x (dec y)]
    (= direction \>) [(inc x) y]
    (= direction \<) [(dec x) y]))

(defn path
  "return a sequence of positions after each
  successive move from start"
  [start moves]
  (reductions move start moves))

(defn visted
  "return disticnt positions visted along path
  obtained from (path start moves)"
  [start moves]
  (distinct (path start moves)))

(comment
  (count (visted [0 0] puzzle)))
;; 2592

(defn two-santa-paths
  "return sequence of coordinate pairs of each santa position
  after each successive move"
  [start moves]
  (reductions (fn [paths [santa m]]
                (update-in paths [santa] #(move % m)))
              [start start]
              (map vector (cycle [0 1]) moves)))

(defn two-santa-visited
  [start puzzle]
  (distinct (apply concat (two-santa-paths start puzzle))))

(comment
  (count (two-santa-visited [0 0] puzzle)))
;; 2360
