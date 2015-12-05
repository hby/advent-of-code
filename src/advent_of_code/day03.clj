(ns advent-of-code.day03)

(def puzzle (slurp "resources/input/day03.txt"))

(defn move
  [[x y] direction]
  (cond
    (= direction \^) [x (inc y)]
    (= direction \v) [x (dec y)]
    (= direction \>) [(inc x) y]
    (= direction \<) [(dec x) y]))

(defn path
  [puzzle]
  (reductions move [0 0] puzzle))

(defn visted
  [puzzle]
  (set (path puzzle)))

(comment
  (count (visted puzzle)))
;; 2592

(defn two-santa-paths
  [puzzle]
  (reductions (fn [paths m]
                (update-in paths [(first m)] #(move % (second m))))
              [[0 0] [0 0]]
              (map vector (cycle [0 1]) puzzle)))

(defn two-santa-visited
  [puzzle]
  (into #{} (apply concat (two-santa-paths puzzle))))

(comment
  (count (two-santa-visited puzzle)))
;; 2360
