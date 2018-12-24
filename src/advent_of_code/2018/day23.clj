(ns advent-of-code.2018.day23)

;; [ [[x y z] r] ...]
(def data (read-string (slurp "resources/2018/day23.txt")))

(def maxr (apply max (map second data)))

(def maxp (ffirst (filter #(= maxr (second %)) data)))

(defn mdist
  [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1)) (Math/abs (- z2 z1))))

(defn part1
  [data]
  (count
    (filter #(>= maxr (mdist maxp %))
            (map first data))))

(comment
  (part1 data))
  ;; 481

(defn fill
  [[x y z] r]
  )