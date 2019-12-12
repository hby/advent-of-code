(ns advent-of-code.2019.day01)

(def input (read-string (slurp "resources/2019/day01.txt")))

(defn fuel [m]
  (-> m
      (/ 3)
      int
      (- 2)))

(defn part1
  [input]
  (reduce + (map fuel input)))

(comment
  (part1 input)
  ;; => 3426455
  )

(defn fuel2
  [m]
  (->> (iterate fuel m)
       (take-while pos?)
       (drop 1)
       (reduce +)))

(defn part2
  [input]
  (reduce + (map fuel2 input)))

(comment
  (part2 input)
  ;; => 5136807
  )


