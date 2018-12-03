(ns advent-of-code.2018.day01)

(def input-data (read-string (slurp "resources/2018/day01.txt")))

(defn part1
  [input]
  (reduce + 0 input))

(comment
  (part1 input-data))
  ;; 520)

(defn part2
  [input]
  (let [fs (reductions + 0 (cycle input))]
    (loop [seen #{} [f & r] fs]
      (cond
        (seen f) f
        :else (recur (conj seen f) r)))))

(comment
  (part2 input-data))
  ;; 394)
