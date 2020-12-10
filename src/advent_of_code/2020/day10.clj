(ns advent-of-code.2020.day10)

(def input (read-string (slurp "resources/2020/day10.edn")))

(defn part1
  [input]
  (as-> input $
        (sort $)
        (concat [0] $ [(+ 3 (last $))])
        (partition 2 1 $)
        (map #(apply - %) $)
        (frequencies $)
        (select-keys $ [-1 -3])
        (vals $)
        (apply * $)))


(comment
  (part1 input)
  ;; 1917
  #_[])

(defn part2
  [input])


(comment
  (part2 input)
  ;; TODO
  #_[])