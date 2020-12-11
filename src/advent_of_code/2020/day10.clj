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

(def arrangements-m
  (memoize
    (fn [f r l]
      (if (empty? r)
        (if (= f l) 1 0)
        (let [[r1 r2 r3] (take-while #(<= % (+ 3 f)) r)]
          (apply +
                 [(if r1 (arrangements-m r1 (drop 1 r) l) 0)
                  (if r2 (arrangements-m r2 (drop 2 r) l) 0)
                  (if r3 (arrangements-m r3 (drop 3 r) l) 0)]))))))

(defn arrangements
  [input]
  (let [si (sort input)]
    (arrangements-m 0 si (last si))))

(defn part2
  [input]
  (arrangements input))

(comment
  (part2 input)
  ;; 113387824750592
  #_[])
