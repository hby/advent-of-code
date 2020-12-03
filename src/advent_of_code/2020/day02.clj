(ns advent-of-code.2020.day02)

(def input (read-string (slurp "resources/2020/day02.edn")))


(defn part1
  [input]
  (->> input
       (filter (fn [[mn mx c s]]
                 (<= mn
                     (count (re-seq (re-pattern c) s))
                     mx)))
       (count)))

(comment

  (part1 input)
  ;; 445
  )


(defn part2
  [input]
  (->> input
       (filter (fn [[i1 i2 c s]]
                 (= 1
                    (+ (if (= c (subs s (dec i1) i1)) 1 0)
                       (if (= c (subs s (dec i2) i2)) 1 0)))))
       (count)))

(comment

  (part2 input)
  ;; 491
  )
