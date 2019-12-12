(ns advent-of-code.2019.day04)

(def input
  (->> (range 145852 (inc 616942))
       (map str)
       (map (partial map int))))

(defn part1
  [input]
  (->> input
       (filter #(= 6 (count %)))
       (filter #(apply <= %))
       (filter #(> 6 (count (partition-by identity %))))
       count))

(comment
  (part1 input)
  ;; => 1767
  )

(defn part2
  [input]
  (->> input
       (filter #(= 6 (count %)))
       (filter #(apply <= %))
       (filter #(->> (partition-by identity %)
                     (some (fn [v] (= 2 (count v))))))
       count))

(comment
  (part2 input)
  ;; => 1192
  )
