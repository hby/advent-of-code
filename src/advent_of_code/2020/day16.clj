(ns advent-of-code.2020.day16)

(def input (read-string (slurp "resources/2020/day16.edn")))

(defn not-valid?
  [n [[l1 u1] [l2 u2]]]
  (not (or (<= l1 n u1) (<= l2 n u2))))

(defn part1
  [input]
  (->> (input "nearby tickets")
       (apply concat)
       (filter #(every? (partial not-valid? %)
                        (-> (input "rules") vals)))
       (reduce +)))

(comment
  (part1 input)
  ;; 24110
  #_[])
