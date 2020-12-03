(ns advent-of-code.2020.day03)

(def input (read-string (slurp "resources/2020/day03.edn")))


(defn part1
  [input]
  (let [grid (map cycle input)
        xs (iterate (partial + 3) 0)
        trees (map (fn [r x] (nth r x)) grid xs)]
    (apply + trees)))

(comment

  (part1 input)
  ;; 148
  )

(defn trees
  [input r d]
  (let [grid (map cycle input)
        rows (map first (partition d grid))
        xs (iterate (partial + r) 0)
        trees (map (fn [r x] (nth r x)) rows xs)]
    (apply + trees)))

(defn part2
  [input]
  (* (trees input 1 1)
     (trees input 3 1)
     (trees input 5 1)
     (trees input 7 1)
     (trees input 1 2)))

(comment

  (part2 input)
  ;; 727923200
  )
