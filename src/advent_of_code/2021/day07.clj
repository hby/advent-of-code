(ns advent-of-code.2021.day07)

(def input (read-string (slurp "resources/2021/day07.edn")))

(defn cost
  [input pos]
  (->> input
       (map #(Math/abs (- pos %)))
       (reduce +)))

;; go with brute force
(defn part1
  [input]
  (let [mn (apply min input)
        mx (apply max input)]
    (->> (range mn (inc mx))
         (map (partial cost input))
         (apply min))))

(comment
  (part1 input)
  ;=> 356179
  :end)

(defn single-cost
  [i p]
  (let [d (Math/abs (- p i))]
    (/ (* d (inc d)) 2)))

(defn cost2
  [input pos]
  (->> input
       (map #(single-cost % pos))
       (reduce +)))

(defn part2
  [input]
  (let [mn (apply min input)
        mx (apply max input)]
    (->> (range mn (inc mx))
         (map (partial cost2 input))
         (apply min))))

(comment
  (part2 input)
  ;=> 99788435
  :end)
