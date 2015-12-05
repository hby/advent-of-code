(ns advent-of-code.day01)

(def puzzle (slurp "resources/input/day01.txt"))

(defn floor
  [puzzle]
  (reduce (fn [s v]
            (cond
              (= v \() (inc s)
              (= v \)) (dec s)
              :else s))
          0
          puzzle))

(comment
  (floor puzzle))
;; 138

(defn floors
  [puzzle]
  (reductions (fn [s v]
                (cond
                  (= v \() (inc s)
                  (= v \)) (dec s)
                  :else s))
              0
              puzzle))

(first (map first
     (filter #(= (second %) -1)
             (map-indexed vector (floors puzzle)))))

(defn enter-basement
  [puzzle]
  (take 1
        (keep-indexed (fn [idx floor] (if (= floor -1) idx))
                      (floors puzzle))))

(comment
  (enter-basement puzzle))
;; 1771
