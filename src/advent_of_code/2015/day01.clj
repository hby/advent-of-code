(ns advent-of-code.2015.day01)

(def puzzle (slurp "resources/2015/day01.txt"))

(defn floors
  "Lazy seq of the floor after each successive step."
  [puzzle]
  (reductions (fn [s v]
                (cond
                  (= v \() (inc s)
                  (= v \)) (dec s)
                  :else s))
              0
              puzzle))

(comment
  (last (floors puzzle)))
;; 138

(defn enter-basement
  [puzzle]
  (ffirst
    (drop-while #(not (= (second %) -1))
                (map-indexed vector (floors puzzle)))))

(comment
  (enter-basement puzzle))
;; 1771
