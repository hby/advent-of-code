(ns advent-of-code.2020.day15)

(def input [0 12 6 13 20 1 17])

(defn next-fn
  "returns [new-n (inc last-p) new-last-ps]"
  [[last-n last-p last-ps]]
  (if (contains? last-ps last-n)
    (let [new-n (- last-p (get last-ps last-n))]
      [new-n (inc last-p) (assoc last-ps last-n last-p)])
    [0 (inc last-p) (assoc last-ps last-n last-p)]))

(defn memory-game-seq
  [input]
  (let [last-ps (into {}
                      (map vector
                           (butlast input)
                           (iterate inc 1)))]
    (concat (butlast input)
            (map first
                 (iterate next-fn [(last input)
                                   (count input)
                                   last-ps])))))

(defn part1
  [input]
  (->> (memory-game-seq input)
       (drop (dec 2020))
       first))

(comment
  (part1 input)
  ;; 620
  #_[])

(defn part2
  [input]
  (->> (memory-game-seq input)
       (drop (dec 30000000))
       first))

(comment
  (part2 input)
  ;; 110871
  ;; "Elapsed time: 34367.687766 msecs"
  #_[])
