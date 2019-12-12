(ns advent-of-code.2019.day08)

(def input
  (->> "resources/2019/day08.txt"
       slurp
       (map int)
       (map #(- % 48))))

(defn counts-n
  [d ds]
  (->> ds
       (filter #(= d %))
       count))

(defn part1
  [input]
  (let [layers (partition (* 25 6) input)
        counts-012-fn (apply juxt (map #(partial counts-n %) [0 1 2]))
        counts-012 (map counts-012-fn layers)]
    (->> counts-012
         (apply min-key first)
         (drop 1)
         (apply *))))

(comment
  (part1 input)
  ;; => 1584
  )

(defn first-non-2
  [ns]
  (->> ns
       (drop-while #(= 2 %))
       first))

(defn p
  [n ns]
  (->> ns
       (partition n)
       (map #(apply println %))))

(defn part2
  [input]
  (let [layers (partition (* 25 6) input)
        pixel-cols (apply map vector layers)]
    (->> pixel-cols
         (map first-non-2)
         (p 25))))

(comment
  (part2 input)
  ; 1 0 0 1 0 0 1 1 0 0 0 1 1 0 0 1 1 1 1 0 0 1 1 0 0
  ; 1 0 1 0 0 1 0 0 1 0 1 0 0 1 0 1 0 0 0 0 1 0 0 1 0
  ; 1 1 0 0 0 1 0 0 0 0 1 0 0 0 0 1 1 1 0 0 1 0 0 0 0
  ; 1 0 1 0 0 1 0 0 0 0 1 0 1 1 0 1 0 0 0 0 1 0 0 0 0
  ; 1 0 1 0 0 1 0 0 1 0 1 0 0 1 0 1 0 0 0 0 1 0 0 1 0
  ; 1 0 0 1 0 0 1 1 0 0 0 1 1 1 0 1 1 1 1 0 0 1 1 0 0
  ;; kcgec
  )
