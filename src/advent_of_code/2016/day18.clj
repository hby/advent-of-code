(ns advent-of-code.2016.day18)

(def input ".^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^..^.^^^.^^..^.^^.^....^.^...^^.^.")

(defn tile
  [pr]
  (case pr
    ([\^ \^ \.] [\. \^ \^] [\^ \. \.] [\. \. \^]) \^
    \.))

(defn triples
  [r]
  (->> (concat [\.] r [\.])
       (partition 3 1)))

(defn next-row
  [r]
  (map tile (triples r)))

(defn rows
  [start]
  (cons start (lazy-seq (rows (next-row start)))))

(defn answer
  [n start]
  (->> (rows start)
       (take n)
       (map (partial filter #(= \. %)))
       (map count)
       (apply +)))

(comment
  (answer 40 input)
  ;; 2035
  (time (answer 400000 input)))
  ;; ~27 secs
  ;;20000577)
