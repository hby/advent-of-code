(ns advent-of-code.2020.day09)

(def input (read-string (slurp "resources/2020/day09.edn")))
(def preamble 25)

(defn sum-of-two?
  [ns s]
  (-> (for [one ns
            two ns
            :when (and (< one two)
                       (= s (+ one two)))]
        [one two])
      first
      nil?
      not))

(defn part1
  [input]
  (->> (partition (inc preamble) 1 input)
       (map #(when-not (sum-of-two? (butlast %) (last %)) (last %)))
       (remove nil?)
       first))

(comment
  (part1 input)
  ;; 10884537
  #_[])

(def the-sum 10884537)

(defn sum [ns] (reduce + ns))
(defn weakness [ns] (+ (apply min ns) (apply max ns)))

(defn trials
  [ns]
  (mapcat #(partition % 1 ns)
          (range 2 (inc (count ns)))))

(defn part2
  [input]
  (->> input
       (take-while #(not= the-sum %))
       (trials)
       (map #(when (= the-sum (sum %)) (weakness %)))
       (remove nil?)
       first))

(comment
  (part2 input)
  ;; 1261309
  #_[])