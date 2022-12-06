(ns advent-of-code.2021.day04)

(def numbers
  [67,31,58,8,79,18,19,45,38,13,40,62,85,10,21,96,56,55,4,
   36,76,42,32,34,39,89,6,12,24,57,93,47,41,52,83,61,5,37,
   28,15,86,23,69,92,70,27,25,53,44,80,65,22,99,43,66,26,
   11,72,2,98,14,82,87,20,73,46,35,7,1,84,95,74,81,63,78,
   94,16,60,29,97,91,30,17,54,68,90,71,88,77,9,64,50,0,49,
   48,75,3,59,51,33])

(def input (read-string (slurp "resources/2021/day04.edn")))

(defn bingo?
  [b]
  (or
    (some (partial every? keyword?) b)
    (some (partial every? keyword?) (apply map vector b))))

(defn sub-n-for-kw
  [n b]
  (->> b
       (mapv #(mapv (fn [nn] (if (= n nn) :x nn)) %))))

(defn play-to-bingo
  [input numbers]
  (reduce (fn [bs n]
            (let [bs' (map (partial sub-n-for-kw n) bs)]
              (if (some bingo? bs')
                (reduced [n (first (filter bingo? bs'))])
                bs')))
          input
          numbers))

(defn part1
  [input numbers]
  (let [[n bg] (play-to-bingo input numbers)]
    (* n
       (->> bg
            flatten
            (remove keyword?)
            (apply +)))))

(comment
  (part1 input numbers)
  ;=> 10680
  :end)

(defn play-to-last-bingo
  [input numbers]
  (reduce (fn [[bs bgs] n]
            (let [bs' (map (partial sub-n-for-kw n) bs)
                  bgs' (filter bingo? bs')
                  bs' (remove bingo? bs')]
              [bs' (into bgs (map vector (repeat n) bgs'))]))
          [input []]
          numbers))

(defn part2
  [input numbers]
  (let [[_ bgs] (play-to-last-bingo input numbers)
        [n bg] (last bgs)]
    (* n
       (->> bg
            flatten
            (remove keyword?)
            (apply +)))))

(comment
  (part2 input numbers)
  ;=> 31892
  :end)
