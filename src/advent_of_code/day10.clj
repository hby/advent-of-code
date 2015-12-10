(ns advent-of-code.day10)

(def puzzle "1113122113")

(defn say
  [s]
  (apply str
         (map (fn [x] (str (count x) (first x)))
              (partition-by identity s))))

(comment
  (count
    (first (drop 40 (iterate say puzzle)))))
;=> 360154

(comment
  (count
    (first (drop 50 (iterate say puzzle)))))
;=> 5103798
