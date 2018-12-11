(ns advent-of-code.2018.day05)

(def input (slurp "resources/2018/day05.txt"))

(defn react
  [^Character c1 ^Character c2]
  (and c1 c2
       (or
         (and (Character/isLowerCase c1)
              (Character/isUpperCase c2))
         (and (Character/isLowerCase c2)
              (Character/isUpperCase c1)))
       (= (Character/toLowerCase c1)
          (Character/toLowerCase c2))))

(defn pass
  [s]
  (let [ps (partition-all 2 1 s)]
    (loop [[[c1 c2] & pr] ps res []]
      (if (nil? c1)
        res
        (if (react c1 c2)
          (recur (rest pr) res)
          (recur pr (conj res c1)))))))

(defn part1
  [s]
  (loop [ss (seq s)]
    (if-let [p (pass ss)]
      (if (= ss p)
        (count ss)
        (recur p))
      0)))

(comment
  (part1 input))
  ;; 11946

(defn trys
  [s]
  (let [ts (->> "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"
                seq
                (partition 2)
                (map set))]
    (into {}
          (for [t ts]
            [t (remove t s)]))))

(defn part2
  [s]
  (let [tm (trys s)]
    (apply min (map part1 (vals tm)))))

(comment
  (part2 input))
  ;; 4240
