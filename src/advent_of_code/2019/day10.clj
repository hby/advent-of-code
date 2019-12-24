(ns advent-of-code.2019.day10
  (:require [clojure.set :as set])
  (:import [java.lang Math]))

(def input (with-open [rdr (clojure.java.io/reader "resources/2019/day10.txt")]
             (into [] (line-seq rdr))))

(def coords
  (->> (keep-indexed
         (fn [y r]
           (keep-indexed
             (fn [x c]
               (when (= \# c)
                 [x y]))
             r))
         input)
       (apply concat)
       set))

(defn gcd
  [[a b]]
  (cond
    (zero? b) a
    :else (gcd [b (mod a b)])))

(defn sectorf
  [[ref-x ref-y] [x y]]
  [(compare x ref-x) (compare y ref-y)])

(defn deltaf
  [[ref-x ref-y] [x y]]
  [(Math/abs (- x ref-x)) (Math/abs (- y ref-y))])

(defn xf
  [ref c]
  (let [s (sectorf ref c)
        d (deltaf ref c)
        factor (gcd d)
        base (mapv / d [factor factor])]
    [base factor s c]))

(defn base
  [xf]
  (nth xf 0))

(defn factor
  [xf]
  (nth xf 1))

(defn sector
  [xf]
  (nth xf 2))

(defn coord
  [xf]
  (nth xf 3))

(defn num-visible
  [ref coords]
  (->> (set/difference coords #{ref})
       (map (partial xf ref))
       (group-by sector)
       vals
       (map #(->> %
                  (group-by base)
                  keys
                  count))
       (apply +)))

(defn part1
  [coords]
  (->> coords
       (map #(vector % (num-visible % coords)))
       (sort-by #(second %))
       last
       second))

(comment
  (part1 coords)
  ;; => 230
  )


