(ns advent-of-code.2019.day03
  (:require [clojure.set :as cset]))

(def raw-input (read-string (slurp "resources/2019/day03.txt")))

(defn mk-move
  [sym]
  (let [nm (name sym)]
    (vector
     (first nm)
     (read-string (apply str (rest nm))))))

(defn mk-input
  [raw]
  (mapv #(mapv mk-move %) raw))

(defn move-coords
  [pos [d n]]
  (let [f ({\U inc \D dec \L dec \R inc} d)
        idx ({\U 1 \D 1 \L 0 \R 0} d)]
    (->> (iterate #(update % idx f) pos)
         (take (inc n)))))

(defn path
  [pos mvs]
  (->> (reduce (fn [cs mv]
                 (apply conj cs (drop 1 (move-coords (last cs) mv))))
               [pos]
               mvs)
       (drop 1)))

(defn abs [v] (Math/abs v))

(defn part1
  [input]
  (->> input
       (map (partial path [0 0]))
       (map set)
       (apply cset/intersection)
       (sort-by #(apply + (map abs %)))
       first
       (map abs)
       (apply +)))

(comment
  (part1 (mk-input raw-input))
  ;; => 303
  )

(defn indexed
  [s]
  (map vector (iterate inc 0) s))

(defn positions
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] [idx elt]))

(defn part2
  [input]
  (let [paths (map (partial path [0 0]) input)
        intersections (apply cset/intersection (map set paths))]
    (->> paths
         (map (partial positions intersections))
         (map (partial group-by second))
         (apply merge-with (fn [a b]
                             (+
                               (apply min (map first a))
                               (apply min (map first b)))))
         vals
         (apply min)
         (+ 2))))

(comment
  (part2 (mk-input raw-input))
  ;; => 11222
  )
