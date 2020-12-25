(ns advent-of-code.2020.day24
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "resources/2020/day24.txt")
      (str/split #"\n")))

(defn moves
  [input]
  (->> input
       (map #(re-seq #"e|w|ne|nw|se|sw" %))))

(defn move->df
  [m]
  (cond
    (= "e" m) (fn [[x y]] [1 0])
    (= "w" m) (fn [[x y]] [-1 0])
    (= "ne" m) (fn [[x y]] [(if (even? y) 0 1) -1])
    (= "nw" m) (fn [[x y]] [(if (odd? y) 0 -1) -1])
    (= "se" m) (fn [[x y]] [(if (even? y) 0 1) 1])
    (= "sw" m) (fn [[x y]] [(if (odd? y) 0 -1) 1])))

(defn delta-fns
  [line]
  (map move->df line))

(defn coord
  "start - [sx sy]"
  [start line]
  (reduce (fn [[cx cy] df]
            (let [[dx dy] (df [cx cy])]
                 [(+ cx dx) (+ cy dy)]))
          start
          (delta-fns line)))

(defn coords
  [input]
  (->> input
       (moves)
       (map (partial coord [0 0]))))

(defn part1
  [input]
  (->> input
       (coords)
       (frequencies)
       (map second)
       (filter odd?)
       (count)))

(comment
  (part1 input)
  ;; 411
  #_[])
