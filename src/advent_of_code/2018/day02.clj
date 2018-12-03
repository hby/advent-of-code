(ns advent-of-code.2018.day02
  (:require [clojure.java.io :as io]))

(def input-data (-> "resources/2018/day02.txt"
                    (io/reader)
                    (line-seq)))

(defn exactly-n
  [n s]
  (->> (frequencies s)
       vals
       (some #(= n %))
       boolean))

(def exactly-2 (partial exactly-n 2))
(def exactly-3 (partial exactly-n 3))

(defn part1
  [input]
  (->> (map (juxt exactly-2 exactly-3) input)
       (apply map vector)
       (map #(filter true? %))
       (map count)
       (apply *)))

(comment
  (part1 input-data))
  ;; 5390)

(defn diff-positions
  [s1 s2]
  (->> (map vector s1 s2)
       (map-indexed #(when (apply not= %2) %1))
       (filter identity)))

(defn remove-idxs
  "Hey, why not make this general and work for a collection of indexes?"
  [s idxs]
  (let [idx-set (set idxs)]
    (->> (map vector s (range))
         (remove #(idx-set (second %)))
         (map first)
         (apply str))))

(defn part2
  [input]
  (for [[s1 i1] (map vector input (range))
        [s2 i2] (map vector input (range)) :when (< i1 i2)
        :let [diffs (diff-positions s1 s2)]
        :when (= 1 (count diffs))]
    (remove-idxs s1 diffs)))

(comment
  (part2 input-data))
  ;; ("nvosmkcdtdbfhyxsphzgraljq"))

