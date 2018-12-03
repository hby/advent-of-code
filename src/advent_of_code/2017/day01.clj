(ns advent-of-code.2017.day01
  (require [clojure.string :as cs]))

(def puzzle (->> (slurp "resources/2017/day01.txt")
                 (cs/trim)
                 (mapv #(- (int %) (int \0)))))

(defn matching-next-digits
  [puzzle]
  (->> (partition 2 1 (conj puzzle (first puzzle)))
       (filter #(apply = %))
       (mapv first)))

(defn one
  [puzzle]
  (apply + (matching-next-digits puzzle)))

(comment
  (one puzzle))
  ; => 1150

(defn pole-idx
  [idx]
  (let [size (count puzzle)
        half (/ size 2)]
    (mod (+ idx half) size)))

(defn matching-pole-digits
  [puzzle]
  (->> (map-indexed (fn [i e] [e (nth puzzle (pole-idx i))]) puzzle)
       (filter #(apply = %))
       (mapv first)))

(defn two
  [puzzle]
  (apply + (matching-pole-digits puzzle)))

(comment
  (two puzzle))
  ; => 1064

