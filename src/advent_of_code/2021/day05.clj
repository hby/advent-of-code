(ns advent-of-code.2021.day05
  (:require [clojure.string :as str]))

(def input (slurp "resources/2021/day05.txt"))

(defn lines
  [input]
  (->> (str/split-lines input)
       (map #(re-matches #"(\d+),(\d+) -> (\d+),(\d+)" %))
       (map (fn [[_ x1 y1 x2 y2]] [[(Integer/parseInt x1) (Integer/parseInt y1)]
                                   [(Integer/parseInt x2) (Integer/parseInt y2)]]))))

(defn h-and-v-lines
  [input]
  (->> input
       (lines)
       (filter (fn [[[x1 y1] [x2 y2]]]
                 (or (= x1 x2) (= y1 y2))))))

(defmulti points (fn [[[x1 y1] [x2 y2]]]
                   (cond
                     (= x1 x2) :h
                     (= y1 y2) :v
                     (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2))) :d)))

(defmethod points :h
  [[[x1 y1] [_x2 y2]]]
  (mapv #(vector x1 %) (range (min y1 y2) (inc (max y1 y2)))))

(defmethod points :v
  [[[x1 y1] [x2 _y2]]]
  (mapv #(vector % y1) (range (min x1 x2) (inc (max x1 x2)))))

(defn part1
  [input]
  (->> input
       h-and-v-lines
       (map points)
       (map frequencies)
       (apply merge-with +)
       (vals)
       (filter #(< 1 %))
       (count)))

(comment
  (part1 input)
  ;=> 7674
  :end)

(defmethod points :d
  [[[x1 y1] [x2 y2]]]
  (mapv #(vector %1 %2)
        (apply range x1 (if (< x1 x2) [(inc x2) 1] [(dec x2) -1]))
        (apply range y1 (if (< y1 y2) [(inc y2) 1] [(dec y2) -1]))))

(defn h-v-and-d-lines
  [input]
  (->> input
       (lines)
       (filter (fn [[[x1 y1] [x2 y2]]]
                 (or (= x1 x2)
                     (= y1 y2)
                     (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))))))

(defn part2
  [input]
  (->> input
       h-v-and-d-lines
       (map points)
       (map frequencies)
       (apply merge-with +)
       (vals)
       (filter #(< 1 %))
       (count)))

(comment
  (part2 input)
  ;=> 20898
  :end)
