(ns advent-of-code.day06
  (:require [clojure.string :as s]))

(set! *warn-on-reflection* true)

(def puzzle-text (slurp "resources/input/day06.txt"))

(def pattern
  #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")

(def puzzle
  (map (fn [p] (into [(first p)] (map #(Integer/parseInt %) (drop 1 p))))
       (map (fn [l] (drop 1 (re-find pattern l)))
            (s/split-lines puzzle-text))))

(defn ranges->points
  "xr, yr - ranges to sweep through.
   f      - function, sequence values are (f [x y])
            identity function is used if not supplied"
  ([xr yr f]
   (for [x xr
         y yr]
     (f [x y])))
  ([xr yr]
   (ranges->points xr yr identity)))

(def board
  (into {}
        (ranges->points (range 1000) (range 1000) #(vector % 0))))

(defn rect->points
  [x1 y1 x2 y2]
  (ranges->points (range (min x1 x2) (inc (max x1 x2)))
                  (range (min y1 y2) (inc (max y1 y2)))))

(defn turn-on
  ([board l]
   (assoc board l 1))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-on b l))
           board
           (rect->points x1 y1 x2 y2))))

(defn turn-off
  ([board l]
   (assoc board l 0))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-off b l))
           board
           (rect->points x1 y1 x2 y2))))

(defn toggle
  ([board l]
   (assoc board l (if (zero? (board l)) 1 0)))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (toggle b l))
           board
           (rect->points x1 y1 x2 y2))))

(def actions {"turn on" turn-on
              "turn off" turn-off
              "toggle" toggle})

(defn new-board
  [board puzzle actions]
  (reduce (fn [b [a x1 y1 x2 y2]]
            ((actions a) b x1 y1 x2 y2))
          board
          puzzle))

(comment
  (reduce +
          0
          (vals (new-board board puzzle actions))))
;; 377891


;; Not be the best organizationally but going to do this to get the answer to part 2
(defn turn-on-2
  ([board l]
   (assoc board l (inc (board l))))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-on-2 b l))
           board
           (rect->points x1 y1 x2 y2))))

(defn turn-off-2
  ([board l]
   (assoc board l (if (zero? (board l)) 0 (dec (board l)))))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-off-2 b l))
           board
           (rect->points x1 y1 x2 y2))))

(defn toggle-2
  ([board l]
   (assoc board l (+ 2 (board l))))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (toggle-2 b l))
           board
           (rect->points x1 y1 x2 y2))))

(def actions-2 {"turn on" turn-on-2
                "turn off" turn-off-2
                "toggle" toggle-2})

(comment
  (reduce +
          0
          (vals (new-board board puzzle actions-2))))
;; 14110788


;;; This had made me curious about a couple performance things that I will
;;; try out later.
