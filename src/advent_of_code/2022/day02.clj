(ns advent-of-code.2022.day02
  (:require [clojure.string :as str]))

(def input (->> (slurp "resources/2022/day02.txt")
                (str/split-lines)))

(def winners #{"A Y" "B Z" "C X"})
(def draws #{"A X" "B Y" "C Z"})
(def outcome-score {:w 6 :l 0 :d 3})
(def selection-score {"X" 1 "Y" 2 "Z" 3})

(def round-score
  (into {}
        (for [p1 ["A" "B" "C"]
              p2 ["X" "Y" "Z"]
              :let [round (str p1 " " p2)]]
          [round (+ (selection-score p2)
                    (outcome-score (cond
                                     (winners round) :w
                                     (draws round) :d
                                     :else :l)))])))

(defn part1
  [input]
  (->> input
       (map round-score)
       (apply +)))

(comment
  (part1 input)
  ;; => 15572
  :end)

(def outcome-need {"X" :l "Y" :d "Z" :w})
(def selection-need-for-outcome
  {:w {"A" "Y" "B" "Z" "C" "X"}
   :l {"A" "Z" "B" "X" "C" "Y"}
   :d {"A" "X" "B" "Y" "C" "Z"}})

(def round-score2
  (into {}
        (for [p1 ["A" "B" "C"]
              p2 ["X" "Y" "Z"]
              :let [round (str p1 " " p2)]]
          [round (+ (selection-score
                      ((selection-need-for-outcome
                         (outcome-need p2))
                       p1))
                    (outcome-score (outcome-need p2)))])))

(defn part2
  [input]
  (->> input
       (map round-score2)
       (apply +)))

(comment
  (part2 input)
  ;; => 16098
  :end)
