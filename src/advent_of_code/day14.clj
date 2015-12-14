(ns advent-of-code.day14
  (:require [clojure.string :as s]))

(def puzzle-text (slurp "resources/input/day14.txt"))

(def pattern
  #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")

(def puzzle-pieces
  (map (fn [l] (drop 1 (re-find pattern l)))
       (s/split-lines puzzle-text)))

(def puzzle
  (map (fn [p]
         (into [(first p)]
               (map (fn [v]
                      (Integer/parseInt v) )
                    (rest p))))
       puzzle-pieces))

(def puzzle-seconds 2503)

; Math is better but then I can't show off
; these nice sequence facilities.

(defn distance-each-sec
  "deer - [name speed secs restsecs]"
  [deer]
  (let [speed (deer 1)
        secs (deer 2)
        restsecs (deer 3)]
    (->> (cycle (concat (repeat secs speed) (repeat restsecs 0)))
         (reductions +))))

(defn distance
  [deer secs]
  (first (drop (dec secs) (distance-each-sec deer))))

(comment
  (->> puzzle
       (map (fn [p] [(first p) (distance p puzzle-seconds)]))
       (apply max-key second)))
;=> ["Vixen" 2660]


;; Part 2 - coming ...