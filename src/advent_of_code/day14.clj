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


;; Part 2

(defn lead-each-sec
  [puzzle]
  (->> (map (fn [deer]
              (map (fn [dist]
                     [(deer 0) dist])
                   (distance-each-sec deer)))
            puzzle)
       (apply map vector)
       (map (fn [pack] (apply max-key second pack)))
       ))

(defn scores
  [puzzle secs]
  (let [lead-seq (lead-each-sec puzzle)]
    (apply merge-with + (map (fn [l] {(l 0) 1}) (take secs lead-seq)))))

(comment
  (scores puzzle puzzle-seconds)
  ;=> {"Prancer" 504, "Blitzen" 1256, "Comet" 158, "Vixen" 422, "Rudolph" 154, "Dasher" 9}
  (apply max-key val (scores puzzle puzzle-seconds))
  ;=> ["Blitzen" 1256]
  )
