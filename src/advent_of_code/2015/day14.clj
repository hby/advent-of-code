(ns advent-of-code.2015.day14
  (:require [clojure.string :as s]))

(def puzzle-text (slurp "resources/2015/day14.txt"))

(def pattern
  #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")

(def puzzle-pieces
  (map (fn [l] (drop 1 (re-find pattern l)))
       (s/split-lines puzzle-text)))

; sequence of ([name speed time rest] ...)
(def puzzle
  (map (fn [p]
         (into [(first p)]
               (map (fn [v]
                      (Integer/parseInt v))
                    (rest p))))
       puzzle-pieces))

(def puzzle-seconds 2503)

; Math is better but then I can't show off
; these nice sequence facilities.

(defn distance-each-sec
  "return a sequence of the distance traveled each second
  for a deer with given speed, sec, and restsecs"
  [[_ speed secs restsecs]]
  (->> (cycle (concat (repeat secs speed) (repeat restsecs 0)))
       (reductions +)))

(defn distance
  [deer secs]
  (->> (distance-each-sec deer)
       (drop (dec secs))
       (first)))

(comment
  (->> puzzle
       (map (fn [[name _ _ _ :as deer]] [name (distance deer puzzle-seconds)]))
       (apply max-key second))
  ;=> ["Vixen" 2660]
  )


;; Part 2

(defn lead-each-sec
  "return a sequence ([deer distance] ...)
  for each second where deer is the lead deer and distance
  is how far it has traveled up to that second"
  [puzzle]
  (->> (map (fn [[name _ _ _ :as deer]]
              (map (fn [dist]
                     [name dist])
                   (distance-each-sec deer)))
            puzzle)
       (apply map vector) ;; sequence of where the pack is at each sec
       (map (fn [pack] (apply max-key second pack)))
       ))

(defn scores
  [puzzle secs]
  (let [lead-seq (lead-each-sec puzzle)]
    (apply merge-with + (map (fn [[name _]] {name 1}) (take secs lead-seq)))))

(comment
  (scores puzzle puzzle-seconds)
  ;=> {"Prancer" 504, "Blitzen" 1256, "Comet" 158, "Vixen" 422, "Rudolph" 154, "Dasher" 9}
  (apply max-key val (scores puzzle puzzle-seconds))
  ;=> ["Blitzen" 1256]
  )
