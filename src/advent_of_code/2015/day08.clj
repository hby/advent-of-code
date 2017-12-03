(ns advent-of-code.2015.day08
  (:require [clojure.string :as s]))

(def puzzle-text (slurp "resources/2015/day08.txt"))

(def puzzle
  (s/split-lines puzzle-text))

;; Gonna just bang out a specific FSM

(defn hex-char?
  [c]
  (or
    (<= 48 (int c) 57)
    (<= 97 (int c) 102)))

(defn n-real-chars
  [s]
  (loop [pending :nil
         cnt 0
         [f & r] (take (- (count s) 2) (drop 1 s))]
    (if f
      (recur
        ; pending
        (case pending
          :nil (if (= f \\) :slash :nil)
          :slash (case f
                   \" :nil
                   \\ :nil
                   \x :hex
                   :nil)
          :hex (cond
                 (hex-char? f) :hex1
                 :default :nil)
          :hex1 :nil)
        ;cnt
        (case pending
          :nil (if (= f \\) cnt (inc cnt))
          :slash (case f
                   \" (inc cnt)
                   \\ (inc cnt)
                   \x cnt
                   (+ 2 count))
          :hex (if (hex-char? f) cnt (+ 3 cnt))
          :hex1 (if (hex-char? f) (inc cnt) (+ 4 cnt)))
        ;[f & r]
        r)
      cnt)))

(comment
  (reduce
    (fn [sum str] (+ sum (- (count str) (n-real-chars str))))
    0
    puzzle))
;=> 1371

(defn n-encoded-chars
  [s]
  (reduce
    (fn [sum c]
      (case c
        \\ (+ 2 sum)
        \" (+ 2 sum)
        (inc sum)))
    2
    s))

(comment
  (reduce
    (fn [sum str] (+ sum (- (n-encoded-chars str) (count str))))
    0
    puzzle))
;=> 2117
