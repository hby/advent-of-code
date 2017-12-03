(ns advent-of-code.2015.day20
  (:require [clojure.math.numeric-tower :as math]))

(def puzzle 29000000)
(def puzzle-div-10 (/ puzzle 10))

;; I tried a couple approaches that avoided brute math that I thought
;; might be faster but none were and each involved some kind of guessing.
;; Sometimes math just speaks for itself.

;; From http://stackoverflow.com/questions/9556393/clojure-tail-recursion-with-prime-factors
(defn primefactors
  ([n]
   (primefactors n 2 '()))
  ([n candidate acc]
   (cond (<= n 1) (reverse acc)
         (zero? (rem n candidate)) (recur (/ n candidate) candidate (cons candidate acc))
         :else (recur n (inc candidate) acc))))

;; Formula at http://mathschallenge.net/library/number/sum_of_divisors
(defn sum-of-divisors
  [n]
  (let [pf (primefactors n)
        freqs (frequencies pf)]
    (apply *
           (map (fn [[p e]]
                  (/ (dec (math/expt p (inc e)))
                     (dec p)))
                freqs))))

(def present-seq
  (map sum-of-divisors (map inc (range))))

(comment
  ;; Part 1
  (first (drop-while #(< (% 1) puzzle-div-10) (map-indexed (fn [i v] [(inc i) v]) present-seq)))
  ;=> [665280 2926080]
  )
