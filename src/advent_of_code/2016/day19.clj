(ns advent-of-code.2016.day19
  (:require [clojure.set :as cset]))


(def input 3014387)

(declare dropping)

(defn skipping
  [order remain]
  (if (= 1 (count remain))
    (first remain)
    #(dropping
       (->> (drop-while (fn [x] (not (remain x))) order)
            (drop 1))
       remain)))

(defn dropping
  [order remain]
  (if (= 1 (count remain))
    (first remain)
    (let [[rem & r] (drop-while (fn [x] (not (remain x))) order)]
      #(skipping r (cset/difference remain #{rem})))))

(defn part1
  [n]
  (let [remain (set (range 1 (inc n)))
        order (cycle (range 1 (inc n)))]
       (trampoline skipping order remain)))

(comment
  (time (part1 input)))
  ;; ~35 secs
  ;; 1834471

