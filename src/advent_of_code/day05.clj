(ns advent-of-code.day05
  (:require [clojure.string :as s]))

(def puzzle-text (slurp "resources/input/day05.txt"))
(def puzzle (s/split-lines puzzle-text))

(defn has-three-vowels
  [s]
  (let [vowels #{\a \e \i \o \u}]
    (< 2
       (reduce +
               (map #(if (contains? vowels %) 1 0)
                    s)))))

(defn has-one-double-char
  [s]
  (not (empty? (filter #(< 1 (count %)) (partition-by identity s)))))

(defn has-no-bad-substrings
  [s]
  (let [bads ["ab" "cd" "pq" "xy"]]
    (reduce (fn [p bad] (and p (not (.contains s bad))))
            true
            bads)))

(defn nice?
  [s]
  (let [predicates [has-three-vowels
                    has-one-double-char
                    has-no-bad-substrings]]
    (reduce (fn [t p] (and t (p s)))
            true
            predicates)))

(comment
  (count (filter nice? puzzle)))
;; 255


(defn has-repeat-non-overlap-double
  [s]
  (re-find #"(\w\w).*\1" s))

(defn has-an-aba-pattern
  [s]
  (re-find #"(\w)\w\1" s))

(defn nice2?
  [s]
  (let [predicates [has-repeat-non-overlap-double
                    has-an-aba-pattern]]
    (reduce (fn [t p] (and t (p s)))
            true
            predicates)))

(comment
  (count (filter nice2? puzzle)))
;; 55
