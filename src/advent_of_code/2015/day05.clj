(ns advent-of-code.2015.day05
  (:require [clojure.string :as s]))

(def puzzle-text (slurp "resources/2015/day05.txt"))
(def puzzle (s/split-lines puzzle-text))

(defn three-vowels?
  [s]
  (let [vowels #{\a \e \i \o \u}]
    (< 2 (count (filter vowels s)))))

(defn one-double-char?
  [s]
  (not (empty? (filter #(< 1 (count %)) (partition-by identity s)))))

(defn no-bad-substrings?
  [s]
  (let [badregex #"ab|cd|pq|xy"]
    (not (re-find badregex s))))

(defn nice?
  [s]
  (let [predicates [three-vowels?
                    one-double-char?
                    no-bad-substrings?]]
    (every? #(% s) predicates)))

(comment
  (count (filter nice? puzzle)))
;; 255


(defn repeated-non-overlap-double?
  [s]
  (re-find #"(\w\w).*\1" s))

(defn aba-pattern?
  [s]
  (re-find #"(\w)\w\1" s))

(defn nice2?
  [s]
  (let [predicates [repeated-non-overlap-double?
                    aba-pattern?]]
    (every? #(% s) predicates)))

(comment
  (count (filter nice2? puzzle)))
;; 55
