(ns advent-of-code.2016.day04
  (:require [clojure.string :as cs]))

(def input (slurp "resources/2016/day04.txt"))
(def lines (cs/split input #"\n"))

(def pattern #"^(?:([a-z\-]*))[-](?:([0-9]*))\[(?:([a-z]*))\]$")

;; [chars id cksum]
(def puzzle (->> lines
                 (mapv #(vec (drop 1 (re-find pattern %))))))

(defn cksum
  [cs]
  (->> cs
       (remove (partial = \-))
       (group-by identity)
       (sort-by (juxt (comp - (partial count) second) first))
       (take 5)
       (map first)
       (apply str)))

(def real
  (map (fn [[e id cs]] [e (Integer/parseInt id) cs])
       (filter #(= (cksum (first %)) (last %)) puzzle)))

(defn sum-ids
  [s]
  (apply + (map second s)))

(comment
  (sum-ids real)
  ;; => 361724
  )

(def alphabet (seq "abcdefghijklmnopqrstuvwxyz"))

(defn decrypt
  [s n]
  (apply str
         (map (fn [c]
                (if (= \- c)
                  \space
                  (nth alphabet (mod (+ n (- (int c) (int \a))) 26))))
              s)))

(def labeled-rooms
  (map (fn [[e id _]] [(decrypt e id) id])
       real))

(comment
  (filter #(re-find #"north" (first %)) labeled-rooms)
  ;; => (["northpole object storage" 482])
  )
