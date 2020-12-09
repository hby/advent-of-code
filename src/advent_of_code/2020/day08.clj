(ns advent-of-code.2020.day08
  (:require [clojure.string :as str]
            [clojure.walk :as w]))

(def input-file (slurp "resources/2020/day08.txt"))

(def input
  (->> (-> input-file
           (str/split #"\n"))
       (map #(str/split % #" "))
       (mapv #(update-in % [1] (fn [s] (Integer/parseInt s))))))

(defn augment-input
  "add explicit next instruction offset"
  [input]
  (w/postwalk
    (fn [n]
      (if (and (vector? n) (= 2 (count n)))
        (cond
          (= "jmp" (first n))
          (conj n (second n))
          :else
          (conj n 1))
        n))
    input))

(defn positions
  [ai start]
  (if (= start (count ai))
    '()
    (lazy-seq
      (cons start
            (positions ai (+ start (last (get ai start))))))))

(defn take-to-first-dup
  [positions seen]
  (if (seen (first positions))
    '()
    (lazy-seq
      (cons (first positions)
            (take-to-first-dup (rest positions) (conj seen (first positions)))))))

(defn part1
  [input]
  (let [ai (augment-input input)]
    (->> (-> ai
             (positions 0)
             (take-to-first-dup #{}))
         (map #(when (= "acc" (first (get ai %))) (second (get ai %))))
         (filter identity)
         (reduce +))))


(comment

  (part1 input)
  ;; 1810
  #_[])

