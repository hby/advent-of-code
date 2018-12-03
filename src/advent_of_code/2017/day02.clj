(ns advent-of-code.2017.day02
  (:require [clojure.string :as cs]))

(def puzzle (->> (cs/split (slurp "resources/2017/day02.txt") #"\n")
                 (map #(cs/trim %))
                 (mapv (fn [l] (->> (cs/split l #"\s+")
                                    (mapv (fn [i] (Integer/parseInt i))))))))

(defn line-cksum
  [l]
  (apply - (apply (juxt max min) l)))

(defn one
  [puzzle]
  (apply + (map line-cksum puzzle)))

(comment
  (one puzzle))
  ; => 44887


(defn evenq
  [n d]
  (let [[q r] ((juxt quot rem) n d)]
    (when (zero? r) q)))

(defn evenqs
  [n ds]
  (some #(evenq n %) ds))

(defn line-value
  [l]
  (let [l (sort #(compare %2 %1) l)]
    (some (fn [[f ds]]
            (evenqs (last f) ds))
          (map #(split-at % l) (range 1 (count l))))))

(defn two
  [puzzle]
  (apply + (filter identity (map line-value puzzle))))

(comment
  (two puzzle))
  ; => 242

