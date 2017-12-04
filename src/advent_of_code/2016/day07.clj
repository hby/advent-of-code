(ns advent-of-code.2016.day07
  (:require [clojure.string :as cs]))

(def input (slurp "resources/2016/day07.txt"))
(def lines (cs/split input #"\n"))

(defn o+i
  [line]
  (let [ois (cs/split line #"\[|\]")
        ois+ (if (odd? (count ois)) (conj ois "") ois)]
    (apply map vector (partition-all 2 ois+))))

(def puzzle (->> lines
                 (mapv o+i)))

(defn contains-abba?
  [s]
  (->> (partition 4 1 s)
       (filter (fn [[w x y z]] (and (= x y)
                                    (= w z)
                                    (not= w x))))
       (empty?)
       (not)))

(defn support-tls?
  [[os is]]
  (and (some contains-abba? os)
       (every? (complement contains-abba?) is)))

(comment
  (count (filter support-tls? puzzle))
  ;; => 115
  )

(defn abas
  [s]
  (->> (partition 3 1 s)
       (filter (fn [[x y z]] (and (= x z)
                                  (not= x y))))))

(defn aba->bab
  [[a b _]]
  [b a b])

(defn support-ssl?
  [[os is]]
  (some (into #{} (map aba->bab (mapcat abas os))) (mapcat abas is)))

(comment
  (count (filter support-ssl? puzzle))
  ;; => 231
  )