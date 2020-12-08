(ns advent-of-code.2020.day07
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(def input-file (slurp "resources/2020/day07.txt"))

;; slightly messy; just not in the mood for regex
;; [["outer" [[n1 "inner1"] [n2 "inner2"] ...] ...]
(def input
  (->> (str/split input-file #"\n")
       (mapv #(-> %
                  (str/split #" bags contain no other bags.| bags contain ")))
       (mapv #(if (second %)
                [(first %)
                 (->> (-> (second %)
                          (str/split #" bag, | bags, | bags.| bag."))
                      (mapv (fn [s] (let [[n a b] (str/split s #" ")]
                                      [(Integer/parseInt n) (str a " " b)]))))]
                %))))

(defn inner->outer
  [input]
  (reduce (fn [i->o [o is]]
            (merge-with #(apply conj %1 %2)
                        i->o
                        (when is
                          (->> is
                               (map #(vector (second %) [o]))
                               (into {})))))
          {}
          input))

(defn transitive-closure
  [k-vs k]
  (let [vs (k-vs k)]
    (if (seq vs)
      (into vs (mapcat #(transitive-closure k-vs %) vs))
      [])))

(defn part1
  [input]
  (-> (inner->outer input)
      (transitive-closure "shiny gold")
      distinct
      count))

(comment

  (part1 input)
  ;; 208
  #_ [])

(defn outer->inner
  [input]
  (->> input
       (map (fn [[k v]] (if v [k v] [k []])))
       (into {})))

(defn total-inner
  [o->is o]
  (let [is (o->is o)]
    (if (seq is)
      (reduce +
              (concat (map first is)
                      (map (fn [[n io]] (* n (total-inner o->is io))) is)))
      0)))

(defn part2
  [input]
  (total-inner (outer->inner input) "shiny gold"))

(comment

  (part2 input)
  ;; 1664
  #_ [])