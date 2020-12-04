(ns advent-of-code.2020.day04
  (:require [clojure.string :as str]))

(def input-file (slurp "resources/2020/day04.txt"))

(def input
  (->> (str/split input-file #"\n\n")
       (map #(str/split % #"[\s\n]"))
       (map (partial map #(str/split % #":")))
       (map #(into {} %))))

(def fields #{"ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"})

(defn valid1?
  [m]
  (= fields (set (keys (select-keys m fields)))))

(defn part1
  [input]
  (->> input
       (filter valid1?)
       count))

(comment

  (part1 input)
  ;; 200
  )

(defn valid2?
  [m]
  (let [tests {"byr" #(<= 1920 (read-string %) 2002)
               "iyr" #(<= 2010 (read-string %) 2020)
               "eyr" #(<= 2020 (read-string %) 2030)
               "hgt" #(cond (str/ends-with? % "cm") (<= 150 (read-string (subs % 0 (- (count %) 2))) 193)
                            (str/ends-with? % "in") (<= 59 (read-string (subs % 0 (- (count %) 2))) 76)
                            :else false)
               "hcl" #(re-find #"^#[0-9a-f]{6}$" %)
               "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
               "pid" #(re-find #"^[0-9]{9}$" %)}]
    (every? #((get tests % (constantly true)) (m %)) (keys m))))

(defn part2
  [input]
  (->> input
       (filter valid1?)
       (filter valid2?)
       count))

(comment

  (part2 input)
  ;; 116
  )