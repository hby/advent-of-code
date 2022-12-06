(ns advent-of-code.2021.day01
  (:require [criterium.core :as crit]))

(def input (read-string (slurp "resources/2021/day01.edn")))

(defn part1
  ([input] (part1 input 0))
  ([[f s :as all] count]
   (cond (and f s (< f s)) (recur (rest all) (inc count))
         (and f s) (recur (rest all) count)
         :else count)))

(comment
  (part1 input)
  ; => 1301
  :end)

(defn part2
  ([input] (part2 input 0))
  ([[e1 _ _ e4 :as all] count]
   (cond (and e1 e4 (< e1 e4))
         (recur (rest all) (inc count))

         (and e1 e4)
         (recur (rest all) count)

         :else count)))

(comment
  (part2 input)
  ; => 1346
  :end)


;; other ways

(defn part1b
  [input]
  (->> input
       (partition 2 1)
       (map (partial apply <))
       (filter true?)
       count))

(defn part1c
  [input]
  (->> (map < input (rest input))
       (filter true?)
       count))

(comment

  ; Machine:
  ; MacBook Pro (16-inch, 2021)
  ; Apple M1 Max
  ; 64 GB
  ; Java:
  ; temurin-17.0.1+12
  ; -Xss1g -Xms1g -Xmx1g
  ; clojure 1.10.3.1040

  (crit/quick-bench (part1 input))
  ; Evaluation count : 3180 in 6 samples of 530 calls.
  ;             Execution time mean : 188.187541 µs
  ;    Execution time std-deviation : 1.216168 µs
  ;   Execution time lower quantile : 186.843581 µs ( 2.5%)
  ;   Execution time upper quantile : 189.383360 µs (97.5%)
  ;                   Overhead used : 2.095688 ns

  (crit/quick-bench (part1b input))
  ; Evaluation count : 1062 in 6 samples of 177 calls.
  ;             Execution time mean : 559.924187 µs
  ;    Execution time std-deviation : 4.115022 µs
  ;   Execution time lower quantile : 555.282277 µs ( 2.5%)
  ;   Execution time upper quantile : 565.319736 µs (97.5%)
  ;                   Overhead used : 2.095688 ns

  (crit/quick-bench (part1c input))
  ; Evaluation count : 3600 in 6 samples of 600 calls.
  ;             Execution time mean : 167.757199 µs
  ;    Execution time std-deviation : 412.950565 ns
  ;   Execution time lower quantile : 167.114988 µs ( 2.5%)
  ;   Execution time upper quantile : 168.149329 µs (97.5%)
  ;                   Overhead used : 2.095688 ns
  :end)

(defn part2b
  [input]
  (->> input
       (partition 3 1)
       (map (partial apply +))
       (part1)))

(defn part2c
  [input]
  (let [s3 (map + input (rest input) (rest (rest input)))]
    (->> (map < s3 (rest s3))
         (filter true?)
         count)))

(comment
  (crit/quick-bench (part2 input))
  ; Evaluation count : 2328 in 6 samples of 388 calls.
  ;             Execution time mean : 258.283377 µs
  ;    Execution time std-deviation : 447.382214 ns
  ;   Execution time lower quantile : 257.643755 µs ( 2.5%)
  ;   Execution time upper quantile : 258.732040 µs (97.5%)
  ;                   Overhead used : 2.081305 ns

  (crit/quick-bench (part2b input))
  ; Evaluation count : 756 in 6 samples of 126 calls.
  ;             Execution time mean : 795.435365 µs
  ;    Execution time std-deviation : 1.613744 µs
  ;   Execution time lower quantile : 793.405000 µs ( 2.5%)
  ;   Execution time upper quantile : 797.465345 µs (97.5%)
  ;                   Overhead used : 2.081305 ns

  (crit/quick-bench (part2c input))
  ; Evaluation count : 2076 in 6 samples of 346 calls.
  ;             Execution time mean : 291.122864 µs
  ;    Execution time std-deviation : 742.774210 ns
  ;   Execution time lower quantile : 290.463101 µs ( 2.5%)
  ;   Execution time upper quantile : 292.129108 µs (97.5%)
  ;                   Overhead used : 2.095688 ns
  :end)
