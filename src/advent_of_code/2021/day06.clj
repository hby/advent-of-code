(ns advent-of-code.2021.day06)

(def input (read-string (slurp "resources/2021/day06.edn")))

(defn step
  [input]
  (let [next (mapv #(if (zero? %) 6 (dec %)) input)]
    (into next (repeat (count (filter zero? input)) 8))))

(defn part1
  [input days]
  (->> (iterate step input)
       (drop days)
       first
       count))

(comment
  (part1 [3 4 3 1 2] 18)
  ;;=> 26
  ; check

  (part1 [3 4 3 1 2] 80)
  ;=> 5934
  ; check

  (part1 input 80)
  ;=> 379414

  (part1 [3 4 3 1 2] 256)
  ; BOOM
  :end)

;; So, there is a difference between running a simulation and calculating a
;; value resulting from the simulation.
;; We care about # of progeny.
;; That number will be the same for a given starting number and will
;; be independent of other starting numbers.
;; So we can add them up.

(defn population-size*
  ([start
    num-iterations]
   (population-size* start num-iterations 1))
  ([start
    num-iterations
    curr-population]
   (if (zero? num-iterations)
     curr-population
     (case start
       (1 2 3 4 5 6 7 8) (population-size* (dec start)
                                           (dec num-iterations)
                                           curr-population)
       0 (+ (population-size* 6
                              (dec num-iterations)
                              1)
            (population-size* 8
                              (dec num-iterations)
                              1))))))

(def population-size (memoize population-size*))

(defn part2
  [input days]
  (reduce + (map #(population-size % days) input)))

(comment
  (part2 [3 4 3 1 2] 256)
  ;=> 26984457539, correct

  (time (part2 input 256))
  ;=> 1705008653296
  :end)
