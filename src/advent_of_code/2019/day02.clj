(ns advent-of-code.2019.day02)

(def base-input (read-string (slurp "resources/2019/day02.txt")))

(defn mk-input
  [base-input n v]
  (-> base-input
      (assoc 1 n)
      (assoc 2 v)))

(defn instruction
  [pos state]
  (->> state
       (drop pos)
       (take 4)))

(defn step
  [[op i1 i2 o] state]
  (let [f ({1 + 2 *} op)]
    (assoc state o (f (state i1) (state i2)))))

(defn run1
  [input]
  (reduce (fn [state pos]
            (let [instr (instruction pos state)]
              (if (= 99 (first instr))
               (reduced state)
               (step instr state))))
          input
          (range 0 (dec (count input)) 4)))

(defn part1
  [input]
  (first (run1 input)))

(comment
  (part1 (mk-input base-input 12 2))
  ;; => 3790689
  )

(def output 19690720)

(defn part2
  [input]
  (for [n (range 100)
        v (range 100)
        :when (= output (part1 (mk-input input n v)))]
    (+ (* 100 n) v)))

(comment
  (first (part2 base-input))
  ;; => 6533
  )