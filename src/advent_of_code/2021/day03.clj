(ns advent-of-code.2021.day03)

;; parsing shmarshing, I have an editor
(def input (read-string (slurp "resources/2021/day03.edn")))

(defn most-common
  [v]
  (let [ones (apply + v)
        zeros (- (count v) ones)]
    (if (< zeros ones) 1 0)))

(defn b->d
  [v]
  (reduce (fn [t d] (+ (* 2 t) d)) v))

(defn part1
  [input]
  (let [gamma (->> input
                   (apply map vector)
                   (map most-common))
        epsilon (map (partial - 1) gamma)]
    (* (b->d gamma) (b->d epsilon))))

(comment
  (part1 input)
  ; => 4138664
  :end)

(defn least
  [coll pos]
  (let [v (-> (apply map vector coll)
              (nth pos))
        ones (apply + v)
        zeros (- (count v) ones)]
    (if (<= zeros ones) 0 1)))

(defn most
  [coll pos]
  (let [v (-> (apply map vector coll)
              (nth pos))
        ones (apply + v)
        zeros (- (count v) ones)]
    (if (>= ones zeros) 1 0)))

(defn whittle
  [coll pos bit]
  (if (= 1 (count coll))
    coll
    (filter #(= bit (nth % pos)) coll)))

(defn step
  [[lcoll mcoll] pos]
  (let [l (least lcoll pos)
        m (most mcoll pos)]
    [(whittle lcoll pos l) (whittle mcoll pos m)]))

(defn whittle-down
  [coll]
  (reduce (fn [curr pos]
            (if (= [1 1] (map count curr))
              (reduced curr)
              (step curr pos)))
          [coll coll]
          (range (count (first coll)))))

(defn part2
  [input]
  (->> (whittle-down input)
       (map first)
       (map b->d)
       (apply *)))

(comment
  (part2 input)
  ; => 4273224
  :end)
