(ns advent-of-code.2022.day05
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;        [H]     [W] [B]
;    [D] [B]     [L] [G] [N]
;[P] [J] [T]     [M] [R] [D]
;[V] [F] [V]     [F] [Z] [B]     [C]
;[Z] [V] [S]     [G] [H] [C] [Q] [R]
;[W] [W] [L] [J] [B] [V] [P] [B] [Z]
;[D] [S] [M] [S] [Z] [W] [J] [T] [G]
;[T] [L] [Z] [R] [C] [Q] [V] [P] [H]
; 1   2   3   4   5   6   7   8   9

;; Might parse this later; just not interested in that part right now
(def initial-stacks
  {1 '(:P :V :Z :W :D :T)
   2 '(:D :J :F :V :W :S :L)
   3 '(:H :B :T :V :S :L :M :Z)
   4 '(:J :S :R)
   5 '(:W :L :M :F :G :B :Z :C)
   6 '(:B :G :R :Z :H :V :W :Q)
   7 '(:N :D :B :C :P :J :V)
   8 '(:Q :B :T :P)
   9 '(:C :R :Z :G :H)})

; move ex.
; move 3 from 2 to 9

(def rx #"move (\d+) from (\d+) to (\d+)")
(def input (->> (slurp "resources/2022/day05.txt")
                (str/split-lines)
                (map (partial re-matches rx))
                (map (partial drop 1))
                (map #(map (fn [s] (Integer/parseInt s)) %))))

(defn move
      [s [n f t]]
      (let [to-move (take n (s f))
            nf (drop n (s f))
            nt (into (s t) to-move)]
        (assoc s t nt f nf)))

(defn part1
  [input]
  (let [r (reduce (fn [s m]
                    (move s m))
                  initial-stacks
                  input)]
    (->> (map (fn [k] (get r k)) (range 1 10))
         (map first)
         (map name)
         (apply str))))

(comment
  (part1 input)
  ;; => "TLFGBZHCN"
  :end)

(defn move-9001
  [s [n f t]]
  (let [to-move (take n (s f))
        nf (drop n (s f))
        nt (concat to-move (s t))]
    (assoc s t nt f nf)))

(defn part2
  [input]
  (let [r (reduce (fn [s m]
                    (move-9001 s m))
                  initial-stacks
                  input)]
    (->> (map (fn [k] (get r k)) (range 1 10))
         (map first)
         (map name)
         (apply str))))

(comment
  (part2 input)
  ;; => "QRQFHFWCL"
  :end)
