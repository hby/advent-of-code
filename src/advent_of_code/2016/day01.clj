(ns advent-of-code.2016.day01
  (:require [clojure.string :as cs]))

(def input (slurp "resources/2016/day01.txt"))

;; ([:R 2] [:L 3] ... [:L 2])
;; ([turn amount] ...)
(def puzzle (->> (cs/split input #",")
                 (map cs/trim)
                 (map #(vector
                         (-> % first str keyword)
                         (-> % (.substring 1) Integer/parseInt)))))

;; Keep track of orientation and position (state)
;; [orientation [xpos ypos]]

(def start [:N [0 0]])

(defn move-n
  "move north"
  [[x y] n]
  [x (+ y n)])

(defn move-s
  "move south"
  [[x y] n]
  [x (- y n)])

(defn move-e
  "move east"
  [[x y] n]
  [(+ x n) y])

(defn move-w
  "move west"
  [[x y] n]
  [(- x n) y])

;; orientation + turn -> new orientation
(def ot->o
  {[:N :R] :E
   [:N :L] :W
   [:S :R] :W
   [:S :L] :E
   [:E :R] :S
   [:E :L] :N
   [:W :R] :N
   [:W :L] :S})

;; function to move given an orientation
(def o->mf {:N move-n
            :S move-s
            :E move-e
            :W move-w})
(defn move
  "From a state, turn t and move n"
  [[o pos] t n]
  (let [no (ot->o [o t])]
    [no ((o->mf no) pos n)]))

(defn solve1
  "Return [orientation position] (state) from start state and set of puzzle moves."
  []
  (reduce (fn [s [t n]] (move s t n))
          start
          puzzle))

(comment
  (solve1)
  ;;=> ["N" [-123 -123]]
  ;; answer: (+ 123 123) => 246
  )

;; Only tracking turn points but now need to record all points traversed

(defn first-dup
  "Returns the first duplicate value of a sequence."
  [s]
  (loop [[f & r] s seen #{}]
    (if (and f (seen f))
      f
      (if r
        (recur r (conj seen f))))))

(defn positions
  "all positions that (mf p n) visits."
  [mf p n]
  (for [i (range 1 (inc n))]
    (mf p i)))

(defn trail
  "return all states in the move from state with turn t, position pos."
  [[o pos] t n]
  (let [no (ot->o [o t])]
    (map vector (repeat no) (positions (o->mf no) pos n))))

(defn solve2
  []
  (let [ps (map second (reduce (fn [ss [t n]] (concat ss (trail (last ss) t n)))
                               [start]
                               puzzle))]
    (first-dup ps)))

(comment
  (solve2)
  ;; => [-109 -15]
  ;; answer: 124
  )
