(ns advent-of-code.day06
  (:require [clojure.string :as s]))

(set! *warn-on-reflection* true)

(def puzzle-text (slurp "resources/input/day06.txt"))

(def pattern
  #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")

(def puzzle
  (map (fn [p] (into [(first p)] (map #(Integer/parseInt %) (drop 1 p))))
       (map (fn [l] (drop 1 (re-find pattern l)))
            (s/split-lines puzzle-text))))

(defn ranges->points
  "xr, yr - ranges to sweep through.
   f      - function, sequence values are (f [x y])
            identity function is used if not supplied"
  ([xr yr f]
   (for [x xr
         y yr]
     (f [x y])))
  ([xr yr]
   (ranges->points xr yr identity)))

(def board
  (into {}
        (ranges->points (range 1000) (range 1000) #(vector % 0))))

(defn rect->points
  [x1 y1 x2 y2]
  (ranges->points (range (min x1 x2) (inc (max x1 x2)))
                  (range (min y1 y2) (inc (max y1 y2)))))

(defn turn-on
  ([board l]
   (assoc board l 1))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-on b l))
           board
           (rect->points x1 y1 x2 y2))))

(defn turn-off
  ([board l]
   (assoc board l 0))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-off b l))
           board
           (rect->points x1 y1 x2 y2))))

(defn toggle
  ([board l]
   (assoc board l (if (zero? (board l)) 1 0)))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (toggle b l))
           board
           (rect->points x1 y1 x2 y2))))

(def actions {"turn on" turn-on
              "turn off" turn-off
              "toggle" toggle})

(defn new-board
  [board puzzle actions]
  (reduce (fn [b [a x1 y1 x2 y2]]
            ((actions a) b x1 y1 x2 y2))
          board
          puzzle))

(comment
  (reduce +
          0
          (vals (new-board board puzzle actions))))
;; 377891


;; Not be the best organizationally but going to do this to get the answer to part 2
(defn turn-on-2
  ([board l]
   (assoc board l (inc (board l))))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-on-2 b l))
           board
           (rect->points x1 y1 x2 y2))))

(defn turn-off-2
  ([board l]
   (assoc board l (if (zero? (board l)) 0 (dec (board l)))))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-off-2 b l))
           board
           (rect->points x1 y1 x2 y2))))

(defn toggle-2
  ([board l]
   (assoc board l (+ 2 (board l))))
  ([board x1 y1 x2 y2]
   (reduce (fn [b l]
             (toggle-2 b l))
           board
           (rect->points x1 y1 x2 y2))))

(def actions-2 {"turn on" turn-on-2
                "turn off" turn-off-2
                "toggle" toggle-2})

(comment
  (time
    (reduce +
            0
            (vals (new-board board puzzle actions-2)))))
;"Elapsed time: 28982.363665 msecs"
;=> 14110788


;;; This had made me curious about a couple performance things.

;; Try transients
(defn turn-on-2-transient
  ([tboard l]
   (assoc! tboard l (inc (tboard l))))
  ([board x1 y1 x2 y2]
   (let [tb (transient board)]
     (persistent!
       (reduce (fn [b l]
                 (turn-on-2-transient b l))
               tb
               (rect->points x1 y1 x2 y2))))))

(defn turn-off-2-transient
  ([tboard l]
   (assoc! tboard l (if (zero? (tboard l)) 0 (dec (tboard l)))))
  ([board x1 y1 x2 y2]
   (let [tb (transient board)]
     (persistent!
       (reduce (fn [b l]
                 (turn-off-2-transient b l))
               tb
               (rect->points x1 y1 x2 y2))))))

(defn toggle-2-transient
  ([tboard l]
   (assoc! tboard l (+ 2 (tboard l))))
  ([board x1 y1 x2 y2]
   (let [tb (transient board)]
     (persistent!
       (reduce (fn [b l]
                 (toggle-2-transient b l))
               tb
               (rect->points x1 y1 x2 y2))))))

(def actions-2-transient {"turn on" turn-on-2-transient
                          "turn off" turn-off-2-transient
                          "toggle" toggle-2-transient})

(comment
  (time
    (reduce +
            0
            (vals (new-board board puzzle actions-2-transient)))))
;"Elapsed time: 24254.964758 msecs"
;=> 14110788
;; Hmmm, that saved just less than 5 seconds. I guess I expected more.


;; I could try moving into transient space only once for all the actions
;; instead of going in and out for each action.

(defn new-board-one-transient
  [board puzzle actions]
  (persistent!
    (reduce (fn [b [a x1 y1 x2 y2]]
              ((actions a) b x1 y1 x2 y2))
            (transient board)
            puzzle)))

(defn turn-on-2-one-transient
  ([tboard l]
   (assoc! tboard l (inc (tboard l))))
  ([tboard x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-on-2-one-transient b l))
           tboard
           (rect->points x1 y1 x2 y2))))

(defn turn-off-2-one-transient
  ([tboard l]
   (assoc! tboard l (if (zero? (tboard l)) 0 (dec (tboard l)))))
  ([tboard x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-off-2-one-transient b l))
           tboard
           (rect->points x1 y1 x2 y2))))

(defn toggle-2-one-transient
  ([tboard l]
   (assoc! tboard l (+ 2 (tboard l))))
  ([tboard x1 y1 x2 y2]
   (reduce (fn [b l]
             (toggle-2-one-transient b l))
           tboard
           (rect->points x1 y1 x2 y2))))

(def actions-2-one-transient {"turn on" turn-on-2-one-transient
                              "turn off" turn-off-2-one-transient
                              "toggle" toggle-2-one-transient})

(comment
  (time
    (reduce +
            0
            (vals (new-board-one-transient board puzzle actions-2-one-transient)))))
;"Elapsed time: 22467.221171 msecs"
;=> 14110788
;; Only another (less than) 2 seconds faster. Maybe I'm wrong in thinking this should be faster.

; I've been repersenting a 2-d array as a map with 2-vector keys. Maybe another
; representation is faster?

;; I'll try a using a 1-d array and value at [x,y] is at x + 1000*y
;;  and transients.

(defn index
  [[x y]]
  (+ x (* 1000 y)))

(def lineboard
  (into [] (repeat (* 1000 1000) 0)))

(defn turn-on-line-transient
  ([tboard l]
   (assoc! tboard (index l) (inc (tboard (index l)))))
  ([tboard x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-on-line-transient b l))
           tboard
           (rect->points x1 y1 x2 y2))))

(defn turn-off-line-transient
  ([tboard l]
   (assoc! tboard (index l) (if (zero? (tboard (index l))) 0 (dec (tboard (index l))))))
  ([tboard x1 y1 x2 y2]
   (reduce (fn [b l]
             (turn-off-line-transient b l))
           tboard
           (rect->points x1 y1 x2 y2))))

(defn toggle-line-transient
  ([tboard l]
   (assoc! tboard (index l) (+ 2 (tboard (index l)))))
  ([tboard x1 y1 x2 y2]
   (reduce (fn [b l]
             (toggle-line-transient b l))
           tboard
           (rect->points x1 y1 x2 y2))))

(def actions-line-transient {"turn on" turn-on-line-transient
                              "turn off" turn-off-line-transient
                              "toggle" toggle-line-transient})

(comment
  (time
    (reduce +
            0
            (new-board-one-transient lineboard puzzle actions-line-transient))))
;"Elapsed time: 3443.695844 msecs"
;=> 14110788

;; Ok, 3.5 second range feels more like it. Lots better that 22.
