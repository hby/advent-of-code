(ns advent-of-code.2016.day08
  (:require [clojure.string :as cs]))

(def input (slurp "resources/2016/day08.txt"))
(def lines (cs/split input #"\n"))

(def pattern #"(rect)\s([0-9]+)x([0-9]+)|(rotate)\s(row|column)\s[x|y]=([0-9]+)\sby\s([0-9]+)")

(def puzzle
  (map (fn [l] (let [p (re-find pattern l)
                     [[cmd-rect rect-r rect-n] [cmd-rot rot-axis rot-rc rot-n]]
                       [(take 3 (drop 1 p)) (drop 4 p)]]
                 (if cmd-rect
                   [cmd-rect (Integer/parseInt rect-r) (Integer/parseInt rect-n)]
                   [rot-axis (Integer/parseInt rot-rc) (Integer/parseInt rot-n)])))
       lines))

(def nrows 6)
(def ncols 50)

(def board (repeat nrows (repeat ncols 0)))

(defn rect
  [b w h]
  (concat
    (map (fn [r] (concat (repeat w 1) (drop w r)))
         (take h b))
    (drop h b)))

(defn rotate-row-right
  [b r n]
  (let [nc (count (first b))
        mnc (mod n nc)]
    (concat
     (take r b)
     (let [row (first (drop r b))] (list (concat (drop (- nc mnc) row) (take (- nc mnc) row))))
     (drop (inc r) b))))

(defn rotate-col-down
  [b c n]
  (let [tb (apply map list b)
        nb (rotate-row-right tb c n)
        tnb (apply map list nb)]
    tnb))

(defn number-on
  [b]
  (apply + (map (partial apply +) b)))

(defn process
  [b cmds]
  (reduce (fn [cb cmd]
            (let [lup {"rect" rect
                       "row" rotate-row-right
                       "column" rotate-col-down}
                  f (lup (nth cmd 0))
                  arg1 (nth cmd 1)
                  arg2 (nth cmd 2)]
              (f cb arg1 arg2)))
          b
          cmds))

(comment
  (number-on (process board puzzle))
  ;; => 123
  )

(defn str-board
  [b]
  (map (fn [l]
         (let [segs (partition-all 5 l)]
           (map #(apply str (map {0 " " 1 "X"} %)) segs)
           ))
       b))

(comment
  (str-board (process board puzzle))
  ;" XX  " "XXXX " "XXX  " "X  X " "XXX  " "XXXX " "XXX  " "  XX " "XXX  " " XXX ")
  ;"X  X " "X    " "X  X " "X  X " "X  X " "   X " "X  X " "   X " "X  X " "X    ")
  ;"X  X " "XXX  " "XXX  " "X  X " "X  X " "  X  " "XXX  " "   X " "X  X " "X    ")
  ;"XXXX " "X    " "X  X " "X  X " "XXX  " " X   " "X  X " "   X " "XXX  " " XX  ")
  ;"X  X " "X    " "X  X " "X  X " "X    " "X    " "X  X " "X  X " "X    " "   X ")
  ;"X  X " "X    " "XXX  " " XX  " "X    " "XXXX " "XXX  " " XX  " "X    " "XXX  "))
  )