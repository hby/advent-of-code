(ns advent-of-code.day07
  (:require [clojure.string :as s]))

(def puzzle-text (slurp "resources/input/day07.txt"))

;; parses out an 8-tuple
;; (leftopnum leftopstr op rightopnum rightopstr noopnum noopstr out)
(def pattern
  #"(?:(?:(?:(\d+)|([a-z]+))* ?(AND|OR|NOT|LSHIFT|RSHIFT) (?:(\d+)|([a-z]+)))|(?:(\d+)|([a-z]+))) -> ([a-z]+)")

(def puzzle-pieces
  (map (fn [l] (drop 1 (re-find pattern l)))
       (s/split-lines puzzle-text)))

(defn parse-int-in
  [m ks]
  (update-in m ks #(if % (Integer/parseInt %) %)))

(def puzzle
  (map (fn [p]
         (-> p
             (vec)
             (parse-int-in [0])
             (parse-int-in [3])
             (parse-int-in [5])))
       puzzle-pieces))

(defn AND
  [l r]
  (bit-and l r))

(defn OR
  [l r]
  (bit-or l r))

(defn NOT
  [_ r]
  (bit-xor 65535 r))

(defn LSHIFT
  [l r]
  (bit-and 65535
           (bit-shift-left l r)))

(defn RSHIFT
  [l r]
  (unsigned-bit-shift-right l r))

(defn NOOP
  [_ v] v)

(def ops
  {"AND" AND
   "OR" OR
   "NOT" NOT
   "RSHIFT" RSHIFT
   "LSHIFT" LSHIFT
   nil NOOP})

;; out -> [op l r]
(def network
  (into {}
        (map (fn [[lopn lops op ropn rops nopn nops out]]
               [out
                [op
                 (if lopn lopn lops)
                 (if op (if ropn ropn rops) (if nopn nopn nops))]])
             puzzle)))

(def output
  (memoize
    (fn [net out]
      (let [[op l r] (net out)]
        ((ops op)
          (if (string? l) (output net l) l)
          (if (string? r) (output net r) r))))))

(comment
  (time
    (output network "a")))
;"Elapsed time: 0.041553 msecs"
;=> 956

(def new-network
  (assoc network "b" [nil nil 956]))

(comment
  (time
    (output new-network "a")))
;"Elapsed time: 0.041161 msecs"
;=> 40149
