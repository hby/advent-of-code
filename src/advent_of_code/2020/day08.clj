(ns advent-of-code.2020.day08
  (:require [clojure.string :as str]
            [clojure.walk :as w]))

;; This problem looked a bit boring to me at first glance.
;; I just didn't want to code up a verbatim simulation of
;; some machine.
;;
;; Then I took another look and saw a way to get at the answer
;; without a complete simulation and that it could be a more
;; functional approach. So, here goes.
;;
;; The first observation was that I do not care about the order
;; of "acc" executions. As long as i have them all I can add them up.
;; For that, if I had a sequence of program counter positions then I
;; can get the set of instructions executed, filter for what I care
;; about, and sum up the values.
;;
;; So, this solution is built on a lazy sequence of program counter (pc)
;; positions. Since our stopping conditions are based on pc values,
;; this is all we need to limit the sequence to either first repeated
;; statement or execution beyond program range.
;;
;; It's still (obviously) a simulation but enough is eliminated to
;; make it interesting to me.

(def input-file (slurp "resources/2020/day08.txt"))

;; It will make things better later if every instruction has an explicit
;; value for the offset to the next instruction. So, adding a third field
;; to each instruction. That will be what I call my input.

(def input
  (->> (-> input-file
           (str/split #"\n"))
       (w/postwalk (fn [n]
                     (if (string? n)
                       (let [[op sn] (str/split n #" ")
                             n (Integer/parseInt sn)]
                         [op n (if (= "jmp" op) n 1)])
                       n)))))

(defn positions
  "Returns a lazy seq of pc positions from an
   execution start position.

  It will stop if the pc ever goes outside of
  number of input elements.

  Also, if finite, the last value will be the pc that
  is out of bounds so we can distinguish later between
  stopping when we hit a value seen before and one that
  is out of bounds."
  [input start]
  (if (>= start (count input))
    [start]
    (lazy-seq
      (cons start
            (positions input (+ start (last (get input start))))))))

(defn first-loop
  "Limit positions seq to first loop.
  Keep repeated position as last element."
  ([positions] (first-loop positions #{}))
  ([positions seen]
   (if (seen (first positions))
     [(first positions)]
     (lazy-seq
       (cons (first positions)
             (first-loop (rest positions) (conj seen (first positions))))))))

(defn sum-of-accs
  "Sum of acc values at positions in input"
  [input positions]
  (reduce (fn [sum p]
            (let [[op n _] (get input p)]
              (if (= "acc" op)
                (+ sum n)
                sum)))
          0
          positions))

(defn part1
  [input]
  (->> (-> input
           (positions 0)
           (first-loop)
           butlast)
       (sum-of-accs input)))

(comment
  (part1 input)
  ;; 1810
  #_[])

(defn input-trial-seq
  "A lazy seq of modified inputs."
  [input]
  (->> (map-indexed (fn [i [op _n _d]] (when (#{"jmp" "nop"} op) i)) input)
       (remove nil?)
       (map (fn [i] (update input i (fn [[op n _d]]
                                     (cond (= "jmp" op) ["nop" n 1]
                                           :else ["jmp" n n])))))))

(defn terminating-positions
  "Returns a position sequence that terminates at 'end', or nil.
  This is the same as first-loop with seen set primed with 'end'."
  [positions end]
  (let [trial (first-loop positions #{end})]
    (when (= end (last trial))
      (butlast trial))))


(defn fixed-input-positions
  "The positions of trail sequence that is fixed
  (that is, terminates)."
  [input]
  (->> (input-trial-seq input)
       (map #(terminating-positions
               (positions % 0)
               (count input)))
       (remove nil?)
       first))

(defn part2
  [input]
  (->> (fixed-input-positions input)
       (sum-of-accs input)))

(comment
  (part2 input)
  ;; 969
  #_[])