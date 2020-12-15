(ns advent-of-code.2020.day13
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

(def input
  (->> (-> (slurp "resources/2020/day13.txt")
           (str/split #","))
       (keep-indexed (fn [i n]
                       (when (not= "x" n)
                         [(Integer/parseInt n) i])))))

(def min-time 1002392)
(def periods (mapv first input))
;; [23,41,37,421,17,19,29,487,13]

(def positions (mapv second input))
;; [0,13,17,23,40,42,52,54,67]


(defn part1 []
  (let [mods (map #(mod min-time %) periods)
        deltas (map - periods mods)
        min-delta (apply min deltas)
        idx (->> (map-indexed vector deltas)
                 (drop-while #(not= min-delta (second %)))
                 (ffirst))]
    (* min-delta (get periods idx))))

(comment
  (part1)
  ;; 3789
  #_[])

;; I know I need a modulo inverse function, I found these here
;; https://rosettacode.org/wiki/Modular_inverse#Clojure
;;
(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn mul_inv
  " Get inverse using extended gcd.  Extended GCD returns
    gcd followed by bezout coefficients. We want the 1st coefficients
   (i.e. second of extend-gcd result).  We compute mod base so result
    is between 0..(base-1) "
  [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (extended-gcd a b)]
    (if (= (first egcd) 1)
      (mod (second egcd) b)
      (str "No inverse since gcd is: " (first egcd)))))

;; And now for part 2, transform problem into a use of the
;; Chinese Remainder Theorem
;; Here's a decent description of how to construct a solution
;; to a "simultaneous linear congruences with coprime moduli"
;; All our moduli are prime so we're good.

(defn part2
  []
  (let [N (apply * periods)]
    (mod
      (->> (mapv #(vector (mod (- %1) %2) (mul_inv (/ N %2) %2) (/ N %2)) positions periods)
           (mapv #(apply * %))
           (reduce +))
      N)))

(comment
  (part2)
  ;; 667437230788118
  #_[])
