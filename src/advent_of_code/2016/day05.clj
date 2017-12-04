(ns advent-of-code.2016.day05
  (:require [clojure.string :as cs])
  (:import (java.security MessageDigest)))

;; from my Advent of Code 2015 solutions
(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(def puzzle "uqwqemis")

(defn pwd-char
  [s]
  (when (cs/starts-with? s "00000")
    (nth s 5)))

(defn pwd-seq
  []
  (->> (range)
       (map #(md5 (str puzzle %)))
       (map pwd-char)
       (filter identity)))

(comment
  (apply str (take 8 (pwd-seq)))
  ;;=> "1a3099aa"
  )

(defn add-char
  [code p c]
  (if (and (not (contains? code p))
           (<= (int \0) (int p) (int \7)))
    (assoc code p c)
    code))

(defn pos-char
  [s]
  (when (cs/starts-with? s "00000")
    [(nth s 5) (nth s 6)]))

(defn pc-seq
  []
  (->> (range)
       (map #(md5 (str puzzle %)))
       (map pos-char)
       (filter identity)))

(comment
  (->> (reduce (fn [code pc]
                 (let [newcode (apply add-char code pc)]
                   (if (= 8 (count code))
                     (reduced newcode)
                     newcode)))
               {}
               (pc-seq))
       (#(map (partial get %) [\0 \1 \2 \3 \4 \5 \6 \7]))
       (apply str))
  ;; => "694190cd"
  )
