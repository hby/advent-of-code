(ns advent-of-code.day04
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

;; From https://gist.github.com/jizhang/4325757
(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(def puzzle "yzbqklnj")

(defn advent-coins
  [puzzle prefix]
  (filter #(.startsWith (first %) prefix)
          (map #(vector (md5 (str puzzle %)) %) (map inc (range)))))

(comment
  (take 1 (advent-coins puzzle "00000"))
  ;; (["000002c655df7738246e88f6c1c43eb7" 282749])
  (take 1 (advent-coins puzzle "000000"))
  ;; (["0000004b347bf4b398b3f62ace7cd301" 9962624])
  )
