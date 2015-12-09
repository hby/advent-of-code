(ns advent-of-code.day09
  (:require [clojure.math.combinatorics :as combo]))

(def graph
  {
   :Faerun {:Tristram 65 :Tambi 129 :Norrath 144 :Snowdin 71 :Straylight 137 :AlphaCentauri 3 :Arbre 149}
   :Tristram {:Tambi 63 :Norrath 4 :Snowdin 105 :Straylight 125 :AlphaCentauri 55 :Arbre 14 :Faerun 65}
   :Tambi {:Norrath 68 :Snowdin 52 :Straylight 65 :AlphaCentauri 22 :Arbre 143 :Faerun 129 :Tristram 63}
   :Norrath {:Snowdin 8 :Straylight 23 :AlphaCentauri 136 :Arbre 115 :Faerun 144 :Tristram 4 :Tambi 68}
   :Snowdin {:Straylight 101 :AlphaCentauri 84 :Arbre 96 :Faerun 71 :Tristram 105 :Tambi 52 :Norrath 8}
   :Straylight {:AlphaCentauri 107 :Arbre 14 :Faerun 137 :Tristram 125 :Tambi 65 :Norrath 23 :Snowdin 101}
   :AlphaCentauri {:Arbre 46 :Faerun 3 :Tristram 55 :Tambi 22 :Norrath 136 :Snowdin 84 :Straylight 107}
   :Arbre {:Faerun 149 :Tristram 14 :Tambi 143 :Norrath 115 :Snowdin 96 :Straylight 14 :AlphaCentauri 46}
   })

;; Brute

(defn cost
  ([graph c1 c2]
   ((graph c1) c2))
  ([graph cities]
   (reduce +
           0
           (map #(cost graph (first %) (second %))
                (partition 2 1 cities)))))

(def cities (keys graph))

(comment
  (apply min (map #(cost graph %) (combo/permutations cities)))
  ;=> 117
  (apply max (map #(cost graph %) (combo/permutations cities)))
  ;=> 909
  )
