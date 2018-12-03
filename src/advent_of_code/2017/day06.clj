(ns advent-of-code.2017.day06)

(def puzzle [10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6])

(defn first-max-i
  [state]
  (reduce (fn [[m mi] [b bi]]
            (if (> b m)
              [b bi]
              [m mi]))
          [-1 -1]
          (map-indexed (fn [i e] [e i]) state)))

(defn distribute
  [state n i]
  (let [c (count state)
        add-all (quot n c)
        n-incs (rem n c)
        inc-is (into #{} (map #(mod % c) (range (inc i) (+ i n-incs 1))))]
    (mapv (fn [[v mi]]
            (cond
              (= mi i) add-all
              (inc-is mi) (+ v add-all 1)
              :else (+ v add-all)))
          (map-indexed (fn [i v] [v i]) state))))

(defn bank-seq
  [puzzle]
  (iterate #(apply distribute % (first-max-i %)) puzzle))

(defn one
  [puzzle]
  (reduce (fn [seen [s i]]
            (if (seen s)
              (reduced [s i])
              (conj seen s)))
          #{}
          (map-indexed (fn [i v] [v i]) (bank-seq puzzle))))

(comment
  (second (one puzzle)))
  ; => 14029


(defn two
  [puzzle]
  (let [[s i2] (one puzzle)
        [_ i1] (first (drop-while
                        #(not= s (first %))
                        (map-indexed (fn [i v] [v i]) (bank-seq puzzle))))]
    (- i2 i1)))

(comment
  (two puzzle))
  ; => 2765))
