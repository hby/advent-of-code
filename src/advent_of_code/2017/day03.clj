(ns advent-of-code.2017.day03)

(def puzzle 347991)

(def nmove-seq (mapcat #(identity [% %]) (iterate inc 1)))

(defn r
  [m]
  (mapv + m [1 0]))

(defn u
  [m]
  (mapv + m [0 1]))

(defn l
  [m]
  (mapv + m [-1 0]))

(defn d
  [m]
  (mapv + m [0 -1]))

(def m-seq
  (mapcat #(repeat % %2) nmove-seq (cycle [r u l d])))

(def coord-seq
  (reductions (fn [s m]
                (m s))
              [0 0]
              m-seq))

(defn one
  [puzzle]
  (let [coord (first (drop (dec puzzle) coord-seq))]
    (apply + (map #(Math/abs %) coord))))

(comment
  (one puzzle)
  ; => 480
  )
