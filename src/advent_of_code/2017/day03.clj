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

; Store value of coord in map
; grid: {[x1 y1] value}

(def start-grid
  {[0 0] 1})

(defn n-coords
  [c]
  [(-> c r)
   (-> c r u)
   (-> c u)
   (-> c u l)
   (-> c l)
   (-> c l d)
   (-> c d)
   (-> c d r)])

(defn add-neighboors
  [grid coord]
  (let [cs (n-coords coord)
        ns (filter identity (map grid cs))]
    (apply + ns)))

(defn two
  [puzzle]
  (reduce (fn [g c]
            (let [ng (assoc g c (add-neighboors g c))]
              (if (> (ng c) puzzle)
                (reduced (ng c))
                ng)))
          start-grid
          (drop 1 coord-seq)))

(comment
  (two puzzle)
  ; => 349975
  )
