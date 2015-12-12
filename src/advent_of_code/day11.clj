(ns advent-of-code.day11)

(def puzzle "hepxcrrq")

(defn is-straight
  [s]
  (let [charints (map int s)]
    (every? #(= % 1)
            (map -
                 (drop 1 charints)
                 charints))))

(defn has-straight
  ([s n]
   (let [nseqs (partition n 1 s)]
     (some is-straight nseqs)))
  ([s]
   (has-straight s 3)))

(defn has-no-confusing-chars
  ([s]
   (has-no-confusing-chars s #{\i \o \l}))
  ([s charset]
   (not (some charset s))))

(defn has-two-differnt-pairs
  [s]
  (->> (partition 2 1 s)
       (filter #(apply = %))
       (distinct)
       (count)
       (< 1)))

(defn valid-pwd?
  [s]
  (and
    (has-straight s)
    (has-no-confusing-chars s)
    (has-two-differnt-pairs s)))

(defn inc-string*
  "Takes a char seq and 'increments' left to right"
  [[f & r]]
  (let [newf (if (= f \z) \a (char (inc (int f))))]
    (if (= newf \a)
      ; carry
      (concat (list newf) (if r (inc-string* r)))
      ; no carry
      (concat (list newf) r))))

(defn inc-string
  [s]
  (-> (reverse s)
      (inc-string*)
      (reverse)
      ((partial apply str))))

(defn next-pwd
  [s]
  (first (drop-while #(not (valid-pwd? %)) (drop 1 (iterate inc-string s)))))

(comment
  (next-pwd puzzle)
  ;=> "hepxxyzz"
  (next-pwd (next-pwd puzzle))
  ;=> "heqaabcc"
  )