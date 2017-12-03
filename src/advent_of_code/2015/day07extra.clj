(ns advent-of-code.2015.day07extra
  (:require [advent-of-code.day07 :refer :all]))

;;
;; I felt like exploring the tree of operations in the Day 7 puzzle.
;;

(defn memoize-count
  [cache-atom f]
  (let [mem cache-atom]
    (fn [& args]
      (if-let [[a [r c]] (find @mem args)]
        (do
          (swap! mem assoc args [r (inc c)])
          r)
        (let [ret (apply f args)]
          (swap! mem assoc args [ret 1])
          ret)))))

(def n-ops-cache (atom {}))

(def n-operations
  "number of operations needed if done naively, without memoization"
  (memoize-count n-ops-cache
    (fn [net out]
      (let [[op l r] (net out)]
        (+ (if op 1N 0N)
           (if (string? l) (n-operations net l) 0N)
           (if (string? r) (n-operations net r) 0N))))))

(comment
  (n-operations network "a")
  ;=> 14495260982518724079344N
  (n-operations network "b")
  ;=> 0N
  (n-operations new-network "a")
  ;=> 14495260982518724079344N
  (n-operations new-network "b")
  ;=> 0N
  )

(comment
  (sort (map #(vector (second (first %)) (second %)) @n-ops-cache)))

(def depth
  (memoize
    (fn [net out]
      (let [[op l r] (net out)]
        (+ (if (or (string? l) (string? r)) 1 0)
           (max
             (if (string? l) (depth net l) 0)
             (if (string? r) (depth net r) 0)))))))

(defn avg
  [coll]
  (/ (reduce + coll) (count coll)))

(comment
  (depth network "a")
  ;=> 208
  (depth network "b")
  ;=> 0
  (-> (map (partial depth network) (keys network))
      avg
      float)
  ;=> 102.9646
  )


(def branch-factor
  (memoize
    (fn [net out]
      (let [[op l r] (net out)
            lb (if (string? l) 1N 0N)
            rb (if (string? r) 1N 0N)
            n (if (or (string? l) (string? r)) 1N 0N)]
        (mapv +
              [n (+ lb rb)]
              (mapv +
                    (if (string? l) (branch-factor net l) [0N 0N])
                    (if (string? r) (branch-factor net r) [0N 0N])))))))

(defn bfvalue
  [[n b]]
  (if (zero? n)
    0
    (/ b n)))

(comment
  (-> (branch-factor network "a")
      bfvalue
      float)
  ;=> 1.4230769
  (-> (map #(bfvalue (branch-factor network %)) (keys network))
      avg
      float)
  ;=> 1.4068846
  )

(def expand-1
  (fn [net out]
    (let [[op l r] (net out)]
      (println out)
      (cond
        (string? r) (expand-1 net r)
        (string? l) (expand-1 net l)
        :else (println (or l r))))))

(comment
  (expand-1 network "a"))
