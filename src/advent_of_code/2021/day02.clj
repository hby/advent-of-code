(ns advent-of-code.2021.day02
  (:require [clojure.string :as str]))

(def input (->> (slurp "resources/2021/day02.txt")
                (str/split-lines)
                (map #(str/split % #" "))
                (map (juxt (comp keyword first) (comp #(Integer/parseInt %) second)))))

(defn part1
  [input]
  (let [{:keys [forward down up] :or {forward 0 down 0 up 0}}
        (->> input
             (group-by first)
             (map (fn [[k v]] [k (apply + (map second v))]))
             (into {}))]
    (* forward (- down up))))

(comment
  (part1 input)
  ; => 2272262
  :end)

;; state: [aim h d]
(defmulti state+input->state (fn [_state [op _]] op))

(defmethod state+input->state :forward
  [[aim h d] [_ x]]
  [aim (+ x h) (+ d (* aim x))])

(defmethod state+input->state :up
  [[aim h d] [_ x]]
  [(- aim x) h d])

(defmethod state+input->state :down
  [[aim h d] [_ x]]
  [(+ aim x) h d])

(defn part2
  [input]
  (->> (reduce state+input->state
               [0 0 0]
               input)
       rest
       (apply *)))

(comment
  (part2 input)
  ; => 2134882034
  :end)
