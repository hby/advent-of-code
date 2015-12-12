(ns advent-of-code.day12
  (:require [clojure.data.json :as json]
            [clojure.walk :as w]))

(def puzzle (json/read-str (slurp "resources/input/day12.txt")))

(defn n-numbers
  [data]
  (let [counter (atom 0)
        walkit (w/postwalk #(if (number? %)
                             (do
                               (swap! counter + %)
                               %)
                             %)
                           data)]
    @counter))

(comment
  (n-numbers puzzle)
  ;=> 119433
  )

(def non-red-puzzle
  (w/prewalk (fn [form]
               (if (and
                     (map? form)
                     (some #{"red"} (vals form)))
                 {}
                 form))
             puzzle))

(comment
  (n-numbers non-red-puzzle)
  ;=> 68466
  )
