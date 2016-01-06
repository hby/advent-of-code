(ns advent-of-code.day12
  (:require [clojure.data.json :as json]
            [clojure.walk :as w]))

(def puzzle (json/read-str (slurp "resources/input/day12.txt")))

(defn n-numbers
  "return sum of all numbers in tree of data"
  [data]
  (let [counter (atom 0)
        _ (w/postwalk #(if (number? %)
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
  "return puzzle tree with structures removed that have a red value"
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
