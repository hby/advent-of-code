(ns advent-of-code.day19
  (:require [clojure.string :as s]))

;(require '[com.inferstructure.repl :as rh])

(def replacements-text (slurp "resources/input/day19.txt"))

(def pattern #"(\w+) => (\w+)")

(defn symbolize
  "Turn s into a vector of strings made by splitting s
   so that its parts are either a single upper case char
   or an upper case followed by a lower case char."
  [s]
  (mapv str
        (reduce (fn [r c]
                  (if (<= (int \A) (int c) (int \Z))
                    (conj r c)
                    (assoc-in r [(dec (count r))] (str (r (dec (count r))) c))))
                []
                s)))

(def replacements
  (apply merge-with (fn [vv [v]] (conj vv v))
         (->> replacements-text
              (s/split-lines)
              (map #(re-matches pattern %))
              (map #(drop 1 %))
              (map #(apply vector %))
              (map (fn [[k v]] [k [(symbolize v)]]))
              (map #(into {} (vector %))))))

(def medicine-str "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl")

(def medicine (symbolize medicine-str))

(defn replace-once
  [rules v]
  (mapv flatten
        (apply concat
               (map-indexed (fn [i sym]
                              (if-let [subs (rules sym)]
                                (mapv (fn [sub] (assoc-in v [i] sub)) subs)))
                            v))))

;; Part 1
(comment
  (count
    (into #{} (replace-once replacements medicine)))
  ;=> 518
  )
