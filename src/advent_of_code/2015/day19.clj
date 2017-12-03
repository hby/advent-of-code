(ns advent-of-code.2015.day19
  (:require [clojure.string :as s]
            [instaparse.core :as insta]))

(def replacements-text (slurp "resources/2015/day19.txt"))

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
                    (let [idx (dec (count r))]
                      (assoc-in r [idx] (str (r idx) c)))))
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


;; Part 2 is a parsing problem as long as your parser gives you the smallest
;; parse tree for an input. The instaparse library should work.

;; This could be generated from the replacements map (which would be interesting)
;; but I wanted to get a feel for typing in the grammar.
(def grammar
  (insta/parser
    "S = H F|N Al|O Mg
     Mg = B F|Ti Mg|'Mg'
     Si = Ca Si|'Si'
     Ti = B P|Ti Ti|'Ti'
     Ca = Ca Ca|P B|P Rn F Ar|Si Rn F Y F Ar|Si Rn Mg Ar|Si Th|'Ca'
     Al = Th F|Th Rn F Ar|'Al'
     H = C Rn Al Ar|C Rn F Y F Y F Ar|C Rn F Y Mg Ar|C Rn Mg Y F Ar|H Ca|N Rn F Y F Ar|N Rn Mg Ar|N Th|O B|O Rn F Ar|'H'
     F = Ca F|P Mg|Si Al|'F'
     B = B Ca|Ti B|Ti Rn F Ar|'B'
     P = Ca P|P Ti|Si Rn F Ar|'P'
     O = C Rn F Y F Ar|C Rn Mg Ar|H P|N Rn F Ar|O Ti|'O'
     N = C Rn F Ar|H Si|'N'
     Th = Th Ca|'Th'
     C = 'C'
     Rn = 'Rn'
     Ar = 'Ar'
     Y = 'Y'
     "))

(def parse-tree (grammar medicine-str))

;; Each keyword in the parse tree represents a use of some replacement rule
;; but we have too many by the amount of terminal symbols, which are strings,
;; in the parse tree.

(def rules-and-terminals
  (group-by keyword? (flatten parse-tree)))

(comment
  (-
    ; replacememt rules
    (count (rules-and-terminals true))
    ; terminal rules
    (count (rules-and-terminals false)))
  ;=> 200
  )
