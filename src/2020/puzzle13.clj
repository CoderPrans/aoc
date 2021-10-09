(ns puzzle13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "939
7,13,x,x,59,x,31,19")

(defn parse-in [inp]
  (let [[ts & bid]
        (map
         read-string
         (str/split
          (str/replace
           inp #"[,x]+" "\n")
          #"\n"))]
    (for [i bid]
      [ts i])))

(parse-in demo-input)

(defn time-left [ts bid]
  [(- bid (mod ts bid)) bid])

(defn part1 [inp]
  (let [diffs (into
               {}
               (map
                #(apply time-left %)
                (parse-in inp)))
        earl (apply min (keys diffs))
        res [earl (get diffs earl)]]
    (apply * res)))

(part1 demo-input)
;; => 295

(def input (slurp (io/resource "2020/puzzle13_input.txt")))

(part1 input)
;; => 2045
