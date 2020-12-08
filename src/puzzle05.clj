(ns puzzle05
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(def demo-input "BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL")

(defn dig [char [n1 n2]]
    (if (#{\L \F} char)
      [n1 (+ n1 (quot (- n2 n1) 2))]
      [(- n2 (quot (- n2 n1) 2)) n2]))

(dig \B [30 31])
;; => [31 31]

(defn get-n [str init]
  (reduce #(dig %2 %1) init str)
  #_(loop [c 0
         i init]
    (if (< c (count str))
      (recur (inc c) (dig (nth str c) i))
      (first i))))

(get-n "FFFBBBF" [0 127])
;; => [14 14]

(get-n "RRR" [0 7])
;; => [7 7]

(defn get-seat [str]
  (let [[rs cs] (partition-all 7 str)
        ri [0 127]
        ci [0 7]]
    (+ (* 8
          (first (get-n rs ri)))
       (first (get-n cs ci)))))

(get-seat "BBFFBBFRLL")
;; => 820

(apply max (map get-seat (str/split demo-input #"\n")))
;; => 820

(def input (line-seq (io/reader (io/resource "puzzle05_input.txt"))))

(apply max (map get-seat input))
;; => 908

; Part 2

(def sorted-nums (sort (map get-seat input)))

(-
 (reduce +
         (range
          (first sorted-nums)
          (inc (last sorted-nums))))
 (reduce + sorted-nums))
;; => 619


