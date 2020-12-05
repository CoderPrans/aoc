(ns puzzle05
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(def rows 128)
(def cols 8)

(def demo-input "BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL")

(defn floor-int [n]
  (int (Math/floor n)))

(defn dig [char seq]
  (let [[n1 n2] seq]
    (if (or (= char \L) (= char \F))
      [n1 (+ n1 (floor-int (/ (- n2 n1) 2)))]
      [(- n2 (floor-int (/ (- n2 n1) 2))) n2])))

(defn get-n [str init]
  (loop [c 0
         i init]
    (if (< c (count str))
      (recur (inc c) (dig (nth str c) i))
      (first i))))

(defn get-seat [str]
  (let [[rs cs] (partition-all 7 str)
        ri [0 127]
        ci [0 7]]
    (+ (* 8 (get-n rs ri)) (get-n cs ci))))

(get-seat "BBFFBBFRLL")

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


