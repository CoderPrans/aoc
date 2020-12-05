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

(defn dig [c s]
  (let [[n1 n2] s]
    (if (or (= c \L) (= c \F))
      [n1 (+ n1 (floor-int (/ (- n2 n1) 2)))]
      [(- n2 (floor-int (/ (- n2 n1) 2))) n2])))

(defn get-n [s r]
  (loop [c 0
         r r]
    (if (< c (count s))
      (recur (inc c) (dig (nth s c) r))
      (first r))))

(defn get-seat [s]
  (let [rs (first (str/split s #"[R|L]"))
        cs (last (str/split s #"[F|B]"))
        tr [0 127]
        tc [0 7]]
    (+ (* 8 (get-n rs tr)) (get-n cs tc))))

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


