(ns puzzle09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "16
10
15
5
1
11
7
19
6
12
4")

(defn parse-input [i]
  (let [in-vec (map
                read-string
                (str/split i #"\n"))]
    (conj in-vec
          (+ 3 (apply max in-vec)) 0)))

(defn diff-groups [i]
  (group-by
   #(- (last %) (first %))
   (partition 2 1 (sort i))))

(map count
     (vals (diff-groups
            (parse-input demo-input))))
;; => (7 5)

(defn part1 [diffs]
  (* (count (get diffs 3))
     (count (get diffs 1))))

(part1
 (diff-groups (parse-input demo-input)))
;; => 35

(def input
  (slurp (io/resource "puzzle10_input.txt")))

(part1
 (diff-groups (parse-input input)))
;; => 2201

; Part 2

;; (let [x (parse-input demo-input)]
;;   (group-by #(= )))

