(ns puzzle04
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as s]))

(def demo-input [
"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
""
"22 13 17 11  0"
" 8  2 23  4 24"
"21  9 14 16  7"
" 6 10  3 18  5"
" 1 12 20 15 19"
""
" 3 15  0  2 22"
" 9 18 13 17  5"
"19  8  7 25 23"
"20 11 10 24  4"
"14 21 16 12  6"
""
"14 21 17 24  4"
"10 16 15  9 19"
"18  8 23 26 20"
"22 11 13  6  5"
" 2  0 12  3  7"
])

(def input (vec (line-seq (io/reader (io/resource "2021/input04.txt")))))

(def to-draw (-> (first input)
                 (s/split #",")))

(apply count (drop 1 (take 2 (rest input))))
;; => 14

(defn get-boards [inp]
  (->> (rest inp)
       (partition-by #(= (count %) 14))
       (filter #(= (count %) 5))
       (map #(map (fn [x] (s/split (s/trim x) #"\s+")) %))))

(first (get-boards-2 demo-input))
;; => (["22" "13" "17" "11" "0"]
;;     ["8" "2" "23" "4" "24"]
;;     ["21" "9" "14" "16" "7"]
;;     ["6" "10" "3" "18" "5"]
;;     ["1" "12" "20" "15" "19"])

