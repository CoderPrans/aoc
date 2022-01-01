(ns puzzle01
  (:require [clojure.java.io :as io]))

(def demo-input [199
                 200
                 208
                 210
                 200
                 207
                 240
                 269
                 260
                 263])

;; acc can be '(<last value> <number of incs> <inc'd at>)

(defn get-incs [input]
  (let [incs (reduce
              (fn [a c]
                (if (< (first a) c)
                  (list c (inc (nth a 1)) (conj (last a) c))
                  (cons c (rest a))))
              (list (first input) 0 [])
              input)]
    (nth incs 1)))

(get-incs demo-input)
;; => 7

(def input (map #(Long/parseLong %)
                (line-seq (io/reader (io/resource "2021/input01.txt")))))

(get-incs input)
;; => 1521

;;; PART 2

(defn get-sliding [input]
  (let [indices (filter
                 (fn [x] x)
                 (for [x (range 0 (count input))]
                   (when (<= (+ x 2) (dec (count input)))
                     [x (+ x 1) (+ x 2)])))
        windows (map (fn [i] (map #(nth input %) i)) indices)]
    (map (fn [w] (reduce + w)) windows)
    )
  )

(get-sliding demo-input)
;; => (607 618 618 617 647 716 769 792)

(get-incs (get-sliding demo-input))
;; => 5

(get-incs (get-sliding input))
;; => 1543
