(ns puzzle04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input [
"MMMSXXMASM"
"MSAMXMSMSA"
"AMXSXMAAMM"
"MSAMASMSMX"
"XMASAMXAMM"
"XXAMMXXAMA"
"SMSMSASXSS"
"SAXAMASAAA"
"MAMMMXMMMM"
"MXMXAXMASX"
])

(defn get-char
  [board {x :x y :y}]
  (let [xl (dec (count board))]
    (when (<= 0 x xl)
      (let [yl (dec (count (nth board x)))]
        (when (<= 0 y yl)
          (nth (nth board x) y)))
      )))

(get-char demo-input {:x 10 :y 9})
;; => nil

;; nw  n   ne
;; w   .   e
;; sw  s   se
(defn check-d [[x y] i]
  {:nw {:x (- x i) :y (- y i)}
   :n  {:x (- x i) :y y}
   :ne {:x (- x i) :y (+ y i)}
   :e  {:x x :y (+ y i)}
   :se {:x (+ x i) :y (+ y i)}
   :s  {:x (+ x i) :y y}
   :sw {:x (+ x i) :y (- y i)}
   :w  {:x x :y (- y i)}
   })

(def dirs
  (keys (check-d [0 0] 0)))

dirs
;; => (:nw :n :ne :e :se :s :sw :w)

(defn check-for
  [board {x :x y :y} d]
  (let [w "XMAS"]
    (= w (reduce
          (fn [a i]
            (str a (get-char board ((check-d [x y] i) d))))
          ""
          (range (count w))))
    ))

(check-for demo-input {:x 0 :y 5} :e)

(defn apply-check [d in]
  (mapcat
   (fn [x]
     (let [match (filter
                  (fn [y]
                    (check-for in {:x x :y y} d))
                  (range (count (nth in x))))]
       (when (> (count match) 0)
         (for [m match]
           {:x x
            :y m}))))
   (range (count in)))
  )

(apply-check :e demo-input)
;; => ({:x 0, :y 5} {:x 4, :y 0} {:x 9, :y 5})

(apply-check :nw demo-input)
;; => ({:x 5, :y 6} {:x 9, :y 3} {:x 9, :y 5} {:x 9, :y 9})

(defn part-1 [in]
  (apply
   +
   (map (fn [m] (count (first (vals m))))
        (map
         (fn [d] {d (apply-check d in)})
         dirs))))

(part-1 demo-input)
;; => 18
;; => (4 2 4 3 1 1 1 2)
;; => ({:nw ({:x 5, :y 6} {:x 9, :y 3} {:x 9, :y 5} {:x 9, :y 9})}
;;     {:n ({:x 4, :y 6} {:x 9, :y 9})}
;;     {:ne ({:x 5, :y 0} {:x 9, :y 1} {:x 9, :y 3} {:x 9, :y 5})}
;;     {:e ({:x 0, :y 5} {:x 4, :y 0} {:x 9, :y 5})}
;;     {:se ({:x 0, :y 4})}
;;     {:s ({:x 3, :y 9})}
;;     {:sw ({:x 3, :y 9})}
;;     {:w ({:x 1, :y 4} {:x 4, :y 6})})

(def input
  (line-seq (io/reader (io/resource "2024/input04.txt"))))

(part-1 input)
;; => 2642

(defn check-mas [board {x :x y :y :as coord}]
 
  (= "AMS"
     (apply str (sort (map #(get-char board %)
                           [{:x (- x 1) :y (- y 1)}
                            coord
                            {:x (+ x 1) :y (+ y 1)}])))
     (apply str (sort (map #(get-char board %)
                           [{:x (- x 1) :y (+ y 1)}
                            coord
                            {:x (+ x 1) :y (- y 1)}])))))

(defn check-for2
  [board coord]
  (when (= \A (get-char board coord))
    (check-mas board coord))
  )

(check-for2 demo-input {:x 1 :y 2})

(defn apply-check2 [in]
  (mapcat
   (fn [x]
     (let [match (filter
                  (fn [y]
                    (check-for2 in {:x x :y y}))
                  (range (count (nth in x))))]
       (when (> (count match) 0)
         (for [m match]
           {:x x
            :y m}))))
   (range (count in)))
  )

(defn part-2 [in]
  (count (apply-check2 in)))

(apply-check2 demo-input)
;; => ({:x 1, :y 2}
;;     {:x 2, :y 6}
;;     {:x 2, :y 7}
;;     {:x 3, :y 2}
;;     {:x 3, :y 4}
;;     {:x 7, :y 1}
;;     {:x 7, :y 3}
;;     {:x 7, :y 5}
;;     {:x 7, :y 7})

(part-2 demo-input)
;; => 9

(part-2 input)
;; => 1974
