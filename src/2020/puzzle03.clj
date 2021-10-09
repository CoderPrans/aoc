(ns puzzle03
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(def demo-input "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def parse-demo (str/split demo-input #"\n"))

(defn tree? [v x y]
  (= \# (nth (nth v y) (mod x (count (first v))))))

(defn path [v n1 n2]
  (loop [x n1 y n2 p []]
    (if (< y (dec (count v)))
      (recur (+ x n1) (+ y n2) (conj p (vector x y)))
      (conj p (vector x y)))))

(count (filter
        #(let [[x y] %]
           (tree? parse-demo x y))
 (path parse-demo 3 1)))
;; => 7

(def input (line-seq (io/reader (io/resource "2020/puzzle03_input.txt"))))

(count (filter
        #(let [[x y] %]
           (tree? input x y))
 (path input 3 1)))
;; => 218

;; Part 2
(reduce
 * (map
    (fn [s]
      (count
       (filter #(let [[x y] %] (tree? parse-demo x y))
        (path parse-demo (first s) (last s)))))
    [[1 1] [3 1] [5 1] [7 1] [1 2]]))
;; => 336

(reduce
 * (map
    (fn [s]
      (count
       (filter #(let [[x y] %] (tree? input x y))
        (path input (first s) (last s)))))
    [[1 1] [3 1] [5 1] [7 1] [1 2]]))
;; => 3847183340

