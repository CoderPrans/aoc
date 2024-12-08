(ns puzzle02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input '("7 6 4 2 1"
                  "1 2 7 8 9"
                  "9 7 6 2 1"
                  "1 3 2 4 5"
                  "8 6 4 4 1"
                  "1 3 6 7 9"))

(defn parse-input [input]
  (map #(->> (str/split % #" ")
             (map (fn [n] (Long/parseLong n))))
       input))

(defn get-consec-diffs [input]
  (map #(->> (partition 2 1 %)
             (map (fn [[x y]] (- x y))))
       input))

(defn unidirectional? [nums]
  (or (every? #(< % 0) nums)
      (every? #(> % 0) nums)))

(defn under3? [nums]
  (not (some #(or (> % 3) (< % -3)) nums)))

(defn safe? [input]
  (->> (get-consec-diffs input)
       (filter unidirectional?)
       (filter under3?)
       count))

(->> demo-input
     parse-input
     safe?)

(def input
  (line-seq (io/reader (io/resource "2024/input02.txt"))))

(->> input
     parse-input
     safe?)
;; => 660

;; Part 2

(defn omit-one [row]
  (map (fn [i] (let [[f s] (split-at i row)]
                 (concat (butlast f) s)))
       (range (inc (count row)))))

(->> demo-input
     parse-input
     (map omit-one)
     (map safe?)
     (filter #(> % 0))
     count)

(->> input
     parse-input
     (map omit-one)
     (map safe?)
     (filter #(> % 0))
     count)
;; => 689

