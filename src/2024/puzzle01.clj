(ns puzzle01
  (:require [clojure.java.io :as io]))

(def demo-input '("3   4"
                  "4   3"
                  "2   5"
                  "1   3"
                  "3   9"
                  "3   3"))

(defn parse-pair [s]
  (let [[_ num1 num2] (first (re-seq #"(\d+)\s+(\d+)" s))]
    [(Long/parseLong num1) (Long/parseLong num2)]))

(defn list-out [input]
  (reduce
   #(assoc
     %1
     :list1 (conj (:list1 %1) (first %2))
     :list2 (conj (:list2 %1) (last %2)))
   {:list1 [] :list2 []}
   input))

(defn sort-lists [m]
  (let [{:keys [list1 list2]} m]
    {:list1 (sort list1)
     :list2 (sort list2)}))

(defn sum-diffs [input]
   (reduce
    +
    (map #(Math/abs (- %1 %2))
         (:list1 input) (:list2 input))))

(->> demo-input
     (map parse-pair)
     list-out
     sort-lists
     sum-diffs)
;; => 11

(def input
  (line-seq (io/reader (io/resource "2024/input01.txt"))))

(->> input
     (map parse-pair)
     list-out
     sort-lists
     sum-diffs)
;; => 2196996

;; --- part 2 ---

(def demo-list-map
  (list-out (map parse-pair demo-input)))

(def demo-freq-map
  (->> demo-list-map
       :list2
       frequencies))

(->> demo-list-map
     :list1
     (map #(* % (if-let [f (get demo-freq-map %)] f 0)))
     (reduce +))
;; => 31

(def list-map
  (list-out (map parse-pair input)))

(def freq-map
  (->> list-map
       :list2
       frequencies))

(->> list-map
     :list1
     (map #(* % (if-let [f (get freq-map %)] f 0)))
     (reduce +))
;; => 23655822

(comment
  (re-seq #"(\d)\s+(\d)" (nth input 0))
  )
