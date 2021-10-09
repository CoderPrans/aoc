(ns puzzle09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(def parsed-demo-input (map #(read-string %) (str/split demo-input #"\n")))

(defn weakness [in n]
  (nth in
       (loop [c n]
         (if ((set (for [x (drop (- c n) (take c in))
                         y (drop (- c n) (take c in))
                         :when (not= x y)]
                     (+ x y))) (nth in c))
           (recur (inc c))
           c))))

(weakness parsed-demo-input 5)
;; => 127

(def input (map #(read-string %) (line-seq (io/reader (io/resource "2020/puzzle09_input.txt")))))

(weakness input 25)
;; => 85848519

; Part 2

(defn get-res [weakness window coll]
  (first (filter #(= (reduce + %) weakness) (partition window 1 coll))))

(defn part2 [weakness input]
  (loop [w 2]
    (let [res (get-res weakness w input)]
      (if (empty? res)
        (recur (inc w))
        (+ (apply min res)
           (apply max res))))))

(part2 (weakness parsed-demo-input 5) parsed-demo-input)
;; => 62

(part2 (weakness input 25) input)
;; => 13414198
