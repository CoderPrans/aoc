(ns puzzle03
  (:require [clojure.java.io :as io]))

(def demo-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn mull [in]
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" in)
       (map rest)
       (map #(let [[n1 n2] %]
               (* (parse-long n1)
                  (parse-long n2))))))

(defn part-1 [in]
  (->> (mull in)
       (apply +)))

(part-1 demo-input)
;; => 161

(def input
  (slurp (io/reader (io/resource "2024/input03.txt"))))

(part-1 input)
;; => 181345830

;; Part 2

(def demo-input2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn part-2 [in]
  (:sum (reduce
       #(let [sum (:sum %1)
              stat (:status %1)
              com (nth %2 0)
              [x y] (rest %2)]
          (cond
            (or (= com "do()")
                (= com "don't()"))  (assoc %1 :status com)
            
            :else  (if (= "do()" stat)
                     (assoc %1 :sum
                            (+ sum (* (parse-long x)
                                      (parse-long y))))
                     %1)))
       {:sum 0 :status "do()"}
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|don't\(\)|do\(\)" in))))

(part-2 demo-input2)
;; => 48

(part-2 input)
;; => 98729041
