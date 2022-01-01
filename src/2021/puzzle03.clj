(ns puzzle03
  (:require [clojure.java.io :as io]))

(def demo-input ["00100"
                 "11110"
                 "10110"
                 "10111"
                 "10101"
                 "01111"
                 "00111"
                 "11100"
                 "10000"
                 "11001"
                 "00010"
                 "01010"])


(defn get-freqs [input index]
  (frequencies (map #(nth %1 index) input)))

(defn sig-bit [fun freqs]
  (apply str
         (map
          (fn [f] (if (fun (f \0) (f \1)) \0 \1))
          freqs)))

(defn gamma-beta [input]
  (let [freqs (map #(get-freqs input %1)
                   (range 0 (count (input 0))))
        gamma (sig-bit > freqs)
        beta (sig-bit < freqs)
        ]
    (* (Integer/parseInt gamma 2)
       (Integer/parseInt beta 2))
    )
  )

(gamma-beta demo-input)
;; => 198

(def input 
  (vec (line-seq (io/reader (io/resource "2021/input03.txt")))))

(gamma-beta input)
;; => 3277364

;;; PART 2

(defn get-oxy-bit [freqs]
  (cond
    (< (freqs \0) (freqs \1)) \1
    (> (freqs \0) (freqs \1)) \0
    (= (freqs \0) (freqs \1)) \1))

(defn get-co2-bit [freqs]
  (cond
    (> (freqs \0) (freqs \1)) \1
    (< (freqs \0) (freqs \1)) \0
    (= (freqs \0) (freqs \1)) \0))

(defn get-num [kw input]
  (let [bit-fn (case kw
                 "co2" get-co2-bit
                 "oxy" get-oxy-bit)]
    (reduce
     (fn [a c]
       (if (> (count a) 1)
         (let [freqs (get-freqs a c)
               my-bit (bit-fn freqs)]
           (filter (fn [s] (= my-bit (nth s c))) a)
           )
         a))
     input
     (range 0 (count (input 0)))
     )))

(defn part-2 [input]
  (let [oxy (first (get-num "oxy" input))
        co2 (first (get-num "co2" input))]
    (* (Integer/parseInt oxy 2)
       (Integer/parseInt co2 2))))

(part-2 demo-input)
;; => 230

(part-2 input)
;; => 5736383
