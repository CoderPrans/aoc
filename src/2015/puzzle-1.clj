(ns puzzle-1
  (:require [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "2015/resource-1.txt"))))
(def instrn (map (fn [l] l) (nth input 0)))

                                        ; part 1

(reduce
 (fn [a c] ((if (= c \() inc dec) a))
 0 instrn)
;; => 280
                                        ; part 2
(let [context
      (reduce
       (fn [a c]
         (list
          ;; curr floor
          ((if (= c \() inc dec) (nth a 0))
          ;; curr instrn number
          (inc (nth a 1))
          ;; curr list of instrn no.s that lead to basement 
          (if (= (nth a 0) -1) (conj (nth a 2) (nth a 1)) (nth a 2)))
         )
       '(0 0 []) instrn)
      ;; => (280 7000 [1797 6101 6103 6105 6107 6109 6111])
      ]

  ((nth context 2) 0))
;; => 1797








