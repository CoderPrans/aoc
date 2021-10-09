(ns puzzle01
  (:require [clojure.java.io :as io]))

(def demo-input [1721
            979
            366
            299
            675
            1456])

(def input
  (map #(Long/parseLong %)
     (line-seq (io/reader (io/resource "2020/puzzle01_input.txt")))))

(first
 (for [x input
      y input
      :when (= 2020 (+ x y))]
  (* x y)))
;; => 73371

; Part two
(first
 (for [x input
       y input
       z input
      :when (= 2020 (+ x y z))]
  (* x y z)))
;; => 127642310
