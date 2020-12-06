(ns puzzle06
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def demo-input "abc

a
b
c

ab
ac

a
a
a
a

b
")

;; (defn uniq-trus [demo-input]
;;   (count
;;    (flatten
;;     (map
;;      #(filter
;;        (fn [c] (not= c "\n"))
;;        (set (str/split % #"")))
;;      (str/split demo-input #"\R\R")))))

;; (uniq-trus demo-input)
;; ;; => 11

(defn all-yes [in]
  (reduce
   +
   (map
    #(count (set (str/replace % "\n" "")))
    (str/split in #"\R\R"))))

(all-yes demo-input)
;; => 11

(def input (slurp (io/resource "puzzle06_input.txt")))

(all-yes input)
;; => 6799

; Part 2

(defn all-yes2 [in]
  (reduce
   +
   (map
    (fn [group]
      (count
       (apply set/intersection
              (map set (str/split-lines group)))))
    (str/split in #"\R\R"))))

(all-yes2 demo-input)
;; => 6

(all-yes2 input)
;; => 3354

