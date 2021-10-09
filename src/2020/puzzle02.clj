(ns puzzle02
  (:require [clojure.java.io :as io]))

(def demo-input "1-3 a: abcde
                 1-3 b: cdefg
                 2-9 c: ccccccccc")

(defn parse-line [s]
  (let [[_ n1 n2 char pwd] (re-find #"(\d+)-(\d+) (.): (.*)" s)]
    [(Long/parseLong n1) (Long/parseLong n2) (first char) pwd]))

(defn entry-ok? [[n1 n2 char pwd]]
  (<= n1 (get (frequencies pwd) char 0) n2))

(def input
  (map parse-line
       (line-seq (io/reader (io/resource "2020/puzzle02_input.txt")))))

(count (filter entry-ok? input))
;; => 447

; Part 2
(defn entry-ok2? [[n1 n2 char pwd]]
  (let [ok1 (= (nth pwd (dec n1)) char)
        ok2 (= (nth pwd (dec n2)) char)]
    (not= ok1 ok2)))

(count (filter entry-ok2? input))
;; => 249
