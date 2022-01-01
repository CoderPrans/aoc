(ns puzzle02
  (:require [clojure.java.io :as io]))

(def demo-input ["forward 5"
                 "down 5"
                 "forward 8"
                 "up 3"
                 "down 8"
                 "forward 2"])

(defn parse-steps [s]
  (let [[_ dirn steps] (re-find #"(\w+) (\d+)" s)]
    [dirn (Long/parseLong steps)]))

(map parse-steps demo-input)
;; => (["forward" 5] ["down" 5] ["forward" 8] ["up" 3] ["down" 8] ["forward" 2])

; [<hor> <dep>]
(defn get-pos [input]
  (let [parsed (map parse-steps input)]
    (reduce
     #(let [[hor dep] %1
            [dirn step] %2]
        (case dirn
          "forward" [(+ hor step) dep]
          "up" [hor (- dep step)]
          "down" [hor (+ dep step)]))
     [0 0]
     parsed)))

(get-pos demo-input)
;; => [15 10]

(apply * (get-pos demo-input))
;; => 150

(def input (line-seq (io/reader (io/resource "2021/input02.txt"))))

(get-pos input)
;; => [2033 750]

(apply * (get-pos input))
;; => 1524750

;;; PART 2

; [<hor> <dep> <aim>]
(defn get-pos-aim [input]
  (let [parsed (map parse-steps input)]
    (reduce
     #(let [[hor dep aim] %1
            [dirn step] %2]
        (case dirn
          "forward" [(+ hor step) (+ dep (* aim step)) aim]
          "up" [hor dep (- aim step)]
          "down" [hor dep (+ aim step)]))
     [0 0 0]
     parsed)))

(get-pos-aim demo-input)
;; => [15 60 10]

(let [[x y] (get-pos-aim demo-input)]
  (* x y))
;; => 900

(get-pos-aim input)
;; => [2033 783289 750]

(let [[x y] (get-pos-aim input)]
  (* x y))
;; => 1592426537


