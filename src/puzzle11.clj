(ns puzzle11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn parse-input [i]
  (map #(apply list (str/split % #"")) (str/split i #"\n")))

(defn seat-status [r c input] 
  (nth (nth input r) c))

(seat-status 0 0 (parse-input demo-input))
;; => "L"

; n ne e es s sw w nw
(defn adjacent-positions [r c input]
  (map #(when % (apply seat-status (conj % input)))
       (let [rw (dec (count input))
             cl (dec (count (first input)))
             n (when (not= r 0) [(dec r) c])
             ne (when (and (not= r 0) (not= c cl))
                  [(dec r) (inc c)])
             e (when (not= c cl) [r (inc c)])
             es (when (and (not= c cl) (not= r rw))
                  [(inc r) (inc c)])
             s (when (not= r rw) [(inc r) c])
             sw (when (and (not= r rw) (not= c 0))
                  [(inc r) (dec c)])
             w (when (not= c 0) [r (dec c)])
             nw (when (and (not= r 0) (not= c 0))
                  [(dec r) (dec c)])]
         [n ne e es s sw w nw])))

(defn occupied [g]
  (count (filter #(= % "#") g)))

(defn next-status [s a]
  (cond
    (and (= s "L")
         (not ((set a) "#"))) "#"
    (and (= s "#")
         (>= (occupied a) 4)) "L"
    :else s))

(defn next-graph [input]
  (let [rm (count input)
        cm (count (first input))]
    (map (fn [r]
           (map (fn [c]
                  (next-status
                   (seat-status r c input)
                   (adjacent-positions
                    r c input)))
                (range cm)))
         (range rm))))

(defn final-graph [input]
  (loop [graph input
         graph' (next-graph graph)]
    (if (= graph' graph)
      graph
      (recur graph' (next-graph graph')))))

(occupied (flatten (final-graph (parse-input demo-input))))
;; => 37

(def input (slurp (io/resource "puzzle11_input.txt")))

(time (occupied (flatten (final-graph (parse-input input)))))
;; => 2178
