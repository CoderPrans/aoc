(ns puzzle12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "F10
N3
F7
R90
F11")

(defn parse-in [i]
  (map
   #(let [[_ ins mag]
          (re-find #"([A-Z])(\d+)" %)]
      [ins (read-string mag)])
   (str/split i #"\n")))

;; R => n 0 e 90 s 180 w 270 
;; L => n 0 w -90 s -180 e -270

(defn parse-ang [num]
  (if (>= (Math/abs num) 360)
    (if (< num 0)
      (* -1 (mod (Math/abs num) 360))
      (mod (Math/abs num) 360))
    num))

(defn turn-to [curr ins]
  (let [[dir deg] ins
        deg' (parse-ang deg)]
    (case dir
      "R" (parse-ang (+ curr deg))
      "L" (parse-ang (- curr deg))
      nil)))

(defn move-to [x y ins]
  (let [[dir mag] ins]
    (case dir
      "N" [x (+ y mag)]
      "E" [(+ x mag) y]
      "S" [x (- y mag)]
      "W" [(- x mag) y])))

(defn move-forward [coor ins]
  (let [[deg x y] coor
        [_ mag] ins]
    (cond
      (zero? deg)
      [deg x (+ y mag)]
      (or (= deg -270) (= deg 90))
      [deg (+ x mag) y]
      (or (= deg -180) (= deg 180))
      [deg x (- y mag)]
      (or (= deg -90) (= deg 270))
      [deg (- x mag) y])))

(defn next-coords [coor ins]
  (let [[deg x y] coor
        [dir mag] ins]
    (cond
      (#{"N" "E" "S" "W"} dir)
      (cons deg (move-to x y ins))
      (#{"R" "L"} dir)
      (cons (turn-to deg ins) [x y])
      :else (move-forward coor ins))))

(reduce
 (fn [acc curr]
   (next-coords acc curr))
 [90 0 0]
 (parse-in demo-input))
;; => [180 17 -8]

(defn distance [inp]
  (let [[_ dx dy]
        (reduce
         (fn [acc curr]
           (next-coords acc curr))
         [90 0 0]
         (parse-in inp))]
    (+ (Math/abs dx) (Math/abs dy))))

(distance demo-input)
;; => 25

(def input (slurp (io/resource "2020/puzzle12_input.txt")))

(distance input)
;; => 1424
