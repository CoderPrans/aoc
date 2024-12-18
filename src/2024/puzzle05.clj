(ns puzzle05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input
  [
   "47|53"
   "97|13"
   "97|61"
   "97|47"
   "75|29"
   "61|13"
   "75|53"
   "29|13"
   "97|29"
   "53|29"
   "61|53"
   "97|53"
   "61|29"
   "47|13"
   "75|47"
   "97|75"
   "47|61"
   "75|61"
   "47|29"
   "75|13"
   "53|13"
   ""
   "75,47,61,53,29"
   "97,61,53,29,13"
   "75,29,13"
   "75,97,47,61,53"
   "61,13,29"
   "97,13,75,29,47"
   ])

(defn correct? [rules nums]
  (reduce
   (fn [{:keys [rule-p correct ord] :as a} c]
     (let [rule-c (filter #(= c (first %))
                          rules)]
       (if (and (not= correct false)
                (or (nil? correct)
                    (some #{c} (map #(second %) rule-p))))
         (assoc a :rule-p rule-c
                :correct true
                :ord (conj ord (Integer/parseInt c)))
         (assoc a :correct false
                :ord nums))))
   {:rule-p [] :correct nil :ord []}
   nums))

(defn mid-val [nums]
  (let [mid (quot (count nums) 2)]
                  (nth nums mid)))

(defn part-1 [in]
  (let [break (.indexOf in "")
        rules (map #(str/split % #"\|") (take break in))
        pages (map #(str/split % #",") (drop (inc break) in))]

    (->> (map #(correct? rules %) pages)
         (filter (fn [{correct :correct}] correct))
         (map (fn [{ord :ord}] (mid-val ord)))
         (apply +))
    ))

(part-1 demo-input)
;; => 143

(def input
  (line-seq (io/reader (io/resource "2024/input05.txt"))))

(part-1 input)
;; => 5064

;; Part 2

(defn sort-by-rules [nums rules]
  (sort
   (comparator
    (fn [x y]
      (let [rule-x (filter
                    #(= x (first %)) rules)]
        (some #{y} (map #(second %) rule-x)))))
   nums))

(defn part-2 [in]
  (let [break (.indexOf in "")
        rules (map #(str/split % #"\|") (take break in))
        pages (map #(str/split % #",") (drop (inc break) in))]

    (->> (map #(correct? rules %) pages)
         (filter (fn [{correct :correct}] (not correct)))
         (map (fn [{ord :ord}] ord))
         (map (fn [n] (sort-by-rules n rules)))
         (map mid-val)
         (map (fn [n] (Integer/parseInt n)))
         (apply +))
    ))

(part-2 demo-input)
;; => 123
;; => (("97" "75" "47" "61" "53")
;;     ("61" "29" "13")
;;     ("97" "75" "47" "29" "13"))

(part-2 input)
;; => 5152
