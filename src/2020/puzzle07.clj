(ns puzzle07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def conditions (str/split demo-input #"\n"))

(def input (line-seq (io/reader (io/resource "2020/puzzle07_input.txt"))))

(def parsed-conditions
  (map 
   (fn [[outer & rest]]
     [outer (map str/trim rest)])
   (map
    #(str/split
      (str/replace % #"no other bags.| bags contain | bag(s?)[, |.]" "")
      #"\d+")
    input)))

(def col-graph
  (reduce
   (fn [m [outer rest]]
     (reduce (fn [m col]
               (update m col conj outer)) m rest))
   {} parsed-conditions))

(defn add-valid [result graph color]
  (into result (get graph color)))

(defn valid-outermost [graph start]
  (loop [result (add-valid #{} graph start)]
    (let [result2 (reduce (fn [res color]
                            (add-valid res graph color))
                          result result)]
      (if (= result result2)
        result
        (recur result2)))))

(count (valid-outermost col-graph "shiny gold"))
;; => 169

; part 2

(def op1
  (map #(map
         (fn [s] (str/replace s #"\s?bag(s?)|\." ""))
         (str/split % #"\s?(contain|,)\s?")) input))

(defn get-contents [bag-col]
  (map
   (fn [s]
     (let [[n & c] (str/split s #" ")]
       (conj (list (read-string n) (str/join " " c)))))
   (rest (first (filter #(= (first %) bag-col) op1)))))

(get-contents "shiny gold")
;; => ((3 "dotted magenta")
;;     (2 "shiny beige")
;;     (3 "plaid brown")
;;     (5 "clear indigo"))

(def graph
  (into {}
        (map #(vector % (get-contents %))
             (map (fn [s] (first s)) op1))))

(take 2 graph)
;; => (["plaid gold" ((5 "wavy magenta") (2 "dotted gray"))]
;;     ["plaid purple"
;;      ((3 "plaid gray") (3 "dim olive") (4 "dotted purple"))])

(defn color-count [graph color]
  (let [entries (get graph color)]
    (if (seq entries)
      (reduce
       (fn [cnt [num color]]
         (+ cnt (* num (color-count graph color))))
       1
       entries)
      1)))

; my graph doesn't work ?
(color-count graph "shiny gold")
;; => class clojure.lang.Symbol cannot be cast to class java.lang.Number

(defn parse-entry [s]
  (let [[bag & deps] (str/split s #"\s?(contain|,)\s?")
        color (re-find #"\w+ \w+" bag)]
    [color (keep (comp next (partial re-find #"(\d+) (\w+ \w+)" )) deps)]))

(defn nesting-graph [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (update m bag conj [(Long/parseLong num) col]))
                    m deps))
          {}
          entries))

(def imp-graph (nesting-graph (map parse-entry input)))

(take 2 imp-graph)
;; => (["plaid gold" ([2 "dotted gray"] [5 "wavy magenta"])]
;;     ["plaid purple"
;;      ([4 "dotted purple"] [3 "dim olive"] [3 "plaid gray"])])

; this graph totaly works 
(dec (color-count imp-graph "shiny gold"))
;; => 82372




