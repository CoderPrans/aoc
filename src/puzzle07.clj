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

(def input (line-seq (io/reader (io/resource "puzzle07_input.txt"))))

(def parsed-conditions
  (map 
   (fn [[outer & rest]]
     [outer (map str/trim rest)])
   (map
    #(str/split
      (str/replace % #"no other bags.| bags contain | ba(g|gs)[, |.]" "")
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
