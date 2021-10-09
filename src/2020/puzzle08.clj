(ns puzzle08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
")


(defn parse-cmd [input]
  (vec (for [[_ op arg] (re-seq #"(\w{3}) ([+-]\d+)" input)]
         [(keyword op) (read-string arg)])))

(defn get-acc [input]
  (let [program (parse-cmd input)
        init-ctx {:pc 0
                  :acc 0
                  :seen #{}}]
    (loop [{:keys [pc acc seen] :as ctx} init-ctx]
      (if (contains? seen pc)
        acc
        (let [[op arg] (get program pc)]
          (case op
            :nop (recur (update (update ctx :pc inc) :seen conj pc))
            :acc (recur (update (update (update ctx :acc + arg) :pc inc) :seen conj pc))
            :jmp (recur (update (update ctx :pc + arg) :seen conj pc))))))))

(get-acc demo-input)
;; => 5

(def input (slurp (io/resource "2020/puzzle08_input.txt")))

(get-acc input)
;; => 1446

; Part 2

(defn run-vm [program]
  (let [init-ctx {:pc 0
                  :acc 0
                  :seen #{}}]
    (loop [{:keys [pc acc seen] :as ctx} init-ctx]
      (cond
        (contains? seen pc)
        :infinite-loop!

        (= pc (count program))
        acc

        :else
        (let [[op arg] (get program pc)]
          (case op
            :nop (recur (update (update ctx :pc inc) :seen conj pc))
            :acc (recur (update (update (update ctx :acc + arg) :pc inc) :seen conj pc))
            :jmp (recur (update (update ctx :pc + arg) :seen conj pc))))))))

(set (let [program (parse-cmd input)]
       (for [i (range (count program))
             :when (#{:nop :jmp} (get-in program [i 0]))
             :let [program (update-in program [i 0] {:jmp :nop, :nop :jmp})
                   result (run-vm program)]]
         result)))
;; => #{1403 :infinite-loop!}
