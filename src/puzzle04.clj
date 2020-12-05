(ns puzzle04
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def demo-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(defn parse-entry [entry]
  (into
   {}
   (map #(vec (next %))
        (re-seq #"(\w{3}):(\S+)" entry))))


(defn valid? [m]
  (= (count (dissoc m "cid")) 7))

(count
 (filter
  valid?
  (map parse-entry
       (str/split demo-input #"\R\R"))))
;; => 2

(def input (slurp (io/resource "puzzle04_input.txt")))

(count
 (filter
  valid?
  (map parse-entry
       (str/split input #"\R\R"))))
;; => 256

; Part 2

(defn valid2? [{:strs [byr iyr eyr hgt hcl ecl pid cid]}]
  (and
   byr (<= 1920 (Long/parseLong byr) 2002)
   iyr (<= 2010 (Long/parseLong iyr) 2020)
   eyr (<= 2020 (Long/parseLong eyr) 2030)
   hgt (let [[_ num unit] (re-find #"(\d+)(in|cm)" hgt)]
         (case unit
           "cm" (<= 150 (Long/parseLong num) 193)
           "in" (<= 59 (Long/parseLong num) 76)
           false))
   hcl (re-find #"^#[0-9a-f]{6}" hcl)
   ecl (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
   pid (re-find #"^\d{9}$" pid)))

(Long/parseLong
 (get (parse-entry (first (str/split input #"\R\R"))) "pid"))

(count
 (filter
  valid2?
  (map parse-entry
       (str/split input #"\R\R"))))
;; => 198
