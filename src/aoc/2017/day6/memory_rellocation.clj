(ns aoc.2017.day6.memory-rellocation
  (:require [clojure.test :refer [deftest is]]))

(defn add-memory [total idx banks]
  (if (zero? total)
    banks
    (recur (dec total) (mod (inc idx) (count banks)) (update banks idx inc))))

(defn rellocate [banks]
  (let [max-v (apply max banks)
        max-i (ffirst (filter #(= (second %) max-v) (map-indexed vector banks)))
        remove-bank (assoc banks max-i 0)]
    (add-memory max-v (mod (inc max-i) (count banks)) remove-bank)))

(defn seq-banks [banks]
  (iterate rellocate banks))

(defn find-first-repeat [coll]
  (loop [pass #{(first coll)}
         to-check (rest (map-indexed vector coll))]
    (if (pass (second (first to-check)))
      (ffirst to-check)
      (recur (conj pass (second (first to-check))) (rest to-check)))))

(defn rellocate-until-repeat [banks]
  (find-first-repeat (seq-banks banks)))

;; test
(deftest example
  (is (= [2 4 1 2] (rellocate [0 2 7 0])))
  (is (= 5 (rellocate-until-repeat [0 2 7 0]))))

;;sol1
(def input [14	0 15 12	11 11 3	5 1 6 8	4 9 1 8	4])
(rellocate-until-repeat input)

;;sol2
(def s-banks (seq-banks input))
(nth s-banks 11137)
(->>
 (map-indexed vector s-banks)
 (drop 11137)
 (filter #(= (nth s-banks 11137) (second %)))
 (take 2)
 (map first)
 reverse
 (apply -))
