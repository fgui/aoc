(ns aoc.2015.day2.presents
  (:require [clojure.test :refer [deftest is are]]))

;; h, l, w
;; h*l*2
;; h*w*2
;; l*w*2

(defn parse-line [str]
  (map #(Integer/parseInt %) (clojure.string/split str #"x")))

(defn paper-for [[h l w]]
  (let [area (map #(apply * %) [[h l] [h w] [l w]])]
    (apply + (apply min area) (map #(* 2 %) area))))

(defn ribbon-for-sorted [sides]
  (+ (* 2 (apply + (take 2 sides)))
     (apply * sides)))

(defn ribbon-for [sides]
  (ribbon-for-sorted (sort sides)))

(defn add-f-list [f coll]
  (apply + (map f coll)))

(deftest test-paper
  (is (paper-for [2 3 4]) 58)
  (is (paper-for [1 1 10]) 43)
  (is (add-f-list paper-for [[2 3 4] [1 1 10]]) 101))

(deftest test-ribbon
  (is (ribbon-for [2 3 4]) 34)
  (is (ribbon-for [1 1 10]) 14)
  (is (add-f-list ribbon-for [[2 3 4] [1 1 10]]) 48))

(->> (slurp "resources/2015/day2.txt")
     clojure.string/split-lines
     (map parse-line)
     (add-f-list paper-for))

(->> (slurp "resources/2015/day2.txt")
     clojure.string/split-lines
     (map parse-line)
     (add-f-list ribbon-for))
