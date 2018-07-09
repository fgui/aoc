(ns aoc.2015.day4.mining
  (:require [clojure.test :refer [deftest is are]]
            [digest]))


(defn coll-md5s [a-str]
  (->> (iterate inc 0)
       (map #(str a-str %))
       (map (fn [x] [x (digest/md5 x)]))))


(comment
  ;; done by brute force
  (first (filter #(clojure.string/starts-with? (second %) "00000") (coll-md5s "abcdef")))
  (first (filter #(clojure.string/starts-with? (second %) "00000") (coll-md5s "pqrstuv")))
  (first (filter #(clojure.string/starts-with? (second %) "000000") (coll-md5s "iwrupvqb"))))
