(ns aoc.2017.day4.high-entropy-passphrases
  (:require [clojure.test :refer [deftest is]]))

;; did not need to check for lower case but hey..
(defn lower-case? [str]
  (= (clojure.string/lower-case str) str))

(defn no-duplicates? [coll]
  (->> coll
       frequencies
       (some (fn [[_ v]] (> v 1)))
       nil?))

;;another version of no-duplicates?
(defn no-duplicates-2? [coll]
  (= (count (set coll))
     (count coll)))

(defn valid? [line]
  (and (lower-case? line)
       (->> (clojure.string/split line #" ")
            no-duplicates?)))

(defn count-valid [lines]
  (count (filter valid? lines)))

;; testing
(deftest test-valid-line
  (is (= true (valid? "hola adeu")))
  (is (= false (valid? "hola adeu hola"))))

;; solution
(count-valid (clojure.string/split-lines (slurp "resources/2017/day4.txt")))

(defn no-anagrams? [coll]
  (no-duplicates? (map frequencies coll))
  )

(defn valid-2? [line]
  (and (lower-case? line)
       (->> (clojure.string/split line #" ")
            no-anagrams?)))

(defn count-valid-2 [lines]
  (count (filter valid-2? lines)))

;;testing 2
(deftest test-anagrams
  (is (= true (no-anagrams? ["hola" "adeu"])))
  (is (= false (no-anagrams? ["hola" "aloh"]))))

;;solution 2
(count-valid-2 (clojure.string/split-lines (slurp "resources/2017/day4.txt")))
