(ns aoc.2017.day7.recursive-circus
  (:require [clojure.test :refer [deftest is]]))

(defn parse-line [line]
  (let [tok (clojure.string/split line #" -> ")
        sub (if (nil? (second tok)) []
                (clojure.string/split (second tok) #", "))
        tok2 (clojure.string/split (first tok) #" ")
        name (first tok2)
        weight (->> (second tok2)
                    (drop 1)
                    drop-last
                    (apply str)
                    read-string)]
    {name {:w weight :sub sub}}))

(defn father [data]
  (let [programs (-> data
                     keys
                     set)
        sub-programs (->>
                      data
                      vals
                      (map :sub)
                      flatten
                      set)]
    (clojure.set/difference programs sub-programs)))

(deftest parse-program
  (is (= {"p1" {:w 25 :sub ["p2" "p3"]}}
         (parse-line "p1 (25) -> p2, p3")))
  (is (= {"p2" {:w 25 :sub []}}
         (parse-line "p2 (25)"))))

;; load input
(def input (reduce #(merge (parse-line %2) %1) {}
                   (clojure.string/split-lines (slurp "resources/2017/day7.txt"))))

;; sol1
(father input)

(declare get-sub-weights)

(defn get-weight [input key]
  (let [current (input key)]
    {key 
     (+ (:w current) (apply + (flatten (map vals (get-sub-weights input key)))))}))

(defn get-sub-weights [input key]
  (let [current (input key)]
    (if (empty? (:sub current))
      []
      (map (partial get-weight input) (:sub current)))))

;; sol2 got to the solution while exploring the problem.
(get-sub-weights input "svugo")
(get-sub-weights input (second (:sub (input "svugo"))))
(get-sub-weights input "yruivis")
(get-sub-weights input "sphbbz")
(input "sphbbz")
(- 1161 9 )
(def fixed-input (assoc-in input ["sphbbz" :w] 1152))
(get-sub-weights fixed-input "svugo")

