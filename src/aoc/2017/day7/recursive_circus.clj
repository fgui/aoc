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
                    read-string
                    )
        ]
    {name {:w weight :sub sub}})
  )

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
         (parse-line "p1 (25) -> p2, p3")
         ))
  (is (= {"p2" {:w 25 :sub []}}
         (parse-line "p2 (25)")
         )))


;; load input
(def input (reduce #(merge (parse-line %2) %1) {}
                   (clojure.string/split-lines (slurp "resources/2017/day7.txt"))))

;; sol1
(father input)

