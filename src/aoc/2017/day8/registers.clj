(ns aoc.2017.day8.registers
  (:require [clojure.test :refer [deftest is]]))



(defn run-instruction [registers instruction]
  (let [[reg-op op val _ reg-cond op-cond val-cond] (clojure.string/split instruction #" ")
        op-cond (clojure.string/replace op-cond "!=" "not=")]
    (if ((eval (read-string op-cond)) (get registers reg-cond 0) (read-string val-cond))
      (assoc registers reg-op
             ((if (= op "inc") + -) (get registers reg-op 0) (read-string val)))
      registers)))

(comment
(run-instruction {} "b inc 5 if a > 1")
  )

(defn run-instructions [instructions]
  (reduce run-instruction {} instructions))

(defn run-instructions-max [instructions]
  (reduce (fn [[max-val reg] instruction]
            (let [new-reg (run-instruction reg instruction)]
              [(max max-val (reduce max (map second new-reg)))
               new-reg])) [0 {}] instructions))

(def example
  [
   "b inc 5 if a > 1"
   "a inc 1 if b < 5"
   "c dec -10 if a >= 1"
   "c inc -20 if c == 10"])


(deftest test-run-instruction
  (is (= {"a" 1 "c" -10} (run-instructions  example))))


(->> (run-instructions (clojure.string/split-lines (slurp "resources/2017/day8.txt")))
     (map second)
     (reduce max))

(first (run-instructions-max (clojure.string/split-lines (slurp "resources/2017/day8.txt"))))
