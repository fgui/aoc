(ns aoc.2018.day1.frequency)

(defn calculate-frequency [coll]
  (apply + coll))

(defn find-duplicat-frequency [curr freq coll]
  (let [new-curr (+ curr (first coll))
        new-freq (update freq new-curr (fnil inc 0 ))]
    (if (> (get new-freq new-curr) 1)
      new-curr
      (if (next coll)
        (recur new-curr new-freq (rest coll))
        new-freq
        )
      )))

(defn parse-input []
  (map read-string
       (clojure.string/split-lines (slurp "resources/2018/day1.txt"))))

(defn answer1 []
  (calculate-frequency (parse-input)))

(defn answer2 []
  (->>
   (parse-input)
   (cycle)
   (find-duplicat-frequency 0 {0 1})))

(comment
  (answer1)
  (answer2)
  (find-duplicat-frequency 0 {0 1} [1 -1])
  (find-duplicat-frequency 0 {0 1} (cycle [7 7 -2 -7 -4])))
