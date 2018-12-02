(ns aoc.2018.day2.scan-boxes)

(defn parse-input []
  (clojure.string/split-lines (slurp "resources/2018/day2.txt")))

(defn times [s]
  (set (vals (frequencies s))))

(defn update-times [m s]
  (reduce #(update %1 %2 (fnil inc 0)) m  s))

(defn times-coll [coll]
  (->>
   coll
   (map times)
   (reduce update-times {})))


(defn answer1 []
  (let [counts (times-coll  (parse-input))]
    (* (get counts 2) (get counts 3))
    ))


(defn diff-one [st1 st2]
  (loop [d 0 s1 st1 s2 st2]
    (if (or (empty? s1) (> d 1))
      (if (empty? s2)
        (= d 1)
        false)
      (recur (if (= (first s1) (first s2))
               d
               (inc d))
             (rest s1)
             (rest s2)))))

(defn answer2 []
  (->>
   (for [x (parse-input)
         y (parse-input)
         :when (diff-one x y)]
     [x y]
     )
   first
   (apply map #(if (= %1 %2) %1 ""))
   (apply str)
   )
  )
