(ns aoc.2018.day10.message)

(defn parse-input []
  (->>
   (slurp "resources/2018/day10.txt")
   clojure.string/split-lines
   (map #(re-find #"position=< *([-\d]+), *([-\d]+). velocity=< *([-\d]+), *([-\d]+)." %))
   (map #(drop 1 %))
   (map #(mapv read-string %))
   (map (fn [[x1 y1 x2 y2]] [[x1 y1] [x2 y2]]))))

(defn tick [input]
  (map (fn [[c v]]
         [(mapv + c v) v]
         ) input))


(defn size-x [input]
  (let [xs (map ffirst input)
        min (apply min xs)
        max (apply max xs)
        ]
    (- max min)))

(defn fast-forward [input]
  (let [next-input (tick input)]
    (if (> (size-x next-input) (size-x input))
      input
      (recur next-input))))

(defn print-sky [sky]
  (let [coordinates (mapv (fn [[c _]] c) sky)
        min-x (apply min (map first coordinates))
        min-y (apply min (map second coordinates))
        coordinates-norm-set (set (map (fn [[x y]] [(- x min-x) (- y min-y)]) coordinates))
        max-x (apply max (map first coordinates-norm-set))
        max-y (apply max (map second coordinates-norm-set))
        ]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (if (coordinates-norm-set [x y]) (print "#") (print " "))
        )
      (println)
      )))

(defn answer1 []
  (print-sky (fast-forward (parse-input))))

(defn fast-forward-second [seconds input]
  (let [next-input (tick input)]
    (if (> (size-x next-input) (size-x input))
      seconds
      (recur (inc seconds) next-input)))
  )

(defn answer2 []
  (fast-forward-second 0 (parse-input)))
