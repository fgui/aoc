(ns aoc.2018.day5.reduction)


(defn parse-input []
  (vec (drop-last (slurp "resources/2018/day5.txt"))))

(parse-input)

(defn abs [n] (max n (- n)))

(defn reduct [pol]
  (let [new-pol
        (reduce  (fn [col next]
                   (let [prev (last col)]
                     (if (and
                          (not (nil? prev))
                          (= 32 (abs (- (int prev)
                                     (int next)))
                                 ))
                       (vec (drop-last col))
                       (conj col next)
                       )
                     )
                   )
                 [] pol
                 )]
    (println (count pol))
    (if (= (count pol) (count new-pol))
      new-pol
      (recur new-pol)
      )
    ))

(def res1 (reduct (parse-input)))

(def answer1 (count res1))

(reduct (vec "dabAcCaCBAcCcaDA"))
(reduct (vec "abBA"))

(count (vec "abBA"))

(def units
  (map (fn [x] #{(char x) (char (- x 32))})
       (range (int \a) (int \z))))

(defn answer2 []
  (apply min (reduce
        #(conj %1 (count (reduct
                          (filter (fn [v] (not (contains? %2 v))) res1))))
       []
       units)))

(answer2)
