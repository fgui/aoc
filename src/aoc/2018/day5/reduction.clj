(ns aoc.2018.day5.reduction)

(defn parse-input []
  (vec (drop-last (slurp "resources/2018/day5.txt"))))


(defn abs [n] (max n (- n)))

(defn reduct [pol]
  (let [new-pol
        (reverse
         (reduce  (fn [col next]
                    (let [prev (first col)]
                      (if (and
                           (not (nil? prev))
                           (= 32 (abs (- (int prev)
                                         (int next)))
                              ))
                        (rest col)
                        (conj col next)
                        )
                      )
                    )
                  '() pol
                  ))]
    (println (count pol))
    (if (= (count pol) (count new-pol))
      new-pol
      (recur new-pol)
      )
    ))


(defn answer1 [] (count (reduct (parse-input))))

(answer1)

(defn answer2 []
  (let [units (map (fn [x] #{(char x) (char (- x 32))})
                   (range (int \a) (int \z)))]
    (apply min (reduce
                #(conj %1 (count (reduct
                                  (filter (fn [v] (not (contains? %2 v))) (reduct (parse-input))))))
                []
                units))))

(answer2)
