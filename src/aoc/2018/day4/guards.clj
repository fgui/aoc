(ns aoc.2018.day4.guards)

(defn parse-cmd [c]
  (let [start (first c)]
    (case start
      \G (read-string (second (re-find #"Guard #(\d+) begins shift" c)))
      \w :w
      \f :f
      "error")
    ))

(defn next-day [day]
  (let [date (.parse
              (java.text.SimpleDateFormat. "yyyy-MM-dd")
              day)
        next-date (new java.util.Date (+ (.getTime date) 86400000))
        ]
    (.format
     (java.text.SimpleDateFormat. "yyyy-MM-dd")
     next-date
     ))
  )

(defn parse-dt [d]
  (let [[_ day hour min]   (re-find #"(.*) (\d+):(\d+)$" d)]
    [(if (= "23" hour) (next-day day) day) (Integer/parseInt min)]
    )
  )

(defn parse-line  [l]
  (->>
   (re-find #"\[(?<date>.+)\] (?<command>.+)$" l)
   ((fn [[_ dt cmd]] [(parse-dt dt) (parse-cmd cmd)]))
   )
  )

(defn parse-input []
  (map parse-line
       (clojure.string/split-lines (slurp "resources/2018/day4.txt"))))

(defn organize-by-day [in]
  (reduce (fn [m [[dt min] cmd]]
            (update m dt #((fnil conj []) % [min cmd]))
            ) {} in)
  )


(defn minute-asleep [day-events]
  (fn[acc minute]
    (let [last-state (if (empty? acc) :w (last acc))
          new-acc (conj acc (get day-events minute last-state))
          ]
      new-acc))
  )

(defn minuts-asleep-day [day]
  (let [day-events (->> day
                        (filter #(contains? #{:w :f} (second %)))
                        (flatten)
                        (apply  hash-map)
                        )]

    [(second (first (filter #(not (keyword? (second %))) day)))
     (reduce (minute-asleep day-events) [] (range 60))]
    )
  )

(filter #(not (keyword? (second %))) [[39 :w] [43 :f] [22 :f] [54 :w] [1 331]])
(minuts-asleep-day [[39 :w] [43 :f] [22 :f] [54 :w] [1 331]])

(defn max-sleep-id []
  (->>
   (organize-by-day
    (parse-input))
   vals
   (map  minuts-asleep-day)
   (reduce (fn [m [id minutes]]
             (update m id (fn [v] (+ (if v v 0) (count (filter (fn[x] (= x :f)) minutes))) ))
             ) {})
   vec
   (sort-by second)
   last
   first
   ))

;; 1823

(defn increase-sleep [acc day]
  (map + (map #(if (= :f %) 1 0) day) acc))



(defn max-sleep-minute [input id]
  (->>
   input
   (filter #(= id (first %)))
   (map second)
   (reduce increase-sleep (repeat 60 0))
   (map-indexed (fn [i v] [i v]))
   (sort-by second)
   last
   ;;first
   ))


(defn answer1 []
  (* (max-sleep-id)  (first (max-sleep-minute
                            (->>(organize-by-day
                                 (parse-input))
                                vals
                                (map  minuts-asleep-day))
                            (max-sleep-id)))))

(answer1)

(defn answer2 []
  (let [guards-ids (->> (organize-by-day (parse-input))
                        (map second)
                        (map (fn [v] (filter #(number? (second %)) v)))
                        (map first)
                        (map second)
                        distinct)
        input    (->>(organize-by-day
                      (parse-input))
                     vals
                     (map  minuts-asleep-day))]
    (->>(map (fn[v] [v (max-sleep-minute input v)])guards-ids)
        (sort-by #(second(second %)))
        last
        (#(* (first %) (first (second %)))))
    )
  )

(answer2)
