(ns aoc.2018.day7.steps)

(defn parse-input []
  (->>
   (slurp "resources/2018/day7.txt")
   (clojure.string/split-lines)
   (map
    #(drop 1 (re-find #"S.+ ([A-Z]) .+ ([A-Z])"
                      %))
    )
   )
  )

(defn empty-map-tasks [input]
  (->>
   input
   flatten
   set
   (reduce #(assoc %1 %2 #{})
           {})))

(defn dependency-map [input]
  (->> input
       (map reverse)
       (reduce (fn [a v]
                 (update a (first v)
                         #(conj % (second v))))
               (empty-map-tasks input) )))



(defn can-be-done-steps [done d]
  (filter (fn [[_ v]]
            (clojure.set/subset? v done)
            ) d))


(defn do-steps [done d]
  (let [s-d (set done)
        nxt (->> d
                 (filter (complement(fn [[k _]] (s-d k))))
             (can-be-done-steps s-d)
             (keys)
             (sort)
             (first)
             )]
    (if nxt
      (recur (conj done nxt) d)
      done)))

(defn answer1 []
  (->>(parse-input)
      (dependency-map)
      (do-steps [])
      (apply str)))

(defn value [s]
  (inc (- (int (first s)) (int \A))))

(defn add-steps [wk nxt]
  (let [num-workers 5
        base-time 60
        wip (count (filter #(> (second %) 0) wk))]
    (if (or (= num-workers wip) (empty? nxt))
      wk
      (recur (assoc wk (first nxt)
                    (+ base-time
                       (value (first nxt))))
             (rest nxt)))
    )
  )

;; work {"A" 10 "C" 0}
(defn do-steps2 [tick work d]
  (let [
        work-dec (reduce (fn [m k] (if (> (get m k) 0)
                                     (update m k dec)
                                     m
                                     )) work (keys work))
        s-d (set (keys (filter #(= 0 (second %)) work-dec)))
        nxt (->> d
                 (filter (complement(fn [[k _]] (s-d k))))
             (can-be-done-steps s-d)
             (keys)
             (filter #(not ((set (keys work-dec)) %)))
             (sort)
             )]
    (println tick)
    (println work-dec)
    (if (and (empty? (filter #(> (second %) 0) work-dec))
             (empty? nxt))
      tick
      (recur (inc tick) 
             (add-steps work-dec nxt)
             d)
      )))

;; 5 workers
;; time step 5+
(defn answer2 []
  (->>(parse-input)
      (dependency-map)
      (do-steps2 0 {})
      ))
