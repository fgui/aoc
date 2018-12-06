(ns aoc.2018.day6.coordinates)

(defn parse-input []
  (->>
   (slurp "resources/2018/day6.txt")
   clojure.string/split-lines
   (map #(map read-string (clojure.string/split % #", ")))
   ))

(def input (parse-input))

(defn abs [n] (max n (- n)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2)))
  )

(defn grid-coordinates [input]
  (for [x (range (inc (apply max (map first input))))
        y (range (inc (apply max (map second input))))]
    [x y]))

(defn edge-coordinates [input]
  (filter #(or (#{0 (apply max (map first input))} (first %))
               (#{0 (apply max (map second input))} (second %))
              ) (grid-coordinates input)))

(defn min-distance-points [point points]
  (let [distances (map (fn [p] [(manhattan-distance point p) p])  points)
        min-d (apply min (map first distances))]
    (->>
     distances
     (filter #(= (first %) min-d)))))

(defn create-map [input grid]
  (reduce
   (fn [m c]
     (let [min-points (min-distance-points c input)
           point (if (= 1(count min-points)) (second (first min-points)) ".")]
       (assoc m c point)))
   {}
   grid))


(defn answer1 []
  (let [grid (create-map (parse-input) (grid-coordinates (parse-input)))
        edge-points (set (vals (create-map (parse-input) (edge-coordinates (parse-input)))))]
    (->>
     grid
     vals
     frequencies
     (filter #(nil? (edge-points (first %))))
     (sort-by second)
     reverse
     first
     second)))

(answer1)
