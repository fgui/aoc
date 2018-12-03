(ns aoc.2018.day3.fabric)

(defn parse-line [l]
  (->>
   (re-find #"#(?<id>\d+) @ (?<left>\d+),(?<top>\d+): (?<wide>\d+)x(?<tall>\d+)$" l)
   (drop 1)
   (map read-string)))

#_(parse-line "#80 @ 72,395: 15x26")

(defn parse-input []
  (map parse-line
       (clojure.string/split-lines (slurp "resources/2018/day3.txt"))))

(defn add-piece [m [id left top wide tall]]
  (let [square (for [x (range left (+ left wide))
                     y (range top (+ top tall))]
                 [x y])]
    (reduce (fn [m p] (update m p #( (fnil conj #{}) % id))) m square)
    )
  )

#_(add-piece {} (parse-line "#80 @ 72,395: 15x26"))

(defn map-fabric [pieces]
  (reduce add-piece {} pieces))

#_(map-fabric (parse-input))

(defn answer1[]
  (->>
   (map-fabric (parse-input))
   vals
   (filter #(> (count %) 1))
   count))

(answer1)

(defn answer2 []
  (let [input (parse-input)
        mf (vals (map-fabric input))
        all-ids (set (map first input))
        same-area-ids (->>
         mf
         (filter #(> (count %) 1))
         (map vec)
         (flatten)
         set
         )]
    (clojure.set/difference all-ids same-area-ids)))

(answer2)
