(ns aoc.2015.day3.santa-grid)

(def initial {:pos [0 0]
              :visited {[0 0] 1}})

(def moves {">" [0 1]
            "<" [0 -1]
            "v" [-1 0]
            "^" [1 0]})

(defn next [{:keys [pos visited]} move]
  (let [new-pos (mapv + pos (moves (str move)))]
    {:pos new-pos
     :visited (update visited new-pos (fnil inc 0))}))

(next initial \<)
(next initial \>)
(next initial \v)
(next initial \^)

(->> (slurp "resources/2015/day3.txt")
     (reduce next initial)
     :visited
     keys
     count
     )
xs
;; part 2
(def initial-duo {:pos [[0 0] [0 0]]
                  :idx 0
                  :visited {[0 0] 1}})

(defn next-duo [{:keys [pos idx visited]} move]
  (let [new-pos (mapv
                 + (get pos idx)
                 (moves (str move)))]
    {:pos (assoc pos idx new-pos)
     :idx (mod (inc idx) 2)
     :visited (update visited new-pos (fnil inc 0))}))

(next-duo initial-duo \<)

(->> (slurp "resources/2015/day3.txt")
     (reduce next-duo initial-duo)
     :visited
     keys
     count)
