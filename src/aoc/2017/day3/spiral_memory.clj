(ns aoc.2017.day3.spiral-memory)



;;quick hacking day3 1

;; reduce is not lazy.
(def layers
    (map-indexed (fn [i v] [i v]) (reduce #(conj %1 (+ (last %1) %2)) [1] 
                                          (take 1000 (drop 1 (map #(* 4 (dec %))
                                                                  (iterate (partial + 2) 1)))))))

;;325489 
(first (filter (fn [[i v]] (< 325489 v)) layers))

;;previous value
(nth layers 284)
  ;; pos 285
(def i0 (- 325489 323761))
i0
;;3.5 closes point to 0 row/col
(+ (- (* 3.5 (* 2 285)) i0) 285)


;; part 2

;;147  142  133  122   59
;;304    5    4    2   57
;;330   10    1    1   54
;;351   11   23   25   26
;;362  747  806


;; brute force map

;;[0 0], [1 0], [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1] [2 -1]
;; d: current dimension of matrix 
(defn next-pos [[x y d]]
  (cond
    (and (neg? y) (= x (- y))) [(inc x) y (inc d)]
    (and (pos? x) (= x (- y))) [(inc x) y d]
    (and (pos? y) (= (- x) y)) [x (dec y) d]
    (and (zero? y) (pos? x)) [x (inc y) d]
    (and (pos? x) (= x y)) [(dec x) y d]
    (and (pos? x) (= x y)) [(dec x) y d]
    (and (neg? x) (= x y)) [(inc x) y d]
    (= y d) [(dec x) y d]
    (= x (- d)) [x (dec y) d]
    (= x d) [x (inc y) d]
    (= y (- d)) [(inc x) y d]))

(take 40 (iterate next-pos [1 0 1]))

(def m {:last [1 0 1]
         [0 0] 1
         [1 0] 1})

(defn neighbors [[x1 y1]]
  (mapv (fn [[x y]] [(+ x x1) (+ y y1)]) [[1 1][1 0][0 1][-1 0][0 -1][-1 -1][-1 1][1 -1]])
  )

(defn add-next [data]
  (let [next (next-pos (:last data))
        pos [(first next) (second next)]]
    (-> data
        (assoc :last next)
        (assoc pos (apply + (map #(get data % 0) (neighbors pos)))))))


(def m100 (nth (iterate add-next m) 100))

(first (filter #(> % 325489) (sort (vals (dissoc m100 :last)))))



