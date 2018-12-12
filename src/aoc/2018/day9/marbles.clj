(ns aoc.2018.day9.marbles)

(defn play [[players board current-idx] num-marble]
  (let [player (mod (dec num-marble) (count players))
        next-pos (mod (+ current-idx 2) (count board))
        next-pos (if (zero? next-pos) (count board) next-pos)
        special-ball (zero? (mod num-marble 23))
        special-pos (mod (- current-idx 7) (count board))
        ]
    (when (zero? (mod num-marble 1000))
      (println num-marble))
    (if special-ball
      [(update players player + num-marble (nth board special-pos))
       (concat (take special-pos board) (drop (inc special-pos) board))
       special-pos]
      [players
       (concat (take next-pos board) [num-marble] (drop next-pos board))
       next-pos
       ])))

(defn play-al [[players  board current-idx] num-marble]
  (let [player (mod (dec num-marble) (count players))
        next-pos (mod (+ current-idx 2) (.size board))
        next-pos (if (zero? next-pos) (.size board) next-pos)
        special-ball (zero? (mod num-marble 23))
        special-pos (mod (- current-idx 7) (.size board))
        ]
    (if (zero? (mod num-marble 10000))
      (println num-marble))
    (if special-ball
      (let [val (.get board special-pos)]
        (.remove ^java.util.ArrayList board (int special-pos))
        [(update players player + num-marble val)
         board
         special-pos])
      (do
        (.add board next-pos num-marble)
        [players
         board
         next-pos
         ]))))

(defn play-ll [[players  board current-idx] num-marble]
  (let [player (mod (dec num-marble) (count players))
        next-pos (mod (+ current-idx 2) (.size board))
        next-pos (if (zero? next-pos) (.size board) next-pos)
        special-ball (zero? (mod num-marble 23))
        special-pos (mod (- current-idx 7) (.size board))
        ]
    (if (zero? (mod num-marble 10000))
      (println num-marble))
    (if special-ball
      (let [val (.get board special-pos)]
        (.remove ^java.util.LinkedList board (int special-pos))
        [(update players player + num-marble val)
         board
         special-pos])
      (do
        (.add board next-pos num-marble)
        [players
         board
         next-pos
         ]))))

(defn game [num-players last-ball]
  (reduce play [(vec (repeat num-players 0)) [0] 0] (range 1 (inc last-ball))))

(defn game-al [num-players last-ball]
  (let [^java.util.ArrayList board (new java.util.ArrayList)]
    (.add board 0 0)
    (reduce play-al [(vec (repeat num-players 0)) board 0] (range 1 (inc last-ball)))))

(defn game-ll [num-players last-ball]
  (let [^java.util.LinkedList board (new java.util.LinkedList)]
    (.add board 0 0)
    (reduce play-ll [(vec (repeat num-players 0)) board 0] (range 1 (inc last-ball)))))

(defn max-score-al [num-players last-ball]
  (apply max (first (game-al num-players last-ball))))

(defn max-score-ll [num-players last-ball]
  (apply max (first (game-ll num-players last-ball))))

(defn max-score [num-players last-ball]
  (apply max (first (game num-players last-ball))))



;;(max-score-ll 452 7125000)
;; optimization 1 use ArrayList java instead of clojure
(defn answer1 []
  (max-score-al 452 71250)
  )

(defn answer2 []
  (max-score-al 452 (* 100 71250)))

(defn initial-board []
  (let [initial-board
        {:prev (atom nil)
         :next (atom nil)
         :value 0}
        ]
    (reset! (:prev initial-board) initial-board)
    (reset! (:next initial-board) initial-board)
    initial-board))


(defn play-board [[players board] num-marble]
  (let [player (mod (dec num-marble) (count players))
        special-ball (zero? (mod num-marble 23))]
    (if (zero? (mod num-marble 10000))
      (println num-marble))
    (if special-ball
      (let [node @(:prev
                   @(:prev
                     @(:prev
                       @(:prev
                         @(:prev
                           @(:prev
                             @(:prev board)))))))
            val (:value node)
            ]
        (reset! (:next @(:prev node)) @(:next node))
        (reset! (:prev @(:next node)) @(:prev node))
        [(update players player + num-marble val)
         @(:next node)]
        )
      (let [node @(:next board)
            new-node {:value num-marble
                      :prev (atom node)
                      :next (atom @(:next node))}]
        (reset! (:prev @(:next node)) new-node)
        (reset! (:next node) new-node)
        [players
         new-node
         ])
      ))
  )


(defn game2 [num-players last-ball]
  (reduce play-board [(vec (repeat num-players 0)) (initial-board)] (range 1 (inc last-ball))))

(defn max-score2 [num-players last-ball]
  (apply max (first (game2 num-players last-ball))))


;;(play-board [[0 0] (initial-board)] 1)

(comment 
  (max-score 9 25)
  (max-score2 9 25)
  (game 9 25)
  (game-al 9 25)
  (game-ll 9 25)
  (max-score-al 9 25)
  (max-score 10 1618)
  (max-score-al 10 1618)
  (max-score2 10 1618)
  (max-score 30 5807)
  (max-score-ll 30 5807)
  (max-score2 452 (* 100 71250))
  )
;; optimization 2 TODO
;; use the right type of struture for add retrieve and delete
;; create my own datastructure to reprent the problem
;; circular list with previous next and keep a reference to pointer.

