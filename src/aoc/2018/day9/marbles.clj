(ns aoc.2018.day9.marbles)

;; first attempt persistent datastrutures not to much tought, too slow
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

(defn game [num-players last-ball]
  (reduce play [(vec (repeat num-players 0)) [0] 0] (range 1 (inc last-ball))))

(defn max-score [num-players last-ball]
  (apply max (first (game num-players last-ball))))

;; second attempt ArrayList delete too slow
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

(defn game-al [num-players last-ball]
  (let [^java.util.ArrayList board (new java.util.ArrayList)]
    (.add board 0 0)
    (reduce play-al [(vec (repeat num-players 0)) board 0] (range 1 (inc last-ball)))))

(defn max-score-al [num-players last-ball]
  (apply max (first (game-al num-players last-ball))))

;; thirt LinkedList still not fast enough
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

(defn game-ll [num-players last-ball]
  (let [^java.util.LinkedList board (new java.util.LinkedList)]
    (.add board 0 0)
    (reduce play-ll [(vec (repeat num-players 0)) board 0] (range 1 (inc last-ball)))))

(defn max-score-ll [num-players last-ball]
  (apply max (first (game-ll num-players last-ball))))


;; last attempt got both solutions with this.
;; jvm memory set to 3Gb and server.
;; fast access to current node which is the board itself
;; fast access to siblings and fast insert and delete.
;; Used a vector [previous next value]

(defn initial-board []
  (let [initial-board
        [(atom nil)
         (atom nil)
         0]
        ]
    (reset! (first initial-board) initial-board)
    (reset! (second initial-board) initial-board)
    initial-board))

(defn play-board [[players board] num-marble]
  (let [player (mod (dec num-marble) (count players))
        special-ball (zero? (mod num-marble 23))]
    (if (zero? (mod num-marble 10000))
      (println num-marble))
    (if special-ball
      (let [node @(first
                   @(first
                     @(first
                       @(first
                         @(first
                           @(first
                             @(first board)))))))
            val (last node)
            ]
        (reset! (second @(first node)) @(second node))
        (reset! (first @(second node)) @(first node))
        [(update players player + num-marble val)
         @(second node)]
        )
      (let [node @(second board)
            new-node [
                      (atom node)
                      (atom @(second node))
                      num-marble]]
        (reset! (first @(second node)) new-node)
        (reset! (second node) new-node)
        [players
         new-node
         ]))))


(defn game-board [num-players last-ball]
  (reduce play-board [(vec (repeat num-players 0)) (initial-board)] (range 1 (inc last-ball))))

(defn max-score-board [num-players last-ball]
  (apply max (first (game-board num-players last-ball))))


(defn answer1 []
  (max-score-board 452 71250)
  )

(defn answer2 []
  (max-score-board 452 (* 100 71250)))

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
  (max-score2 452 71250)
  (max-score2 452 (* 100 71250))
  )


