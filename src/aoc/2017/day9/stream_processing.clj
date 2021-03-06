(ns aoc.2017.day9.stream-processing
  (:require [clojure.test :refer [deftest is are]]))

;; acc
;; {:status {:escape false :garbage false} :clean-s ""}

(defn remove-garbage [s]
  (reduce
   (fn [{:keys [escape garbage clean-s g-count]} ch]
     {:escape (and garbage (not escape) (= \! ch))
      :garbage (or (and (not garbage) (= \< ch)) (and garbage (not= \> ch)) escape)
      :clean-s (if (or garbage (= \< ch)) clean-s (str clean-s ch))
      :g-count (if (and garbage
                        (not escape)
                        (not= \> ch)
                        (not= \! ch))
                 (inc g-count) g-count)})
   {:escape false :garbage false :clean-s "" :g-count 0}
   s))


(defn seq-score [coll]
  (:acc (reduce (fn [{:keys [level acc]} ch]
                 (let [open (= ch \{)
                       close (= ch \})]
                   {:level ((cond
                              open inc
                              close dec
                              :else identity) level)
                    :acc (if open (conj acc level) acc)
                    }
                   )
                 )
               {:level 1 :acc []} coll)))

(deftest test-remove-garbage
  (are [x y] (= x (:clean-s y))
    "hola" (remove-garbage "hola")
    "hola" (remove-garbage "ho<>la")
    "hola" (remove-garbage "ho<!>>la")
    "ho>la" (remove-garbage "ho<!!>>la")
    "hola" (remove-garbage "ho<!!!>>la")
    ))

(deftest test-sequence-score
  (are [a b] (= a b)
    [1 2] (seq-score "{{}}")
    )
  )

;; part 1
(->> (slurp "resources/2017/day9.txt")
     remove-garbage
     :clean-s
     seq-score
     (reduce +))

;; part 2
(->> (slurp "resources/2017/day9.txt")
     remove-garbage
     :g-count)
