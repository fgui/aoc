(ns aoc.2015.day5.nice-strings)

(defn has-three-vowels [a-str]
  (> (count (filter #{\a \e \i \o \u} a-str)) 2))

(defn has-twice-in-row [a-str]
  (first
   (reduce (fn [a e]
             [(boolean (or (first a) (= (second a) e)))
              e]) [false ""] a-str)))

(def bad-strs ["ab" "cd" "pq" "xy"])

(defn has-no-bad-str [a-str]
  (not
   (some identity (map #(clojure.string/includes? a-str %) bad-strs))))



(comment
  (->> (slurp "resources/2015/day5.txt")
       clojure.string/split-lines
       (filter has-three-vowels)
       (filter has-twice-in-row)
       (filter has-no-bad-str)
       count)
  )

(defn get-pairs-positions [s]
  (reduce (fn [{:keys [i last-e pairs] :as ac} e]
              {:i (inc i)
               :last-e e
               :pairs (update pairs (str e last-e) #(concat [i] %))}
            )
          {:i 0 :last-e nil :pairs {}} s)
  )


(defn has-repeated-pair [s]
  (->> (get-pairs-positions s)
       :pairs
       (map #((juxt (partial apply max) (partial apply min)) (second %)))
       (some #(> (Math/abs (- (first %) (second %))) 1))
       )
  )

(defn has-repeated-btw [s]
  (some identity (map =  s (rest (rest s))) )
  )

(comment
  (->> (slurp "resources/2015/day5.txt")
       clojure.string/split-lines
       (filter has-repeated-pair)
       (filter has-repeated-btw)
       count
       )
  )
