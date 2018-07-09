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
