(ns aoc.2018.day8.license)

(defn parse-input []
  (map read-string
       (->
        (slurp "resources/2018/day8.txt")
        (clojure.string/split #" "))))

(defn parse-meta [[acc [nodes meta & rest]]]
  (->> [acc rest]
       (#(reduce (fn [n i] (parse-meta n)) % (range nodes)))
       ((fn [[m r]] [(+ m (apply + (take meta r))) (drop meta r)]) )
       ))

(defn metadata [in]
  (first (parse-meta [0 in])))

(defn answer1 []
  (metadata (parse-input)))


(def in (atom (parse-input)))

(defn parse-tree-2 []
  (let [[num-nodes num-meta & rest] @in]
    (reset! in rest)
    (->> {:nodes []
          :meta nil}
         ((fn [x] (reduce (fn [n i] (update n :nodes
                                            conj
                                            (parse-tree-2)))
                          x (range num-nodes))))
         ((fn [x] (let [mv (take num-meta @in)
                        nodes (:nodes x)]
                    (reset! in (drop num-meta @in))
                    (assoc x :meta (if (= 0 num-nodes)
                                      (apply + mv)
                                      (apply +
                                             (map
                                              #(get (get nodes (dec %)) :meta 0)
                                              mv
                                              ))))))))))

(defn answer2 []
  (reset! in (parse-input))
  (:meta
   (parse-tree-2)))


