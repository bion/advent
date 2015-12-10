(require '[clojure.math.combinatorics :as combo])

(def distances-graph
  (reduce (fn [graph distance]
            (let [[locations distance] (clojure.string/split distance #" = ")
                  distance (Integer. distance)
                  [l1 l2] (clojure.string/split locations #" to ")]
              (update-in (update-in graph [l1] assoc l2 distance)
                         [l2] assoc l1 distance)))
          {}
          (clojure.string/split (slurp "input.txt") #"\n")))

(defn distance-for-route [route]
  (let [segments (partition 2 1 route)]
    (reduce (fn [sum [l1 l2]]
              (+ sum (get-in distances-graph [l1 l2])))
            0
            segments)))

(let [routes (combo/permutations (keys distances-graph))
      total-distances (map distance-for-route routes)]
  [(apply min total-distances)
   (apply max total-distances)])
