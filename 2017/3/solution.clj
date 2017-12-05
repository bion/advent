(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])
(defn top [[x y]] [x (inc y)])
(defn bottom [[x y]] [x (dec y)])

(defn get-next [table current]
  (cond (table (left current))
        (if (table (top current))
          (right current)
          (top current))

        (table (bottom current))
        (left current)

        (table (right current))
        (bottom current)

        :else
        (right current)))

(defn find-nth [n]
  (loop [table #{[0 0]}
         current-n 0
         current-coord [0 0]]
    (let [[x y] (get-next table current-coord)]

      (if (= n (inc current-n))
        current-coord
        (recur (conj table [x y]) (inc current-n) [x y])))))

(defn abs [ls] (apply + (map #(Math/abs %) ls)))

(defn distance [n]
  (abs (find-nth n)))

(distance 347991)

(defn top-left [[x y]] [(dec x) (inc y)])
(defn top-right [[x y]] [(inc x) (inc y)])
(defn bottom-left [[x y]] [(dec x) (dec y)])
(defn bottom-right [[x y]] [(inc x) (dec y)])

(def neighbors [top bottom left right top-left top-right bottom-left bottom-right])

(defn neighbor-vals [table coord]
  (map (fn [f] (get table (f coord) 0)) neighbors))

(defn get-val [table coord]
  (reduce + (neighbor-vals table coord)))

(defn find-bigger-than [input]
  (loop [table {[0 0] 1}
         current-coord [0 0]]
    (let [[x y] (get-next table current-coord)
          next-val (get-val table [x y])]

      (if (> next-val input)
        next-val
        (recur (assoc table [x y] next-val) [x y])))))

(find-bigger-than 347991)
