(require '[clojure.string :as string])

(defn areas-of-sides [[l w h]]
  [(* l w) (* w h) (* l h)])

(def input-dimensions
  (seq (map (fn [raw-dims]
              (map #(Integer/parseInt %) (string/split raw-dims #"x")))
            (string/split (slurp "input.txt") #"\n"))))

;; Part 1
(defn total-paper-area-reducer
  [memo dimensions]
  (let [areas (areas-of-sides dimensions)
        smallest-side (apply min areas)]
    (+ memo
       (+ (* 2 (reduce + areas))
          smallest-side))))

(prn (str "paper: " (reduce total-paper-area-reducer 0 input-dimensions)))

;; Part 2
(defn shortest-distance [[l w h]]
  (let [distances [(+ l w) (+ w h) (+ l h)]]
    (* 2 (apply min distances))))

(defn total-ribbon-length-reducer
  [memo dimensions]
  (+ memo
     (shortest-distance dimensions)
     (apply * dimensions)))

(prn (str "ribbon: " (reduce total-ribbon-length-reducer 0 input-dimensions)))
