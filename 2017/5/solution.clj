(def instructions
  (into []
        (map #(Integer/parseInt % 10)
             (-> "input.txt"
                 slurp
                 (clojure.string/split #"\n")))))

(defn steps-to-escape [jumps]
  (loop [list jumps
         step 0
         position 0]
    (if-let [jump (get list position nil)]
      (recur (update list position inc) (inc step) (+ jump position))
      step)))

(steps-to-escape instructions)

(defn steps-to-escape-2 [jumps]
  (loop [list jumps
         step 0
         position 0]
    (if-let [jump (get list position nil)]
      (recur (if (>= jump 3) (update list position dec) (update list position inc))
             (inc step)
             (+ jump position))
      step)))

(steps-to-escape-2 instructions)
