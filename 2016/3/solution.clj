(def nogged-instructions (seq (slurp "input.txt")))

(def instruction->direction
  {\< [-1 0], \> [1 0],
   \v [0 -1], \^ [0 1]})

(defn move [location direction]
  [(+ (first location) (first direction))
   (+ (last location) (last direction))])

(defn visit-houses [instructions]
  (loop [remaining-instructions instructions
         visited #{[0 0]}
         location [0 0]]

    (if (empty? remaining-instructions) visited
        (let [direction (instruction->direction (first remaining-instructions))
              new-location (move location direction)]

          (recur (rest remaining-instructions)
                 (conj visited new-location)
                 new-location)))))

;; Part 1

(count (clojure.set/union (visit-houses nogged-instructions)))

;; Part 2

(defn partition-instructions [santa-count instructions]
  ((apply juxt (for [x (range santa-count)]
                 (fn [coll] (take-nth santa-count (nthrest coll x)))))
   instructions))

(defn deliver-presents [santa-count instructions]
  (let [santa-instructions (partition-instructions santa-count instructions)
        each-santas-houses (pmap visit-houses santa-instructions)]
    (count (apply clojure.set/union each-santas-houses))))

(deliver-presents 2 nogged-instructions)
