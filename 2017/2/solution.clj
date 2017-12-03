(require 'clojure.math.combinatorics)

(def table (map
            #(let [row (clojure.string/split % #"\t")]
               (map read-string row))
            (-> "input.txt"
                slurp
                (clojure.string/split #"\n"))))

(apply + (map #(- (apply max %) (apply min %))
              table))

(defn divisible? [a b]
  (zero? (mod b a)))

(defn find-even-quotient [row]
  (let [combos (map sort (clojure.math.combinatorics/combinations row 2))
        even-pairs (filter #(apply divisible? %) combos)
        even-pair (first even-pairs)]

    (/ (last even-pair) (first even-pair))))

(apply + (map find-even-quotient table))
