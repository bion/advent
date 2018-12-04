(def initial-memory-banks (let [string-vals (-> "input.txt"
                                                slurp
                                                (clojure.string/split #"\t"))
                                num-vals (map #(Integer/parseInt (clojure.string/trim %) 10)
                                              string-vals)]
                            (into [] num-vals)))

(defn index-of-max [col]
  (loop [remaining col
         current-max -1
         max-i -1
         i 0]

    (if (empty? remaining) max-i
        (let [new-max (max (first remaining) current-max)
              new-max-i (if (not (= new-max current-max)) i max-i)]
          (recur (rest remaining) new-max new-max-i (inc i))))))

(defn redistribute [col n]
  (let [size (count col)
        next-i #(-> % inc (mod size))]
    (loop [remaining (col n)
           result (assoc col n 0)
           current-i (next-i n)]
      (if (zero? remaining)
        result
        (recur (dec remaining)
               (update result current-i inc)
               (next-i current-i))))))

;; part one

(println (loop [memory-banks initial-memory-banks
                iterations 0
                seen #{}]
           (if (seen memory-banks) iterations
               (recur (redistribute memory-banks (index-of-max memory-banks))
                      (inc iterations)
                      (conj seen memory-banks)))))

;; part two

(println (loop [memory-banks initial-memory-banks
                iterations 0
                seen {}]
           (if (seen memory-banks) (- iterations (seen memory-banks))
               (recur (redistribute memory-banks (index-of-max memory-banks))
                      (inc iterations)
                      (assoc seen memory-banks iterations)))))
