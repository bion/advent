(def num-list (->> "input.txt" slurp seq (map #(Character/digit % 10)) (drop-last 1)))
(def len (count num-list))

(reduce #(+ %1 (first %2))
        0
        (filter (fn [[a b]] (= a b))
                (partition 2 1 (take (inc len) (cycle num-list)))))

(reduce #(+ %1 (apply + %2))
        0
        (filter (fn [[a b]] (= a b))
                (apply map
                       list
                       (partition (/ (count num-list) 2)
                                  num-list))))
