(def phrases
  (let [result (slurp "input.txt")
        result (clojure.string/split result #"\n")
        result (map #(clojure.string/split % #" ") result)]
    result))

(defn all-uniq? [phrase]
  (loop [seen #{}
         current phrase]
    (let [word (first current)
          remaining (rest current)]
      (cond
        (seen word) false
        (empty? remaining) true
        :else (recur (conj seen word) remaining)))))

(count (filter all-uniq? phrases))

(defn no-anagrams? [phrase]
  (loop [seen #{}
         current phrase]
    (let [word (first current)
          sorted (apply str (sort word))
          remaining (rest current)]
      (cond
        (seen sorted) false
        (empty? remaining) true
        :else (recur (conj seen sorted) remaining)))))

(count (filter no-anagrams? phrases))
