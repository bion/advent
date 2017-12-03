(def lighting-instructions (clojure.string/split (slurp "input.txt") #"\n"))

(defn extract-corners [instruction]
  (map #(Integer. %) (re-seq #"[0-9]{1,3}" instruction)))

(defn light-locations [instruction]
  (let [[x1 y1 x2 y2] (extract-corners instruction)]
    (for [x (range x1 (inc x2))
          y (range y1 (inc y2))]
      [x y])))

;; Part 1
(defn interpret-instruction [instruction]
  (let [locations (set (light-locations instruction))]
    (cond (.contains instruction "on")
          #(clojure.set/union % locations)

          (.contains instruction "off")
          #(clojure.set/difference % locations)

          :else
          #(let [to-turn-off (clojure.set/intersection % locations)]
             (clojure.set/difference (clojure.set/union % locations)
                                     to-turn-off)))))

(let [instruction-fns (map interpret-instruction lighting-instructions)]
  (count (reduce (fn [lit-lights func] (func lit-lights)) #{} instruction-fns)))

;; Part 2
(defn brightness-reduce-fn [change]
  (fn [light-memo loc]
    (let [current-brightness (light-memo loc 0)]
      (assoc light-memo loc (change current-brightness)))))

(defn interpret-instruction-v2 [instruction]
  (let [locations (light-locations instruction)]
    (cond (.contains instruction "on")
          #(reduce (brightness-reduce-fn inc) % locations)

          (.contains instruction "off")
          (fn [lights]
            (reduce (brightness-reduce-fn #(max (dec %) 0)) lights locations))

          :else
          #(reduce (brightness-reduce-fn (partial + 2)) % locations))))

(let [instruction-fns (map interpret-instruction-v2 lighting-instructions)]
  (apply + (vals (reduce (fn [lit-lights func]
                           (func lit-lights))
                         {}
                         instruction-fns))))
