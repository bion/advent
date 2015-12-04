(def floor-instructions (slurp "input.txt"))

;; Day 1 Part 1
(let [char-freqs (frequencies floor-instructions)
      up-char-count (get char-freqs \()
      down-char-count (get char-freqs \))]
  (prn (- up-char-count down-char-count)))

;; Day 1 Part 2
(defn step-val [step]
  (cond (= step \() 1
        (= step \)) -1
        :else 0))

(defn first-enters-basement [instructions]
  (loop [steps (seq instructions)
         step-position 0
         current-floor 0]
    (let [next-floor (+ current-floor (step-val (first steps)))]
      (if (neg? next-floor)
        (inc step-position)
        (recur (rest steps) (inc step-position) next-floor)))))

(first-enters-basement floor-instructions)
