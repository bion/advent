(defn parse-instruction [raw]
  (let [[var fn val] (clojure.string/split raw #" ")
        val (Integer/parseInt val 10)]
    {:var (keyword var) :fn (keyword fn) :val val}))

(defn parse-condition [raw]
  (let [[var cond-fn val] (clojure.string/split raw #" ")
        val (Integer/parseInt val 10)]
    {:var (keyword var) :cond-fn (keyword cond-fn) :val val}))

(def instructions
  (map (fn [line]
         (let [[instruction condition] (clojure.string/split line #" if ")
               instruction (parse-instruction instruction)
               condition (parse-condition condition)]
           {:instruction instruction :condition condition}))
       (clojure.string/split (slurp "input.txt") #"\n")))

(defn run-cond [registers condition]
  (let [current-val (get registers (condition :var) 0)
        test-val (condition :val)]
    (case (:cond-fn condition)
      :> (> current-val test-val)
      :>= (>= current-val test-val)
      :<= (<= current-val test-val)
      :< (< current-val test-val)
      :== (= current-val test-val)
      :!= (not (= current-val test-val)))))

(defn run-instruction [registers instruction]
  (let [key (instruction :var)
        current-val (get registers key 0)
        new-val (case (instruction :fn)
                  :inc (+ current-val (instruction :val))
                  :dec (- current-val (instruction :val)))]
    (assoc registers key new-val)))

(defn exec [registers instruction]
  (if (run-cond registers (instruction :condition))
    (run-instruction registers (-> instruction :instruction))
    registers))

;; question one

(apply max (map second (reduce exec {} instructions)))

;; question two

(def highest (atom 0))

(defn run-instruction-set-highest [registers instruction]
  (let [key (instruction :var)
        current-val (get registers key 0)
        new-val (case (instruction :fn)
                  :inc (+ current-val (instruction :val))
                  :dec (- current-val (instruction :val)))]
    (if (> new-val @highest) (reset! highest new-val))
    (assoc registers key new-val)))

(defn exec-with-highest [registers instruction]
  (if (run-cond registers (instruction :condition))
    (run-instruction-set-highest registers (-> instruction :instruction))
    registers))

(apply max (map second (reduce exec-with-highest {} instructions)))

@highest
