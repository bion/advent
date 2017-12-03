(require '[clojure.string :as s])

(def operations
  {"RSHIFT" bit-shift-right
   "LSHIFT" bit-shift-left
   "OR" bit-or
   "AND" bit-and
   "NOT" bit-not})

(defn parse-expression [expression]
  (let [bitwise-op-name (re-find #"[A-Z]+" expression)]
    (cond
      bitwise-op-name
      (let [args (map #(parse-expression (s/trim %))
                      (filter not-empty
                              (s/split expression
                                       (re-pattern bitwise-op-name))))]
        {:type :operation :op (operations bitwise-op-name) :args args})

      (let [char-val (int (first expression))]
        (and (< char-val 58) (> char-val 47)))
      {:type :value :val (Integer. expression)}

      :else
      {:type :wire :wire-name expression})))

(def circuit
  (loop [instructions (s/split (slurp "input.txt") #"\n")
         assembly {}]
    (if (empty? instructions)
      assembly
      (let [[expression wire-name] (s/split (first instructions) #"->")
            clean-wire-name (s/trim wire-name)]
        (recur (rest instructions)
               (assoc assembly clean-wire-name
                      (parse-expression (s/trim expression))))))))

(defmulti eval-arg :type)
(def eval-arg-memo (memoize eval-arg))

(defn read-wire [wire-name]
  (let [wire (circuit wire-name)]
    (eval-arg-memo wire)))

(defmethod eval-arg :value [arg] (:val arg))
(defmethod eval-arg :wire [arg] (read-wire (:wire-name arg)))
(defmethod eval-arg :operation [arg]
  (apply (:op arg) (map eval-arg (:args arg))))

(read-wire "a")
