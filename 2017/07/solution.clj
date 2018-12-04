(def raw-input (-> "input.txt" slurp (clojure.string/split #"\n")))

(defn make-node-record [text-entry]
  (let [node-name (first (clojure.string/split text-entry #"\s"))
        node-weight (Integer/parseInt (second (re-find #"\((\d+)\)" text-entry)) 10)
        children (-> text-entry (clojure.string/split #"->"))
        children (second children)
        children (if children
                   (into (set {})
                         (map #(keyword (clojure.string/trim %))
                              (clojure.string/split children #","))))]
    {:name (keyword node-name)
     :weight node-weight
     :children (or children (set {}))}))

(defn assoc-parents [col node]
  (reduce
   (fn [result child-name] (assoc result child-name node))
   col
   (:children node)))

(def parent-lookup (reduce
                    (fn [col node]
                      (assoc-parents col node))
                    {}
                    (map make-node-record raw-input)))

;; answer one

(first (filter (fn [node-name] (nil? (parent-lookup node-name)))
               (map :name node-list)))

;; answer two

(def child-lookup (reduce
                   (fn [col row]
                     (let [node (make-node-record row)]
                       (assoc col (:name node) node)))
                   {}
                   raw-input))

(defn check-balance [current-node-name]
  (let [current-node (child-lookup current-node-name)
        children (current-node :children)
        child-weights (map check-balance children)
        grouped-weights (sort (map first (group-by identity child-weights)))
        weight (:weight current-node)]

    (cond
      (empty? children) weight
      (> (count grouped-weights) 1)
      (let [bad-weight (second grouped-weights)
            bad-child-index (.indexOf (into [] child-weights) bad-weight)
            bad-child-name ((into [] children) bad-child-index)
            bad-child (child-lookup bad-child-name)
            bad-child-weight (bad-child :weight)]
        (throw (Error. (str "answer is "
                            (+ bad-child-weight (apply - grouped-weights))))))

      :else
      (reduce + (conj child-weights weight)))))

(check-balance :veboyvy)
