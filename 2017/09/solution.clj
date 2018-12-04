(def input (drop-last (seq (slurp "input.txt"))))

(defn skip-garbage [in-seq]
  (cond
    (= (first in-seq) \!)
    (skip-garbage (-> in-seq rest rest))

    (= (first in-seq) \>)
    (rest in-seq)

    :else
    (skip-garbage (rest in-seq))))

(defn count-groups [in-seq depth score]
  (let [current (first in-seq)]
    (case current
      \{ (count-groups (rest in-seq) (inc depth) score)
      \} (count-groups (rest in-seq) (dec depth) (+ score depth))
      \, (count-groups (rest in-seq) depth score)
      \< (count-groups (skip-garbage (rest in-seq)) depth score)
      \! (count-groups (-> in-seq rest rest) depth score)
      nil score)))

(count-groups input 0 0) ;; baaaah stack overflow

(defn count-groups [in-seq depth score]
  (loop [[in-seq depth score] [in-seq depth depth score score]]
    (let [current (first in-seq)
          args (case current
                 \{ [(rest in-seq) (inc depth) score]
                 \} [(rest in-seq) (dec depth) (+ score depth)]
                 \, [(rest in-seq) depth score]
                 \< [(skip-garbage (rest in-seq)) depth score]
                 \! [(-> in-seq rest rest) depth score]
                 nil nil)]
      (if args (recur args) score))))

;; answer one

(count-groups input 0 0)

;; answer two

(def skipped-count (atom 0))

(defn skip-garbage-count [in-seq]
  (cond
    (= (first in-seq) \!)
    (skip-garbage-count (-> in-seq rest rest))

    (= (first in-seq) \>)
    (rest in-seq)

    :else
    (do
      (swap! skipped-count inc)
      (skip-garbage-count (rest in-seq)))))

(defn count-groups [in-seq depth score]
  (loop [[in-seq depth score] [in-seq depth depth score score]]
    (let [current (first in-seq)
          args (case current
                 \{ [(rest in-seq) (inc depth) score]
                 \} [(rest in-seq) (dec depth) (+ score depth)]
                 \, [(rest in-seq) depth score]
                 \< [(skip-garbage-count (rest in-seq)) depth score]
                 \! [(-> in-seq rest rest) depth score]
                 nil nil)]
      (if args (recur args) score))))

(count-groups input 0 0)
@skipped-count
