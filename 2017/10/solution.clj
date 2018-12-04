(def rotation-lengths
  (map #(Integer/parseInt (clojure.string/trim %) 10)
       (clojure.string/split
        (slurp "input.txt")
        #",")))

(def digest-size 256)

(defn rotate [digest base length]
  (let [length (- length 1)]
    (loop [digest digest
           offset 0]
      (if (< (/ length 2) offset)
        digest
        (let [left (mod (+ base offset) digest-size)
              right (mod (- (+ base length) offset) digest-size)
              tmp (digest left)]
          (recur (assoc digest left (digest right) right tmp)
                 (inc offset)))))))

;; answer one

(loop [digest (into [] (range 256))
       offset 0
       skip-size 0
       remaining-lengths rotation-lengths]
  (let [current-length (first remaining-lengths)]
    (if current-length
      (recur (rotate digest offset current-length)
             (+ offset current-length skip-size)
             (inc skip-size)
             (rest remaining-lengths))
      (* (first digest) (second digest)))))

;; answer two

(def rotation-lengths
  (concat (->> "input.txt"
               slurp
               seq
               drop-last
               (map int))
          '(17 31 73 47 23)))

(loop [digest (into [] (range 256))
       offset 0
       skip-size 0
       remaining-lengths rotation-lengths]
  (let [current-length (first remaining-lengths)]
    (if current-length
      (recur (rotate digest offset current-length)
             (+ offset current-length skip-size)
             (inc skip-size)
             (rest remaining-lengths))
      (* (first digest) (second digest)))))
