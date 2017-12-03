(defn md5 [input]
  (apply str
         (map (partial format "%02x")
              (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                         .reset
                         (.update (.getBytes input)))))))

(defn find-coin [padding-length]
  (loop [candidate 0]
    (let [result (md5 (str "iwrupvqb" candidate))]
      (if (= (clojure.string/join (replicate padding-length "0"))
             (subs result 0 padding-length)) candidate
          (recur (inc candidate))))))

(prn (find-coin 5)) ;; Part 1
(prn (find-coin 6)) ;; Part 2
