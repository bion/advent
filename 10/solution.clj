(count
 (loop [current-string "1113122113"
        i 0]
   (if (= i 50) current-string
       (let [numbers (map first (re-seq #"(\d)\1*" current-string))
             next-string (apply str (for [cluster numbers]
                                      (str (count cluster) (first cluster))))]
         (recur next-string (inc i))))))
