(def santas-list (clojure.string/split (slurp "input.txt") #"\n"))

(defn check-with [checkers]
  (let [checker (apply juxt checkers)]
    (frequencies (map (fn [line]
                        (every? true? (checker line)))
                      santas-list))))

;; Part 1
(defn has-at-least-three-vowels? [line]
  (let [vowel-count (reduce + ((apply juxt (map #(fn [arg] (arg % 0)) "aeiou"))
                               (frequencies line)))]
    (> vowel-count 2)))

(defn has-repeated-char? [line]
  (boolean (some #(> (count %) 1) (re-find #"(.)\1" line))))

(defn no-naughty-substrings? [line]
  (not (re-find #"ab|cd|pq|xy" line)))

(check-with [has-at-least-three-vowels?
             has-repeated-char?
             no-naughty-substrings?])

;; Part 2
(defn pair-occurs-more-than-once-for [line]
  (fn [pair]
    (let [first-occurence-i (.indexOf line pair)
          remaining (subs line (+ 2 first-occurence-i))
          second-occurence-i (.indexOf remaining pair)]
      (not= -1 second-occurence-i))))

(defn has-multiple-pair-occurences? [line]
  (let [char-pairs (for [x (range (- (count line) 1))]
                     (subs line x (+ x 2)))]
    (boolean (some (pair-occurs-more-than-once-for line) char-pairs))))

(defn has-xyx-pattern? [line]
  (boolean (some #(> (count %) 1) (re-find #"(.).\1" line))))

(check-with [has-multiple-pair-occurences? has-xyx-pattern?])
