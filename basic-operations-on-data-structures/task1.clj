(ns basic-operations-on-data-structures.task1)

(defn permutations [alphabet n]
      (cond
        (neg? n) (throw (IllegalArgumentException. "n cannot be negative"))
        (zero? n) '("")
        :else (for [p (permutations alphabet (dec n))
                    ch alphabet
                    :when (or (empty? p) (not= (last p) ch))]
                   (str p ch))))

(defn -main []
      (println (permutations "abc" 0))
      (println (permutations "abc" 1))
      (println (permutations "abc" 2))
      (println (permutations "abc" 3))
      (println (permutations "abc" 4)))
(-main)