(ns basic-operations-on-data-structures.task2)

(defn permutations-tail [alphabet n]
      (cond
        (neg? n) (throw (IllegalArgumentException. "n cannot be negative"))
        :else
        (loop [k n
               acc '("")]
              (if (zero? k)
                acc
                (recur (dec k)
                       (mapcat (fn [p]
                                   (for [ch alphabet
                                         :when (or (empty? p)
                                                   (not= (last p) ch))]
                                        (str p ch)))
                               acc))))))

(defn -main []
      (println (permutations-tail "abc" 0))
      (println (permutations-tail "abc" 1))
      (println (permutations-tail "abc" 2))
      (println (permutations-tail "abc" 3))
      (println (permutations-tail "abc" 4)))
(-main)