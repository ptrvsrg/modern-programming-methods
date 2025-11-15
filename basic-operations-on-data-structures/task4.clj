(ns basic-operations-on-data-structures.task4)

(defn extend-one
      [alphabet s]
      (let [allowed (if (empty? s)
                      alphabet
                      (filter #(not= (last s) %) alphabet))]
           (map #(str s %) allowed)))

(defn extend-all
      [alphabet acc]
      (loop [xs acc
             res []]
            (if (empty? xs)
              res
              (let [p (first xs)
                    exts (extend-one alphabet p)]
                   (recur (rest xs) (into res exts))))))

(defn permutations-tail
      [alphabet n]
      (cond
        (neg? n) (throw (IllegalArgumentException. "n cannot be negative"))
        :else
        (loop [step n
               acc '("")]
              (if (zero? step)
                (apply list acc)
                (recur (dec step) (extend-all alphabet acc))))))


(defn -main []
      (println (permutations-tail "abc" 0))
      (println (permutations-tail "abc" 1))
      (println (permutations-tail "abc" 2))
      (println (permutations-tail "abc" 3))
      (println (permutations-tail "abc" 4)))
(-main)
