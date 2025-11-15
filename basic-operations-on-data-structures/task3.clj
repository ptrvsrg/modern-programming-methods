(ns basic-operations-on-data-structures.task3)

(defn my-map
      "Manipulates a collection of one type and transforms it into a slice of another type"
      [f coll]
      (reverse
        (reduce (fn [acc x]
                    (cons (f x) acc))
                '()
                coll)))

(defn my-filter
      "Iterates over a collection and returns a collection of all the elements the predicate function returns true for"
      [pred coll]
      (reverse
        (reduce (fn [acc x]
                    (if (pred x)
                      (cons x acc)
                      acc))
                '()
                coll)))

(defn -main []
      (println (my-map inc [1 2 3 4]))
      (println (my-map #(* % %) '(1 2 3 4)))
      (println (my-map str [\a \b \c]))

      (println (my-filter odd? [1 2 3 4 5]))
      (println (my-filter even? '(1 2 3 4 5)))
      (println (my-filter #(<= % 3) [1 2 3 4]))

      (println (my-map identity nil))
      (println (my-filter even? nil))
      (println (my-map inc '()))
      (println (my-filter odd? '())))
(-main)
