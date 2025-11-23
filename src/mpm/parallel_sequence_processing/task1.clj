(ns mpm.parallel_sequence_processing.task1
    (:gen-class))

(defn partition-by-size [coll size]
      (lazy-seq
        (when (seq coll)
              (cons (take size coll)
                    (partition-by-size (drop size coll) size)))))

(defn parallel-filter [pred coll n-block]
      (let [parts (partition-by-size coll n-block)
            futures (doall (map (fn [part]
                                    (future (doall (filter pred part))))
                                parts))
            results (map deref futures)]
           (doall (apply concat results))))

(defn busy-pred [x]
      (let [limit 10000]
           (loop [i 0, acc x]
                 (if (< i limit)
                   (recur (inc i) (bit-xor acc (+ (* i 123) (bit-and acc 65535))))
                   (odd? acc)))))

(defn -main []
      (let [data (range 0 100000)
            pred busy-pred
            cores (.availableProcessors (Runtime/getRuntime))
            n-block (quot (count data) cores)]

           (println "Cores:" cores ", block size:" n-block)

           (println "SEQUENTIAL...")
           (time
             (let [res (doall (filter pred data))]
                  (println "Count:" (count res))))

           (println "PARALLEL...")
           (time
             (let [res (doall (parallel-filter pred data n-block))]
                  (println "Count:" (count res))))

           (shutdown-agents)))
(-main)