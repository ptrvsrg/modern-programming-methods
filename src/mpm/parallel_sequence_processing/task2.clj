(ns mpm.parallel_sequence_processing.task2
    (:gen-class))

(defn lazy-partition-by-size [size coll]
      (lazy-seq
        (when-let [block (seq (take size coll))]
                  (cons block (lazy-partition-by-size size (drop size coll))))))

(defn lazy-parallel-filter
      ([pred coll n-block]
       (let [parallelism (.availableProcessors (Runtime/getRuntime))]
            (lazy-parallel-filter pred coll n-block parallelism)))
      ([pred coll n-block parallelism]
       (let [process-block (fn [block]
                               (future (doall (filter pred block))))]
            (letfn [
                    (drain-futures [futs-vec acc]
                                   (if (seq futs-vec)
                                     (let [first-result @(first futs-vec)]
                                          (recur (subvec futs-vec 1)
                                                 (lazy-cat acc first-result)))
                                     acc))
                    (parallel-process [chunks-seq]
                                      (lazy-seq
                                        (let [block-batch (take parallelism chunks-seq)
                                              remaining (drop parallelism chunks-seq)]
                                             (when (seq block-batch)
                                                   (let [futures-batch (mapv process-block block-batch)
                                                         results (drain-futures futures-batch (lazy-seq []))]
                                                        (lazy-cat results (parallel-process remaining)))))))]
                   (parallel-process (lazy-partition-by-size n-block coll))))))

(defn busy-pred [x]
      (let [limit 10000]
           (loop [i 0, acc x]
                 (if (< i limit)
                   (recur (inc i) (bit-xor acc (+ (* i 123) (bit-and acc 65535))))
                   (odd? acc)))))

(defn -main []
      (let [data (range)
            pred busy-pred
            cores (.availableProcessors (Runtime/getRuntime))
            n-block 10000]

           (println "Cores:" cores ", block size:" n-block)

           (println "SEQUENTIAL...")
           (time
             (let [res (take 100000 (filter pred data))]
                  (println "Count:" (count res))))

           (println "LAZY PARALLEL...")
           (time
             (let [res (take 100000 (lazy-parallel-filter pred data n-block cores))]
                  (println "Count:" (count res))))

           (shutdown-agents)))
(-main)