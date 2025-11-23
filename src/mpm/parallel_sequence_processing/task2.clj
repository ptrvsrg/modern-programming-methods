(ns mpm.parallel_sequence_processing.task2
    (:gen-class))

(defn lazy-partition [size coll]
      (lazy-seq
        (when-let [block (seq (take size coll))]
                  (cons block (lazy-partition size (drop size coll))))))

(defn lazy-parallel-filter
      ([pred coll n-block]
       (let [parallelism (.availableProcessors (Runtime/getRuntime))]
            (lazy-parallel-filter pred coll n-block parallelism)))
      ([pred coll n-block parallelism]
       (let [process-block (fn [block]
                               (future (doall (filter pred block))))
             chunks (lazy-partition n-block coll)]
            (letfn [(fill-buffer [futs chs]
                                 (loop [f futs, c chs]
                                       (if (and (< (count f) parallelism) (seq c))
                                         (recur (conj f (process-block (first c))) (rest c))
                                         [f c])))
                    (produce [chunks futs]
                             (lazy-seq
                               (let [[new-futs new-chunks] (fill-buffer futs chunks)]
                                    (when (seq new-futs)
                                          (let [first-result @(first new-futs)]
                                               (concat first-result
                                                       (produce new-chunks (subvec new-futs 1))))))))]
                   (produce chunks [])))))

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

           (println "LAZY PARALLEL...")
           (time
             (let [res (doall (lazy-parallel-filter pred data n-block cores))]
                  (println "Count:" (count res))))

           (shutdown-agents)))
(-main)