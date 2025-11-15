(ns numerical-integration.task2)

(defn partial-sums-trapezoid
      [f h]
      (let [xs (map #(* % h) (range))
            ys (map f xs)
            trapezoids (map (fn [y1 y2] (* 0.5 h (+ y1 y2))) ys (rest ys))]
           (reductions + 0 trapezoids)))

(defn trapezoid-integral
      "Returns `F(x) ≈ int_{0}^{x} f(t)dt` using the trapezoidal method with step h."
      [f h]
      (let [partial-sums (partial-sums-trapezoid f h)]
           (fn [x]
               (let [idx (int (/ x h))]
                    (nth partial-sums idx)))))

(defn -main []
      (let [h 0.001
            f1 (fn [t] (* t t))
            f2 (fn [t] (Math/sin t))

            F1 (trapezoid-integral f1 h)
            F2 (trapezoid-integral f2 h)]

           (println "F1(x) = int_{0}^{x} t^2 dt:")
           (time (println "\nF1(10000) ≈" (F1 10000.0)))
           (dotimes [_ 10] (time (println "\nCached F1(10000) ≈" (F1 10000.0))))

           (println "\n\nF2(x) = int_{0}^{x} sin(t) dt:")
           (time (println "\nF2(pi) ≈" (F2 Math/PI)))
           (dotimes [_ 10] (time (println "\nCached F2(pi) ≈" (F2 Math/PI))))))
(-main)