(ns numerical-integration.task1)

(defn trapezoid-integral
      "Returns `F(x) ≈ int_{0}^{x} f(t)dt` using the trapezoidal method with step h."
      [f h]
      (let [integrate
            (fn [x]
                (let [n (int (/ x h))
                      xs (map #(* % h) (range (inc n)))
                      ys (map f xs)
                      areas (map (fn [y1 y2]
                                     (* 0.5 h (+ y1 y2)))
                                 ys (rest ys))]
                     (reduce + 0 areas)))
            memo-integrate (memoize integrate)]
           memo-integrate))

(defn -main []
      (let [h 0.001
            f1 (fn [t] (* t t))
            f2 (fn [t] (Math/sin t))

            F1 (trapezoid-integral f1 h)
            F2 (trapezoid-integral f2 h)]

           (println "F1(x) = int_{0}^{x} t^2 dt:")
           (time (println "F1(10000) ≈" (F1 10000.0)))
           (dotimes [_ 10] (time (println "Cached F1(10000) ≈" (F1 10000.0))))

           (println "\nF2(x) = int_{0}^{x} sin(t) dt:")
           (time (println "F2(pi) ≈" (F2 Math/PI)))
           (dotimes [_ 10] (time (println "Cached F2(pi) ≈" (F2 Math/PI))))))
(-main)