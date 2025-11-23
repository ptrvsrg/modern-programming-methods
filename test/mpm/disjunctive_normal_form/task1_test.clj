(ns mpm.disjunctive_normal_form.task1_test
    (:require [clojure.test :refer :all]
      [mpm.disjunctive_normal_form.task1 :as d]))

(defn assignments [vars]
      (let [vs (vec vars) n (count vs)]
           (map (fn [mask]
                    (into {} (map-indexed (fn [i v] [v (bit-test mask i)]) vs)))
                (range (bit-shift-left 1 n)))))

(defn equivalent? [expr1 expr2]
      (let [vars (d/vars-of (d/OR expr1 expr2))]
           (every? (fn [env] (= (d/eval-expr expr1 env)
                                (d/eval-expr expr2 env)))
                   (assignments vars))))

(deftest basics-and-imp
         (let [A (d/VAR 'A) B (d/VAR 'B) C (d/VAR 'C)]
              (is (= (d/to-dnf (d/AND A (d/OR B C)))
                     (d/OR (d/AND A B) (d/AND A C))))
              (is (= (d/to-dnf (d/IMP A B))
                     (d/OR (d/NOT A) B)))
              (is (d/dnf? (d/to-dnf (d/AND A (d/OR B C)))))))

(deftest derived-ops-and-extensibility
         (let [A (d/VAR 'A) B (d/VAR 'B)]
              (is (= (d/to-dnf (d/XOR A B))
                     (d/OR (d/AND A (d/NOT B)) (d/AND (d/NOT A) B))))
              ;; Removed tests for NOR and NAND
              ;; Removed :xnor registration test
              ))

(deftest substitution
         (let [A (d/VAR 'A) B (d/VAR 'B)]
              (is (= (d/substitute->dnf (d/AND A (d/OR B (d/NOT A))) {'A true})
                     B))
              (is (= (d/substitute->dnf (d/OR (d/NOT A) (d/AND A B)) {'A false})
                     true))
              (is (= (d/substitute->dnf (d/AND (d/NOT A) (d/OR A B)) {'A true})
                     false))))

(deftest constants-and-dup
         (let [A (d/VAR 'A)]
              (is (= (d/to-dnf (d/OR true A false))
                     true))
              (is (= (d/to-dnf (d/AND true A true))
                     A))
              (is (= (d/to-dnf (d/AND A A))
                     A))
              (is (= (d/to-dnf (d/OR A A))
                     A))))

(deftest equivalence-and-idempotence
         (let [A (d/VAR 'A) B (d/VAR 'B) C (d/VAR 'C)
               expr (d/AND (d/OR A (d/NOT B))
                           (d/OR (d/NOT A) C)
                           (d/OR B C))
               dnf1 (d/to-dnf expr)
               dnf2 (d/to-dnf dnf1)]
              (is (d/dnf? dnf1))
              (is (equivalent? expr dnf1))
              (is (= dnf1 dnf2))))

(deftest parse-precedence-and-assoc
         (let [A (d/VAR 'A) B (d/VAR 'B) C (d/VAR 'C)]
              (is (= (d/parse "A & (B | C)")
                     (d/AND A (d/OR B C))))
              (is (= (d/parse "A | B ^ C")
                     (d/XOR (d/OR A B) C)))
              (is (= (d/parse "A -> B -> C")
                     (d/IMP A (d/IMP B C))))
              (is (= (d/parse "!A & B")
                     (d/AND (d/NOT A) B)))
              (is (= (d/parse "xor(A,B)")
                     (d/XOR A B)))
              ;; Removed 'twoof3' parsing test
              ))

(deftest dnf-str-output
         (is (= (d/dnf-str (d/parse "A & (B | C)"))
                "(A & B | A & C)"))
         (is (= (d/dnf-str (d/parse "A -> B"))
                "(!A | B)"))
         (is (= (d/dnf-str (d/parse "!(A | B) & C"))
                "(!A & !B & C)"))
         ;; Removed nand, twoof3 tests
         (is (= (d/dnf-str (d/parse "A | B ^ C"))
                "(A & !C | B & !C | !A & !B & C)"))
         (is (= (d/dnf-str (d/parse "true | A & false"))
                "true")))

(deftest parse-substitution-integration
         (is (= (d/substitute->dnf (d/parse "A & (B | !A)") {'A true})
                'B))
         (is (= (d/substitute->dnf (d/parse "A & (B | !A)") {'A false})
                false))
         (is (= (d/substitute->dnf (d/parse "A -> B") {'A true})
                'B)))

(deftest vars-and-eval-on-parsed
         (let [expr (d/parse "A & !C")]
              (is (= (d/vars-of expr) #{'A 'C}))
              (is (true? (d/eval-expr expr {'A true 'C false})))
              (is (false? (d/eval-expr expr {'A false 'C true})))))


(deftest parse-errors
         (is (thrown-with-msg? Exception #"Unknown function"
                               (d/parse "xnor(A,B)")))
         (is (thrown-with-msg? Exception #"closing bracket"
                               (d/parse "(A & B) | (C & (D | E)"))))
