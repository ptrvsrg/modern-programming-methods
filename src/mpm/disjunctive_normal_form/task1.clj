(ns mpm.disjunctive_normal_form.task1
    (:require [clojure.set :as set]
      [clojure.string :as str]))

(defn VAR [sym] sym)
(defn BOOL [b] (boolean b))
(defn NOT [x] {:op :not :args [x]})
(defn AND [& xs] {:op :and :args (vec xs)})
(defn OR [& xs] {:op :or :args (vec xs)})
(defn IMP [a b] {:op :imp :args [a b]})
(defn XOR [a b] {:op :xor :args [a b]})

(defn op-node? [e] (and (map? e) (contains? e :op)))
(defn op-of [e] (when (op-node? e) (:op e)))
(defn args-of [e] (when (op-node? e) (:args e)))

(def ^:private derived-reg (atom {}))

(defn register-derived-op!
      "Register transformation of derived operation k to basic (and/or/not)."
      [k lower-fn]
      (swap! derived-reg assoc k lower-fn))

(register-derived-op! :xor    (fn [[a b]] (OR (AND a (NOT b)) (AND (NOT a) b))))

(declare desugar simplify nnf nnf-neg)

(defn desugar
      "Lower derived operations (:imp and registered) to basic :and/:or/:not."
      [e]
      (cond
        (boolean? e) e
        (symbol? e) e
        (op-node? e)
        (let [op (op-of e) as (map desugar (args-of e))]
             (case op
                   :imp (desugar (OR (NOT (first as)) (second as)))
                   :not (NOT (first as))
                   :and (apply AND as)
                   :or (apply OR as)
                   (if-let [f (@derived-reg op)]
                           (desugar (f as))
                           (throw (ex-info "Unknown op" {:op op}))))
             )
        :else (throw (ex-info "Bad expr" {:expr e}))))

(defn subst
      "Substitute env (symbol→bool). Symbols not in env remain variables."
      [e env]
      (cond
        (boolean? e) e
        (symbol? e) (if (contains? env e) (BOOL (env e)) e)
        (op-node? e) (let [as (map #(subst % env) (args-of e))]
                          {:op (op-of e) :args (vec as)})
        :else (throw (ex-info "Bad expr" {:expr e}))))

(defn flatten-op
      "Flatten nested same-type operations (associativity)."
      [k xs]
      (mapcat (fn [x] (if (and (op-node? x) (= (op-of x) k)) (args-of x) [x])) xs))

(defn simplify
      "Local simplifications: constants, ¬¬X, flatten, dedup."
      [e]
      (cond
        (boolean? e) e
        (symbol? e) e
        (op-node? e)
        (let [op (op-of e) as (map simplify (args-of e))]
             (case op
                   :not (let [x (first as)]
                             (cond
                               (boolean? x) (not x)
                               (and (op-node? x) (= (op-of x) :not)) (simplify (first (args-of x)))
                               :else (NOT x)))
                   :and (let [xs (flatten-op :and as) xs (distinct xs) xs (remove true? xs)]
                             (if (some false? xs) false
                                                  (case (count xs) 0 true 1 (first xs) (apply AND xs))))
                   :or (let [xs (flatten-op :or as) xs (distinct xs) xs (remove false? xs)]
                            (if (some true? xs) true
                                                (case (count xs) 0 false 1 (first xs) (apply OR xs))))
                   :imp (simplify (OR (NOT (first as)) (second as)))
                   (simplify (desugar {:op op :args (vec as)}))))
        :else (throw (ex-info "Bad expr" {:expr e}))))

(defn nnf
      "Negation Normal Form (¬ only on variables/constants)."
      [e]
      (let [e (simplify (desugar e))]
           (cond
             (boolean? e) e
             (symbol? e) e
             (op-node? e)
             (case (op-of e)
                   :not (nnf-neg (first (args-of e)))
                   :and (apply AND (map nnf (args-of e)))
                   :or (apply OR (map nnf (args-of e)))
                   (throw (ex-info "Unexpected op in NNF" {:op (op-of e)})))
             :else (throw (ex-info "Bad expr" {:expr e})))))

(defn nnf-neg
      "NNF for expression under external negation."
      [e]
      (let [e (simplify e)]
           (cond
             (boolean? e) (not e)
             (symbol? e) (NOT e)
             (op-node? e)
             (case (op-of e)
                   :not (nnf (first (args-of e)))
                   :and (apply OR (map nnf-neg (args-of e)))
                   :or (apply AND (map nnf-neg (args-of e)))
                   (throw (ex-info "Unexpected op in nnf-neg" {:op (op-of e)})))
             :else (throw (ex-info "Bad expr" {:expr e})))))

(defn literal?
      "Literal: Var, ¬Var, or boolean constant."
      [e]
      (or (symbol? e)
          (and (op-node? e) (= (op-of e) :not) (symbol? (first (args-of e))))
          (boolean? e)))

(defn clauses
      "NNF → list of conjuncts (each a list of literals) for DNF."
      [e]
      (let [e (simplify e)]
           (cond
             (= e true) [[]]
             (= e false) []
             (literal? e) [[e]]
             (and (op-node? e) (= (op-of e) :or)) (into [] (mapcat clauses (args-of e)))
             (and (op-node? e) (= (op-of e) :and))
             (reduce (fn [acc sub] (for [c1 acc, c2 (clauses sub)] (concat c1 c2)))
                     [[]] (args-of e))
             :else (throw (ex-info "Expected NNF node" {:expr e})))))

(defn lit-key [l] (if (symbol? l) [:pos l] [:neg (first (args-of l))]))

(defn simplify-conj
      "Simplify one conjunct: remove duplicates, check A ∧ ¬A, sort."
      [conj]
      (let [pos (set (map second (filter #(= :pos (first %)) (map lit-key conj))))
            neg (set (map second (filter #(= :neg (first %)) (map lit-key conj))))]
           (when (empty? (set/intersection pos neg))
                 (->> conj distinct
                      (sort-by (fn [l] (let [[sgn v] (lit-key l)] [v (if (= sgn :pos) 0 1)])))))))

(defn build-dnf
      "Build DNF AST from list of conjuncts."
      [cls]
      (let [cls (keep simplify-conj cls) cls (distinct cls)]
           (cond
             (empty? cls) false
             (some empty? cls) true
             (= 1 (count cls)) (apply AND (first cls))
             :else (apply OR (map #(apply AND %) cls)))))

(defn to-dnf
      "expr → (desugar → simplify → nnf → clauses → build-dnf → simplify)."
      [expr]
      (-> expr desugar simplify nnf clauses build-dnf simplify))

(defn substitute->dnf
      "Substitute env and convert to DNF."
      [expr env]
      (to-dnf (subst expr env)))

(defn dnf?
      "Check if expression is in DNF."
      [e]
      (let [e (simplify e)]
           (cond
             (boolean? e) true
             (literal? e) true
             (and (op-node? e) (= (op-of e) :and)) (every? literal? (args-of e))
             (and (op-node? e) (= (op-of e) :or))
             (every? (fn [t] (or (literal? t)
                                 (and (op-node? t) (= (op-of t) :and) (every? literal? (args-of t)))))
                     (args-of e))
             :else false)))

(defn vars-of
      "Set of variables (symbols) in expression."
      [e]
      (cond
        (boolean? e) #{}
        (symbol? e) #{e}
        (op-node? e) (apply set/union (map vars-of (args-of e)))
        :else #{}))

(defn eval-expr
      "Evaluate expression in env (after desugar)."
      [expr env]
      (let [e (desugar expr)]
           (cond
             (boolean? e) e
             (symbol? e) (boolean (env e))
             (op-node? e)
             (case (op-of e)
                   :not (not (eval-expr (first (args-of e)) env))
                   :and (every? true? (map #(eval-expr % env) (args-of e)))
                   :or (boolean (some true? (map #(eval-expr % env) (args-of e))))
                   (throw (ex-info "Unexpected op in eval" {:op (op-of e)})))
             :else (throw (ex-info "Bad expr in eval" {:expr e})))))

(def ^:private prec
  "Precedence for printing (higher = tighter)."
  {:lit 5 :not 4 :and 3 :or 2 :xor 1 :imp 0})

(defn- op->pp [op]
       (case op
             :not "!"
             :and " & "
             :or " | "
             :xor " ^ "
             :imp " -> "
             "?"))

(defn pp
      "Return infix string for expr (minimal parentheses)."
      ([expr] (pp expr (:lit prec)))
      ([expr ctxp]
       (cond
         (true? expr) "true"
         (false? expr) "false"
         (symbol? expr) (name expr)
         (op-node? expr)
         (let [op (op-of expr) as (args-of expr)]
              (case op
                    :not (let [s (pp (first as) (:not prec))]
                              (str "!" (if (< (if (op-node? (first as)) (prec (op-of (first as))) (:lit prec))
                                              (:not prec))
                                         (str "(" s ")")
                                         s)))
                    :and (let [p (:and prec)
                               parts (map #(let [s (pp % p)
                                                 subp (if (op-node? %) (prec (op-of %)) (:lit prec))]
                                                (if (< subp p) (str "(" s ")") s))
                                          as)
                               s (str/join (op->pp :and) parts)]
                              (if (< (:and prec) ctxp) (str "(" s ")") s))
                    :or (let [p (:or prec)
                              parts (map #(let [s (pp % p)
                                                subp (if (op-node? %) (prec (op-of %)) (:lit prec))]
                                               (if (< subp p) (str "(" s ")") s))
                                         as)
                              s (str/join (op->pp :or) parts)]
                             (if (< (:or prec) ctxp) (str "(" s ")") s))
                    :xor (let [lhs (first as) rhs (second as)
                               p (:xor prec)
                               L (pp lhs p) R (pp rhs (dec p))
                               Lp (if (op-node? lhs) (prec (op-of lhs)) (:lit prec))
                               Rp (if (op-node? rhs) (prec (op-of rhs)) (:lit prec))
                               Ls (if (< Lp p) (str "(" L ")") L)
                               Rs (if (<= Rp (dec p)) (str "(" R ")") R)
                               s (str Ls (op->pp :xor) Rs)]
                              (if (< p ctxp) (str "(" s ")") s))
                    :imp (let [lhs (first as) rhs (second as)
                               p (:imp prec)
                               L (pp lhs (inc p)) R (pp rhs p)
                               s (str L (op->pp :imp) R)]
                              (if (< p ctxp) (str "(" s ")") s))
                    (pp (desugar expr) ctxp)))
         :else (str expr))))

(defn dnf-str
      "Return DNF string for expr."
      [expr]
      (pp (to-dnf expr)))

(defn parse
      "Parse infix string s into our AST.
       Supported: !, &, |, ^, ->, brackets, identifiers A,B,C..., and calls:
       and(...), or(...), not(x), xor(a,b)."
      [^String s]
      (let [len (.length s)
            idx (atom 0)
            peekc (fn [] (when (< @idx len) (.charAt s @idx)))
            skip-ws (fn [] (while (and (< @idx len)
                                       (Character/isWhitespace (.charAt s @idx)))
                                  (swap! idx inc)))
            accept (fn [^String t]
                       (skip-ws)
                       (let [i @idx L (.length t)]
                            (if (and (<= (+ i L) len)
                                     (= (.substring s i (+ i L)) t))
                              (do (swap! idx #(+ % L)) true)
                              false)))
            parse-ident (fn []
                            (skip-ws)
                            (let [i @idx]
                                 (when (and (< @idx len)
                                            (let [c (.charAt s @idx)]
                                                 (or (Character/isLetter c) (= \_ c))))
                                       (swap! idx inc)
                                       (while (and (< @idx len)
                                                   (let [c (.charAt s @idx)]
                                                        (or (Character/isLetterOrDigit c) (= \_ c))))
                                              (swap! idx inc))
                                       (.substring s i @idx))))]
           (letfn [(parse-expr [] (parse-imp))
                   (parse-imp []
                              (let [lhs (parse-xor)]
                                   (skip-ws)
                                   (if (accept "->")
                                     (IMP lhs (parse-imp))
                                     lhs)))
                   (parse-xor []
                              (loop [lhs (parse-or)]
                                    (skip-ws)
                                    (if (accept "^")
                                      (recur (XOR lhs (parse-or)))
                                      lhs)))
                   (parse-or []
                             (loop [lhs (parse-and)]
                                   (skip-ws)
                                   (if (accept "|")
                                     (recur (OR lhs (parse-and)))
                                     lhs)))
                   (parse-and []
                              (loop [lhs (parse-unary)]
                                    (skip-ws)
                                    (if (accept "&")
                                      (recur (AND lhs (parse-unary)))
                                      lhs)))
                   (parse-unary []
                                (skip-ws)
                                (cond
                                  (accept "!") (NOT (parse-unary))
                                  (accept "(") (let [e (parse-expr)]
                                                    (when-not (accept ")")
                                                              (throw (ex-info "Expected closing bracket ')'" {:pos @idx})))
                                                    e)
                                  :else (parse-primary)))
                   (parse-args []
                               (skip-ws)
                               (let [args (transient [])]
                                    (if (accept ")")
                                      (persistent! args)
                                      (do
                                        (conj! args (parse-expr))
                                        (loop []
                                              (skip-ws)
                                              (cond
                                                (accept ",") (do (conj! args (parse-expr))
                                                                 (recur))
                                                (accept ")") (persistent! args)
                                                :else (throw (ex-info "Expected ',' or ')' in argument list" {:pos @idx}))))))))
                   (parse-primary []
                                  (skip-ws)
                                  (let [name (parse-ident)]
                                       (when-not name
                                                 (throw (ex-info "Expected identifier/variable" {:pos @idx})))
                                       (skip-ws)
                                       (if (accept "(")
                                         (let [args (parse-args)
                                               lname (str/lower-case name)]
                                              (case lname
                                                    "not" (NOT (first args))
                                                    "and" (apply AND args)
                                                    "or" (apply OR args)
                                                    "xor" (apply XOR args)
                                                    "imp" (apply IMP  args)
                                                    (throw (ex-info (str "Unknown function: " lname) {:name lname}))))
                                         (case (str/lower-case name)
                                               "true" true
                                               "false" false
                                               (symbol name)))))]
                  (let [res (parse-expr)]
                       (skip-ws)
                       (when (< @idx len)
                             (throw (ex-info "Extra characters at end of input" {:pos @idx})))
                       res))))

(defn -main
      "Interactive run: input infix string → print DNF."
      [& _args]
      (println "Operators: !  &  |  ^  ->  and parentheses ().")
      (println "Enter a boolean expression.")
      (while true
             (println "")
             (print "Input     : ") (flush)
             (if-let [line (read-line)]
                     (try
                       (let [expr (parse line)
                             dnf (to-dnf expr)]
                            (println "AST       :" expr)
                            (println "AST (DNF) :" dnf)
                            (println "DNF       :" (dnf-str expr)))
                       (catch Exception e
                         (println "Error:" (.getMessage e))))
                     (println "Empty input."))))
(-main)
