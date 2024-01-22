(ns stackfn.lang
  (:require
   [clojure.string :as string]))

;;tokens
(defrecord VarToken [sym])
(defrecord VarDeclareToken [sym])
(defrecord PopFn [])
(defrecord ConstantToken [const])
(defrecord InvokeFn [op arity])
(defrecord IfElseToken [truthy falsely])

;;read parsing
(defprotocol ReadSymbols
  (read-symbols [sym]))

(defn var? [sym]
  (and (string/starts-with? (name sym) "!")
       (> (-> sym name (string/replace "+" "") count) 1)))

(defn assigned-var? [sym]
  (and (var? sym)
       (string/ends-with? (name sym) "+")))

(defn if? [xs]
  (let [[[head & truthy] else' f]
        (partition-by (partial = 'else>) xs)]
    (when (or (and (nil? (first  else'))
                   (=  head 'if>))
              (and (first else')
                   (nil? head)))

      (throw (ex-info (or "Statement  missing branch"  (if (first else')
                                                         "else"
                                                         "if"))
                      {:cause :invalid-statement
                       :message (or "Statement  missing branch"  (if (first else')
                                                                   "else"
                                                                   "if"))
                       :form  (str xs)})))

    (and (= head 'if>)
         (seq truthy)
         (seq f))))

(extend-protocol ReadSymbols
  Object
  (read-symbols [const]
    (ConstantToken. const))

  clojure.lang.PersistentList
  (read-symbols [xs]
    (cond
      (if? xs) (let [[[_ & truthy] _ falsely]
                     (partition-by (partial = 'else>) xs)]
                 (IfElseToken. truthy falsely))

      (and (= (first xs) 'invoke>)
           (= (count xs) 3)
           (nat-int? (last xs)))
      (InvokeFn. (second xs) (last xs))))

  clojure.lang.Symbol
  (read-symbols [sym]
    (cond
      (=  sym '<pop>) (PopFn.)
      (assigned-var? sym) (VarDeclareToken. (->> sym
                                                 name
                                                 butlast
                                                 (apply str)
                                                 symbol))
      (var? sym) (VarToken. sym))))

;;eval
(defprotocol IEval
  (eval> [token state]))

(defn read! [expr]
  (map (fn [x] (read-symbols x)) expr))

(defn eval! [tokens state]
  (reduce #(eval> %2 %1) state  tokens))

(defn read-eval [expr state]
  (eval! (read! expr) state))

(extend-protocol IEval
  ConstantToken
  (eval> [{const :const} state]
    (update state :state conj const))

  IfElseToken
  (eval> [{:keys [truthy falsely]}
          state]

    (let [pop-element (fn [s] (cond-> s (not-empty s) pop))
          new-state (update state :state pop-element)]
      (if (-> (:state state) last boolean)
        (read-eval truthy new-state)
        (read-eval falsely new-state))))

  VarToken
  (eval> [{sym :sym} {:keys [bindings] :as state}]
    (if-let [value (get bindings sym)]
      (update state :state conj value)
      (throw (ex-info "Unbounded variable"
                      {:cause :unbounded-variable
                       :message "Unbound variable"
                       :form sym}))))

  InvokeFn
  (eval> [{:keys [op arity]}  {:keys [state] :as st}]
    (when (> arity
             (count state))
      (throw (ex-info "Unbounded variable"
                      {:cause :invalid-stack
                       :message (str  "Stack should have number of "  arity)
                       :form  '(invoke> ...)})))

    (-> st
        (update :state #(->> %  (drop-last arity) vec))
        (update :state conj (apply (resolve op)
                                   (take-last arity  state)))))

  VarDeclareToken
  (eval> [{sym :sym} {:keys [state] :as st}]
    (update st :bindings assoc sym (last state)))

  PopFn
  (eval> [_ state]
    (let [pop-element (fn [s] (cond-> s (not-empty s) pop))]
      (update state :state pop-element))))


