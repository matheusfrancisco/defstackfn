(ns stackfn.lang
  (:require
   [clojure.string :as string]))

(defrecord VarToken [sym])
(defrecord VarDeclareToken [sym])
(defrecord PopFn [])
(defrecord ConstantToken [const])
(defrecord InvokeFn [op arity])
(defrecord IfElseToken [truthy falshy])

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
    (when (and (nil? (first  else'))
               (=  head 'if>))
      (throw (ex-info "Invalid syntax"
                      #_(str "Statement if  missing else branch"
                             xs  \newline
                             "^^^^^^^^")
                      {:cause :invalid-syntax
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
      (if? xs) (let [[[_ & truthy] _ falshy]
                     (partition-by (partial = 'else>) xs)]
                 (IfElseToken. truthy falshy))

      (and (= (first xs) 'invoke>)
           (= (count xs) 3)
           (nat-int? (last xs)))
      (InvokeFn. (second xs) (last xs))
      :else  (prn  xs)))

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

(defn read-eval [expr state]
  (->> expr
       (map read-symbols)
       (reduce
        (fn [prev-state token]
          (eval> token prev-state))
        state)))

(extend-protocol IEval
  ConstantToken
  (eval> [{const :const} state]
    (update state :state conj const))

  IfElseToken
  (eval> [{:keys [truthy falshy]}
          state]
    (let [pop-element (fn [s] (cond-> s (not-empty s) pop))
          new-state (update state :state pop-element)]
      (if (-> (:state state) last boolean)
        (read-eval truthy new-state)
        (read-eval falshy new-state))))

  VarToken
  (eval> [{sym :sym} {:keys [bindings] :as state}]
    (if-let [value (get bindings sym)]
      (update state :state conj value)
      (throw (Exception. (str "Unbound variable " sym)))))

  InvokeFn
  (eval> [{:keys [op arity]}  {:keys [state] :as st}]
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

