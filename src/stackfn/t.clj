(ns stackfn.t)

(defmacro defstackfn [fn-name args & body]
  (let [line-numbered-body (map-indexed (fn [idx item]
                                          (with-meta item {:line (inc idx)}))
                                        body)]

    (clojure.pprint/pprint  line-numbered-body)
    `(defn ~fn-name [~@args]
       (+ 1 1)
       #_(let [stack (atom [])
               vars (atom (zipmap (map (fn [arg] (name (second arg))) ~args)
                                  (map first ~args)))
               push (fn [val] (swap! stack conj val))
               pop (fn [] (let [val (peek @stack)]
                            (swap! stack pop)
                            val))
               invoke-fn (fn [f arity]
                           (let [args (reverse (take arity (repeatedly pop)))]
                             (push (apply f args))))
               assign-var (fn [var-name]
                            (swap! vars assoc var-name (pop)))]
           (doseq [op ~line-numbered-body]
             (try
               (cond
                 (symbol? op) (if-let [val (@vars (name op))]
                                (push val)
                                (if (= (last (name op)) \+)
                                  (assign-var (butlast (name op)))
                                  (push op)))
                 (list? op) (let [[_ f arity] op] (invoke-fn f arity))
                 :else (push op))
               (catch Exception e
                 (throw (Exception. (str "Error at line " (meta op :line) ": " (.getMessage e)))))))
           (pop)))))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(defstackfn f [!a !b !c]
  !a ; 1
  !b ; 2
  (invoke> + 2) ;3
  !v1+
  !c
  !c
  <pop>
  2
  (invoke> * 2)
  !v2+
  (invoke> = 2)
  (if>
   !v1
   !v2
   (invoke> - 2)
   else>
   "false!!"
   (invoke> println 1)
   <pop>
   !v1
   !v2
   (invoke> * 2)))

#_{:clj-kondo/ignore [:unresolved]}
(f 1 2 4)
