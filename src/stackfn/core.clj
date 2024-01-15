(ns stackfn.core)

(defmacro defstackfn [& body]
  (prn {:env &env :form &form  :body body})
  (let [[*fn args & xs-exprs] body
        result (last xs-exprs)]
    (prn :c result)))

(defstackfn f [!a !b !c]
  !a
  !b
  !c)
