(ns stackfn.core
  {:clj-kondo/config '{:linters {:ignore true
                                 :unresolved-symbol {:level :off}}}}
  (:require
   [stackfn.lang :as lang]))

(defmacro defstackfn [fn-name args & body]
  `(defn ~fn-name [& args#]
     (-> (lang/read-eval '(~@body)
                         (assoc {:bindings {}
                                 :state []}
                                :bindings (zipmap '~args args#)))
         :state
         last)))

#_(defstackfn fa [!a  !b]
    !a
    !b
    (invoke>  * 2))

; (err) Execution error (IllegalArgumentException) at stackfn.lang/eval60840$fn$G (REPL:60).
; (err) No implementation of method: :eval> of protocol: #'stackfn.lang/IEval found for class: nil
(defstackfn fb [!a  !b]
  !a
  !b
  (invoke>  * 2)
  !v1+
  (if>
   !v1
   !v1
   (invoke> - 2)
   #_else>
   "false!!"
   (invoke> println 1)
   <pop>
   !v1
   !v1
   (invoke> * 2)))

(fb 1 2)

#_(defstackfn f [!a !b !c]
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

#_(f 1 2 4)

#_(comment
    (defstackfn fa [!a  !b]
      !a
      !b
      (invoke>  * 2))

    (fa 2 4)

;
    )

; print false and returns 24
