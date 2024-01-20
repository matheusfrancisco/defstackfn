(ns stackfn.core
  {:clj-kondo/config '{:linters {:ignore true
                                 :unresolved-symbol {:level :off}}}}
  (:require
   [stackfn.lang :as lang]))

(defmacro defstackfn [fn-name args & body]
  `(defn ~fn-name [& args#]
     (try
       (-> (lang/read-eval '(~@body)
                           (assoc {:bindings {}
                                   :state []}
                                  :bindings (zipmap '~args args#)))
           :state
           last)
       (catch Exception e#
         (let [s#  (first (.getStackTrace e#))]
           (prn (.getLineNumber s#)))))))

#_(defstackfn fa [!a  !b]
    !a
    !b
    (invoke>  * 2))

(defstackfn f [!a !b !c]
  !a
  !b
  (invoke> + 2)
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

(f 1 2 4)
