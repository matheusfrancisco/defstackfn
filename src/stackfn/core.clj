(ns stackfn.core
  (:require
   [stackfn.error :as error]
   [stackfn.lang :as lang]))

(defmacro defstackfn [fn-name args & body]
  `(defn ~fn-name ~args
     (let [state#  {:bindings (zipmap '~args ~args)  :state  []}]
       (try
         (-> (lang/read-eval '(~@body)
                             state#)
             :state
             last)
         (catch  clojure.lang.ExceptionInfo e#
           (let [message# (error/explain '~fn-name '~args  e#)]
             message#))))))
