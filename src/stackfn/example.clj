(ns stackfn.example
  (:require
   [stackfn.core :refer [defstackfn]]))

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
