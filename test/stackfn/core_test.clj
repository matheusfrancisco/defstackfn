(ns stackfn.core-test
  (:require
   [stackfn.core :refer [defstackfn]]
   [clojure.test :refer [deftest is testing]]))

(defmacro with-captured-stdout [& body]
  `(let [sw# (new java.io.StringWriter)
         pw# (new java.io.PrintWriter sw#)]
     (binding [*out* pw#]
       ~@body
       (.flush pw#)
       (str sw#))))

(deftest testing-defstack-fn
  (testing "testing  f  call should return  24"
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (let  [fcall (defstackfn f [!a !b !c]
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
                    (invoke> * 2)))]

      #_{:clj-kondo/ignore [:unresolved]}
      (is  (= (fcall  1 2 4) 24))))

  (testing  "simple test defining function and call"

    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (defstackfn multiplicate-two-on-stack [!a  !b]
      !a
      !b
      (invoke>  * 2))

    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (is (=  (multiplicate-two-on-stack 3 2)  6)))

  (testing  "capture stdout"

    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (defstackfn f1 [!a]
      !a
      (if>
       !a
       (invoke> println  1)
       else>
       "false!!"))
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (is (= (with-captured-stdout (f1 1))  (str 1 "\n")))))

