(ns stackfn.lang-test
  (:require
   [clojure.test :as t :refer [deftest is testing do-report]]
   [stackfn.lang :as l]))

(defmethod t/assert-expr 'thrown-with-data? [msg form]
  (let [data (second form)
        body (nthnext form 2)]
    `(try ~@body
          (do-report {:type :fail, :message ~msg,
                      :expected '~form, :actual nil})
          (catch clojure.lang.ExceptionInfo e#
            (let [expected# ~data
                  actual# (ex-data e#)]
              (if (= expected# actual#)
                (do-report {:type :pass, :message ~msg,
                            :expected expected#, :actual actual#})
                (do-report {:type :fail, :message ~msg,
                            :expected expected#, :actual actual#})))
            e#))))

(deftest read-test
  (testing  " should return the  list of tokens"
    (is (=
         (l/read!
          '(!a  !b  !c  (invoke> * 2)))
         (list (l/->VarToken  '!a)  (l/->VarToken  '!b) (l/->VarToken  '!c)
               (l/->InvokeFn '* 2)))))

  (testing  "should  return  the list of tokens with statements"
    (is (=
         (l/read!
          '(!a  !b  !c  (if> !a !c else>  "false!")))
         (list (l/->VarToken  '!a)  (l/->VarToken  '!b) (l/->VarToken  '!c)
               (l/->IfElseToken  '(!a  !c) '("false!")))))))

(deftest eval-state-test
  (testing "testing state is adding values"
    (let [state {:bindings  {'!a 1 '!b 2  '!c 3}
                 :state  []}]
      (is (= (l/eval! (list (l/->VarToken  '!a)  (l/->VarToken  '!b))
                      state)
             {:bindings  {'!a 1 '!b 2  '!c 3}
              :state  [1 2]}))
      (is (= (l/eval!
              (list (l/->VarToken  '!a)  (l/->VarToken  '!b) (l/->VarToken  '!c)
                    (l/->IfElseToken  '(!a  (invoke> + 2) !v1+) '("false!")))

              state)
             {:bindings  {'!a 1 '!b 2 '!c 3 '!v1 3}
              :state  [1 3]}))
      (is (= (l/eval!
              (list (l/->VarToken  '!a)  (l/->VarToken  '!b) (l/->VarToken  '!c)
                    (l/->IfElseToken  '(!a  !c) '("false!")))

              state)
             {:bindings  {'!a 1 '!b 2 '!c 3}
              :state  [1 2 1 3]})))))

(deftest poping-empty-stack
  (testing "testing poping empty values"
    (let [state {:bindings  {'!b 2}
                 :state  []}]
      (is (= (l/eval! (list (l/->PopFn)  (l/->VarToken  '!b) (l/->PopFn))
                      state)
             {:bindings  {'!b 2}
              :state  []})))))

(deftest
  try-useunbonded-var
  (testing "unbounded var error"
    (let [state {:bindings  {'!b 2}
                 :state  []}]
      (is (thrown-with-data?
           {:cause :unbounded-variable, :message "Unbound variable", :form '!c}
           (l/eval! (list (l/->PopFn)  (l/->VarToken  '!c))
                    state))))))

(deftest
  try-invalid-arity
  (testing "invalid stack"
    (let [state {:bindings  {'!b 2}
                 :state  []}]
      (is (thrown-with-data?
           {:cause :invalid-stack, :message "Stack should have number of 2", :form '(invoke> ...)}
           (l/eval! (list (l/->PopFn)  (l/->VarToken  '!b)  (l/->InvokeFn  '*  2))
                    state))))))



