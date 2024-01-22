(ns stackfn.error)

(defn explain
  [^clojure.lang.Symbol fn-name
   ^clojure.lang.PersistentList initial-args
   ^clojure.lang.ExceptionInfo e]
  (let [{:keys  [cause form message]} (ex-data e)]
    (->
     (str "function call: ("  fn-name " "   initial-args "  ) "

          (cond
            (= :unbounded-variable cause) (->> (str message " " form))
            (= :invalid-syntax  cause) (->>
                                        (str message \newline
                                             "" \newline
                                             form))
            :else  (ex-data e))))))
