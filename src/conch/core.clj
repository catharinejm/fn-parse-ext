(ns conch.core
  (:use name.choi.joshua.fnparse
        conch.fnparse-ext))

(def <newline> (lit \newline))
(def <=> (lit \=))
(def <space> (lit \space))

(def <assignment> (<> (conc !<=>+ <space>* <=> <space>* !<=>+)))

(def <statement> (<> (conc <assignment> <newline>?)))

(defn parse [script]
  (rule-match (<> <statement>*)
              #(println "Error!" (:remainder %))
              #(println "Incomplete!" (:remainder %1) (:remainder %2))
              {:remainder script}))

(def read-script (comp (partial apply str) flatten parse))
