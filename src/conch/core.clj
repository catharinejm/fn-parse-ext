(ns conch.core
  (:use name.choi.joshua.fnparse))

(def <newline>? (opt (lit \newline)))
(def <=> (lit \=))
(def !<=> (except anything <=>))
(def <space> (lit \space))

(defn crazy-processing* [forms]
  (for [form forms
        :let [fname (name form)]]
    ((complex [neg? (opt (lit \!))
               _ (lit \<)
               name (rep+ (except anything (lit \>)))
               _ (lit \>)
               opt? (opt (lit \?))]
              (let [sym (symbol (str "<" (apply str name) ">"))
                    negated-form (if neg?
                                   `(except anything ~sym)
                                   sym)
                    optionized-form (if opt?
                                      `(opt ~negated-form)
                                      negated-form)]
                optionized-form))
     {:remainder fname})))

(defmacro <> [& rules]
  `(ffirst (crazy-processing* '~rules)))

(def assignment (conc (rep+ !<=>)
                      (rep* <space>)
                      <=>
                      (rep* <space>)
                      (rep+ !<=>)))

(def statement (rep* (conc assignment <newline>?)))

(defn parse [script]
  (rule-match statement
              #(println "Error!" (:remainder %))
              #(println "Incomplete!" (:remainder %1) (:remainder %2))
              {:remainder script}))

(def read-script (comp (partial apply str) flatten parse))
