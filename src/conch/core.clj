(ns conch.core
  (:use name.choi.joshua.fnparse))

(def <newline>? (opt (lit \newline)))
(def <=> (lit \=))
(def !<=> (except anything <=>))
(def <space> (lit \space))

(defn crazy-processing* [forms]
  #_(println (str forms))
  (for [form forms]
    (if (seq? form)
      (crazy-processing* form)
      (rule-match
       (complex [neg? (opt (lit \!))
                 _ (lit \<)
                 name (rep+ (except anything (lit \>)))
                 _ (lit \>)
                 rep-val (alt (lit \+)
                              (lit \*)
                              (lit \?)
                              emptiness)]
                (let [sym (symbol (str "<" (apply str name) ">"))
                      negated-form (if neg?
                                     `(except anything ~sym)
                                     sym)
                      optionized-form (cond
                                       (= \? rep-val)
                                       `(opt ~negated-form)
                                       (= \+ rep-val)
                                       `(rep+ ~negated-form)
                                       (= \* rep-val)
                                       `(rep* ~negated-form)
                                       :else
                                       negated-form)]
                  optionized-form))
       (fn [_] form)
       (fn [_ _] form)
       {:remainder (name form)}))))

(defmacro <> [& rules]
  `(apply eval (crazy-processing* '~rules)))

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
