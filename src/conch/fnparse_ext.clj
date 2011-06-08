(ns conch.fnparse-ext
  (:use name.choi.joshua.fnparse))

(def negate-and-optionize
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
             optionized-form)))

(defn crazy-processing* [forms]
  (for [form forms]
    (if (seq? form)
      (crazy-processing* form)
      (rule-match negate-and-optionize
                  (fn [_] form)
                  (fn [_ _] form)
                  {:remainder (name form)}))))

(defmacro <> [& rules]
  `(apply eval (crazy-processing* '~rules)))