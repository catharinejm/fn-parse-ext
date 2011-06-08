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

(def conc-and-altify
  (complex [conc-first (rep+ (except anything (lit '||)))
            alts (rep* (conc (lit '||)
                             (rep+ (except anything (lit '||)))))]
           (letfn [(conc-it! [[x & xs :as rules]]
                     (if (seq xs)
                       (cons 'conc rules)
                       x))]
             (if-not alts
               (-> conc-first conc-it! (cons '()) flatten)
               (cons 'alt
                     (map conc-it!
                      (cons conc-first
                            (map (comp first rest) alts))))))))

(defn deserialize-tokens [forms]
  (for [form forms]
    (if (seq? form)
      (deserialize-tokens form)
      (rule-match negate-and-optionize
                  (fn [_] form)
                  (fn [_ _] form)
                  {:remainder (name form)}))))

(defn crazy-processing* [forms]
  (let [[fst & rst :as deserialized] (deserialize-tokens
                                       (first
                                        (conc-and-altify
                                         {:remainder forms})))]
    (if (seq rst)
      deserialized
      fst)))

(defmacro <> [& rules]
  `(eval (crazy-processing* '~rules)))