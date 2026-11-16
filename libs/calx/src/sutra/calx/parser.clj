(ns sutra.calx.parser
  (:require [clojure.string :as str]
            [sutra.calx.address :as addr]
            [sutra.calx.value :as v]))

(set! *warn-on-reflection* true)

(defn- looking-at
  [^java.util.regex.Pattern re ^String s pos]
  (let [m (.matcher re s)]
    (.region m (int pos) (count s))
    (when (.lookingAt m)
      (if (zero? (.groupCount m))
        (.group m)
        (into [(.group m)] (map #(.group m (int %))) (range 1 (inc (.groupCount m))))))))

(def ^:private sheet-prefix-re #"('(?:[^']|'')+'|[A-Za-z_À-￿][\w.À-￿]*)!")
(def ^:private cell-area-re #"(\$?)([A-Za-z]{1,3})(\$?)(\d+)(?::(\$?)([A-Za-z]{1,3})(\$?)(\d+))?")
(def ^:private col-area-re #"(\$?)([A-Za-z]{1,3}):(\$?)([A-Za-z]{1,3})")
(def ^:private row-area-re #"(\$?)(\d+):(\$?)(\d+)")
(def ^:private number-re #"(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?")
(def ^:private ident-re #"[A-Za-z_\\À-￿][\w.À-￿]*")
(def ^:private error-re
  #"(?i)#(?:NULL!|DIV/0!|VALUE!|REF!|NAME\?|NUM!|N/A|SPILL!|CALC!|CYCLE!|ERROR!|GETTING_DATA)")
(def ^:private string-re #"\"((?:[^\"]|\"\")*)\"")

(defn- ident-char? [ch]
  (and ch (or (Character/isLetterOrDigit (char ch)) (#{\_ \. \(} ch))))

(defn- ref-not-followed? [s end]
  (not (ident-char? (get s end))))

(defn- ref-token
  [s pos sheet-name]
  (let [base (cond-> {:kind :ref} sheet-name (assoc :sheet-name sheet-name))]
    (or
     (when-let [[m a1 l1 b1 d1 a2 l2 b2 d2] (looking-at cell-area-re s pos)]
       (let [end (+ pos (count m))]
         (when (ref-not-followed? s end)
           (let [c1 (addr/letters->col l1) r1 (dec (parse-long d1))
                 [c2 r2] (if l2 [(addr/letters->col l2) (dec (parse-long d2))] [c1 r1])]
             (when (and c1 c2 (< -1 r1 addr/max-rows) (< -1 r2 addr/max-rows))
               (let [ac1 (= a1 "$") ar1 (= b1 "$")
                     [ac2 ar2] (if l2 [(= a2 "$") (= b2 "$")] [ac1 ar1])
                     [r1 ar1 r2 ar2] (if (<= r1 r2) [r1 ar1 r2 ar2] [r2 ar2 r1 ar1])
                     [c1 ac1 c2 ac2] (if (<= c1 c2) [c1 ac1 c2 ac2] [c2 ac2 c1 ac1])]
                 [(assoc base :shape (if l2 :area :cell)
                         :r1 r1 :c1 c1 :r2 r2 :c2 c2 :abs [ar1 ac1 ar2 ac2])
                  end]))))))
     (when-let [[m a1 l1 a2 l2] (looking-at col-area-re s pos)]
       (let [end (+ pos (count m))]
         (when (ref-not-followed? s end)
           (let [c1 (addr/letters->col l1) c2 (addr/letters->col l2)]
             (when (and c1 c2)
               (let [[c1 a1 c2 a2] (if (<= c1 c2) [c1 a1 c2 a2] [c2 a2 c1 a1])]
                 [(assoc base :shape :cols :r1 nil :r2 nil :c1 c1 :c2 c2
                         :abs [false (= a1 "$") false (= a2 "$")])
                  end]))))))
     (when-let [[m a1 d1 a2 d2] (looking-at row-area-re s pos)]
       (let [end (+ pos (count m))]
         (when (ref-not-followed? s end)
           (let [r1 (dec (parse-long d1)) r2 (dec (parse-long d2))]
             (when (and (< -1 r1 addr/max-rows) (< -1 r2 addr/max-rows))
               (let [[r1 a1 r2 a2] (if (<= r1 r2) [r1 a1 r2 a2] [r2 a2 r1 a1])]
                 [(assoc base :shape :rows :r1 r1 :r2 r2 :c1 nil :c2 nil
                         :abs [(= a1 "$") false (= a2 "$") false])
                  end])))))))))

(defn- unquote-sheet [s]
  (if (str/starts-with? s "'")
    (str/replace (subs s 1 (dec (count s))) "''" "'")
    s))

(def ^:private two-char-ops #{"<=" ">=" "<>"})
(def ^:private one-char-tokens
  {\+ [:op "+"] \- [:op "-"] \* [:op "*"] \/ [:op "/"] \^ [:op "^"] \& [:op "&"]
   \= [:op "="] \< [:op "<"] \> [:op ">"] \% [:op "%"] \: [:op ":"]
   \( [:lparen] \) [:rparen] \, [:comma] \; [:semi] \{ [:lbrace] \} [:rbrace] \# [:hash]})

(defn- parse-error-ex [msg pos]
  (ex-info msg {:calx/parse-error true :pos pos}))

(defn tokenize
  [^String s]
  (loop [pos 0 out []]
    (if (>= pos (count s))
      out
      (let [ch (.charAt s pos)]
        (cond
          (Character/isWhitespace ch) (recur (inc pos) out)

          (= ch \")
          (if-let [[m body] (looking-at string-re s pos)]
            (recur (+ pos (count m))
                   (conj out {:t :str :v (str/replace body "\"\"" "\"") :pos pos}))
            (throw (parse-error-ex "Unterminated string" pos)))

          (and (= ch \#) (looking-at error-re s pos))
          (let [m (looking-at error-re s pos)]
            (recur (+ pos (count m))
                   (conj out {:t :err :v (or (v/parse-error m) (v/err :na)) :pos pos})))

          :else
          (let [
                sheet (when-let [[m sh] (looking-at sheet-prefix-re s pos)] [m (unquote-sheet sh)])
                ref-start (if sheet (+ pos (count (first sheet))) pos)
                ref (ref-token s ref-start (second sheet))]
            (cond
              ref (let [[node end] ref] (recur (long end) (conj out {:t :ref :v node :pos pos})))

              sheet (if-let [id (looking-at ident-re s ref-start)]
                      (recur (+ ref-start (count id))
                             (conj out {:t :name :v id :sheet-name (second sheet) :pos pos}))
                      (throw (parse-error-ex "Invalid reference after sheet name" pos)))

              (looking-at number-re s pos)
              (let [m (looking-at number-re s pos)]
                (recur (+ pos (count m)) (conj out {:t :num :v (Double/parseDouble m) :pos pos})))

              (looking-at ident-re s pos)
              (let [m (looking-at ident-re s pos)
                    end (+ pos (count m))
                    next-non-ws (loop [i end]
                                  (if (and (< i (count s)) (Character/isWhitespace (.charAt s i)))
                                    (recur (inc i)) i))
                    call? (= \( (get s next-non-ws))
                    up (str/upper-case m)]
                (cond
                  call? (recur end (conj out {:t :func :v m :pos pos}))
                  (= up "TRUE") (recur end (conj out {:t :bool :v true :pos pos}))
                  (= up "FALSE") (recur end (conj out {:t :bool :v false :pos pos}))
                  :else (recur end (conj out {:t :name :v m :pos pos}))))

              (and (< (inc pos) (count s)) (two-char-ops (subs s pos (+ pos 2))))
              (recur (+ pos 2) (conj out {:t :op :v (subs s pos (+ pos 2)) :pos pos}))

              (one-char-tokens ch)
              (let [[t val] (one-char-tokens ch)]
                (recur (inc pos) (conj out (cond-> {:t t :pos pos} val (assoc :v val)))))

              :else (throw (parse-error-ex (str "Unexpected character '" ch "'") pos)))))))))

(def ^:private binary-ops
  {"=" [:eq 10 11] "<>" [:ne 10 11] "<" [:lt 10 11] ">" [:gt 10 11] "<=" [:le 10 11]
   ">=" [:ge 10 11]
   "&" [:concat 20 21]
   "+" [:add 30 31] "-" [:sub 30 31]
   "*" [:mul 40 41] "/" [:div 40 41]
   "^" [:pow 50 51]
   ":" [:range 80 81]})

(def ^:private prefix-bp 60)
(def ^:private percent-bp 70)
(def ^:private spill-bp 90)
(def ^:private invoke-bp 95)

(defn- normalize-fn-name [s]
  (-> (str/upper-case s)
      (str/replace #"^(_XLFN\.|_XLWS\.)+" "")
      (str/replace #"^_XLFN\._XLWS\." "")))

(declare parse-expr)

(defn- peek-t [st] (get (:tokens st) (:i st)))
(defn- advance [st] (update st :i inc))

(defn- expect [st t]
  (let [tok (peek-t st)]
    (if (= t (:t tok))
      (advance st)
      (throw (parse-error-ex (str "Expected " (name t)
                                  (when tok (str " but found " (or (:v tok) (name (:t tok))))))
                             (or (:pos tok) :end))))))

(defn- parse-args
  [st]
  (if (= :rparen (:t (peek-t st)))
    [[] (advance st)]
    (loop [st st args []]
      (let [tok (peek-t st)
            [arg st] (if (#{:comma :rparen} (:t tok))
                       [{:kind :missing} st]
                       (parse-expr st 0))
            args (conj args arg)
            tok (peek-t st)]
        (case (:t tok)
          :comma (recur (advance st) args)
          :rparen [args (advance st)]
          (throw (parse-error-ex "Expected , or )" (or (:pos tok) :end))))))))

(defn- parse-array-literal [st]
  (loop [st st rows [] row []]
    (let [[item st] (parse-expr st 0)
          row (conj row item)
          tok (peek-t st)]
      (case (:t tok)
        :comma (recur (advance st) rows row)
        :semi (recur (advance st) (conj rows row) [])
        :rbrace (let [rows (conj rows row)]
                  (when-not (apply = (map count rows))
                    (throw (parse-error-ex "Array literal rows must have equal length" (:pos tok))))
                  [{:kind :array :rows rows} (advance st)])
        (throw (parse-error-ex "Expected , ; or } in array literal" (or (:pos tok) :end)))))))

(defn- parse-prefix [st]
  (let [tok (peek-t st)
        st' (advance st)]
    (case (:t tok)
      nil (throw (parse-error-ex "Unexpected end of formula" :end))
      :num [{:kind :lit :value (:v tok)} st']
      :str [{:kind :lit :value (:v tok)} st']
      :bool [{:kind :lit :value (:v tok)} st']
      :err [{:kind :lit :value (:v tok)} st']
      :ref [(:v tok) st']
      :name [(cond-> {:kind :name :name (:v tok)}
               (:sheet-name tok) (assoc :sheet-name (:sheet-name tok)))
             st']
      :func (let [st (expect st' :lparen)
                  [args st] (parse-args st)]
              [{:kind :call :fn (normalize-fn-name (:v tok)) :args args} st])
      :lparen (let [[e st] (parse-expr st' 0)]
                [{:kind :paren :expr e} (expect st :rparen)])
      :lbrace (parse-array-literal st')
      :op (case (:v tok)
            "-" (let [[e st] (parse-expr st' prefix-bp)] [{:kind :op :op :neg :args [e]} st])
            "+" (let [[e st] (parse-expr st' prefix-bp)] [{:kind :op :op :pos :args [e]} st])
            (throw (parse-error-ex (str "Unexpected operator " (:v tok)) (:pos tok))))
      (throw (parse-error-ex (str "Unexpected token " (or (:v tok) (name (:t tok)))) (:pos tok))))))

(defn- parse-expr [st min-bp]
  (loop [[lhs st] (parse-prefix st)]
    (let [tok (peek-t st)]
      (cond
        (nil? tok) [lhs st]

        (and (= :op (:t tok)) (= "%" (:v tok)) (>= percent-bp min-bp))
        (recur [{:kind :op :op :pct :args [lhs]} (advance st)])

        (and (= :hash (:t tok)) (>= spill-bp min-bp))
        (recur [{:kind :spill :ref lhs} (advance st)])

        (and (= :lparen (:t tok)) (>= invoke-bp min-bp) (#{:call :paren :name :invoke} (:kind lhs)))
        (let [[args st] (parse-args (advance st))]
          (recur [{:kind :invoke :fn lhs :args args} st]))

        (and (= :op (:t tok)) (binary-ops (:v tok)))
        (let [[op lbp rbp] (binary-ops (:v tok))]
          (if (< lbp min-bp)
            [lhs st]
            (let [[rhs st] (parse-expr (advance st) rbp)]
              (recur [{:kind :op :op op :args [lhs rhs]} st]))))

        :else [lhs st]))))

(defn parse
  [formula]
  (let [body (if (str/starts-with? formula "=") (subs formula 1) formula)
        tokens (tokenize body)
        [ast st] (parse-expr {:tokens tokens :i 0} 0)]
    (when-let [tok (peek-t st)]
      (throw (parse-error-ex (str "Unexpected " (or (:v tok) (name (:t tok)))) (:pos tok))))
    ast))

(defn try-parse
  [formula]
  (try {:ast (parse formula)}
       (catch clojure.lang.ExceptionInfo e
         (if (:calx/parse-error (ex-data e)) {:error (ex-message e)} (throw e)))))

(def ^:private op-strings
  {:add "+" :sub "-" :mul "*" :div "/" :pow "^" :concat "&"
   :eq "=" :ne "<>" :lt "<" :gt ">" :le "<=" :ge ">=" :range ":"})

(def ^:private op-prec
  {:eq 10 :ne 10 :lt 10 :gt 10 :le 10 :ge 10 :concat 20 :add 30 :sub 30 :mul 40 :div 40 :pow 50
   :neg 60 :pos 60 :pct 70 :range 80})

(defn- node-prec [node]
  (case (:kind node)
    :op (op-prec (:op node))
    100))

(defn- print-number [d]
  (v/format-number d))

(defn- print-ref [{:keys [shape r1 c1 r2 c2 abs]}]
  (let [[ar1 ac1 ar2 ac2] abs
        d (fn [a] (if a "$" ""))]
    (case shape
      :cell (str (d ac1) (addr/col->letters c1) (d ar1) (inc r1))
      :area (str (d ac1) (addr/col->letters c1) (d ar1) (inc r1) ":"
                 (d ac2) (addr/col->letters c2) (d ar2) (inc r2))
      :cols (str (d ac1) (addr/col->letters c1) ":" (d ac2) (addr/col->letters c2))
      :rows (str (d ar1) (inc r1) ":" (d ar2) (inc r2)))))

(defn unparse
  ([ast] (unparse ast {}))
  ([ast {:keys [sheet-name] :or {sheet-name str} :as opts}]
   (letfn [(sheet-prefix [node]
             (cond
               (:sheet-name node) (str (addr/quote-sheet-name (:sheet-name node)) "!")
               (:sheet node) (if-let [n (sheet-name (:sheet node))]
                               (str (addr/quote-sheet-name n) "!")
                               "#REF!")
               :else ""))
           (wrap [child parent-prec right?]
             (let [p (node-prec child)
                   s (pr* child)]
               (if (or (< p parent-prec) (and right? (= p parent-prec) (< p 60)))
                 (str "(" s ")")
                 s)))
           (pr* [node]
             (case (:kind node)
               :lit (let [x (:value node)]
                      (cond (number? x) (print-number x)
                            (string? x) (str "\"" (str/replace x "\"" "\"\"") "\"")
                            (boolean? x) (if x "TRUE" "FALSE")
                            (v/error? x) (v/error-string x)
                            :else (str x)))
               :ref (let [prefix (sheet-prefix node)]
                      (if (= prefix "#REF!") "#REF!" (str prefix (print-ref node))))
               :name (str (sheet-prefix node) (:name node))
               :call (str (:fn node) "(" (str/join "," (map pr* (:args node))) ")")
               :invoke (str (pr* (:fn node)) "(" (str/join "," (map pr* (:args node))) ")")
               :paren (str "(" (pr* (:expr node)) ")")
               :missing ""
               :spill (str (pr* (:ref node)) "#")
               :array (str "{" (str/join ";" (map #(str/join "," (map pr* %)) (:rows node))) "}")
               :op (let [op (:op node) p (op-prec op) [a b] (:args node)]
                     (case op
                       :neg (str "-" (wrap a p false))
                       :pos (str "+" (wrap a p false))
                       :pct (str (wrap a p false) "%")
                       (str (wrap a p false) (op-strings op) (wrap b p true))))))]
     (pr* ast))))
