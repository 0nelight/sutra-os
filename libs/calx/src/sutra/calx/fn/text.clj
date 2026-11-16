(ns sutra.calx.fn.text
  (:require [clojure.string :as str]
            [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.format :as fmt]
            [sutra.calx.i18n :as i18n]
            [sutra.calx.fn.util :as u])
  (:import (java.math BigDecimal RoundingMode)
           (java.nio.charset Charset)
           (java.util.regex Pattern PatternSyntaxException Matcher)))

(set! *warn-on-reflection* true)

(def ^:private max-text-length 32767)

(defn- text-result
  [^String s]
  (if (> (count s) max-text-length) v/ERR-VALUE s))

(defn- texts
  [x]
  (if (v/omitted? x)
    []
    (reduce (fn [acc e] (let [t (v/to-text e)] (if (error? t) (reduced t) (conj acc t))))
            [] (u/flat x))))

(defn- non-negative [n] (if (neg? n) v/ERR-VALUE n))

(defn- clamp [n lo hi] (max lo (min n hi)))

(defn- concatenate [& args]
  (let [ts (map u/text args)]
    (or (first (filter error? ts)) (text-result (apply str ts)))))

(defn- concat* [& args]
  (u/check [ts
            (reduce (fn [acc a] (let [t (texts a)] (if (error? t) (reduced t) (into acc t)))) []
                    args)]
    (text-result (apply str ts))))

(defn- textjoin [delimiter ignore-empty & args]
  (u/check [delims (texts delimiter)
            skip? (u/bool ignore-empty false)
            vals
            (reduce (fn [acc a] (let [t (texts a)] (if (error? t) (reduced t) (into acc t)))) []
                    args)]
    (let [vals (if skip? (remove #(= "" %) vals) vals)
          delims (if (empty? delims) [""] delims)]
      (text-result (apply str (butlast (interleave vals (cycle delims))))))))

(defn- left [t n]
  (u/check [s (u/text t) k (u/int* n 1) k (non-negative k)]
    (subs s 0 (min k (count s)))))

(defn- right [t n]
  (u/check [s (u/text t) k (u/int* n 1) k (non-negative k)]
    (subs s (max 0 (- (count s) k)))))

(defn- mid [t start n]
  (u/check [s (u/text t) st (u/int* start) k (u/int* n)]
    (if (or (< st 1) (neg? k))
      v/ERR-VALUE
      (let [len (count s) from (min len (dec st))]
        (subs s from (min len (+ from k)))))))

(defn- replace* [t start n new]
  (u/check [s (u/text t) st (u/int* start) k (u/int* n) nw (u/text new)]
    (if (or (< st 1) (neg? k))
      v/ERR-VALUE
      (let [len (count s) from (min len (dec st))]
        (text-result (str (subs s 0 from) nw (subs s (min len (+ from k)))))))))

(defn- nth-index-of
  [^String s ^String needle n]
  (loop [from 0 k 1]
    (let [i (.indexOf s needle (int from))]
      (cond (neg? i) nil
            (= k n) i
            :else (recur (+ i (count needle)) (inc k))))))

(defn- substitute [t old new instance]
  (u/check [s (u/text t) o (u/text old) nw (u/text new)
            k (if (u/present? instance) (u/int* instance) 0)]
    (cond
      (= "" o) s
      (not (u/present? instance)) (text-result (str/replace s o nw))
      (< k 1) v/ERR-VALUE
      :else (if-let [i (nth-index-of s o k)]
              (text-result (str (subs s 0 i) nw (subs s (+ i (count o)))))
              s))))

(defn- search-fn
  [locate]
  (fn [needle within start]
    (u/check [n (u/text needle) h (u/text within) st (u/int* start 1)]
      (if (or (< st 1) (> st (max 1 (count h))))
        v/ERR-VALUE
        (if-let [i (locate n h (dec st))]
          (double (inc i))
          v/ERR-VALUE)))))

(defn- find-exact [^String needle ^String hay from]
  (let [i (.indexOf hay needle (int from))] (when-not (neg? i) i)))

(defn- find-wildcard [^String needle ^String hay from]
  (if (= "" needle)
    from
    (let [m (.matcher ^Pattern (u/wildcard->regex needle) hay)]
      (when (.find m (int from)) (.start m)))))

(defn- proper [^String s]
  (let [sb (StringBuilder. (count s))]
    (reduce (fn [after-letter? c]
              (.append sb
                       (if after-letter? (Character/toLowerCase (char c))
                         (Character/toUpperCase (char c))))
              (Character/isLetter (char c)))
            false s)
    (str sb)))

(defn- trim-spaces
  [s]
  (-> s (str/replace #" {2,}" " ") (str/replace #"^ | $" "")))

(defn- width-shift
  [to-full? ^String s]
  (apply str (map (fn [c]
                    (let [i (int c)]
                      (cond
                        (and to-full? (= i 0x20)) (char 0x3000)
                        (and to-full? (<= 0x21 i 0x7E)) (char (+ i 0xFEE0))
                        (and (not to-full?) (= i 0x3000)) \space
                        (and (not to-full?) (<= 0xFF01 i 0xFF5E)) (char (- i 0xFEE0))
                        :else c)))
                  s)))

(def ^:private cp1252-chars
  (let [cs (Charset/forName "windows-1252")]
    (into {} (map (fn [i]
                    (let [c (.charAt (String. (byte-array [(unchecked-byte i)]) cs) 0)]
                      [i (if (= c (char 0xFFFD)) (char i) c)])))
          (range 1 256))))

(def ^:private cp1252-codes (into {} (map (fn [[i c]] [c i])) cp1252-chars))

(defn- char* [n]
  (u/check [k (u/int* n)]
    (if-let [c (cp1252-chars k)] (str c) v/ERR-VALUE)))

(defn- code [t]
  (u/check [s (u/text t)]
    (if (= "" s) v/ERR-VALUE (double (get cp1252-codes (.charAt ^String s 0) 63)))))

(defn- unichar [n]
  (u/check [k (u/int* n)]
    (cond (or (< k 1) (> k 0x10FFFF)) v/ERR-VALUE
          (<= 0xD800 k 0xDFFF) v/ERR-NA
          :else (String. (Character/toChars (int k))))))

(defn- unicode [t]
  (u/check [s (u/text t)]
    (if (= "" s) v/ERR-VALUE (double (.codePointAt ^String s 0)))))

(defn- text* [loc x format]
  (u/check [f (u/text format)]
    (let [f (i18n/delocalize-format f loc)
          x (cond (string? x) (or (v/parse-number loc x) (v/parse-date-time loc x) x)
                  (or (nil? x) (v/omitted? x)) 0.0
                  :else x)]
      (cond
        (= "" f) ""
        (number? x) (or (fmt/format-number loc x f) v/ERR-VALUE)
        (boolean? x) (v/boolean-text loc x)
        :else (fmt/format-text x f)))))

(defn- parse-value-text
  [loc s]
  (let [sym (java.util.regex.Pattern/quote (get-in loc [:currency :symbol] "$"))
        s (str/trim s)
        [neg? body] (if-let [[_ inner] (re-matches #"\((.*)\)" s)] [true inner] [false s])
        [_ sign body] (re-matches (re-pattern (str "(?s)([+-]?)\\s*(?:" sym ")?\\s*(.*?)\\s*(?:"
                                                   sym ")?"))
                                  body)
        n (when-not (str/blank? body) (v/to-number loc (str sign body)))]
    (cond (or (nil? n) (error? n)) v/ERR-VALUE
          neg? (- n)
          :else n)))

(defn- value* [loc x]
  (cond (number? x) (double x)
        (nil? x) 0.0
        (string? x) (parse-value-text loc x)
        :else v/ERR-VALUE))

(defn- numbervalue [loc t dec-sep group-sep]
  (u/check [s (u/text t)
            d (u/text dec-sep (str (:decimal loc \.)))
            g (u/text group-sep (str (:group loc \,)))]
    (if (or (= "" d) (= "" g) (= (first d) (first g)))
      v/ERR-VALUE
      (let [d (first d) g (first g)
            s (str/replace s #"\s" "")
            pcts (count (re-find #"%*$" s))
            s (subs s 0 (- (count s) pcts))
            di (str/index-of s (str d))
            [int-part frac-part] (if di [(subs s 0 di) (subs s (inc di))] [s nil])]
        (cond
          (= "" s) 0.0
          (and frac-part
               (or (str/index-of frac-part (str g)) (str/index-of frac-part (str d)))) v/ERR-VALUE
          :else
          (let [plain (str (str/replace int-part (str g) "") (when frac-part (str "." frac-part)))]
            (if (re-matches #"[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?" plain)
              (/ (Double/parseDouble plain) (Math/pow 100.0 pcts))
              v/ERR-VALUE)))))))

(defn- round-to
  [x places]
  (-> (BigDecimal/valueOf (double x)) (.setScale (int places) RoundingMode/HALF_UP) .doubleValue
      v/normalize-number))

(defn- decimals-pattern [places]
  (if (pos? places) (apply str "." (repeat places "0")) ""))

(defn- fixed [loc n decimals no-commas]
  (u/check [x (u/num n) d (u/int* decimals 2) plain? (u/bool no-commas false)]
    (if (> d 127)
      v/ERR-VALUE
      (fmt/format-number loc (round-to x d)
                         (str (if plain? "0" "#,##0") (decimals-pattern d))))))

(defn- dollar [loc n decimals]
  (u/check [x (u/num n) d (u/int* decimals 2)]
    (if (> d 127)
      v/ERR-VALUE
      (let [{:keys [symbol after?] :or {symbol "$"}} (:currency loc)
            amount (str "#,##0" (decimals-pattern d))
            p (if after? (str amount " \"" symbol "\"") (str "\"" symbol "\"" amount))]
        (fmt/format-number loc (round-to x d) (if after? (str p ";-" p) (str p ";(" p ")")))))))

(defn- delimiters
  [x]
  (u/check [ds (texts x)] (vec (sort-by count > ds))))

(defn- delimiter-matches
  [^String s delims ignore-case?]
  (let [n (count s)]
    (loop [i 0 acc []]
      (if (>= i n)
        acc
        (if-let [^String d
                   (some #(when (.regionMatches s (boolean ignore-case?) (int i) ^String % 0
                                                (count %)) %) delims)]
          (recur (+ i (count d)) (conj acc [i (+ i (count d))]))
          (recur (inc i) acc))))))

(defn- text-around
  [before?]
  (fn [text delimiter instance match-mode match-end if-not-found]
    (u/check [s (u/text text)
              ds (delimiters delimiter)
              k (u/int* instance 1)
              mode (u/int* match-mode 0)
              end? (u/bool match-end false)]
      (let [n (count s)
            span (cond
                   (some #(= "" %) ds) (if (pos? k) [0 0] [n n])
                   :else
                   (let [ms (delimiter-matches s ds (= 1 mode))
                         ms (cond (not end?) ms
                                  (pos? k) (conj ms [n n])
                                  :else (into [[0 0]] ms))]
                     (get ms (if (pos? k) (dec k) (+ (count ms) k)))))]
        (cond
          (or (zero? k) (> (abs k) (max n 1)) (not (#{0 1} mode)) (empty? ds)) v/ERR-VALUE
          span (if before? (subs s 0 (first span)) (subs s (second span)))
          (u/present? if-not-found) if-not-found
          :else v/ERR-NA)))))

(defn- splitter
  [delims ignore-case?]
  (when-let [ds (seq (remove #(= "" %) delims))]
    (let [re (Pattern/compile (str/join "|" (map #(Pattern/quote %) ds))
                              (if ignore-case?
                                (bit-or Pattern/CASE_INSENSITIVE Pattern/UNICODE_CASE) 0))]
      (fn [s] (vec (.split re ^String s -1))))))

(defn- textsplit-one [s col-split row-split ignore-empty? pad]
  (let [drop-empty (fn [xs] (if ignore-empty? (filterv #(not= "" %) xs) xs))
        rows (drop-empty (if row-split (row-split s) [s]))
        cells (mapv #(drop-empty (if col-split (col-split %) [%])) rows)
        width (reduce max 0 (map count cells))]
    (if (zero? width)
      v/ERR-CALC
      (mapv #(into % (repeat (- width (count %)) pad)) cells))))

(defn- textsplit [text col-delim row-delim ignore-empty match-mode pad-with]
  (u/check [cds (if (u/present? col-delim) (delimiters col-delim) [])
            rds (if (u/present? row-delim) (delimiters row-delim) [])
            ie? (u/bool ignore-empty false)
            mode (u/int* match-mode 0)]
    (let [ci? (= 1 mode)
          col-split (splitter cds ci?)
          row-split (splitter rds ci?)
          pad (if (u/present? pad-with) (v/scalarize pad-with) v/ERR-NA)
          split (fn [x] (u/check [s (v/to-text x)] (textsplit-one s col-split row-split ie? pad)))]
      (cond
        (not (#{0 1} mode)) v/ERR-VALUE
        (and (nil? col-split) (nil? row-split)) v/ERR-VALUE
        (v/array? text) (v/amap2 #(let [r (split %)] (if (v/array? r) (ffirst r) r)) text)
        :else (split (if (v/omitted? text) "" text))))))

(def ^:private compile-regex
  (memoize
   (fn [pattern ignore-case?]
     (try (Pattern/compile pattern
                           (if ignore-case? (bit-or Pattern/CASE_INSENSITIVE Pattern/UNICODE_CASE)
                             0))
          (catch PatternSyntaxException _ v/ERR-VALUE)))))

(defn- regex-arg
  [pattern case-sensitivity]
  (u/check [p (u/text pattern) cs (u/int* case-sensitivity 0)]
    (if (#{0 1} cs) (compile-regex p (= 1 cs)) v/ERR-VALUE)))

(defn- regextest [text pattern cs]
  (u/check [s (u/text text) re (regex-arg pattern cs)]
    (.find (.matcher ^Pattern re ^String s))))

(defn- all-matches [^Pattern re ^String s]
  (let [m (.matcher re s)]
    (loop [acc []] (if (.find m) (recur (conj acc (.group m))) acc))))

(defn- regexextract [text pattern return-mode cs]
  (u/check [s (u/text text) re (regex-arg pattern cs) mode (u/int* return-mode 0)]
    (let [m (.matcher ^Pattern re ^String s)]
      (case (int (clamp mode -1 3))
        0 (if (.find m) (.group m) v/ERR-NA)
        1 (let [ms (all-matches re s)] (if (seq ms) (mapv vector ms) v/ERR-NA))
        2 (if (.find m)
            (if (zero? (.groupCount m))
              [[(.group m)]]
              [(mapv #(or (.group m (int %)) "") (range 1 (inc (.groupCount m))))])
            v/ERR-NA)
        v/ERR-VALUE))))

(defn- regexreplace [text pattern replacement occurrence cs]
  (u/check [s (u/text text) re (regex-arg pattern cs) r (u/text replacement) k
            (u/int* occurrence 0)]
    (try
      (let [^Matcher m (.matcher ^Pattern re ^String s)]
        (if (zero? k)
          (text-result (.replaceAll m ^String r))
          (let [total (count (all-matches re s))
                idx (if (pos? k) k (+ total k 1))]
            (if (<= 1 idx total)
              (let [sb (StringBuilder.)]
                (dotimes [_ idx] (.find m))
                (.appendReplacement m sb ^String r)
                (.appendTail m sb)
                (text-result (str sb)))
              s))))
      (catch IllegalArgumentException _ v/ERR-VALUE)
      (catch IndexOutOfBoundsException _ v/ERR-VALUE))))

(defn- value->text
  [loc x strict?]
  (cond
    (string? x) (if strict? (str "\"" (str/replace x "\"" "\"\"") "\"") x)
    (error? x) (v/error-string loc x)
    (nil? x) ""
    (v/lambda? x) (v/error-string loc v/ERR-CALC)
    :else (v/to-text loc x)))

(defn- format-flag
  [x]
  (u/check [f (if (error? x) x (u/int* x 0))]
    (case (long f) 0 false 1 true v/ERR-VALUE)))

(defn- arraytotext [loc array format]
  (u/check [strict? (format-flag format)]
    (let [a (u/->2d array)
          col-sep (str (:array-col-sep loc \,))
          row-sep (str (:array-row-sep loc \;))]
      (text-result
       (if strict?
         (str "{"
              (str/join row-sep
                        (map (fn [row] (str/join col-sep (map #(value->text loc % true) row))) a))
              "}")
         (str/join (str (:list-sep loc \,) " ") (map #(value->text loc % false) (u/flat a))))))))

(defn- valuetotext [loc x format]
  (u/check [strict? (format-flag format)] (value->text loc x strict?)))

(defn- padded [[_ hi] f]
  (fn [& args] (apply f (concat args (repeat (- hi (count args)) v/omitted)))))

(defn- spec
  ([arity f] (spec arity :all f))
  ([arity lift f] {:arity arity :lift lift :impl (padded arity f)}))

(defn- unary-text [f] (u/text-fn [1 1] f))

(defn- local-spec
  ([arity f] (local-spec arity :all f))
  ([arity lift f]
   {:arity arity :lift lift :ctx? true
    :impl (fn [ctx & args] (apply (padded arity (partial f (:locale ctx))) args))}))

(def ^:private text-params
  {"CONCATENATE" :all "CONCAT" :all "TEXTJOIN" [:except #{1}]
   "LEFT" #{0} "LEFTB" #{0} "RIGHT" #{0} "RIGHTB" #{0} "MID" #{0} "MIDB" #{0}
   "LEN" :all "LENB" :all "REPLACE" #{0 3} "REPLACEB" #{0 3} "SUBSTITUTE" #{0 1 2}
   "FIND" #{0 1} "FINDB" #{0 1} "SEARCH" #{0 1} "SEARCHB" #{0 1}
   "LOWER" :all "UPPER" :all "PROPER" :all "TRIM" :all "CLEAN" :all "ASC" :all "DBCS" :all
   "REPT" #{0} "EXACT" :all
   "TEXTBEFORE" #{0 1} "TEXTAFTER" #{0 1} "TEXTSPLIT" #{0 1 2}
   "REGEXTEST" #{0 1} "REGEXEXTRACT" #{0 1} "REGEXREPLACE" #{0 1 2}})

(def ^:private base-functions
  {"CONCATENATE" (spec [1 255] concatenate)
   "CONCAT"      {:arity [1 254] :impl concat*}
   "TEXTJOIN"    {:arity [3 252] :impl textjoin}

   "LEFT"  (spec [1 2] left)   "LEFTB"  (spec [1 2] left)
   "RIGHT" (spec [1 2] right)  "RIGHTB" (spec [1 2] right)
   "MID"   (spec [3 3] mid)    "MIDB"   (spec [3 3] mid)
   "LEN"   (unary-text #(double (count %)))
   "LENB"  (unary-text #(double (count %)))
   "REPLACE"  (spec [4 4] replace*)
   "REPLACEB" (spec [4 4] replace*)
   "SUBSTITUTE" (spec [3 4] substitute)

   "FIND"    (spec [2 3] (search-fn find-exact))
   "FINDB"   (spec [2 3] (search-fn find-exact))
   "SEARCH"  (spec [2 3] (search-fn find-wildcard))
   "SEARCHB" (spec [2 3] (search-fn find-wildcard))

   "LOWER"  (unary-text str/lower-case)
   "UPPER"  (unary-text str/upper-case)
   "PROPER" (unary-text proper)
   "TRIM"   (unary-text trim-spaces)
   "CLEAN"  (unary-text #(str/replace % #"[\x00-\x1F]" ""))
   "ASC"    (unary-text #(width-shift false %))
   "DBCS"   (unary-text #(width-shift true %))
   "REPT"   (spec [2 2] (fn [t n]
                          (u/check [s (u/text t) k (u/int* n) k (non-negative k)]
                            (if (> (* k (count s)) max-text-length) v/ERR-VALUE
                                (str/join (repeat k s))))))
   "EXACT"  (u/text-fn [2 2] =)
   "T"      (spec [1 1] #(if (string? %) % ""))

   "CHAR"    (spec [1 1] char*)
   "CODE"    (spec [1 1] code)
   "UNICHAR" (spec [1 1] unichar)
   "UNICODE" (spec [1 1] unicode)

   "TEXT"        (local-spec [2 2] text*)
   "VALUE"       (local-spec [1 1] value*)
   "NUMBERVALUE" (local-spec [1 3] numbervalue)
   "FIXED"       (local-spec [1 3] fixed)
   "DOLLAR"      (local-spec [1 2] dollar)

   "TEXTBEFORE" (spec [2 6] #{0 2 3 4 5} (text-around true))
   "TEXTAFTER"  (spec [2 6] #{0 2 3 4 5} (text-around false))
   "TEXTSPLIT"  {:arity [2 6] :impl (padded [2 6] textsplit)}

   "REGEXTEST"    (spec [2 3] regextest)
   "REGEXEXTRACT" (spec [2 4] #{0 1 3} regexextract)
   "REGEXREPLACE" (spec [3 5] regexreplace)

   "ARRAYTOTEXT" (assoc (local-spec [1 2] nil arraytotext) :errors :pass)
   "VALUETOTEXT" (assoc (local-spec [1 2] valuetotext) :errors :pass)

   "HYPERLINK" (spec [1 2] (fn [link friendly]
                             (if (u/present? friendly)
                               (if (nil? friendly) 0.0 friendly)
                               (u/text link))))})

(def functions
  (reduce-kv (fn [fs f which] (assoc-in fs [f :text-args] which)) base-functions text-params))
