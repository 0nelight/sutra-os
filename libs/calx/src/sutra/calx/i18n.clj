(ns sutra.calx.i18n
  (:require [clojure.string :as str]
            [sutra.calx.format :as fmt]
            [sutra.calx.value :as v]))

(set! *warn-on-reflection* true)

(def de
  {:decimal \, :group \. :arg-sep \; :array-col-sep \. :array-row-sep \; :list-sep \;
   :date-order :dmy
   :system-formats {"m/d/yyyy" "dd.mm.yyyy" "m/d/yyyy h:mm" "dd.mm.yyyy hh:mm"
                    "m/d/yyyy h:mm:ss" "dd.mm.yyyy hh:mm:ss" "h:mm" "hh:mm" "h:mm:ss" "hh:mm:ss"}
   :month-names ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September"
                 "Oktober" "November" "Dezember"]
   :month-abbr ["Jan" "Feb" "Mär" "Apr" "Mai" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dez"]
   :day-names ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"]
   :day-abbr ["So" "Mo" "Di" "Mi" "Do" "Fr" "Sa"]
   :format-letters {\J \y \T \d \M \m}
   :general "Standard"
   :format-colors {"Schwarz" "Black" "Blau" "Blue" "Zyan" "Cyan" "Grün" "Green"
                   "Magenta" "Magenta" "Rot" "Red" "Weiß" "White" "Gelb" "Yellow" "Farbe" "Color"}
   :currency {:symbol "€" :after? true}
   :errors {"#NULL!" :null "#DIV/0!" :div0 "#WERT!" :value "#BEZUG!" :ref "#NAME?" :name
            "#ZAHL!" :num "#NV" :na "#ÜBERLAUF!" :spill "#KALK!" :calc}
   :booleans {"WAHR" true "FALSCH" false}
   :functions
   {"SUMME" "SUM" "MITTELWERT" "AVERAGE" "ANZAHL" "COUNT" "ANZAHL2" "COUNTA"
    "ANZAHLLEEREZELLEN" "COUNTBLANK"
    "WENN" "IF" "WENNFEHLER" "IFERROR" "WENNNV" "IFNA" "WENNS" "IFS" "UND" "AND" "ODER" "OR"
    "NICHT" "NOT"
    "XODER" "XOR" "ERSTERWERT" "SWITCH" "WAHL" "CHOOSE" "WAHR" "TRUE" "FALSCH" "FALSE"
    "SVERWEIS" "VLOOKUP" "WVERWEIS" "HLOOKUP" "XVERWEIS" "XLOOKUP" "VERWEIS" "LOOKUP"
    "VERGLEICH" "MATCH"
    "XVERGLEICH" "XMATCH" "INDIREKT" "INDIRECT" "BEREICH.VERSCHIEBEN" "OFFSET" "ADRESSE" "ADDRESS"
    "ZEILE" "ROW" "SPALTE" "COLUMN" "ZEILEN" "ROWS" "SPALTEN" "COLUMNS" "MTRANS" "TRANSPOSE"
    "BEREICHE" "AREAS"
    "SORTIEREN" "SORT" "SORTIERENNACH" "SORTBY" "EINDEUTIG" "UNIQUE" "SEQUENZ" "SEQUENCE"
    "ZUFALLSMATRIX" "RANDARRAY" "VSTAPELN" "VSTACK" "HSTAPELN" "HSTACK" "ÜBERNEHMEN" "TAKE"
    "WEGLASSEN" "DROP"
    "ZEILENWAHL" "CHOOSEROWS" "SPALTENWAHL" "CHOOSECOLS" "ZUSPALTE" "TOCOL" "ZUZEILE" "TOROW"
    "ZEILENUMBRUCH" "WRAPROWS" "SPALTENUMBRUCH" "WRAPCOLS" "ERWEITERN" "EXPAND"
    "NACHZEILE" "BYROW" "NACHSPALTE" "BYCOL" "MATRIXERSTELLEN" "MAKEARRAY" "ZUORDNEN" "MAP"
    "REDUZIEREN" "REDUCE"
    "ISTAUSGELASSEN" "ISOMITTED"
    "VERKETTEN" "CONCATENATE" "TEXTKETTE" "CONCAT" "TEXTVERKETTEN" "TEXTJOIN" "LINKS" "LEFT"
    "RECHTS" "RIGHT"
    "TEIL" "MID" "LÄNGE" "LEN" "GROSS" "UPPER" "KLEIN" "LOWER" "GROSS2" "PROPER" "GLÄTTEN" "TRIM"
    "SÄUBERN" "CLEAN"
    "WECHSELN" "SUBSTITUTE" "ERSETZEN" "REPLACE" "FINDEN" "FIND" "SUCHEN" "SEARCH"
    "WIEDERHOLEN" "REPT"
    "IDENTISCH" "EXACT" "WERT" "VALUE" "ZAHLENWERT" "NUMBERVALUE" "FEST" "FIXED" "DM" "DOLLAR"
    "ZEICHEN" "CHAR" "CODE" "CODE" "UNIZEICHEN" "UNICHAR" "TEXTVOR" "TEXTBEFORE"
    "TEXTNACH" "TEXTAFTER"
    "TEXTTEILEN" "TEXTSPLIT"
    "RUNDEN" "ROUND" "AUFRUNDEN" "ROUNDUP" "ABRUNDEN" "ROUNDDOWN" "GANZZAHL" "INT" "KÜRZEN" "TRUNC"
    "REST" "MOD"
    "POTENZ" "POWER" "WURZEL" "SQRT" "VORZEICHEN" "SIGN" "PRODUKT" "PRODUCT"
    "SUMMENPRODUKT" "SUMPRODUCT"
    "QUADRATESUMME" "SUMSQ" "SUMMEWENN" "SUMIF" "SUMMEWENNS" "SUMIFS" "ZÄHLENWENN" "COUNTIF"
    "ZÄHLENWENNS" "COUNTIFS"
    "MITTELWERTWENN" "AVERAGEIF" "MITTELWERTWENNS" "AVERAGEIFS" "MAXWENNS" "MAXIFS"
    "MINWENNS" "MINIFS"
    "TEILERGEBNIS" "SUBTOTAL" "AGGREGAT" "AGGREGATE" "OBERGRENZE" "CEILING" "UNTERGRENZE" "FLOOR"
    "OBERGRENZE.MATHEMATIK" "CEILING.MATH" "UNTERGRENZE.MATHEMATIK" "FLOOR.MATH" "VRUNDEN" "MROUND"
    "GERADE" "EVEN" "UNGERADE" "ODD" "FAKULTÄT" "FACT" "KOMBINATIONEN" "COMBIN"
    "VARIATIONEN" "PERMUT"
    "GGT" "GCD" "KGV" "LCM" "RÖMISCH" "ROMAN" "ZUFALLSZAHL" "RAND" "ZUFALLSBEREICH" "RANDBETWEEN"
    "GRAD" "DEGREES" "BOGENMASS" "RADIANS" "ARCSIN" "ASIN" "ARCCOS" "ACOS" "ARCTAN" "ATAN"
    "ARCTAN2" "ATAN2"
    "MMULT" "MMULT" "MDET" "MDETERM" "MINV" "MINVERSE" "MEINHEIT" "MUNIT" "BASIS" "BASE"
    "DEZIMAL" "DECIMAL"
    "DEZINBIN" "DEC2BIN" "DEZINHEX" "DEC2HEX" "DEZINOKT" "DEC2OCT" "BININDEZ" "BIN2DEC"
    "HEXINDEZ" "HEX2DEC"
    "OKTINDEZ" "OCT2DEC" "UMWANDELN" "CONVERT"
    "MEDIAN" "MEDIAN" "MODALWERT" "MODE" "MODUS.EINF" "MODE.SNGL" "STABW" "STDEV"
    "STABW.S" "STDEV.S"
    "STABW.N" "STDEV.P" "STABWN" "STDEVP" "VARIANZ" "VAR" "VAR.S" "VAR.S" "VAR.P" "VAR.P"
    "VARIANZEN" "VARP"
    "KGRÖSSTE" "LARGE" "KKLEINSTE" "SMALL" "RANG" "RANK" "RANG.GLEICH" "RANK.EQ"
    "RANG.MITTELW" "RANK.AVG"
    "QUANTIL" "PERCENTILE" "QUANTIL.INKL" "PERCENTILE.INC" "QUANTIL.EXKL" "PERCENTILE.EXC"
    "QUARTILE" "QUARTILE" "QUARTILE.INKL" "QUARTILE.INC" "QUARTILE.EXKL" "QUARTILE.EXC"
    "KORREL" "CORREL" "STEIGUNG" "SLOPE" "ACHSENABSCHNITT" "INTERCEPT" "BESTIMMTHEITSMASS" "RSQ"
    "PROGNOSE" "FORECAST" "PROGNOSE.LINEAR" "FORECAST.LINEAR" "VARIATION" "GROWTH" "RGP" "LINEST"
    "HÄUFIGKEIT" "FREQUENCY" "GEOMITTEL" "GEOMEAN" "HARMITTEL" "HARMEAN"
    "KOVARIANZ.P" "COVARIANCE.P"
    "KOVARIANZ.S" "COVARIANCE.S" "NORM.VERT" "NORM.DIST" "NORM.S.VERT" "NORM.S.DIST"
    "NORM.INV" "NORM.INV"
    "NORM.S.INV" "NORM.S.INV" "BINOM.VERT" "BINOM.DIST" "POISSON.VERT" "POISSON.DIST"
    "EXPON.VERT" "EXPON.DIST"
    "HEUTE" "TODAY" "JETZT" "NOW" "DATUM" "DATE" "DATWERT" "DATEVALUE" "ZEIT" "TIME"
    "ZEITWERT" "TIMEVALUE"
    "JAHR" "YEAR" "MONAT" "MONTH" "TAG" "DAY" "STUNDE" "HOUR" "MINUTE" "MINUTE" "SEKUNDE" "SECOND"
    "WOCHENTAG" "WEEKDAY" "KALENDERWOCHE" "WEEKNUM" "ISOKALENDERWOCHE" "ISOWEEKNUM" "EDATUM" "EDATE"
    "MONATSENDE" "EOMONTH" "TAGE" "DAYS" "TAGE360" "DAYS360" "BRTEILJAHRE" "YEARFRAC"
    "NETTOARBEITSTAGE" "NETWORKDAYS" "NETTOARBEITSTAGE.INTL" "NETWORKDAYS.INTL"
    "ARBEITSTAG" "WORKDAY"
    "ARBEITSTAG.INTL" "WORKDAY.INTL"
    "RMZ" "PMT" "ZINSZ" "IPMT" "KAPZ" "PPMT" "BW" "PV" "ZW" "FV" "ZZR" "NPER" "ZINS" "RATE"
    "NBW" "NPV"
    "XKAPITALWERT" "XNPV" "IKV" "IRR" "XINTZINSFUSS" "XIRR" "QIKV" "MIRR" "LIA" "SLN" "DIA" "SYD"
    "GDA" "DDB"
    "GDA2" "DB" "EFFEKTIV" "EFFECT" "NOMINAL" "NOMINAL" "KUMZINSZ" "CUMIPMT" "KUMKAPITAL" "CUMPRINC"
    "ISTLEER" "ISBLANK" "ISTZAHL" "ISNUMBER" "ISTTEXT" "ISTEXT" "ISTKTEXT" "ISNONTEXT"
    "ISTLOG" "ISLOGICAL"
    "ISTFEHLER" "ISERROR" "ISTFEHL" "ISERR" "ISTNV" "ISNA" "ISTBEZUG" "ISREF" "ISTGERADE" "ISEVEN"
    "ISTUNGERADE" "ISODD" "ISTFORMEL" "ISFORMULA" "FORMELTEXT" "FORMULATEXT" "NV" "NA"
    "FEHLER.TYP" "ERROR.TYPE"
    "TYP" "TYPE" "BLATT" "SHEET" "BLÄTTER" "SHEETS" "ZELLE" "CELL"
    "MATRIXZUTEXT" "ARRAYTOTEXT" "WERTZUTEXT" "VALUETOTEXT"}})

(def packs {:de de})

(defn- upper-keys [m] (into {} (map (fn [[k x]] [(str/upper-case k) x])) m))

(defn- compile-pack
  [pack]
  (let [errors (upper-keys (:errors pack))
        bools  (upper-keys (:booleans pack))]
    (assoc pack
           :booleans            bools
           :errors              errors
           :boolean-text        (into {} (map (fn [[s b]] [b s])) (:booleans pack))
           :error-text          (into {} (map (fn [[s t]] [t s])) (:errors pack))
           :canonical-functions (into {} (map (fn [[l c]] [c l])) (:functions pack))
           :error-literals      (into {} (map (fn [[s t]] [s (v/error-string (v/err t))]))
                                      errors)
           :canonical-errors    (into {} (map (fn [[s t]] [(v/error-string (v/err t)) s]))
                                      (:errors pack))
           :canonical-booleans  (into {} (map (fn [[s b]] [(if b "TRUE" "FALSE") s]))
                                      (:booleans pack)))))

(def ^:private compiled (memoize compile-pack))

(defn locale
  [l]
  (cond (nil? l) nil
        (map? l) (compiled (dissoc l :boolean-text :error-text :canonical-functions
                                   :error-literals :canonical-errors :canonical-booleans))
        :else (some-> (packs l) compiled)))

(defn- ident-char? [ch] (and ch (or (Character/isLetterOrDigit (char ch)) (#{\_ \. \$} ch))))

(defn- error-at
  [^String s i err-map]
  (->> (keys err-map)
       (filter #(.regionMatches s true (int i) ^String % 0 (count %)))
       (apply max-key count nil)))

(defn- translate
  [^String s {:keys [fn-map bool-map err-map from to]}]
  (let [n (count s) sb (StringBuilder.)]
    (loop [i 0 depth 0]
      (if (>= i n)
        (str sb)
        (let [ch (.charAt s i)
              e  (when (= ch \#) (error-at s i err-map))]
          (cond
            e (do (.append sb ^String (err-map e)) (recur (+ i (count e)) depth))

            (#{\" \'} ch)
            (let [end (loop [j (inc i)]
                        (cond (>= j n) n
                              (and (= ch (.charAt s j)) (< (inc j) n)
                                   (= ch (.charAt s (inc j)))) (recur (+ j 2))
                              (= ch (.charAt s j)) (inc j)
                              :else (recur (inc j))))]
              (.append sb (subs s i end))
              (recur (long end) depth))

            (Character/isDigit ch)
            (let [end (loop [j i]
                        (if (and (< j n) (or (Character/isDigit (.charAt s j))
                                             (and (= (Character/valueOf (.charAt s j))
                                                     (:decimal from))
                                                  (< (inc j) n)
                                                  (Character/isDigit (.charAt s (inc j))))))
                          (recur (inc j)) j))]
              (.append sb (.replace (subs s i end) (char (:decimal from)) (char (:decimal to))))
              (recur (long end) depth))

            (or (Character/isLetter ch) (= ch \_))
            (let [end (loop [j i] (if (ident-char? (get s j)) (recur (inc j)) j))
                  word (subs s i end)
                  up (str/upper-case word)
                  call? (= \(
                           (get s
                                (loop [j end]
                                  (if (and (< j n) (Character/isWhitespace (.charAt s j)))
                                    (recur (inc j)) j))))]
              (.append sb (cond
                            (and call? (fn-map up)) (fn-map up)
                            (and (not call?) (contains? bool-map up)) (bool-map up)
                            :else word))
              (recur (long end) depth))

            (= ch \{) (do (.append sb ch) (recur (inc i) (inc depth)))
            (= ch \}) (do (.append sb ch) (recur (inc i) (max 0 (dec depth))))
            (and (pos? depth)
                 (= (Character/valueOf ch)
                    (:array-col-sep from))) (do (.append sb (:array-col-sep to))
                                                (recur (inc i) depth))
            (and (pos? depth)
                 (= (Character/valueOf ch)
                    (:array-row-sep from))) (do (.append sb (:array-row-sep to))
                                                (recur (inc i) depth))
            (and (zero? depth)
                 (= (Character/valueOf ch) (:arg-sep from)))
            (do (.append sb (:arg-sep to)) (recur (inc i) depth))
            :else (do (.append sb ch) (recur (inc i) depth))))))))

(def ^:private canonical {:decimal \. :arg-sep \, :array-col-sep \, :array-row-sep \;})

(defn delocalize
  [formula l]
  (if-let [loc (locale l)]
    (translate formula {:fn-map   (:functions loc)
                        :bool-map (into {} (map (fn [[k b]] [k (if b "TRUE" "FALSE")]))
                                        (:booleans loc))
                        :err-map  (:error-literals loc)
                        :from loc :to canonical})
    formula))

(defn localize
  [formula l]
  (if-let [loc (locale l)]
    (translate formula {:fn-map   (:canonical-functions loc)
                        :bool-map (:canonical-booleans loc)
                        :err-map  (:canonical-errors loc)
                        :from canonical :to loc})
    formula))

(defn function-name
  [canonical-name l]
  (get (:canonical-functions (locale l)) canonical-name canonical-name))

(defn- code-tokens [^String section letter-of]
  (let [n (count section)
        at? (fn [i ^String w] (.regionMatches section true (int i) w 0 (count w)))]
    (loop [i 0 out []]
      (if (>= i n)
        out
        (let [ch  (.charAt section i)
              end (cond (= ch \") (or (some-> (str/index-of section "\"" (inc i)) inc) n)
                        (= ch \[) (or (some-> (str/index-of section "]" (inc i)) inc) n)
                        (#{\\ \_ \*} ch) (min n (+ i 2))
                        (at? i "AM/PM") (+ i 5)
                        (at? i "A/P") (+ i 3))
              c   (when-not end (letter-of ch))]
          (cond
            (and end (= ch \[) (< (inc i) (dec end)))
            (recur (long end) (conj out [:bracket (subs section (inc i) (dec end))]))

            end (recur (long end) (conj out [:raw (subs section i end)]))
            c   (let [same? #(and (< % n) (= c (letter-of (.charAt section %))))
                      j (loop [j i] (if (same? j) (recur (inc j)) j))]
                  (recur (long j) (conj out [:date c (- j i)])))
            :else (recur (inc i) (conj out [:char ch]))))))))

(defn- date-tokens? [tokens]
  (let [letters (keep (fn [[k c]] (when (= k :date) c)) tokens)]
    (boolean (or (some #(not= \m %) letters)
                 (and (seq letters)
                      (not-any? (fn [[k c]] (and (= k :char) (#{\0 \# \?} c))) tokens))))))

(defn- minutes? [tokens i]
  (let [dates (fn [xs] (keep (fn [[k c]] (when (= k :date) c)) xs))]
    (or (= \h (last (dates (subvec tokens 0 i))))
        (= \s (first (dates (subvec tokens (inc i))))))))

(defn- color-in [names content]
  (when-let [[_ word n] (re-matches #"(?i)(\p{L}+)(\d*)" content)]
    (some-> (names (str/upper-case word)) (str n))))

(defn- print-code [tokens {:keys [seps letter colors]}]
  (let [date? (date-tokens? tokens)]
    (apply str
           (map-indexed
            (fn [i [k x n]]
              (case k
                :raw  x
                :bracket (str "[" (or (color-in colors x) x) "]")
                :char (str (if date? x (get seps x x)))
                :date (apply str (repeat n (letter x (and (= x \m) (minutes? tokens i)))))))
            tokens))))

(defn- translate-format [fmt {:keys [general letter-of] :as printer}]
  (let [[from to] general]
    (str/join ";"
              (for [section (str/split fmt #";" -1)]
                (if (= (str/lower-case (str/trim section)) (str/lower-case from))
                  to
                  (print-code (code-tokens section letter-of) printer))))))

(def ^:private canonical-letters #{\y \d \m \h \s})

(defn- canonical-letter [ch]
  (canonical-letters (Character/toLowerCase (char ch))))

(defn delocalize-format
  [fmt l]
  (if-let [loc (locale l)]
    (let [local (into {} (map (fn [[a b]] [(Character/toUpperCase (char a)) b]))
                      (:format-letters loc))]
      (translate-format fmt {:general   [(:general loc "General") "General"]
                             :seps      {(:decimal loc) \. (:group loc) \,}
                             :letter-of #(or (local (Character/toUpperCase (char %)))
                                             (canonical-letter %))
                             :letter    (fn [c _] c)
                             :colors    (upper-keys (:format-colors loc))}))
    fmt))

(defn localize-format
  [fmt l]
  (if-let [loc (locale l)]
    (let [local (into {} (map (fn [[a b]] [b (Character/toUpperCase (char a))]))
                      (:format-letters loc))]
      (translate-format fmt {:general   ["General" (:general loc "General")]
                             :seps      {\. (:decimal loc) \, (:group loc)}
                             :letter-of canonical-letter
                             :letter    (fn [c minutes?] (if minutes? c (get local c c)))
                             :colors    (upper-keys (into {} (map (fn [[a b]] [b a]))
                                                          (:format-colors loc)))}))
    fmt))

(defn canonical-input
  ([input l] (canonical-input input l {}))
  ([input l {:keys [parse-dates?] :or {parse-dates? true}}]
   (let [loc (locale l)]
     (if (or (nil? loc) (not (string? input)))
       input
       (let [{:keys [formula value date?]}
             (v/parse-input input {:locale loc :parse-dates? parse-dates?})]
         (cond
           formula         (delocalize formula loc)
           date?           (v/serial->iso value)
           (string? value) (if (= {:value value} (v/parse-input value {:parse-dates? parse-dates?}))
                             value
                             (str "'" value))
           :else           value))))))

(defn- edit-format [x]
  (let [day (Math/floor x) secs (Math/round (* (- x day) 86400.0))]
    (cond (zero? day) "h:mm:ss" (zero? secs) "m/d/yyyy" :else "m/d/yyyy h:mm:ss")))

(declare local-input)

(defn cell-input [input style l]
  (cond
    (and (string? input) (= "@" (:format style))) input
    (and (number? input) (fmt/date-format? (:format style)))
    (fmt/format-value (locale l) input (edit-format input))
    :else (local-input input l)))

(defn local-input
  [input l]
  (let [loc (locale l)]
    (cond
      (nil? loc)      input
      (string? input) (cond
                        (str/starts-with? input "=") (localize input loc)
                        (= {:value input} (v/parse-input input {:locale loc})) input
                        :else (str "'" input))
      (number? input) (v/format-number loc input)
      (boolean? input) (v/boolean-text loc input)
      (v/error? input) (v/error-string loc input)
      :else input)))
