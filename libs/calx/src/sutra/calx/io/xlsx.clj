(ns sutra.calx.io.xlsx
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [sutra.calx.value :as v]
            [sutra.calx.address :as addr]
            [sutra.calx.parser :as parser]
            [sutra.calx.ast :as ast]
            [sutra.calx.workbook :as wb]
            [sutra.calx.io.edn :as edn-io]
            [sutra.calx.io.zip :as zip])
  (:import (java.util.zip ZipOutputStream ZipEntry)
           (javax.xml.parsers DocumentBuilderFactory)
           (org.w3c.dom Element Node)
           (java.io ByteArrayInputStream ByteArrayOutputStream)))

(set! *warn-on-reflection* true)

(defn- read-zip [in]
  (zip/read-entries in))

(defn- parse-xml ^Element [^bytes bs]
  (let [f (doto (DocumentBuilderFactory/newInstance)
            (.setNamespaceAware true)
            (.setFeature "http://apache.org/xml/features/disallow-doctype-decl" true)
            (.setExpandEntityReferences false))]
    (.getDocumentElement (.parse (.newDocumentBuilder f) (ByteArrayInputStream. bs)))))

(defn- children
  ([^Element el] (filter #(instance? Element %)
                         (let [nl (.getChildNodes el)]
                           (map #(.item nl %) (range (.getLength nl))))))
  ([el local] (filter #(= local (.getLocalName ^Element %)) (children el))))

(defn- child [el local] (first (children el local)))

(defn- descendants* [^Element el local]
  (let [nl (.getElementsByTagNameNS el "*" local)] (map #(.item nl %) (range (.getLength nl)))))

(defn- attr [^Element el name] (let [a (.getAttribute el name)] (when-not (str/blank? a) a)))

(defn- attr-ns [^Element el local]
  (let [attrs (.getAttributes el)]
    (some (fn [i]
            (let [^Node a (.item attrs i)] (when (= local (.getLocalName a)) (.getNodeValue a))))
          (range (.getLength attrs)))))

(defn- text [^Element el] (when el (.getTextContent el)))

(def ^:private builtin-formats
  {1 "0" 2 "0.00" 3 "#,##0" 4 "#,##0.00" 9 "0%" 10 "0.00%" 11 "0.00E+00" 12 "# ?/?" 13 "# ??/??"
   14 "m/d/yyyy" 15 "d-mmm-yy" 16 "d-mmm" 17 "mmm-yy" 18 "h:mm AM/PM" 19 "h:mm:ss AM/PM"
   20 "h:mm" 21 "h:mm:ss" 22 "m/d/yyyy h:mm" 37 "#,##0 ;(#,##0)" 38 "#,##0 ;[Red](#,##0)"
   39 "#,##0.00;(#,##0.00)" 40 "#,##0.00;[Red](#,##0.00)" 45 "mm:ss" 46 "[h]:mm:ss" 47 "mmss.0"
   48 "##0.0E+0" 49 "@"})

(defn- resolve-path [base target]
  (if (str/starts-with? target "/")
    (subs target 1)
    (let [parts (reduce (fn [acc p] (case p ".." (pop acc) "." acc (conj acc p)))
                        (vec (butlast (str/split base #"/")))
                        (str/split target #"/"))]
      (str/join "/" parts))))

(defn- rels [files path]
  (when-let [bs (files path)]
    (into {} (map (fn [r] [(attr r "Id") (attr r "Target")]))
          (descendants* (parse-xml bs) "Relationship"))))

(defn- shared-strings [files path]
  (if-let [bs (and path (files path))]
    (mapv (fn [si] (apply str (map text (descendants* si "t")))) (children (parse-xml bs) "si"))
    []))

(defn- styles->formats
  [files path]
  (if-let [bs (and path (files path))]
    (let [root (parse-xml bs)
          custom (into {} (map (fn [nf] [(parse-long (attr nf "numFmtId")) (attr nf "formatCode")]))
                       (some-> (child root "numFmts") (children "numFmt")))
          xfs (some-> (child root "cellXfs") (children "xf"))]
      (into {} (keep-indexed (fn [i xf]
                               (let [id (parse-long (or (attr xf "numFmtId") "0"))
                                     code (or (custom id) (builtin-formats id))]
                                 (when (and code (pos? id)) [i code]))))
            xfs))
    {}))

(defn- clean-formula
  [f]
  (-> f (str/replace "_xlpm." "") (str/replace "_xlfn._xlws." "") (str/replace "_xlfn." "")
      (str/replace "_xlws." "")))

(defn- shared-formula [master-text [mr mc] [r c]]
  (let [{:keys [ast error]} (parser/try-parse master-text)]
    (if error
      master-text
      (parser/unparse (ast/offset ast (- r mr) (- c mc)) {}))))

(defn- read-sheet [files path strings formats]
  (let [root (parse-xml (files path))
        rows (some-> (child root "sheetData") (children "row"))
        shared (atom {})
        array-areas (atom [])
        cells (transient [])
        styles (transient [])]
    (doseq [row rows, c (children row "c")]
      (let [a (attr c "r")
            [r col] (addr/parse-cell a)
            t (or (attr c "t") "n")
            f-el (child c "f")
            ftype (some-> f-el (attr "t"))
            fsi (some-> f-el (attr "si"))
            _ (when (and (= ftype "array") (attr f-el "ref"))
                (swap! array-areas conj (addr/parse-area (attr f-el "ref"))))
            ftext (some-> f-el text clean-formula)
            formula (cond
                      (and (= ftype "shared")
                           (seq ftext)) (do (swap! shared assoc fsi [ftext [r col]]) ftext)
                      (= ftype
                         "shared") (when-let [[m pos] (@shared fsi)] (shared-formula m pos [r col]))
                      (seq ftext) ftext)
            raw (text (child c "v"))
            value (case t
                    "s" (some->> raw parse-long (get strings))
                    "inlineStr" (apply str (map text (descendants* c "t")))
                    "str" raw
                    "b" (= raw "1")
                    "e" (some-> raw v/parse-error)
                    "d" (some-> raw v/parse-date-time)
                    (some-> raw parse-double))
            fmt (some-> (attr c "s") parse-long formats)]
        (cond
          formula (conj! cells [a {:formula (str "=" formula)}])
          (some? value) (conj! cells [a value]))
        (when fmt (conj! styles [a {:format fmt}]))))
    {:cells (let [areas @array-areas
                  spilled? (fn [a]
                             (let [[r c] (addr/parse-cell a)]
                               (some (fn [{:keys [r1 c1 r2 c2]}]
                                       (and (<= r1 r r2) (<= c1 c c2)
                                            (not (and (= r r1) (= c c1)))))
                                     areas)))]
              (into (array-map) (remove (fn [[a x]] (and (not (map? x)) (spilled? a))))
                    (persistent! cells)))
     :styles (into (array-map) (persistent! styles))
     :rows (into {} (keep (fn [row] (when-let [ht (attr row "ht")]
                                      [(dec (parse-long (attr row "r")))
                                       {:height (parse-double ht)}]))
                          rows))
     :cols (into {} (for [col (some-> (child root "cols") (children "col"))
                          :let [w (attr col "width")]
                          :when w
                          i (range (dec (parse-long (attr col "min")))
                                   (parse-long (attr col "max")))
                          :when (< i 256)]
                      [i {:width (parse-double w)}]))}))

(defn xlsx->doc
  [in]
  (let [files (read-zip (if (bytes? in) (ByteArrayInputStream. in) in))
        root-rels (rels files "_rels/.rels")
        wb-path (or (some (fn [[_ t]] (when (str/ends-with? t "workbook.xml") (resolve-path "" t)))
                          root-rels)
                    "xl/workbook.xml")
        wb-rels-path (str (str/join "/" (butlast (str/split wb-path #"/"))) "/_rels/"
                          (last (str/split wb-path #"/")) ".rels")
        wb-rels (rels files wb-rels-path)
        wb-xml (parse-xml (files wb-path))
        target-of (fn [suffix]
                    (some (fn [[_ t]] (when (str/ends-with? t suffix) (resolve-path wb-path t)))
                          wb-rels))
        strings (shared-strings files (target-of "sharedStrings.xml"))
        formats (styles->formats files (target-of "styles.xml"))
        sheet-els (some-> (child wb-xml "sheets") (children "sheet"))
        sheet-names (mapv #(attr % "name") sheet-els)
        sheets (mapv (fn [s]
                       (let [path (resolve-path wb-path (wb-rels (attr-ns s "id")))]
                         (merge {:name (attr s "name")} (read-sheet files path strings formats))))
                     sheet-els)
        names (for [dn (some-> (child wb-xml "definedNames") (children "definedName"))
                    :let [n (attr dn "name")]
                    :when (and n (not (str/starts-with? n "_xlnm.")) (wb/valid-name? n))]
                {:name n
                 :scope (some->> (attr dn "localSheetId") parse-long (get sheet-names))
                 :formula (str "=" (clean-formula (text dn)))})]
    {:calx/format 1 :sheets sheets :names (vec names)}))

(defn read-xlsx
  ([in] (read-xlsx in {}))
  ([in opts] (edn-io/doc->workbook (xlsx->doc in) opts)))

(defn- esc [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace #"[\x00-\x08\x0B\x0C\x0E-\x1F]" "")))

(def ^:private future-fns
  #{"XLOOKUP" "XMATCH" "SORT" "SORTBY" "FILTER" "UNIQUE" "SEQUENCE" "RANDARRAY" "LET" "LAMBDA" "MAP"
    "REDUCE"
    "SCAN" "BYROW" "BYCOL" "MAKEARRAY" "ISOMITTED" "TEXTJOIN" "CONCAT" "IFS" "SWITCH" "MAXIFS"
    "MINIFS" "IFNA"
    "XOR" "DAYS" "ISOWEEKNUM" "TEXTBEFORE" "TEXTAFTER" "TEXTSPLIT" "VSTACK" "HSTACK" "TAKE" "DROP"
    "CHOOSEROWS"
    "CHOOSECOLS" "TOCOL" "TOROW" "WRAPROWS" "WRAPCOLS" "EXPAND" "SHEET" "SHEETS" "FORMULATEXT"
    "ISFORMULA"
    "NUMBERVALUE" "UNICHAR" "UNICODE" "ARABIC" "BASE" "DECIMAL" "COMBINA" "PERMUTATIONA" "ACOT"
    "ACOTH" "COT"
    "COTH" "CSC" "CSCH" "SEC" "SECH" "BITAND" "BITOR" "BITXOR" "BITLSHIFT" "BITRSHIFT" "IMCOSH"
    "PDURATION" "RRI"
    "GAMMA" "GAUSS" "PHI" "BINOM.DIST.RANGE" "ARRAYTOTEXT" "VALUETOTEXT" "REGEXTEST" "REGEXEXTRACT"
    "REGEXREPLACE" "GROUPBY" "PIVOTBY" "TRIMRANGE"})

(def ^:private xlws-fns #{"FILTER" "SORT"})

(defn- file-fn-name [f]
  (cond
    (xlws-fns f) (str "_xlfn._xlws." f)
    (or (future-fns f)
        (and (str/includes? f ".") (not (#{"ERROR.TYPE"} f)))) (str "_xlfn." f)
    :else f))

(defn- lambda-params
  [a]
  (into #{}
        (mapcat (fn [n]
                  (case (:fn n)
                    "LAMBDA" (keep #(when (= :name (:kind %)) (str/upper-case (:name %)))
                                   (butlast (:args n)))
                    "LET" (keep #(when (= :name (:kind %)) (str/upper-case (:name %)))
                                (take-nth 2 (butlast (:args n))))
                    nil)))
        (filter #(= :call (:kind %)) (ast/nodes a))))

(defn- file-formula [w a]
  (let [params (lambda-params a)
        a (ast/postwalk (fn [n]
                          (cond
                            (= :call (:kind n)) (if (params (:fn n))
                                                  (assoc n :fn (str "_xlpm." (:fn n)))
                                                  (assoc n :fn (file-fn-name (:fn n))))
                            (and (= :name (:kind n)) (params (str/upper-case (:name n))))
                            (assoc n :name (str "_xlpm." (:name n)))
                            :else n))
                        a)]
    (parser/unparse a {:sheet-name #(wb/sheet-name w %)})))

(defn- value-xml
  [x]
  (cond
    (nil? x) nil
    (number? x) ["" (str "<v>" (if (== x (Math/rint x)) (str (long x)) (str x)) "</v>")]
    (boolean? x) [" t=\"b\"" (str "<v>" (if x 1 0) "</v>")]
    (v/error? x) [" t=\"e\"" (str "<v>" (esc (v/error-string x)) "</v>")]
    (string? x) [" t=\"str\"" (str "<v>" (esc x) "</v>")]
    :else nil))

(defn- cell-xml [w id r c cl style-idx]
  (let [a (addr/cell->a1 r c)
        k [id r c]
        s-attr (if style-idx (str " s=\"" style-idx "\"") "")
        x (wb/value w k)]
    (cond
      (:ast cl)
      (let [[t inner] (or (value-xml x) ["" ""])
            spill (wb/spill-area w k)
            f (esc (file-formula w (:ast cl)))
            f-xml (if spill
                    (str "<f t=\"array\" ref=\"" a ":"
                         (addr/cell->a1 (+ r (dec (first spill))) (+ c (dec (second spill)))) "\">"
                         f "</f>")
                    (str "<f>" f "</f>"))]
        (str "<c r=\"" a "\"" s-attr t ">" f-xml inner "</c>"))

      (string? (:value cl))
      (str "<c r=\"" a "\"" s-attr " t=\"inlineStr\"><is><t xml:space=\"preserve\">"
           (esc (:value cl)) "</t></is></c>")

      (some? x)
      (let [[t inner] (value-xml x)] (str "<c r=\"" a "\"" s-attr t ">" inner "</c>"))

      :else (if style-idx (str "<c r=\"" a "\"" s-attr "/>") ""))))

(defn- sheet-xml [w id style-index]
  (let [{:keys [cells rows cols]} (get-in w [:sheets id])
        spilled (for [[[s r c] _] (:owner w) :when (= s id)] [r c])
        positions (sort (distinct (concat (keys cells) spilled)))
        by-row (group-by first positions)]
    (str "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
         "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" "
         "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
         (when (seq cols)
           (str "<cols>" (apply str (for [[c {:keys [width]}] (sort cols) :when width]
                                      (str "<col min=\"" (inc c) "\" max=\"" (inc c) "\" width=\""
                                           width "\" customWidth=\"1\"/>")))
                "</cols>"))
         "<sheetData>"
         (apply str
                (for [r (sort (distinct (concat (keys by-row) (keys rows))))]
                  (let [ht (get-in rows [r :height])]
                    (str "<row r=\"" (inc r) "\""
                         (when ht (str " ht=\"" ht "\" customHeight=\"1\"")) ">"
                         (apply str
                                (for [[_ c] (get by-row r)]
                                  (let [cl (get cells [r c])
                                        fmt (get-in cl [:style :format])]
                                    (if cl
                                      (cell-xml w id r c cl (style-index fmt))
                                      (let [[t
                                             inner] (or (value-xml (wb/value w [id r c])) ["" ""])]
                                        (str "<c r=\"" (addr/cell->a1 r c) "\"" t ">" inner
                                             "</c>"))))))
                         "</row>"))))
         "</sheetData></worksheet>")))

(defn- styles-xml [formats]
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
       "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">"
       (when (seq formats)
         (str "<numFmts count=\"" (count formats) "\">"
              (apply str
                     (map-indexed (fn [i f]
                                    (str "<numFmt numFmtId=\"" (+ 164 i) "\" formatCode=\"" (esc f)
                                         "\"/>")) formats))
              "</numFmts>"))
       "<fonts count=\"1\"><font><sz val=\"11\"/><name val=\"Calibri\"/></font></fonts>"
       "<fills count=\"2\"><fill><patternFill patternType=\"none\"/></fill><fill><patternFill "
       "patternType=\"gray125\"/></fill></fills>"
       "<borders count=\"1\"><border><left/><right/><top/><bottom/><diagonal/></border></borders>"
       "<cellStyleXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" "
       "borderId=\"0\"/></cellStyleXfs>"
       "<cellXfs count=\"" (inc (count formats))
       "\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\"/>"
       (apply str
              (map-indexed
               (fn [i _]
                 (str "<xf numFmtId=\"" (+ 164 i)
                      "\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\" "
                      "applyNumberFormat=\"1\"/>")) formats))
       "</cellXfs></styleSheet>"))

(defn- workbook-xml [w]
  (let [order (:order w)
        idx (into {} (map-indexed (fn [i id] [id i]) order))]
    (str "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
         "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" "
         "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><sheets>"
         (apply str
                (map-indexed (fn [i id]
                               (str "<sheet name=\"" (esc (wb/sheet-name w id)) "\" sheetId=\""
                                    (inc i) "\" r:id=\"rId" (inc i) "\"/>")) order))
         "</sheets>"
         (when (seq (:names w))
           (str "<definedNames>"
                (apply str (for [[_ {:keys [name scope ast]}] (sort-by (comp str key) (:names w))]
                             (str "<definedName name=\"" (esc name) "\""
                                  (when scope (str " localSheetId=\"" (idx scope) "\"")) ">"
                                  (esc (file-formula w ast)) "</definedName>")))
                "</definedNames>"))
         "<calcPr fullCalcOnLoad=\"1\"/></workbook>")))

(defn write-xlsx
  [w out]
  (let [formats (vec (distinct (keep #(get-in % [:style :format])
                                     (for [[_ sh] (:sheets w) [_ cl] (:cells sh)] cl))))
        style-index (into {} (map-indexed (fn [i f] [f (inc i)]) formats))
        n (count (:order w))
        entries
        (concat
         [["[Content_Types].xml"
           (str "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
                "<Default Extension=\"rels\" "
                "ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>"
                "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
                "<Override PartName=\"/xl/workbook.xml\" "
                "ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>"
                "<Override PartName=\"/xl/styles.xml\" "
                "ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/>"
                (apply str
                       (for [i (range 1 (inc n))]
                         (str "<Override PartName=\"/xl/worksheets/sheet" i
                              ".xml\" "
                              "ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>")))
                "</Types>")]
          ["_rels/.rels"
           (str "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                "<Relationships "
                "xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                "<Relationship Id=\"rId1\" "
                "Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>"
                "</Relationships>")]
          ["xl/workbook.xml" (workbook-xml w)]
          ["xl/_rels/workbook.xml.rels"
           (str "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                "<Relationships "
                "xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                (apply str
                       (for [i (range 1 (inc n))]
                         (str "<Relationship Id=\"rId" i
                              "\" "
                              "Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet" i ".xml\"/>")))
                "<Relationship Id=\"rId" (inc n)
                "\" "
                "Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>"
                "</Relationships>")]
          ["xl/styles.xml" (styles-xml formats)]]
         (map-indexed (fn [i id]
                        [(str "xl/worksheets/sheet" (inc i) ".xml") (sheet-xml w id style-index)])
                      (:order w)))]
    (with-open [zout (ZipOutputStream. (io/output-stream out))]
      (doseq [[path content] entries]
        (.putNextEntry zout (ZipEntry. ^String path))
        (.write zout (.getBytes ^String content "UTF-8"))
        (.closeEntry zout)))))

(defn xlsx-bytes
  ^bytes [w]
  (let [bos (ByteArrayOutputStream.)] (write-xlsx w bos) (.toByteArray bos)))
