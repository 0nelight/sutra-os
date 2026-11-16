(ns sutra.kit.read-only-sql
  (:require [clojure.string :as str]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(def ^:private lexers
  {:postgres {:nested-comments? true :dollar-quotes? true :e-strings? true :unicode-idents? true}
   :mysql    {:backslash? true :hash-comments? true :dash-needs-space? true :bang-comments? true}
   :mssql    {:nested-comments? true :brackets? true}
   :sqlite   {:brackets? true}
   :oracle   {:q-quotes? true}})

(def ^:private leading-verbs #{"select" "with" "values" "table" "explain"})

(def ^:private forbidden-words
  #{"insert" "update" "delete" "drop" "alter" "create" "truncate" "merge" "grant" "revoke"
    "call" "copy" "replace" "upsert" "attach" "detach" "vacuum" "pragma" "exec" "execute"
    "into" "lock" "begin" "commit" "rollback" "backup" "restore" "dbcc" "kill" "shutdown"
    "reconfigure" "deny" "waitfor"})

(def ^:private also-functions #{"replace" "insert" "truncate"})

(def ^:private forbidden-functions
  #{"pg_read_file" "pg_read_binary_file" "pg_ls_dir" "pg_stat_file" "pg_ls_logdir"
    "pg_ls_waldir" "pg_ls_tmpdir" "pg_ls_archive_statusdir" "lo_import" "lo_export"
    "lo_from_bytea" "lo_put" "lo_create" "lo_unlink" "pg_terminate_backend"
    "pg_cancel_backend" "pg_reload_conf" "pg_rotate_logfile" "pg_promote" "pg_notify"
    "set_config" "query_to_xml" "query_to_xml_and_xmlschema" "query_to_xmlschema"
    "cursor_to_xml" "ts_stat" "ts_rewrite" "setval" "nextval" "xp_cmdshell" "openrowset"
    "opendatasource" "openquery" "load_file" "load_extension" "sys_exec" "sys_eval"
    "writefile" "fts3_tokenizer"})

(defn- forbidden-function? [w]
  (or (contains? forbidden-functions w) (str/starts-with? w "dblink")))

(defn- at? [^String s i ^String piece] (.startsWith s piece (int i)))

(defn- quote-end
  [^String s i ^Character q bs?]
  (let [n (.length s)]
    (loop [j (inc i)]
      (when (< j n)
        (let [c (.charAt s j)]
          (cond (and bs? (= \\ c))          (recur (+ j 2))
                (not= q c)                  (recur (inc j))
                (and (< (inc j) n)
                     (= q (.charAt s (inc j)))) (recur (+ j 2))
                :else                       (inc j)))))))

(defn- comment-end [^String s i nested?]
  (let [n (.length s)]
    (loop [j (+ i 2) depth 1]
      (when (< j n)
        (cond (at? s j "*/")                (if (= 1 depth) (+ j 2) (recur (+ j 2) (dec depth)))
              (and nested? (at? s j "/*"))  (recur (+ j 2) (inc depth))
              :else                         (recur (inc j) depth))))))

(defn- line-end ^long [^String s i]
  (let [j (str/index-of s "\n" i)] (if j (inc j) (.length s))))

(defn- dollar-end [^String s i]
  (when-let [tag (re-find #"^\$(?:[A-Za-z_][A-Za-z0-9_]*)?\$" (subs s i))]
    (when-let [j (str/index-of s tag (+ i (count tag)))]
      (+ j (count tag)))))

(def ^:private q-closers {\[ \] \{ \} \( \) \< \>})

(defn- q-quote-end [^String s i]
  (let [d (+ i (if (#{\n \N} (.charAt s i)) 3 2))]
    (when (< d (.length s))
      (let [close (str (q-closers (.charAt s d) (.charAt s d)) "'")]
        (some-> (str/index-of s close (inc d)) (+ (count close)))))))

(defn- word-char? [c]
  (or (Character/isLetterOrDigit (char c)) (= \_ c) (= \$ c)))

(defn- skip! ^long [sql end what]
  (or end (refusal/refuse! :refusal.foreign/sql-unterminated {:sql sql :what what})))

(defn- tokens
  [^String sql {:keys [nested-comments? dollar-quotes? e-strings? unicode-idents? backslash?
                       hash-comments? dash-needs-space? bang-comments? brackets? q-quotes?]}]
  (let [n (.length sql)]
    (loop [i 0 out (transient [])]
      (if (>= i n)
        (persistent! out)
        (let [c  (.charAt sql i)
              c2 (when (< (inc i) n) (.charAt sql (inc i)))]
          (cond
            (Character/isWhitespace c) (recur (inc i) out)

            (and (= \- c) (= \- c2)
                 (or (not dash-needs-space?) (>= (+ i 2) n)
                     (<= (int (.charAt sql (+ i 2))) 32)))
            (recur (line-end sql i) out)

            (and hash-comments? (= \# c)) (recur (line-end sql i) out)

            (and (= \/ c) (= \* c2))
            (if (and bang-comments? (at? sql i "/*!"))
              (refusal/refuse! :refusal.foreign/sql-ambiguous {:sql sql :what "/*!"})
              (recur (skip! sql (comment-end sql i nested-comments?) "comment") out))

            (and q-quotes? (re-find #"^[nN]?[qQ]'" (subs sql i (min n (+ i 3)))))
            (recur (skip! sql (q-quote-end sql i) "q-quote") (conj! out [:literal]))

            (and e-strings? (#{\e \E} c) (= \' c2))
            (recur (skip! sql (quote-end sql (inc i) \' true) "string") (conj! out [:literal]))

            (and unicode-idents? (#{\u \U} c) (= \& c2) (at? sql (+ i 2) "\""))
            (refusal/refuse! :refusal.foreign/sql-ambiguous {:sql sql :what "U&"})

            (= \' c)
            (recur (skip! sql (quote-end sql i \' backslash?) "string") (conj! out [:literal]))

            (or (= \" c) (= \` c))
            (let [end (skip! sql (quote-end sql i c (and backslash? (= \" c))) "identifier")]
              (recur end (conj! out [:ident (subs sql (inc i) (dec end))])))

            (and brackets? (= \[ c))
            (let [end (skip! sql (quote-end sql i \] false) "identifier")]
              (recur end (conj! out [:ident (subs sql (inc i) (dec end))])))

            (and dollar-quotes? (= \$ c) (dollar-end sql i))
            (recur (long (dollar-end sql i)) (conj! out [:literal]))

            (word-char? c)
            (let [end (loop [j i]
                        (if (and (< j n) (word-char? (.charAt sql j))) (recur (inc j)) j))]
              (recur (long end) (conj! out [:word (str/lower-case (subs sql i end))])))

            :else (recur (inc i) (conj! out [:punct c]))))))))

(defn- statements [toks]
  (->> (partition-by #(= [:punct \;] %) toks)
       (remove #(= [:punct \;] (first %)))))

(defn- lead-word [stmt]
  (let [[kind v] (first (drop-while #(= [:punct \(] %) stmt))]
    (when (= :word kind) v)))

(defn- offence
  [stmt]
  (let [explain? (= "explain" (lead-word stmt))]
    (some (fn [[[kind w] [_ nxt]]]
            (cond
              (= :ident kind)               (when (forbidden-function? (str/lower-case w)) w)
              (not= :word kind)             nil
              (forbidden-function? w)       w
              (and (also-functions w) (= \( nxt)) nil
              (forbidden-words w)           w
              (and explain? (= "analyze" w)) w
              (and (= "for" w) (#{"update" "share" "no" "key"} nxt)) (str "for " nxt)))
          (partition 2 1 [nil] stmt))))

(defn- check-reading! [sql lexer]
  (let [stmts (statements (tokens sql lexer))]
    (when (empty? stmts)
      (refusal/refuse! :refusal.foreign/sql-empty {:sql sql}))
    (when (next stmts)
      (refusal/refuse! :refusal.foreign/sql-multiple {:sql sql}))
    (let [stmt (first stmts)
          lead (lead-word stmt)]
      (when-not (leading-verbs lead)
        (refusal/refuse! :refusal.foreign/sql-not-select {:sql sql :leading lead}))
      (when-let [bad (offence stmt)]
        (refusal/refuse! :refusal.foreign/sql-forbidden-verb {:sql sql :verb bad})))))

(defn assert-read-only-sql!
  ([sql] (assert-read-only-sql! sql nil))
  ([sql dialect]
   (let [s (str sql)]
     (doseq [lexer (if-let [l (lexers dialect)] [l] (vals lexers))]
       (check-reading! s lexer)))
   sql))
