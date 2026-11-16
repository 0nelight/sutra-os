(ns sutra.types.sql-traps
  (:require [clojure.string :as str]))

(def ^:private literal
  #"--[^\n]*|/\*[\s\S]*?\*/|'(?:[^']|'')*'|\"(?:[^\"]|\"\")*\"")

(defn- blank
  [sql]
  (str/replace (str sql) literal (fn [m] (apply str (repeat (count m) \space)))))

(def ^:private bare-char
  #"(?i)^(char|character|bpchar)$")

(def ^:private colon-char
  #"(?i)::\s*(?:char|character|bpchar)\b(?!\s*(?:\(|varying\b))")

(defn- word-at?
  [s i w]
  (let [n (count w)
        word? (fn [j] (and (< -1 j (count s)) (re-matches #"[A-Za-z0-9_$]" (str (nth s j)))))]
    (and (<= (+ i n) (count s))
         (= w (subs s i (+ i n)))
         (not (word? (dec i)))
         (not (word? (+ i n))))))

(defn- cast-target
  [s open]
  (loop [i (inc open) depth 1 as-at nil]
    (when (< i (count s))
      (let [c (nth s i)]
        (cond
          (= \( c) (recur (inc i) (inc depth) as-at)
          (= \) c) (if (= 1 depth)
                     (when as-at [as-at (str/trim (subs s (+ as-at 2) i)) (inc i)])
                     (recur (inc i) (dec depth) as-at))
          (and (= 1 depth) (nil? as-at) (word-at? s i "as")) (recur (+ i 2) depth i)
          :else (recur (inc i) depth as-at))))))

(defn- cast-hits
  [sql s]
  (let [lower (str/lower-case s)]
    (loop [from 0 acc []]
      (if-let [i (str/index-of lower "cast" from)]
        (let [open (some-> (re-find #"^\s*\(" (subs lower (+ i 4))) count (+ i 3))
              [_ target end] (when (and open (word-at? lower i "cast")) (cast-target lower open))]
          (recur (inc i)
                 (cond-> acc
                   (and target (re-matches bare-char target)) (conj (subs sql i end)))))
        acc))))

(defn char-casts
  [sql]
  (let [s (blank sql)]
    (vec (concat (cast-hits (str sql) s) (re-seq colon-char s)))))

(defn char-cast-findings
  [transform query]
  (for [hit (char-casts query)]
    {:rule :sql-char-without-length :severity :error
     :entity-class :transform :entity transform :hit hit
     :message (str "SQL transform " (pr-str transform) " casts to CHAR without a length: "
                   (pr-str hit) ". In Postgres a bare CHAR is char(1) — every value is"
                   " cut to its first character, silently, with no error (\"23-2026\""
                   " became \"23-2\" and a key collapsed). Write AS VARCHAR (or"
                   " AS TEXT); if one character is really meant, say CHAR(1).")}))

(defn- elide [n s] (if (> (count s) n) (str (subs s 0 n) "…") s))

(defn verdict-findings
  [verdicts]
  (for [{:keys [transform state detail missing]} verdicts
        :when (not= :ok state)]
    (if (= :refused state)
      {:rule :sql-refused :severity :error :entity-class :transform :entity transform
       :message (str "SQL transform " (pr-str transform) " does not compile in Postgres,"
                     " the store it runs on: " (elide 1500 (str detail))
                     " — the build would die on exactly this. DuckDB/MySQL spellings are"
                     " not Postgres: YEAR(d) is EXTRACT(YEAR FROM d), INTERVAL 21 DAY is"
                     " INTERVAL '21 days'.")}
      {:rule :sql-unverified :severity :warn :entity-class :transform :entity transform
       :missing missing
       :message (str "SQL transform " (pr-str transform) " was NOT compiled against Postgres"
                     (if (seq missing)
                       (str ": input(s) " (pr-str missing) " do not exist in the dataset"
                            " store yet (new in this push, or never built)")
                       (str ": " (elide 500 (str detail))))
                     ". Unverified, not approved — its first build is the check.")})))

(def ^:private select-end
  #{"from" "union" "except" "intersect" "where" "group" "order" "limit" "window"
    "into" "having" "offset" "fetch"})

(def ^:private not-an-alias
  #{"end" "and" "or" "not" "is" "null" "true" "false" "between" "in" "like" "ilike"
    "then" "else" "when" "as" "distinct" "asc" "desc"})

(defn- outer-select-list
  [s]
  (let [n (count s)]
    (loop [i 0 depth 0 start nil]
      (if (>= i n)
        (when start (subs s start n))
        (let [c (nth s i)]
          (cond
            (= \( c) (recur (inc i) (inc depth) start)
            (= \) c) (recur (inc i) (dec depth) start)
            (and (zero? depth) (nil? start) (word-at? s i "select")) (recur (+ i 6) depth (+ i 6))
            (and (zero? depth) start (some #(word-at? s i %) select-end)) (subs s start i)
            :else (recur (inc i) depth start)))))))

(defn- split-top
  [s]
  (loop [i 0 depth 0 from 0 acc []]
    (if (>= i (count s))
      (conj acc (subs s from))
      (let [c (nth s i)]
        (cond
          (= \( c) (recur (inc i) (inc depth) from acc)
          (= \) c) (recur (inc i) (dec depth) from acc)
          (and (zero? depth) (= \, c)) (recur (inc i) depth (inc i) (conj acc (subs s from i)))
          :else (recur (inc i) depth from acc))))))

(defn- item-name
  [item]
  (let [t (str/trim item)
        ident "[a-z_][a-z0-9_$]*"
        alias (fn [a] (when-not (not-an-alias a) a))]
    (or (some-> (re-find (re-pattern (str "(?s).*\\bas\\s+(" ident ")$")) t) second alias)
        (some-> (re-find (re-pattern (str "^(?:" ident "\\.)*(" ident ")$")) t) second alias)
        (some-> (re-find (re-pattern (str "^(?:" ident "\\.)*" ident "\\s+(" ident ")$")) t) second
                alias)
        (some-> (re-find (re-pattern (str "(?s).*\\)\\s*(" ident ")$")) t) second alias))))

(defn select-output-names
  [sql]
  (let [s (str/lower-case (blank sql))
        body (some-> (outer-select-list s)
                     (str/replace #"^\s*(?:all\b|distinct\s+on\s*\([^)]*\)|distinct\b)" ""))]
    (when body
      (into [] (comp (keep item-name) (distinct)) (split-top body)))))
