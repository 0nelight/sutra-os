(ns sutra.store.pg.tokens
  (:require [clojure.string :as str]
            [sutra.ref.tokens :as rt]
            [sutra.store.pg.ident :as ident]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.sql :as sql]
            [sutra.types.column :as column]
            [sutra.types.sql-ident :as sqli]))

(set! *warn-on-reflection* true)

(def ^:private document-cap
  200000)

(def ^:private separators
  (str/replace rt/separators #"['`]" ""))

(def ^:private blanks (apply str (repeat (count separators) " ")))

(defn document
  [cols]
  (str "to_tsvector('simple'::regconfig, translate(left("
       (str/join " || ' ' || " (map #(str "coalesce((" % ")::text, '')") cols))
       ", " document-cap "), '" separators "', '" blanks "'))"))

(defn- ts-query
  [[op & args]]
  (let [quoted #(str "'" % "'")
        group  (fn [sep xs] (str "(" (str/join sep xs) ")"))]
    (case op
      :word   (group " & " (map #(str (quoted %) ":*") (first args)))
      :phrase (group " <-> " (map quoted (first args)))
      :not    (str "!" (ts-query (first args)))
      :and    (group " & " (map ts-query args))
      :or     (group " | " (map ts-query args)))))

(defn tsquery
  [text]
  (some-> (rt/search-tree text) ts-query))

(defn index-name
  [table expr]
  (ident/fit (str table "_kw_" (Integer/toHexString (hash expr)))))

(def recheck-ms
  (* 60 60 1000))

(defn text-attrs
  [sig T]
  (when (get-in sig [:types T])
    (vec (sort (keep (fn [{:keys [name type]}] (when (#{:string :text} type) name))
                     (layout/attrs sig T))))))

(defn searchable-tables
  [sig]
  (vec (for [T (keys (:types sig))
             :let [text (text-attrs sig T)]
             :when (seq text)]
         [(layout/object-table T) text])))

(defn sweep-due?
  [{:keys [sig at]} sig' now]
  (or (nil? at)
      (< recheck-ms (- now at))
      (and (not (identical? sig sig'))
           (< 60000 (- now at))
           (not= sig sig'))))

(defn- drop-index [nm]
  (str "DROP INDEX CONCURRENTLY IF EXISTS main." (sql/dq nm)))

(defn wanted-index
  [table attrs]
  (let [expr (sql/render (document (map #(sqli/quote-ident (column/sql-name %)) attrs)))
        nm   (index-name table expr)]
    {:name   nm
     :create (str "CREATE INDEX CONCURRENTLY IF NOT EXISTS " (sql/dq nm)
                  " ON main." (sql/dq table) " USING gin ((" expr "))")}))

(defn statements
  [table {:keys [name create]} have analyzed?]
  (let [stands? (true? (get have name))]
    (concat (when (false? (get have name)) [(drop-index name)])
            (when-not stands? [create])
            (when-not stands?
              (for [old (sort (keys have))
                    :when (and (str/starts-with? old (str table "_kw_")) (not= old name))]
                (drop-index old)))
            (when-not (and stands? analyzed?) [(str "ANALYZE main." (sql/dq table))]))))

(defn due?
  [asked k now]
  (< recheck-ms (- now (get asked k 0))))
