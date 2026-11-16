(ns sutra.types.languages
  (:refer-clojure :exclude [compute]))

(def ^:private sql-engine
  {:body :query :executor :engine :fenced-by :no-user-process :ext "sql" :export-inputs :always})

(def compute-languages
  {:sql        (assoc sql-engine :dialect :postgres :label "SQL" :alias-of :pg-sql)
   :pg-sql     (assoc sql-engine :dialect :postgres :label "Postgres SQL")
   :sqlite-sql (assoc sql-engine :dialect :sqlite :label "SQLite SQL")
   :mssql-sql  (assoc sql-engine :dialect :mssql :label "MSSQL SQL")
   :python     {:body :code  :executor :subprocess :label "Python" :ext "py"
                :fenced-by :bubblewrap :export-inputs :when-present}
   :clojure    {:body :code  :executor :in-process :label "Clojure" :ext "clj"
                :fenced-by :interpreter-sandbox :export-inputs :always}})

(def default-query-language :pg-sql)

(def selectable-languages [:python :clojure :pg-sql :sqlite-sql :mssql-sql])

(defn label [lang] (get-in compute-languages [lang :label]))

(defn file-ext [lang] (get-in compute-languages [lang :ext]))

(defn export-inputs [lang] (get-in compute-languages [lang :export-inputs]))

(defn compute-language?
  [lang]
  (contains? compute-languages lang))

(defn code-language?
  [lang]
  (= :code (get-in compute-languages [lang :body])))

(defn query-language?
  [lang]
  (= :query (get-in compute-languages [lang :body])))

(defn dialect
  [lang]
  (get-in compute-languages [lang :dialect]))

(defn body-field
  [lang]
  (get-in compute-languages [lang :body]))

(defn executor
  [lang]
  (get-in compute-languages [lang :executor]))

(def compute-language-names
  (vec (sort (keys compute-languages))))
