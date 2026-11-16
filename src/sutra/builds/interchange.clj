(ns sutra.builds.interchange
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.error :as me]
            [malli.json-schema :as js]
            [sutra.parser.spec :as spec]))

(def version "1.0")

(def column
  [:map {:closed false
         :description (str "One column: its name and value type. A keyword travels as a "
                           "string; a parameterised type as an array: [\"decimal\", "
                           "precision, scale], [\"array\", type], [\"struct\", {name: type}].")}
   [:attribute-name keyword?]
   [:attribute-type spec/ValueType]
   [:opts {:optional true} [:maybe map?]]])

(def dataset-schema
  [:map {:title "dataset schema"
         :description "The columns of a dataset, in order, and the columns that key a row."}
   [:dataset {:optional true :description "The dataset's name or rid."} :any]
   [:kind {:optional true :description "transform-output, standalone, materialization, …"}
    keyword?]
   [:columns [:vector column]]
   [:primary-key {:optional true} [:maybe [:vector keyword?]]]])

(def rows-write
  [:map {:title "rows write"
         :description "POST /api/datasets/:name/rows — rows written to a dataset."}
   [:rows [:vector [:map-of keyword? any?]]]
   [:mode {:optional true :description "append (default) or snapshot"}
    [:enum :append :snapshot]]
   [:key {:optional true :description "The column a row is keyed by."} keyword?]])

(def formats
  {:csv     {:media-type "text/csv; charset=utf-8" :extension "csv" :sandbox? false}
   :parquet {:media-type "application/vnd.apache.parquet" :extension "parquet" :sandbox? true}
   :arrow   {:media-type "application/vnd.apache.arrow.file" :extension "arrow"
             :sandbox? true}})

(defn- tidy [x]
  (walk/postwalk (fn [v]
                   (cond (map? v) (cond-> (into (sorted-map-by #(compare (str %1) (str %2))) v)
                                    (vector? (:enum v)) (update :enum #(vec (sort-by str %))))
                         :else v))
                 x))

(defn json-schema
  []
  (tidy
   {"$schema"     "http://json-schema.org/draft-07/schema#"
    "$id"         "https://sutra.dev/boundaries/data-interchange/dataset-schema.schema.json"
    "title"       "Sutra dataset schema"
    "description" (str "The schema a dataset carries across the platform boundary (B-13). "
                       "Version " version ".")
    "version"     version
    "x-sutra-file-formats"
    (into {}
          (for [[k {:keys [media-type extension]}] formats]
            [(name k) {"media-type" media-type "extension" extension}]))
    "oneOf"       [{"$ref" "#/definitions/dataset-schema"}
                   {"$ref" "#/definitions/rows-write"}]
    "definitions" (let [d (js/transform dataset-schema)
                        w (js/transform rows-write)]
                    (merge (:definitions d) (:definitions w)
                           {"dataset-schema" (dissoc d :definitions)
                            "rows-write"     (dissoc w :definitions)}))}))

(defn validate
  [x]
  (some-> (m/explain dataset-schema x) me/humanize))

(def max-file-bytes
  (* 32 1024 1024))

(defn csv-field
  [s sep]
  (let [s (str s)]
    (if (some #(or (= % sep) (= % \") (= % \return) (= % \newline)) s)
      (str \" (str/replace s "\"" "\"\"") \")
      s)))

(defn csv
  ([header rows] (csv header rows {}))
  ([header rows {:keys [separator] :or {separator \,}}]
   (let [line (fn [vs] (str (str/join (str separator) (map #(csv-field % separator) vs))
                            "\r\n"))]
     (apply str (line header) (map line rows)))))

(defn- type-json [t]
  (cond (keyword? t) (name t)
        (vector? t)  (mapv type-json t)
        (map? t)     (into {} (map (fn [[k v]] [(name k) (type-json v)])) t)
        :else        t))

(defn column-json
  [{:keys [attribute-name attribute-type]}]
  {"name" (name attribute-name) "type" (type-json attribute-type)})

(defn plain
  [v]
  (cond (nil? v)                            nil
        (instance? java.math.BigDecimal v)  (.toPlainString ^java.math.BigDecimal v)
        (instance? java.sql.Timestamp v)    (str (.toInstant ^java.sql.Timestamp v))
        (instance? java.sql.Date v)         (str (.toLocalDate ^java.sql.Date v))
        (instance? java.util.Date v)        (str (.toInstant ^java.util.Date v))
        (instance? java.time.temporal.Temporal v) (str v)
        (instance? java.util.UUID v)        (str v)
        (keyword? v)                        (subs (str v) 1)
        (map? v)                            (into {} (map (fn [[k x]] [(if (keyword? k)
                                                                         (name k)
                                                                         (str k))
                                                                       (plain x)]))
                                                  v)
        (coll? v)                           (mapv plain v)
        :else                               v))

(defn- base-type [t] (if (vector? t) (first t) t))

(defn typed
  [t v]
  (if (or (nil? v) (and (string? v) (str/blank? v) (not (#{:string :text} t))))
    nil
    (try
      (case (base-type t)
        (:decimal :number)            (bigdec v)
        (:byte :short :integer :long) (if (string? v) (or (parse-long (str/trim v)) v) (long v))
        (:float :double)              (if (string? v) (or (parse-double (str/trim v)) v)
                                          (double v))
        :boolean                      (if (string? v)
                                        (contains? #{"true" "1" "yes"} (str/lower-case v))
                                        (boolean v))
        :date                         (java.time.LocalDate/parse (subs (str v) 0 10))
        (:timestamp :instant)         (if (string? v)
                                        (.toInstant (java.time.OffsetDateTime/parse
                                                     (str/replace v #"Z$" "+00:00")))
                                        v)
        v)
      (catch Exception _ v))))

(defn file-rows
  [columns rows]
  (let [types (into {} (map (juxt (comp name :attribute-name) :attribute-type)) columns)]
    (mapv (fn [r]
            {:values (into {}
                           (map (fn [[k v]]
                                  (let [k (name k)]
                                    [(keyword k) (if-let [t (types k)] (typed t v) v)])))
                           r)})
          rows)))
