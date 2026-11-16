(ns sutra.compute.protocol
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.error :as me]
            [malli.json-schema :as js]))

(def version
  "1.1")

(defn major [v] (some-> v str (str/split #"\.") first))

(defn compatible?
  [v]
  (or (nil? v) (= (major v) (major version))))

(def wire-value
  [:any {:description
         (str "A JSON value. A string starting with \"~\" is tagged: \"~f<digits>\" is an "
              "exact decimal (Python decimal.Decimal), \"~~…\" is the literal text \"~…\". "
              "Dates cross as ISO-8601 text (instants in UTC with Z).")}])

(def ^:private row
  [:map-of {:description "One row: column name → wire value."} :keyword wire-value])

(def ^:private rows-kind
  [:enum {:description "What the rows file holds; absent for a datasource input."}
   "transform" "object-type" "dataset" "datasource" "media-set" "media-set-items" "csv"
   "file"])

(def ^:private media-reach
  [[:media-dir {:optional true :description "A directory of <ref>.bin blobs, read-only."}
    :string]
   [:media-url {:optional true :description "URL the process fetches media from."} :string]
   [:media-grants {:optional true :description "Grants the media URL requires."} :any]
   [:media-refs {:optional true} [:sequential :string]]])

(def rows-entry
  (into [:map {:title "rows input"
               :description (str "Rows in a file at :path: a JSON array of rows (csv: RFC 4180 "
                                 "text with a header, file: raw UTF-8 text).")}
         [:kind {:optional true} rows-kind]
         [:path {:description "Absolute path inside the sandbox, read-only."} :string]
         [:aliases {:optional true :description "Column spellings the row also answers to."}
          [:map-of :keyword :string]]
         [:derived-names {:optional true
                          :description "Derived attributes a row refuses instead of None."}
          [:sequential :string]]]
        media-reach))

(def files-entry
  [:map {:title "files input" :description "A directory the transform reads as a FileSystem."}
   [:kind [:enum "files"]]
   [:path :string]])

(def scalar-entry
  [:map {:title "scalar input"}
   [:kind [:enum "scalar"]]
   [:value wire-value]])

(def media-entry
  (into [:map {:title "media input" :description "One media item by its address."}
         [:kind [:enum "media"]]
         [:address :string]]
        media-reach))

(def language-model-entry
  [:map {:title "language-model input"
         :description "A model the process calls through the platform's relay."}
   [:kind [:enum "language-model"]]
   [:source {:optional true} :string]
   [:model {:optional true} :string]
   [:base-url {:optional true} :string]])

(def manifest
  [:map-of {:title "inputs manifest"
            :description (str "The file passed as --inputs: input alias → entry. Written by "
                              "the host, read once by the process.")}
   :keyword
   [:or scalar-entry media-entry language-model-entry files-entry rows-entry]])

(def ^:private media-item
  [:map {:description (str "A blob the process produced (put_media_item): its bytes as b64, "
                           "or a file of that name it wrote into SUTRA_MEDIA_SPOOL.")}
   [:hash {:optional true} :string]
   [:mime {:optional true} :string]
   [:size {:optional true} :int]
   [:b64 {:optional true} :string]
   [:spool {:optional true :description "A plain file name inside SUTRA_MEDIA_SPOOL."}
    :string]])

(def result
  [:map {:title "run result"
         :description (str "The one JSON object the process writes to stdout on exit 0. "
                           "Exactly one of rows and outputs.")}
   [:protocol_version {:optional true :description "The version the process speaks."} :string]
   [:rows {:optional true} [:sequential row]]
   [:outputs {:optional true :description "Output name → rows (a multi-output transform)."}
    [:map-of :keyword [:sequential row]]]
   [:media {:optional true} [:sequential media-item]]
   [:media_set_items {:optional true} [:sequential [:map-of :keyword :any]]]
   [:trace {:optional true} [:sequential [:map-of :keyword :any]]]
   [:incremental-complete {:optional true
                           :description "false: the run deferred rows to a later run."}
    :boolean]
   [:pending-observed {:optional true} :int]])

(def error-line
  [:map {:title "error line"
         :description "The last JSON line on stderr when the process exits 1."}
   [:error :string]
   [:type {:optional true} :string]
   [:where {:optional true :description "file:line in function | source line"} :string]
   [:traceback {:optional true} :string]])

(def span-line
  [:map {:title "span line" :description "A JSON line on stderr: a span opens or closes."}
   [:sutra_span [:enum "open" "close"]]
   [:id :any]
   [:name {:optional true} :string]
   [:at_ms {:optional true} number?]])

(def intermediate-line
  [:map {:title "intermediate line" :description "A named value the transform reports."}
   [:sutra_intermediate :string]
   [:value :any]])

(def environment
  [:map {:title "environment"
         :description "Environment variables the host sets; all optional but the version."}
   [:SUTRA_PROTOCOL_VERSION {:description "The version the host speaks."} :string]
   [:SUTRA_PARAMETERS {:optional true :description "JSON object of parameter values."}
    :string]
   [:SUTRA_SECRETS {:optional true :description "Path of a JSON file: source → fields."}
    :string]
   [:SUTRA_PREVIOUS_OUTPUT {:optional true :description "Path of the previous output rows."}
    :string]
   [:SUTRA_INCREMENTAL_RUN {:optional true} [:enum "1"]]
   [:SUTRA_REFUSE_CAP {:optional true} [:enum "1"]]
   [:SUTRA_TRACE {:optional true} [:enum "1"]]
   [:SUTRA_OUTPUT_ENUMS {:optional true :description "JSON: column → allowed values."}
    :string]
   [:SUTRA_TRANSFORM_NAME {:optional true} :string]
   [:SUTRA_RUN_ID {:optional true} :string]
   [:SUTRA_LM_SERVICE {:optional true :description "JSON: how to reach the model relay."}
    :string]
   [:SUTRA_CPU_CORES {:optional true :description "CPU cores the run may use."} :string]
   [:SUTRA_MEDIA_SPOOL {:optional true
                        :description "A writable directory for media too large to inline."}
    :string]])

(def ^:private defs
  [["manifest" manifest] ["result" result] ["error-line" error-line]
   ["span-line" span-line] ["intermediate-line" intermediate-line]
   ["environment" environment] ["wire-value" wire-value]])

(defn- sorted [x]
  (walk/postwalk #(if (map? %) (into (sorted-map-by (fn [a b] (compare (str a) (str b)))) %) %)
                 x))

(defn json-schema
  []
  (sorted
   {"$schema"     "https://json-schema.org/draft/2020-12/schema"
    "$id"         "https://sutra.dev/boundaries/transform-runtime/protocol.schema.json"
    "title"       "Sutra transform runtime protocol"
    "description" (str "Host ↔ process protocol of a sandboxed transform run (B-6). "
                       "Version " version ".")
    "version"     version
    "$defs"       (into {} (for [[k s] defs] [k (js/transform s)]))}))

(defn validate
  [def-name x]
  (some-> (some (fn [[k s]] (when (= k def-name) s)) defs)
          (m/explain x)
          me/humanize))
