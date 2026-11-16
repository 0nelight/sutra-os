(ns sutra.documents.widgets
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.objects.api :as objects]
            [sutra.documents.diagrams :as diagrams]
            [sutra.documents.use-case :as use-case]
            [sutra.catalog.lookup :as lookup]
            [sutra.actions.api :as actions]
            [sutra.kit.html :refer [esc]])
  (:import (java.net URLDecoder)
           (java.time ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter FormatStyle)
           (java.util Locale)
           (org.commonmark.ext.gfm.tables TablesExtension)
           (org.commonmark.parser Parser)
           (org.commonmark.renderer.html HtmlRenderer)
           (org.jsoup Jsoup)
           (org.jsoup.safety Safelist)))

(set! *warn-on-reflection* true)

(def ^:private widget-re #"(?s)<sutra-widget\b([^>]*?)(?:/>|>(.*?)</sutra-widget>)")
(def ^:private attr-re #"([a-zA-Z_:][-a-zA-Z0-9_:.]*)=\"([^\"]*)\"")
(def ^:private max-rows 200)

(defn- unescape [s]
  (-> s (str/replace "&quot;" "\"") (str/replace "&#39;" "'") (str/replace "&lt;" "<")
      (str/replace "&gt;" ">") (str/replace "&amp;" "&")))

(defn- attrs-of [s]
  (into {} (for [[_ k v] (re-seq attr-re (or s ""))
                 :when (str/starts-with? k "data-")]
             [(keyword (subs k 5)) (unescape v)])))

(defn widgets
  [body]
  (for [[m a] (re-seq widget-re (str body))]
    {:attrs (attrs-of a) :match m}))

(def ^:private md
  (let [exts [(TablesExtension/create)]]
    {:parser   (-> (Parser/builder) (.extensions exts) .build)
     :renderer (-> (HtmlRenderer/builder) (.extensions exts) (.escapeHtml true)
                   (.sanitizeUrls true) .build)}))

(defn markdown->html [s]
  (.render ^HtmlRenderer (:renderer md) (.parse ^Parser (:parser md) (str s))))

(defn- type-rid [schema t] (lookup/rid-of schema :object-types (keyword (str t))))

(defn- attributes [schema rid]
  (vec (get-in schema [:object-types rid :attribute-list])))

(defn- attr-label [schema rid a]
  (let [k (keyword (name a))]
    (or (some #(when (= k (keyword (name (:attribute-name %))))
                 (not-empty (get-in % [:opts :display-name])))
              (attributes schema rid))
        (-> (name a) (str/replace #"[-_]" " ") str/capitalize))))

(defn- prominent [schema rid]
  (let [as    (attributes schema rid)
        title (get-in schema [:object-types rid :title-key])
        cand  (remove #(= (keyword (name (:attribute-name %))) title) as)
        prom  (filter #(= "prominent" (some-> (get-in % [:opts :visibility]) name)) cand)]
    (mapv #(keyword (name (:attribute-name %))) (if (seq prom) prom (take 2 cand)))))

(defn- value-of [row a]
  (let [vs (if (map? (:values row)) (:values row) row)
        k  (keyword (name a))]
    (get vs k (get vs (keyword (str/replace (name k) "-" "_"))))))

(defn- show [v]
  (cond (nil? v) "—"
        (sequential? v) (str/join ", " (map str v))
        :else (str v)))

(defn- props-of [s]
  (when-not (str/blank? s) (mapv (comp keyword str/trim) (str/split s #","))))

(defn- object-href [t id] (str "/object/" (esc t) "/" (esc id)))

(defn- linked
  [cls no-link t id text]
  (if (= "true" no-link)
    (str "<span class=\"" cls "\">" text "</span>")
    (str "<a class=\"" cls "\" href=\"" (object-href t id) "\">" text "</a>")))

(defn- title-of [schema rid row id]
  (let [tk (get-in schema [:object-types rid :title-key])]
    (or (some-> (when tk (value-of row tk)) str not-empty) (str id))))

(defn- type-label [schema rid t]
  (or (not-empty (get-in schema [:object-types rid :display-name]))
      (-> (str t) (str/replace #"[-_]" " ") str/capitalize)))

(defn- read-rows [store schema t spec]
  (:rows (objects/run-object-query store schema (merge {:find (keyword t) :limit max-rows} spec))))

(defn- read-one [store schema t id]
  (first (read-rows store schema t {:ids [(str id)] :limit 1})))

(defn- where-of [s]
  (when-not (str/blank? s)
    (into {} (for [part (str/split s #";")
                   :let [[k v] (map str/trim (str/split part #"=" 2))]
                   :when (and (seq k) (some? v))]
               [(keyword k) {:= v}]))))

(defn- dt-format [fmt]
  (case fmt
    "datetime"       (DateTimeFormatter/ofLocalizedDateTime FormatStyle/MEDIUM)
    "datetime-short" (DateTimeFormatter/ofLocalizedDateTime FormatStyle/SHORT)
    "time"           (DateTimeFormatter/ofLocalizedTime FormatStyle/SHORT)
    "iso"            DateTimeFormatter/ISO_OFFSET_DATE_TIME
    (DateTimeFormatter/ofLocalizedDate FormatStyle/MEDIUM)))

(defn current-date
  [fmt tz]
  (let [z (try (ZoneId/of (or (not-empty tz) "Europe/Vienna"))
               (catch java.time.DateTimeException _ (ZoneId/of "Europe/Vienna")))]
    (.format (.withLocale ^DateTimeFormatter (dt-format fmt) Locale/GERMAN)
             (ZonedDateTime/now z))))

(defmulti ^:private render-kind (fn [_ctx attrs] (:kind attrs)))

(defmethod render-kind "object-property"
  [{:keys [store schema]} {:keys [type id property show-name no-link]}]
  (let [rid (type-rid schema type)
        row (read-one store schema type id)
        a   (or (some-> property not-empty keyword)
                (get-in schema [:object-types rid :title-key]))]
    (if-not row
      (str "<span class=\"np-missing\">" (esc type) " " (esc id) " nicht gefunden</span>")
      (linked "np-prop" no-link type id
              (str (when (= "true" show-name) (str (esc (attr-label schema rid a)) ": "))
                   (esc (show (if a (value-of row a) (title-of schema rid row id)))))))))

(defmethod render-kind "object-card"
  [{:keys [store schema]} {:keys [type id properties no-link]}]
  (let [rid (type-rid schema type)
        row (read-one store schema type id)]
    (if-not row
      (str "<div class=\"np-missing\">" (esc type) " " (esc id) " nicht gefunden</div>")
      (str "<div class=\"np-card\"><div class=\"np-card__type\">"
           (esc (type-label schema rid type)) "</div>"
           (linked "np-card__title" no-link type id (esc (title-of schema rid row id)))
           "<dl class=\"np-card__props\">"
           (str/join (for [a (or (props-of properties) (prominent schema rid))]
                       (str "<div><dt>" (esc (attr-label schema rid a)) "</dt><dd>"
                            (esc (show (value-of row a))) "</dd></div>")))
           "</dl></div>"))))

(defmethod render-kind "object-table"
  [{:keys [store schema]} {:keys [type ids where properties order-by limit no-link]}]
  (let [rid   (type-rid schema type)
        cols  (or (props-of properties)
                  (into (vec (keep identity [(get-in schema [:object-types rid :title-key])]))
                        (prominent schema rid)))
        n     (min max-rows (or (some-> limit not-empty parse-long) 50))
        rows  (read-rows store schema type
                         (cond-> {:limit n}
                           (seq (props-of ids)) (assoc :ids (mapv name (props-of ids)))
                           (where-of where)     (assoc :where (where-of where))
                           (seq order-by)       (assoc :order-by (keyword order-by))))]
    (str "<table class=\"np-objtable\"><thead><tr>"
         (str/join (for [a cols] (str "<th>" (esc (attr-label schema rid a)) "</th>")))
         "</tr></thead><tbody>"
         (if (empty? rows)
           (str "<tr><td colspan=\"" (max 1 (count cols)) "\">Keine Objekte</td></tr>")
           (str/join (for [r rows]
                       (str "<tr>"
                            (str/join (map-indexed
                                       (fn [i a]
                                         (let [v (esc (show (value-of r a)))]
                                           (str "<td>"
                                                (if (zero? i)
                                                  (linked "np-rowlink" no-link type (:id r) v)
                                                  v)
                                                "</td>")))
                                       cols))
                            "</tr>"))))
         "</tbody></table>")))

(defmethod render-kind "markdown-property"
  [{:keys [store schema]} {:keys [type id property]}]
  (if-let [row (read-one store schema type id)]
    (str "<div class=\"np-md\">" (markdown->html (or (value-of row property) "")) "</div>")
    (str "<div class=\"np-missing\">" (esc type) " " (esc id) " nicht gefunden</div>")))

(defn- function-text [rows]
  (str/join "\n\n" (for [r rows
                         :let [vs (if (map? (:values r)) (:values r) (dissoc r :id))]]
                     (str (if (= 1 (count vs)) (val (first vs)) (str/join " · " (vals vs)))))))

(defmethod render-kind "function"
  [{:keys [store schema]} {:keys [function args output]}]
  (let [args (if (str/blank? args) {}
                 (into {} (map (fn [[k v]] [(keyword (name k)) v]))
                       (json/read-str args)))
        rows (actions/run-query-function store schema (keyword function) args)]
    (if (= "table" output)
      (let [cols (vec (distinct (mapcat #(keys (if (map? (:values %)) (:values %) (dissoc % :id)))
                                        rows)))]
        (str "<table class=\"np-objtable\"><thead><tr>"
             (str/join (for [c cols] (str "<th>" (esc (name c)) "</th>")))
             "</tr></thead><tbody>"
             (str/join (for [r rows]
                         (str "<tr>" (str/join (for [c cols]
                                                 (str "<td>" (esc (show (value-of r c))) "</td>")))
                              "</tr>")))
             "</tbody></table>"))
      (str "<div class=\"np-md\">" (markdown->html (function-text rows)) "</div>"))))

(defmethod render-kind "current-date"
  [_ {:keys [auto value format tz]}]
  (str "<span class=\"np-date\">"
       (esc (if (or (= "true" auto) (str/blank? value)) (current-date format tz) value))
       "</span>"))

(defmethod render-kind "mention"
  [_ {:keys [user label]}]
  (str "<span class=\"np-mention\">@" (esc (or (not-empty label) user)) "</span>"))

(defmethod render-kind "resource-link"
  [_ {:keys [href label]}]
  (let [h (str href)]
    (if (re-matches #"(?i)(/|https?://)\S*" h)
      (str "<a class=\"np-resource\" href=\"" (esc h) "\">" (esc (or (not-empty label) h)) "</a>")
      (str "<span class=\"np-resource\">" (esc label) "</span>"))))

(defmethod render-kind "graph"
  [{:keys [store schema]} attrs]
  (diagrams/graph store schema attrs))

(defmethod render-kind "object-model"
  [{:keys [store schema]} attrs]
  (diagrams/object-model store schema attrs))

(defmethod render-kind "lifecycle"
  [{:keys [store schema]} attrs]
  (diagrams/lifecycle store schema attrs))

(defmethod render-kind "use-case-draft"
  [{:keys [store schema auth]} attrs]
  (use-case/widget store auth schema attrs))

(defmethod render-kind :default
  [_ {:keys [kind]}]
  (str "<span class=\"np-missing\">unbekanntes Widget " (esc kind) "</span>"))

(def ^:private snapshot-safelist
  (-> (Safelist/relaxed)
      (.addTags (into-array String ["span" "div" "dl" "dt" "dd" "figure" "figcaption"]))
      (.addAttributes ":all" (into-array String ["class"]))
      (.addProtocols "a" "href" (into-array String ["#"]))
      (.addProtocols "img" "src" (into-array String ["data"]))
      (.preserveRelativeLinks true)))

(defn clean-snapshot
  [html]
  (Jsoup/clean (str html) "http://sutra.local/" snapshot-safelist))

(defn render-widget
  ([store schema attrs] (render-widget store schema attrs nil))
  ([store schema attrs auth]
   (if (and (= "true" (:locked attrs)) (some? (:snapshot attrs)))
     (clean-snapshot (:snapshot attrs))
     (try (render-kind {:store store :schema schema :auth auth} attrs)
          (catch InterruptedException e (throw e))
          (catch Exception e
            (str "<span class=\"np-missing\">" (esc (:kind attrs)) ": "
                 (esc (or (ex-message e) (.getName (class e)))) "</span>"))))))

(def css
  (str ".np-prop{color:#215db0;text-decoration:none}"
       ".np-card{border:1px solid #d8dde4;border-radius:6px;padding:8pt 10pt;margin:6pt 0;"
       "break-inside:avoid;max-width:420px}"
       ".np-card__type{font-size:8pt;color:#5f6b7c;text-transform:uppercase;letter-spacing:.04em}"
       ".np-card__title{display:block;font-weight:700;color:#1c2127;text-decoration:none;"
       "margin:2pt 0 4pt}"
       ".np-card__props{margin:0;display:grid;grid-template-columns:auto 1fr;gap:2pt 10pt}"
       ".np-card__props div{display:contents}.np-card__props dt{color:#5f6b7c}"
       ".np-card__props dd{margin:0}"
       ".np-objtable{border-collapse:collapse;width:100%;margin:6pt 0}"
       ".np-objtable th,.np-objtable td{border:1px solid #d8dde4;padding:3pt 5pt;text-align:left}"
       ".np-objtable th{background:#f3f5f7}"
       ".np-mention{color:#215db0;background:#eaf1fb;border-radius:3px;padding:0 2px}"
       ".np-missing{color:#ac2f33}"
       ".np-cols{display:flex;gap:16pt}.np-col{flex:1;min-width:0}"
       ".page-break{break-after:page}"
       ".np-plan td{vertical-align:top}.np-plan--keep td{color:#5f6b7c}"
       ".np-plan__hints{color:#ac2f33;font-size:9pt}"
       diagrams/css))

(defn resolve-body
  ([store schema body] (resolve-body store schema body nil))
  ([store schema body auth]
   (let [ws (widgets body)]
     (if (empty? ws)
       body
       (str "<style>" css "</style>"
            (reduce (fn [b {:keys [attrs match]}]
                      (str/replace-first b match
                                         (str/re-quote-replacement
                                          (render-widget store schema attrs auth))))
                    (str body) ws))))))

(defn- linked-graph
  [{:keys [kind href graph]}]
  (case kind
    "resource-link" (some-> (re-find #"^/(?:branch/[^/]+/)?view/([^/?#]+)/" (str href))
                            second (URLDecoder/decode "UTF-8"))
    "graph"         (when (str/starts-with? (str graph) "ri.") graph)
    nil))

(defn embed-links
  [body]
  (let [ws (map :attrs (widgets body))]
    (vec (distinct
          (concat
           (for [{:keys [kind type id]} ws
                 :when (and (#{"object-property" "object-card" "markdown-property"} kind)
                            (seq type) (seq id))]
             {:object-type type :id id :via "embed"})
           (for [a ws :let [g (linked-graph a)] :when g]
             {:resource g :via "embed"}))))))

(defn mentions
  [body]
  (into #{} (for [{{:keys [kind user]} :attrs} (widgets body)
                  :when (and (= "mention" kind) (seq user))]
              user)))
