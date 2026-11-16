(ns platform-sdk.scope
  (:require [clojure.string :as str]
            [platform-sdk.gen :as gen]
            [platform-sdk.schema :as schema]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn- name-of [spec]
  (some (fn [k] (get spec k))
        [:object-type-name :interface-name :function-name :transform-name
         :link-type-name :custom-application-name :action-name]))

(defn- wanted-set
  [schema section decls]
  (let [by-rid (get schema section)]
    (into #{}
          (keep (fn [d]
                  (cond
                    (and (string? d) (get by-rid d)) (some-> (name-of (get by-rid d)) name keyword)
                    (rid/rid? d)                      nil
                    :else                             (some-> d name keyword))))
          (or decls []))))

(defn- select-by-name
  [section wanted]
  (into {} (filter (fn [[_ spec]]
                     (contains? wanted (some-> (name-of spec) name keyword))))
        section))

(defn declared
  [schema app]
  (let [sc (:scope app)]
    {:object-types (into (wanted-set schema :object-types (:object-types sc))
                         (wanted-set schema :interfaces (:object-types sc)))
     :interfaces   (wanted-set schema :interfaces (:interfaces sc))
     :functions    (wanted-set schema :functions (:functions sc))
     :actions      (wanted-set schema :actions (:actions sc))
     :operations   (into #{} (map #(keyword (name %))) (or (:operations sc) []))}))

(defn narrow
  [schema app]
  (let [{:keys [object-types interfaces functions actions]} (declared schema app)
        objs   (select-by-name (:object-types schema) object-types)
        ifaces (select-by-name (:interfaces schema) interfaces)
        keep-rid (into #{} (keys objs))
        links  (into {} (filter (fn [[_ l]]
                                  (and (contains? keep-rid (:from-object-type l))
                                       (contains? keep-rid (:to-object-type l)))))
                     (:link-types schema))
        fns    (select-by-name (:functions schema) functions)
        qfns   (into {} (filter (fn [[_ spec]]
                              (contains? functions
                                         (some-> (schema/function-name spec) name keyword))))
                     (:transforms schema))
        acts   (into {} (filter (fn [[k _]] (contains? actions (keyword (name k)))))
                     (schema/action-types schema))]
    {:object-types objs :interfaces ifaces :link-types links
     :functions fns :transforms qfns :actions acts}))

(defn permits?
  [schema app kind nm]
  (let [d (declared schema app)
        k (some-> nm name keyword)]
    (case kind
      :object-type (contains? (:object-types d) k)
      :interface   (contains? (:interfaces d) k)
      :action      (contains? (:actions d) k)
      :function    (contains? (:functions d) k)
      :operation   (contains? (:operations d) k)
      false)))

(defn find-app
  [schema app-name]
  (some (fn [spec] (when (= (name app-name) (some-> (:custom-application-name spec) name)) spec))
        (vals (:custom-applications schema))))

(defn query-types
  [schema {:keys [find around]}]
  (let [rid-of (schema/name->rid schema)
        as-kw  #(some-> % name keyword)]
    (when find
      (reduce (fn [seen side]
                (conj seen (get-in (schema/sides schema (rid-of (peek seen)))
                                   [(as-kw side) :far-type])))
              [(as-kw find)]
              around))))

(defn generate-sdk
  [schema app {:keys [ontology] :or {ontology "sutra"}}]
  (let [sub (narrow schema app)
        ns- (or (get-in app [:sdk :namespace])
                (some-> (:custom-application-name app) name))
        {:keys [files fingerprint]} (gen/generate sub {:ns ns- :ontology ontology})]
    {:ns ns- :ontology ontology
     :fingerprint fingerprint
     :files files
     :source (str/join "\n" (for [[path src] files]
                              (str ";; ── " path "\n" src)))}))

(defn- fn-name-of [spec]
  (or (:function-name spec) (:name spec)))

(defn- md-table [headers rows]
  (str/join
   "\n"
   (concat [(str "| " (str/join " | " headers) " |")
            (str "|" (str/join "|" (repeat (count headers) "---")) "|")]
           (for [r rows] (str "| " (str/join " | " (map #(str (or % "")) r)) " |")))))

(defn docs
  [schema app {:keys [ontology] :or {ontology "sutra"}}]
  (let [sub   (narrow schema app)
        ns-   (or (get-in app [:sdk :namespace])
                  (some-> (:custom-application-name app) name))
        title (or (:display-name app)
                  (some-> (:custom-application-name app) name))
        objs  (sort-by (comp str :object-type-name val) (:object-types sub))]
    (str/join
     "\n\n"
     (remove
      nil?
      (concat
       [(str "# " title " — SDK reference")
        (when-let [d (:description app)] d)
        (str "Generated from the `" ontology "` ontology. Namespace `" ns- "`.\n\n"
             "Ontology fingerprint `" (schema/fingerprint sub) "` — call `(" ns- "/verify!)` "
             "at startup; it throws when the live ontology has drifted from the one "
             "this SDK was generated against.")
        "## Scope\n\nThis application can reach ONLY what is listed here. A call against anything else is refused by the token, not by the SDK."
        (md-table ["Kind" "Declared"]
                  (let [d (declared schema app)]
                    [["Object types" (str/join ", " (sort (map name (:object-types d))))]
                     ["Actions"      (str/join ", " (sort (map name (:actions d))))]
                     ["Functions"    (str/join ", " (sort (map name (:functions d))))]
                     ["Operations"   (str/join ", " (sort (map name (:operations d))))]]))
        "## Object types"]
       (for [[rid o] objs
             :let [nm (name (:object-type-name o))
                   ls (for [[side info] (schema/sides sub rid)]
                        [(name side) (name (:far-type info)) (name (:cardinality info))])]]
         (str "### `" nm "`\n\n"
              "Primary key `" (or (some-> (schema/primary-key o) name) "—") "` · `(" ns- ".objects/"
              nm ")` returns its object set.\n\n"
              (md-table ["Property" "Type"]
                        (for [[p t] (sort-by (comp str first) (schema/attributes o))]
                          [(str "`" (name p) "`") (str "`" (name t) "`")]))
              (when (seq ls)
                (str "\n\nLinks:\n\n"
                     (md-table ["Link side" "To" "Cardinality"]
                               (for [[l t c] (sort ls)] [(str "`" l "`") (str "`" t "`") c]))))))
       ["## Actions\n\nEvery write goes through an action — the SDK has no direct object write, and neither does the token."]
       (for [[k spec] (sort-by (comp str key) (:actions sub))]
         (str "### `" (name k) "`\n\n"
              "`(" ns- ".actions/" (name k) "! {…})`\n\n"
              (md-table ["Parameter" "Type" "Required"]
                        (for [[p s] (schema/action-params spec)]
                          [(str "`" (name p) "`")
                           (str "`" (some-> (:type s) name) "`")
                           (if (:required s) "yes" "no")]))))
       (when (seq (schema/functions sub))
         (cons "## Functions"
               (for [spec (sort-by (comp str fn-name-of) (schema/functions sub))]
                 (str "### `" (name (fn-name-of spec)) "`\n\n"
                      (md-table ["Input" "Type"]
                                (for [[p t] (schema/function-params spec)]
                                  [(str "`" (name p) "`") (str "`" (some-> t name) "`")])))))))))))
