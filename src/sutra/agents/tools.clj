(ns sutra.agents.tools
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.objects.api :as objects]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.agents.builder :as builder]
            [sutra.actions.api :as actions]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.attribute :as attribute]
            [sutra.types.link-side :as ls]
            [sutra.types.run-limits :as limits])
  (:import (java.time Instant LocalDate ZoneId)
           (java.util Date)))

(set! *warn-on-reflection* true)

(def rows-default 20)
(def string-max 600)
(def ^:private list-max 25)

(defn- zone ^ZoneId [] (ZoneId/systemDefault))

(defn- base-type [t] (if (vector? t) (first t) t))

(defn type-name [schema rid]
  (or (get-in schema [:object-types rid :object-type-name])
      (get-in schema [:interfaces rid :interface-name])))

(defn type-rid [schema t]
  (when t (lookup/rid-of schema :object-types (keyword (name t)))))

(defn- attr-rows [schema rid] (map attribute/normalize (lookup/attrs schema rid)))

(defn- attr-types [schema rid]
  (into {} (map (juxt :attribute-name :attribute-type)) (attr-rows schema rid)))

(defn readable?
  [store schema rid]
  (try (grants/authorize-read! (grants/read-rights store) (get-in schema [:object-types rid]) rid)
       true
       (catch clojure.lang.ExceptionInfo e
         (if (anomaly/acl-denied? (ex-data e)) false (throw e)))))

(defn- listed? [schema rid]
  (not= :hidden (get-in schema [:object-types rid :visibility])))

(defn- instant ^Instant [v] (if (instance? Instant v) v (.toInstant ^Date v)))

(defn- clip [s] (if (> (count s) string-max) (str (subs s 0 string-max) "…") s))

(defn- plain-decimal [^BigDecimal d]
  (let [s (.stripTrailingZeros d)] (if (neg? (.scale s)) (.setScale s 0) s)))

(defn json-value
  [t v]
  (cond
    (nil? v)                               nil
    (instance? java.sql.Date v)            (str (.toLocalDate ^java.sql.Date v))
    (and (inst? v) (= :date (base-type t))) (str (LocalDate/ofInstant (instant v) (zone)))
    (inst? v)                              (str (.toOffsetDateTime (.atZone (instant v) (zone))))
    (decimal? v)                           (plain-decimal v)
    (keyword? v)                           (name v)
    (uuid? v)                              (str v)
    (string? v)                            (clip v)
    (map? v)
      (into {} (map (fn [[k x]] [(if (keyword? k) (name k) (str k)) (json-value nil x)])) v)
    (sequential? v)                        (if (> (count v) list-max)
                                             (str "[" (count v) " values]")
                                             (mapv #(json-value nil %) v))
    :else                                  v))

(defn- title-of [schema rid row]
  (let [tk (get-in schema [:object-types rid :title-key])]
    (some-> (or (and tk (get-in row [:values tk])) (:id row)) str)))

(defn ref-of [schema rid row]
  {:type (some-> (type-name schema rid) name) :id (str (:id row)) :title (title-of schema rid row)})

(defn- row-json [schema rid types row]
  (into {"ref" (str (name (type-name schema rid)) ":" (:id row)) "id" (str (:id row))}
        (for [[k v] (:values row) :when (some? v)]
          [(name k) (json-value (get types k) v)])))

(def ^:private op-names
  {"=" :=, "==" :=, "eq" :=, "!=" :not=, "<>" :not=, "not=" :not=, "ne" :not=,
   "<" :<, "lt" :<, ">" :>, "gt" :>, "<=" :<=, "lte" :<=, ">=" :>=, "gte" :>=,
   "contains" :contains-ci, "contains-ci" :contains-ci, "like" :contains-ci, "ilike" :contains-ci,
   "contains-cs" :contains, "starts-with" :starts-with, "starts_with" :starts-with,
   "is-null" :nil?, "is_null" :nil?, "nil?" :nil?})

(defn- op-of [k]
  (or (op-names (str/lower-case (name k)))
      (refusal/refuse! :refusal.agents/unknown-filter-operator
                       {:op k}
                       {:op-name (name k) :known (str/join " " (sort (keys op-names)))})))

(defn- day-start ^java.sql.Timestamp [^LocalDate d]
  (java.sql.Timestamp/from (.toInstant (.atStartOfDay d (zone)))))

(defn- day? [v] (and (string? v) (re-matches #"\d{4}-\d{2}-\d{2}" v)))

(defn- attr-filter
  [t m]
  (reduce-kv
   (fn [acc k v]
     (let [op (op-of k)]
       (if (and (#{:date :timestamp :datetime} (base-type t)) (day? v)
                (#{:= :not= :< :> :<= :>=} op))
         (let [d    (LocalDate/parse v)
               from (day-start d)
               to   (day-start (.plusDays d 1))]
           (case op
             :=    (assoc acc :>= from :< to)
             :not= (assoc acc :not= from)
             :<    (assoc acc :< from)
             :<=   (assoc acc :< to)
             :>    (assoc acc :>= to)
             :>=   (assoc acc :>= from)))
         (assoc acc op (if (= :nil? op) (boolean v) v)))))
   {}
   (if (map? m) m {"=" m})))

(defn- id-filter [m]
  {:= (if (map? m) (val (first m)) m)})

(defn- where-of
  [schema rid w]
  (let [types (attr-types schema rid)
        sides (coll/index-by (comp name :api-name) (ls/outgoing schema rid))]
    (into {}
          (for [[k v] w
                :let [kn (name k)]]
            (if-let [{:keys [far-type]} (get sides kn)]
              (let [ft (attr-types schema far-type)]
                [(keyword kn) (into {} (for [[a m] (if (map? v) v {"id" v})
                                             :let [an (name a)]]
                                         (if (= "id" an)
                                           [:id (id-filter m)]
                                           [(keyword an) (attr-filter (get ft (keyword an)) m)])))])
              (if (= "id" kn)
                [:id (id-filter v)]
                [(keyword kn) (attr-filter (get types (keyword kn)) v)]))))))

(defn- group-term [s]
  (let [[a unit] (str/split (name s) #":" 2)]
    (if unit [:bucket (keyword a) (keyword unit)] (keyword a))))

(defn- agg-of [a]
  (let [a (if (map? a) a {"fn" (str a)})]
    (cond-> {:fn (keyword (str/lower-case (str (get a "fn" "count"))))}
      (get a "attr") (assoc :attr (keyword (get a "attr"))))))

(defn- as-long [v]
  (cond (number? v) (long v)
        (string? v) (parse-long (str/trim v))
        :else nil))

(defn run-query
  [store schema rid args max-rows]
  (let [aggs  (seq (get args "aggregate"))
        limit (max 1 (min max-rows (or (as-long (get args "limit")) rows-default)))
        spec  (cond-> {:find (type-name schema rid) :limit limit}
                (map? (get args
                           "where"))
                  (assoc :where (where-of schema rid (get args "where")))
                (not (str/blank? (get args "search"))) (assoc :keywords (str (get args "search")))
                (seq (get args
                          "around"))
                  (assoc :around (mapv keyword (get args "around")))
                (and (seq (get args "show"))
                     (not aggs)) (assoc :show (mapv keyword (get args "show")))
                (get args
                     "order_by")                   (assoc :order-by (keyword (get args "order_by")))
                (get args "descending")                 (assoc :sort-dir :desc)
                aggs                                    (assoc :aggs (mapv agg-of aggs))
                (and aggs
                     (seq (get args
                               "group_by")))
                  (assoc :group-by (mapv group-term (get args "group_by")))
                (not aggs)                              (assoc :count? true))
        r     (objects/run-object-query store schema spec)]
    (if aggs
      {:result {"type" (name (:object-type r))
                "groups" (mapv (fn [{:keys [group metrics]}]
                                 (merge (json-value nil group) (json-value nil metrics)))
                               (:aggregations r))}
       :refs []}
      (let [res-rid (or (type-rid schema (:object-type r)) rid)
            types   (attr-types schema res-rid)
            rows    (:rows r)]
        {:result {"type" (name (:object-type r)) "count" (:count r) "returned" (count rows)
                  "objects" (mapv #(row-json schema res-rid types %) rows)}
         :refs   (mapv #(ref-of schema res-rid %) rows)}))))

(defn- attr-line [{:keys [attribute-name attribute-type opts]}]
  (str (name attribute-name) " ("
       (if (keyword? attribute-type) (name attribute-type) (pr-str attribute-type)) ")"
       (when-let [l (:name opts)] (str " " l))
       (when-let [d (:description opts)] (str " — " d))))

(defn- side-line [schema {:keys [api-name far-type link-type dir]}]
  (str (name api-name) " → " (some-> (type-name schema far-type) name)
       (when (= :many (ls/far-cardinality (:cardinality link-type) dir)) " (many)")))

(defn describe-type
  [schema rid]
  (let [t (get-in schema [:object-types rid])]
    (cond-> {"type" (name (:object-type-name t))
             "attributes" (mapv attr-line (attr-rows schema rid))
             "links" (mapv #(side-line schema %) (ls/outgoing schema rid))}
      (:display-name t) (assoc "label" (:display-name t))
      (:description t)  (assoc "description" (:description t))
      (:title-key t)    (assoc "title" (name (:title-key t))))))

(defn- type-summary [schema rid]
  (let [t (get-in schema [:object-types rid])]
    (cond-> {"type" (name (:object-type-name t))}
      (:display-name t) (assoc "label" (:display-name t))
      (:description t)  (assoc "description" (:description t)))))

(def ^:private query-help
  (str "where: {attribute: {op: value}}, op one of = != < > <= >= contains (case-insensitive) "
       "starts-with is-null; a bare value means =. Dates are calendar days YYYY-MM-DD. "
       "A link side filters by the linked object: {side: {attribute: {op: value}}}. "
       "search: text matched in every text attribute. around: link sides to follow, in order — "
       "the answer is the objects at the far end. aggregate: compute instead of listing, "
       "[{fn: count|sum|avg|min|max|cardinality, attr}], grouped by group_by "
       "(attribute, or attribute:year|quarter|month|week|day for a date). "
       "Every object carries a ref (type:id) to cite."))

(def ^:private query-props
  {"where"      {"type" "object" "description" "filters, see the tool description"}
   "search"     {"type" "string"}
   "around"     {"type" "array" "items" {"type" "string"}}
   "show"       {"type" "array" "items" {"type" "string"}
                 "description" "attributes to return; default all"}
   "order_by"   {"type" "string"}
   "descending" {"type" "boolean"}
   "limit"      {"type" "integer"}
   "aggregate"  {"type" "array"
                 "items" {"type" "object"
                          "properties" {"fn" {"type" "string"
                                              "enum" ["count" "sum" "avg" "min" "max"
                                                      "cardinality"]}
                                        "attr" {"type" "string"}}}}
   "group_by"   {"type" "array" "items" {"type" "string"}}})

(defn- tool-name [prefix n]
  (let [s (str prefix (str/replace (name n) #"[^A-Za-z0-9_-]" "_"))]
    (subs s 0 (min 64 (count s)))))

(defn- function-def [nm description properties required]
  {"type" "function"
   "function" {"name" nm "description" description
               "parameters" (cond-> {"type" "object" "properties" properties}
                              (seq required) (assoc "required" required))}})

(defn- query-type-tool [schema {:keys [object-type limit description]}]
  (let [rid (or (type-rid schema object-type)
                (refusal/refuse! :refusal.agents/tool-unknown-type {:object-type object-type}))
        d   (describe-type schema rid)
        nm  (tool-name "query_" object-type)]
    {:kind :object-query :name nm :rid rid
     :max-rows (min limits/query-tool-rows-max (or limit limits/query-tool-rows-max))
     :def (function-def nm
                        (str (or description
                                 (str "Read " (or (get d "label") (name object-type)) " objects."))
                             (when-let [x (get d "description")] (str " " x))
                             "\nAttributes: " (str/join "; " (get d "attributes"))
                             (when (seq (get d "links"))
                               (str "\nLink sides: " (str/join "; " (get d "links"))))
                             "\n" query-help)
                        query-props [])}))

(defn- scope-rids
  [schema {:keys [object-types]}]
  (if (seq object-types)
    (into [] (keep #(type-rid schema %)) object-types)
    (into [] (filter #(listed? schema %)) (keys (:object-types schema)))))

(defn- ontology-tools [schema {:keys [limit] :as t}]
  (let [rids  (scope-rids schema t)
        names (sort (map #(name (type-name schema %)) rids))]
    [{:kind :list-types :name "list_object_types" :rids rids
      :def (function-def "list_object_types"
                         "The object types you may read: name, label and what each holds. Call it first."
                         {} [])}
     {:kind :describe-types :name "describe_object_types" :rids rids
      :def (function-def "describe_object_types"
                         "Attributes (name, type, meaning) and link sides of the named object types — read them before you filter."
                         {"types" {"type" "array" "items" {"type" "string" "enum" names}}}
                         ["types"])}
     {:kind :query-any :name "query_objects" :rids rids
      :max-rows (min limits/query-tool-rows-max (or limit limits/query-tool-rows-max))
      :def (function-def "query_objects"
                         (str "Read objects of one type. " query-help)
                         (assoc query-props "type" {"type" "string" "enum" names}) ["type"])}]))

(defn- action-binding [schema {:keys [action description]}]
  (let [t (actions/action-tool schema action)]
    {:kind :action :action action :name (:name t)
     :def (cond-> (:def t) description (assoc-in ["function" "description"] description))}))

(defn- function-binding [schema {:keys [function description]}]
  (let [f  (get-in schema [:functions (lookup/rid-of schema :functions function)])
        nm (tool-name "function_" function)]
    {:kind :function :function function :name nm
     :def (assoc-in (function-def nm
                                  (or description (:description f)
                                      (str "Run the " (name function) " function.")) {} [])
                    ["function" "parameters" "additionalProperties"] true)}))

(defn- order-key [schema rid]
  (let [t (get-in schema [:object-types rid])]
    (or (first (:match-keys t)) (:title-key t))))

(defn- semantic-search
  [store schema rid text k embed]
  (let [{:keys [property model metric]} (get-in schema [:object-types rid :semantic-search])
        qv (when (and property model embed) (embed model text))]
    (when (seq qv)
      (let [nm   (type-name schema rid)
            ok   (order-key schema rid)
            hits (objects/nearest-paged qv
                                    (fn [off n]
                                      (:rows (objects/run-object-query
                                              store schema
                                              (cond-> {:find nm :show [property] :limit n
                                                       :offset off}
                                                ok (assoc :order-by ok)))))
                                    {:property property :k k :metric metric})
            ids  (mapv (comp str :id) hits)
            rows (when (seq ids)
                   (:rows (objects/run-object-query store schema
                                                  {:find nm :ids ids :limit (count ids)})))
            by   (coll/index-by (comp str :id) rows)]
        (keep (fn [h] (some-> (by (str (:id h))) (assoc :score (:score h)))) hits)))))

(defn- run-search [store schema rid args embed]
  (let [text  (str (get args "text"))
        k     (max 1 (min limits/query-tool-rows-max (or (as-long (get args "k")) 8)))
        types (attr-types schema rid)
        prop  (get-in schema [:object-types rid :semantic-search :property])
        hits  (semantic-search store schema rid text k embed)]
    (if hits
      {:result {"mode" "semantic"
                "objects"
                  (mapv #(assoc (row-json schema rid (dissoc types prop)
                                          (update % :values dissoc prop))
                                "score" (:score %))
                        hits)}
       :refs   (mapv #(ref-of schema rid %) hits)}
      (-> (run-query store schema rid {"search" text "limit" k} k)
          (assoc-in [:result "mode"]
                    "text — no embedding model answered, matched the words instead")))))

(defn- search-tool [schema {:keys [object-type]}]
  (let [rid (or (type-rid schema object-type)
                (refusal/refuse! :refusal.agents/retrieval-unknown-type {:object-type object-type}))
        d   (describe-type schema rid)
        nm  (tool-name "search_" object-type)]
    {:kind :search :name nm :rid rid
     :def (function-def nm
                        (str "Find the " (or (get d "label") (name object-type))
                             " objects closest in meaning to a text — for questions in words, not "
                             "filters.")
                        {"text" {"type" "string"} "k" {"type" "integer"}} ["text"])}))

(def ^:private calculator
  {:kind :calculator :name "calculator"
   :def (function-def "calculator"
                      "Evaluate an arithmetic expression: numbers, + - * / and parentheses."
                      {"expression" {"type" "string"}} ["expression"])})

(defn bindings
  [schema {:keys [tools retrieval]}]
  (vec (mapcat (fn [{:keys [kind] :as t}]
                 (case kind
                   :object-query [(query-type-tool schema t)]
                   :ontology     (ontology-tools schema t)
                   :action       [(action-binding schema t)]
                   :function     [(function-binding schema t)]
                   :calculator   [calculator]
                   ::search      [(search-tool schema t)]
                   :builder      (builder/bindings schema t)
                   (refusal/refuse! :refusal.agents/tool-unknown-kind {:tool t} {:kind kind})))
               (cond-> (vec tools)
                 retrieval (conj (assoc retrieval :kind ::search))))))

(defn- in-scope! [store schema rids t]
  (let [rid (type-rid schema t)]
    (when-not (and rid (some #{rid} rids) (readable? store schema rid))
      (refusal/refuse! :refusal.agents/type-not-readable {:type t}))
    rid))

(defn- propose-or-apply
  [store schema {:keys [writes thread-id run-id agent writing]} action args]
  (let [reason (get args "_reason")
        params (update-keys (dissoc args "_reason") keyword)]
    (case writes
      :apply
      (do (actions/dispatch-action! @writing schema action params
                                     {:provenance {:agent (some-> agent name) :thread thread-id
                                                   :run-id run-id}})
          {:result {"status" "applied"}})

      :dry
      {:result   {"status" "proposed" "note" "dry run — recorded, not staged"}
       :proposal {:action (name action) :args params :dry true}}
      (if-let [p (actions/propose! store (:branch store)
                                     {:automation-id thread-id :action action :args params
                                      :reason reason
                                      :provenance
                                        {:agent (name agent) :thread thread-id :run-id run-id}})]
        {:result   {"status" "proposed" "proposal" (:id p)
                    "note" "staged for the asker to accept or reject, not applied"}
         :proposal {:id (:id p) :action (name action)}}
        {:result {"status" "already proposed"
                  "note" "an open proposal with these arguments exists"}}))))

(defn execute
  [store schema tool args opts]
  (case (:kind tool)
    :object-query   (run-query store schema
                               (do (in-scope! store schema [(:rid tool)]
                                              (type-name schema (:rid tool))) (:rid tool))
                               args (:max-rows tool))
    :list-types     {:result {"types" (mapv #(type-summary schema %)
                                            (filter #(readable? store schema %) (:rids tool)))}}
    :describe-types
      {:result {"types" (mapv #(describe-type schema (in-scope! store schema (:rids tool) %))
                              (get args "types"))}}
    :query-any      (run-query store schema (in-scope! store schema (:rids tool) (get args "type"))
                               args
                               (:max-rows tool))
    :search         (do (in-scope! store schema [(:rid tool)] (type-name schema (:rid tool)))
                        (run-search store schema (:rid tool) args (:embed opts)))
    :calculator     {:result {"result" (actions/calculate (get args "expression"))}}
    :action         (propose-or-apply store schema opts (:action tool) args)
    :builder        (builder/execute store schema tool args opts)
    :function       (do (actions/dispatch-action! @(:writing opts) schema (:function tool)
                                            (update-keys args keyword)
                                            {:provenance {:agent (some-> (:agent opts) name)
                                                          :thread (:thread-id opts)
                                                          :run-id (:run-id opts)}})
                        {:result {"status" "done"}})))
