(ns sutra.types.rid
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [sutra.types.entities :as ents]
            [sutra.types.refs :as refs]))

(def kinds-key->tag ents/section->tag)

(def rid-kinds (set (keys kinds-key->tag)))
(def name-field-of
  (into {} (map (juxt identity ents/name-field)) ents/rid-sections))

(def kind-tags
  (into #{:view :folder :placement :temporary-object-set :exploration :portfolio :collection :job :transaction}
        (vals kinds-key->tag)))

(defn new-rid
  [kind locator]
  (when-not (contains? kind-tags kind)
    (anomaly/raise! :fault (str "unknown RID kind tag: " (pr-str kind))
                           {:kind kind :known (sort kind-tags)}))
  (str "ri.sutra.main." (name kind) "." locator))

(def ontology-repo-rid
  "ri.sutra.main.repository.ontology")

(defn ontology-repo?
  [repo-rid]
  (= ontology-repo-rid (str repo-rid)))

(defn slug-kw
  [x]
  (when (some? x)
    (let [s (-> (name x) str/lower-case str/trim
                (str/replace #"[^a-z0-9]+" "-")
                (str/replace #"^-+|-+$" ""))]
      (keyword (if (= s "") "graph" s)))))

(defn ->name [x] (cond (keyword? x) (name x) (nil? x) nil :else (str x)))

(def ^:private rid-re #"ri\.sutra\.main\.([a-z-]+)\.([0-9A-Za-z_-]+)")

(defn rid?
  [x]
  (boolean (and (string? x) (re-matches rid-re x))))

(defn rid-kind
  [rid]
  (when-let [[_ k] (re-matches rid-re (str rid))]
    (keyword k)))

(defn name-rids-in
  [rid->name s]
  (if (and (string? s) (seq rid->name))
    (str/replace s rid-re
                 (fn [[m]] (if-let [n (get rid->name m)] (str (->name n) " (" m ")") m)))
    s))

(def tag->kinds-key
  (into {} (map (juxt val key)) kinds-key->tag))

(defn resource-ref
  [family id]
  (case family
    :datasources (str "datasource/" (name id))
    (str id)))

(defn parse-ref
  [ref]
  (cond
    (nil? ref) nil
    (rid? ref) {:family (tag->kinds-key (rid-kind ref)) :id ref}
    (str/starts-with? ref "datasource/")
    {:family :datasources :id (subs ref (count "datasource/"))}
    (str/starts-with? ref "view/")
    {:family :graphs :id (subs ref (count "view/"))}
    :else nil))

(defn rid-locator
  [rid]
  (when-let [[_ _ loc] (re-matches rid-re (str rid))]
    loc))

(defn well-formed-for?
  [kinds-key id]
  (and (rid? id) (= (kinds-key->tag kinds-key) (rid-kind id))))

(defn mint-for
  [kinds-key uuid]
  (new-rid (kinds-key->tag kinds-key) uuid))

(defn rid-keyed?
  [schema-map]
  (boolean
   (some (fn [kk]
           (when-let [ents (seq (get schema-map kk))]
             (well-formed-for? kk (key (first ents)))))
         rid-kinds)))

(defn name->rid-index
  [schema-map]
  (into {}
        (for [kinds-key rid-kinds
              :let [entities (get schema-map kinds-key)
                    nf       (name-field-of kinds-key)]
              :when entities]
          [kinds-key (into {} (for [[_rid spec] entities
                                    :let [nm (get spec nf)]
                                    :when nm]
                                [nm (:id spec)]))])))

(defn rid->name-index
  [schema-map]
  (into {}
        (for [kinds-key rid-kinds
              :let [nf (name-field-of kinds-key)]
              [k spec] (get schema-map kinds-key)
              :let [r (or k (:id spec))]
              :when (and r (get spec nf))]
          [r (get spec nf)])))

(defn ambiguous
  [nm candidates]
  {::ambiguous nm ::candidates (vec candidates)})

(defn ambiguous? [v] (and (map? v) (contains? v ::ambiguous)))

(defn- unambiguous!
  [v]
  (if (ambiguous? v)
    (anomaly/coded! :refusal.dialect/ambiguous-name :incorrect
      "the name {name:pr} is given in several spaces — {names:pr} in {spaces:pr}; name a project of the space you mean (X-Sutra-Project, `--project`) or use the rid"
      {:name (::ambiguous v) :candidates (::candidates v)}
      {:names  (mapv first (::candidates v))
       :spaces (mapv second (::candidates v))})
    v))

(defn resolve-name->rid
  [idx target-kinds name-kw]
  (cond
    (rid? name-kw) name-kw
    (nil? name-kw) nil
    :else (let [k (if (string? name-kw) (keyword name-kw) name-kw)]
            (unambiguous! (some (fn [kk] (get-in idx [kk k])) target-kinds)))))

(defn resolve-rid->name
  [idx rid]
  (if (rid? rid) (get idx rid) rid))

(def ref-fields ents/refs)

(def project-owned-kinds
  (into (sorted-set)
        (for [[kind refs] ref-fields
              :when (some #(= [:project] (:at %)) refs)]
          kind)))

(defn ref-values
  [kinds-key spec]
  (refs/ref-values (get ref-fields kinds-key) spec))

(defn rewrite-refs
  [kinds-key spec resolve]
  (refs/rewrite-refs (get ref-fields kinds-key) spec resolve))

(defn rewrite-action-param-refs
  [resolve action]
  (refs/rewrite-refs ents/action-param-refs action resolve))

(defn rewrite-action-refs
  [resolve step]
  (refs/rewrite-refs ents/action-rule-refs step resolve))

(defn dataset-rid-for
  [producer-rid]
  (new-rid :dataset (rid-locator producer-rid)))

(defn name-keyed
  [schema-map]
  (let [idx     (rid->name-index schema-map)
        resolve (fn [_targets v] (resolve-rid->name idx v))]
    (reduce
     (fn [sm kinds-key]
       (cond-> sm
         (get sm kinds-key)
         (update kinds-key
                 (fn [ents]
                   (reduce-kv
                    (fn [m rid spec]
                      (assoc m (or (get spec (name-field-of kinds-key)) rid)
                             (rewrite-refs kinds-key spec resolve)))
                    {} ents)))))
     (cond-> schema-map
       (map? (:actions schema-map))
       (update :actions update-vals
               (partial rewrite-action-param-refs (fn [_ v] (or (resolve-rid->name idx v) v)))))
     rid-kinds)))
