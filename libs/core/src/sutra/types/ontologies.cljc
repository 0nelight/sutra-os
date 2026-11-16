(ns sutra.types.ontologies
  (:require [clojure.set :as set]
            [sutra.types.authority :as authority]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(def default-ontology sch/default-space)

(def sections
  [:object-types :link-types :actions :interfaces :functions :shared-properties :value-types
   :object-type-groups])

(defn ontology-of
  [schema spec]
  (or (sch/scope-of schema spec) default-ontology))

(defn resolve-ontology
  [schema x]
  (cond (nil? x)                                        nil
        (contains? #{"default" (str default-ontology)} (str x)) default-ontology
        (= default-ontology x)                          default-ontology
        :else (authority/space-rid schema x)))

(defn kind
  [schema s]
  (cond (= default-ontology s)                                     :default
        (< 1 (count (authority/space-organizations schema s)))     :shared
        :else                                                      :private))

(defn ontology-name
  [schema s]
  (if (= default-ontology s)
    "default"
    (some-> (or (get-in schema [:spaces s :space-name]) s) rid/->name)))

(defn ontologies
  [schema]
  (let [schema (sch/indexed schema)
        counts (frequencies (for [section sections
                                  [_ spec] (get schema section)]
                              [(ontology-of schema spec) section]))]
    (vec (for [s (cons default-ontology (sort-by str (keys (:spaces schema))))]
           {:ontology      (str s)
            :name          (ontology-name schema s)
            :kind          (kind schema s)
            :organizations (vec (sort (map str (authority/space-organizations schema s))))
            :counts        (into (sorted-map)
                                 (keep (fn [section]
                                         (when-let [n (get counts [s section])] [section n])))
                                 sections)}))))

(defn in-ontology
  [schema s]
  (let [schema (sch/indexed schema)]
    (reduce (fn [m section]
              (if-let [es (get m section)]
                (assoc m section
                       (into (empty es) (filter #(= s (ontology-of schema (val %)))) es))
                m))
            schema
            sections)))

(defn- referenced
  [section spec]
  (rid/ref-values section spec))

(defn- target-rid
  [index section v]
  (if (rid/rid? v)
    v
    (let [r (get-in index [section (if (string? v) (keyword v) v)])]
      (when-not (rid/ambiguous? r) r))))

(defn cross-ontology-references
  [schema]
  (let [schema (sch/indexed schema)
        onts   (set sections)
        idx    (memoize #(:name->rid (sch/index (sch/in-scope schema %))))]
    (for [section sections
          [r spec] (get schema section)
          :let [own (ontology-of schema spec)]
          [targets v] (referenced section spec)
          t (filter onts targets)
          :let [tr (target-rid (idx own) t v)
                ts (some->> tr (get (get schema t)))]
          :when ts
          :let [other (ontology-of schema ts)]
          :when (and (not= other own) (not= other default-ontology))]
      {:section section :resource r :ontology own :references tr :in other})))

(defn organization-violations
  [schema]
  (let [schema (sch/indexed schema)]
    (for [[p spec] (:projects schema)
          :let [s     (sch/project-space schema p)
                extra (set/difference (into #{} (keep #(authority/organization-rid schema %))
                                            (:shared-with spec))
                                      (authority/space-organizations schema s))]
          :when (seq extra)]
      {:project p :space s :organizations (vec (sort (map str extra)))})))

(defn- member-ids [spec]
  (keep #(some-> (if (map? %) (:principal %) %) str) (:members spec)))

(defn primary-conflicts
  [schema]
  (for [[p os] (reduce-kv (fn [m o spec]
                            (reduce #(update %1 %2 (fnil conj #{}) o) m (member-ids spec)))
                          {} (:organizations schema))
        :when (< 1 (count os))]
    {:principal p :organizations (vec (sort (map str os)))}))

(defn primary-organization
  [schema ids]
  (let [ids (set (map str (remove nil? ids)))]
    (some (fn [[o spec]] (when (some ids (member-ids spec)) o))
          (sort-by (comp str key) (:organizations schema)))))

(defn home-space
  [schema o]
  (or (authority/space-rid schema (get-in schema [:organizations o :home-space]))
      (let [ss (filter #(= o (authority/organization-of schema %)) (keys (:spaces schema)))]
        (when (= 1 (count ss)) (first ss)))))

(defn with-creator-space
  [schema ids args]
  (if (some? (:space args))
    args
    (if-let [s (some->> (primary-organization schema ids) (home-space schema))]
      (assoc args :space (or (some-> (get-in schema [:spaces s :space-name]) rid/->name keyword)
                             s))
      args)))

(defn collaborators
  [schema o]
  (let [orgs (:organizations schema)
        rid  #(authority/organization-rid schema %)]
    (-> (into #{} (keep rid) (get-in orgs [o :collaborates-with]))
        (into (keep (fn [[x spec]]
                      (when (some #(= o (rid %)) (:collaborates-with spec)) x)))
              orgs)
        (into (mapcat (fn [s]
                        (let [os (authority/space-organizations schema s)]
                          (when (contains? os o) os))))
              (keys (:spaces schema)))
        (disj o))))

(def discovery-aspects #{:name :users :groups})

(defn discoverable?
  [schema viewer-orgs o aspect]
  (let [viewer-orgs (set viewer-orgs)
        rule        (keyword (name (get-in schema [:organizations o :discovery aspect]
                                           :everyone)))]
    (or (contains? viewer-orgs o)
        (case rule
          :members       false
          :collaborators (boolean (seq (set/intersection viewer-orgs (collaborators schema o))))
          true))))

(defn hidden-organizations
  [schema viewer-orgs aspect]
  (into #{} (remove #(discoverable? schema viewer-orgs % aspect)) (keys (:organizations schema))))

(defn principal-hidden?
  ([schema viewer-orgs ids] (principal-hidden? schema viewer-orgs ids :users))
  ([schema viewer-orgs ids aspect]
   (if-let [o (primary-organization schema ids)]
     (not (discoverable? schema viewer-orgs o aspect))
     false)))
