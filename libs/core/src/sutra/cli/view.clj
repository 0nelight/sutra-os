(ns sutra.cli.view
  (:require [sutra.types.kinds :as kinds]
            [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [sutra.types.links :refer [links-of]]
            [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn text [v]
  (cond (nil? v) "" (keyword? v) (name v) (string? v) v
        (and (coll? v) (every? (complement coll?) v)) (str/join ", " (map text v))
        :else (pr-str v)))

(defn cell [v]
  (let [s (text v)] (if (> (count s) 60) (str (subs s 0 59) "…") s)))

(defn flat? [v] (or (not (coll? v)) (and (sequential? v) (every? (complement coll?) v))))

(def review-glyph
  {"create" "+" "update" "~" "delete" "−" "connect" "⇄" "disconnect" "⊘"})

(def verb-codes
  {"create" :green "update" :yellow "delete" :red "connect" :cyan "disconnect" :magenta})

(defn row-verb [a]
  (or (:verb a) (first (str/split (name (:action a)) #"-" 2))))

(defn row-class
  [a]
  (or (:type a) (second (str/split (name (:action a)) #"-" 2)) "?"))

(defn row-name
  [a]
  (cond
    (#{"connect" "disconnect"} (row-verb a))
    (str (:from-short a) "→" (:to-short a))
    :else
    (str (or (:type-name a)
             (get-in a [:args :name])
             (get-in a [:args :values :name])
             (:short-id a)
             "?"))))

(defn plural [n s] (if (or (= 1 n) (str/ends-with? s "s")) s (str s "s")))

(defn- sa-cell
  [linked attrs]
  (->> linked
       (map (fn [o] (->> attrs (map #(get o %)) (remove nil?) (map str) (str/join " "))))
       (remove str/blank?)
       (str/join " · ")))

(defn query-columns
  [show]
  (for [item (if (seq show) show [:name])]
    (if (map? item)
      (let [[conn attrs] (first item)]
        [(name conn) (fn [row] (sa-cell (get-in row [:values conn]) attrs))])
      [(name item) (fn [row] (str (some-> (get-in row [:values item]))))])))

(defn project-rows
  [show rows]
  (if (seq show)
    (let [ks (map #(if (map? %) (ffirst %) %) show)]
      (mapv (fn [r] {:id (:id r) :values (select-keys (:values r) ks)}) rows))
    rows))

(def schema-section-order
  [:value-types :shared-properties :object-types :object-type-groups :interfaces
   :link-types :transforms :pipelines :datasets :media-sets :sources :exports :actions
   :object-sets :functions :applications :graphs])

(def ^:private overview-omit
  #{:projects})

(def name-fields
  [:object-type-name :link-type-name :interface-name :transform-name :action-name
   :function-name :source-name :object-set-name :shared-property-name :project-name
   :value-type-name :object-type-group-name :graph-name :dataset-name :pipeline-name
   :repository-name :media-set-name :export-name :application-name
   :name])

(defn entity-names
  [section-map]
  (->> section-map
       (map (fn [[k v]]
              (let [nm (if (map? v) (or (some v name-fields) k) k)]
                (if (keyword? nm) (clojure.core/name nm) (str nm)))))
       (remove str/blank?)
       distinct sort))

(defn rid->otname
  [otypes]
  (into {} (for [[rid spec] otypes]
             [rid (some-> (:object-type-name spec) clojure.core/name)])))

(defn- entry-by-name
  [smap field nm]
  (let [want (clojure.core/name nm)]
    (some (fn [[rid spec]]
            (when (= want (some-> (get spec field) clojure.core/name)) [rid spec]))
          smap)))

(defn object-type-entry [otypes nm] (entry-by-name otypes :object-type-name nm))

(defn kw-name [x] (cond (keyword? x) (clojure.core/name x) (nil? x) nil :else (str x)))

(defn in-project
  [projects project section-map]
  (let [ids (into #{project}
                  (comp (filter (fn [[_ v]] (= project (some-> (some v name-fields) kw-name))))
                        (map (comp str kw-name key)))
                  projects)]
    (into {} (filter (fn [[_ v]] (contains? ids (some-> (:project v) kw-name str))))
          section-map)))

(defn- spec-name
  [spec]
  (when (map? spec) (some-> (some spec name-fields) kw-name)))

(defn section-entry
  [smap nm]
  (let [want (clojure.core/name nm)]
    (or (some (fn [[rid spec]] (when (= want (spec-name spec)) [rid spec])) smap)
        (some (fn [[k spec]] (when (and (map? spec) (= want (kw-name k))) [nil spec]))
              smap))))

(defn schema-entry-view
  [body sec nm]
  (when-let [[rid spec] (section-entry (get body sec) nm)]
    (cond-> (assoc spec :name (clojure.core/name nm))
      rid (assoc :rid rid)
      (= sec :interfaces)
      (assoc :implemented-by
             (vec (sort (for [[_ ot] (:object-types body)
                              :when (some #{rid} (:implements ot))]
                          (kw-name (:object-type-name ot)))))))))

(defn dataset-entry [datasets nm] (entry-by-name datasets :dataset-name nm))

(defn dataset-view
  [body nm]
  (when-let [[rid spec] (dataset-entry (:datasets body) nm)]
    {:dataset (clojure.core/name nm)
     :rid     rid
     :title   (:title spec)
     :kind    (kw-name (:kind spec))
     :description (:description spec)
     :columns (vec (for [c (:columns spec)]
                     {:name (kw-name (:attribute-name c))
                      :type (kw-name (:attribute-type c))}))}))

(defn rid->name-index
  [body]
  (into {} (for [[_ m] body :when (map? m)
                 [rid spec] m :when (map? spec)
                 :let [nm (some spec name-fields)] :when nm]
             [(kw-name rid) (kw-name nm)])))

(defn transform-spec
  [body nm]
  (let [want (clojure.core/name nm)]
    (some (fn [[_ s]]
            (when (and (= want (some-> (:transform-name s) clojure.core/name))
                       (not (:function-name s)))
              s))
          (:transforms body))))

(def ^:private row-stamp-attrs
  #{"created-at" "updated-at"})

(defn- temporal-attrs
  [body nm]
  (when-let [[_ spec] (object-type-entry (:object-types body) nm)]
    (let [all (for [a (:attribute-list spec)
                    :when (#{"timestamp" "date"} (kw-name (:attribute-type a)))]
                (kw-name (:attribute-name a)))]
      [(remove row-stamp-attrs all) (vec all)])))

(defn latest-order
  [body nm]
  (let [[domain all] (temporal-attrs body nm)
        t            (clojure.core/name nm)]
    (cond
      (= 1 (bounded-count 2 domain)) {:order-by (keyword (first domain)) :sort-dir :desc}
      (seq domain)
      (refusal/refuse! :refusal.cli/latest-ambiguous {:type t :candidates (vec domain)}
                       {:properties (str/join ", " domain)})
      (seq all) {:order-by (keyword (first all)) :sort-dir :desc}
      :else
      (refusal/refuse! :refusal.cli/latest-no-time {:type t}))))

(defn transform-view
  [body nm]
  (when-let [spec (transform-spec body nm)]
    (let [idx    (rid->name-index body)
          rn     (fn [r] (when r (or (get idx (kw-name r)) (kw-name r))))
          in-ref (fn [v]
                   (str/trim
                    (str (kw-name (:kind v)) ":"
                         (rn (or (:object-type v) (:dataset v) (:transform v)
                                 (:datasource v) (:source v)))
                         (when (:table v) (str ":" (:table v)))
                         (when (:model v) (str ":" (:model v))))))
          fk?    (fn [o] (and (= :link-type (keyword (kw-name (:kind o)))) (:fk-attribute o)))
          outs   (mapv (fn [o]
                         (cond-> (reduce-kv (fn [m k v]
                                              (assoc m k (if (kinds/output-ref-keys k)
                                                           (rn v)
                                                           (if (keyword? v) (kw-name v) v))))
                                            {}
                                            (cond-> o
                                              (fk? o) (dissoc :dataset :on-null :on-unresolved)))
                           (fk? o) (assoc :build-node false
                                          :edges (str "join on " (kw-name (:fk-attribute o))
                                                      " at read time"))))
                       (tio/outputs-of spec))
          inputs (if (seq (:inputs spec))
                   (into {} (for [[k v] (:inputs spec)]
                              [(clojure.core/name k) (in-ref v)]))
                   (when-let [in (:input spec)]
                     {"source" (in-ref (merge in (select-keys (:source spec) [:table])))}))
          code   (or (:code spec) (:query spec) (get-in spec [:source :query]))]
      {:transform        (clojure.core/name nm)
       :language         (kw-name (:language spec))
       :inputs           (or inputs {})
       :outputs          outs
       :primary-key      (kw-name (:primary-key spec))
       :incremental      (or (:incremental spec) false)
       :external-systems (mapv kw-name (:external-systems spec))
       :code             code
       :has-code?        (boolean code)})))

(defn link-type-entry [ltypes nm] (entry-by-name ltypes :link-type-name nm))

(defn rid-entry
  [body token]
  (let [want (clojure.core/name token)]
    (when (rid/rid? want)
      (some (fn [[sec m]]
              (when (map? m)
                (when-let [spec (get m want)]
                  (when (map? spec) [sec want spec]))))
            body))))

(defn describe-token->json [{:keys [kind section view names]}]
  (case kind
    :ambiguous {:error "ambiguous" :topic (clojure.core/name section) :matches view}
    :unknown   {:error "unknown" :topic (clojure.core/name section)}
    :section   {:names names}
    view))

(defn- overview-rows
  [body]
  (let [secs (remove overview-omit
                     (concat schema-section-order
                             (remove (set schema-section-order) (keys body))))]
    (vec (for [s secs
               :let [m (get body s)]
               :when (map? m)
               :let [names (entity-names m)]
               :when (seq names)]
           {:section s :show (vec names)}))))

(defn schema-overview-result
  [body]
  {:sections (into {} (for [{:keys [section show]} (overview-rows body)]
                        [section show]))})

(defn object-views
  [body nm]
  (let [me  (kw-name nm)
        rid (first (object-type-entry (:object-types body) (keyword me)))]
    (vec (sort (for [[k spec] (:applications body)
                     :when (and (map? spec) (:form-factor spec)
                                (#{me rid} (kw-name (:object-type spec))))]
                 (or (spec-name spec) (kw-name k)))))))

(defn- type-view
  [body nm {:keys [backing]}]
  (when-let [[rid spec] (object-type-entry (:object-types body) nm)]
    (let [me  (clojure.core/name nm)
          fields (vec (for [a (:attribute-list spec)]
                        {:name (kw-name (:attribute-name a))
                         :type (kw-name (:attribute-type a))}))
          links (mapv #(select-keys % [:name :from :to :cardinality])
                      (links-of body rid))
          proj  (if-let [prid (:project spec)]
                  (or (some (fn [[prid* p]]
                              (when (= prid* prid) (kw-name (:project-name p))))
                            (:projects body))
                      (str "⚠ dangling ref " prid))
                  "unassigned")]
      {:object-type me
       :rid         rid
       :project     proj
       :status      (:status spec)
       :symbol      (:symbol spec)
       :plural      (:plural spec)
       :implements  (mapv kw-name (or (:implements spec) []))
       :primary-key (get-in backing [:object-types me :primary-key])
       :title       (kw-name (:title-key spec))
       :match-keys  (mapv kw-name (or (:match-keys spec) []))
       :edit-conflict (:edit-conflict spec)
       :views       (object-views body nm)
       :fields      fields
       :links       links})))

(defn- link-type-view
  [body nm {:keys [backing]}]
  (when-let [[rid spec] (link-type-entry (:link-types body) nm)]
    (let [idx (rid->otname (:object-types body))
          nom (fn [r] (or (get idx r) (kw-name r)))]
      {:link-type   (clojure.core/name nm)
       :rid         rid
       :from        (nom (:from-object-type spec))
       :to          (nom (:to-object-type spec))
       :cardinality (kw-name (:cardinality spec))
       :status      (kw-name (:status spec))
       :backing     (get-in backing [:link-types (clojure.core/name nm)])})))

(defn- describe-token-view
  [body sections project section {:keys [count-of] :as env}]
  (let [rid? (rid-entry body section)
        ot? (and (not rid?) (not (sections section))
                 (object-type-entry (:object-types body) section))
        tf? (and (not rid?) (not (sections section)) (not ot?)
                 (transform-spec body section))
        lt? (and (not rid?) (not (sections section)) (not ot?) (not tf?)
                 (link-type-entry (:link-types body) section))
        ds? (and (not rid?) (not (sections section)) (not ot?) (not tf?) (not lt?)
                 (dataset-entry (:datasets body) section))
        secs (when (and (not rid?) (not (sections section)) (not ot?) (not tf?)
                        (not lt?) (not ds?))
               (filterv (fn [s] (section-entry (get body s) section))
                        [:interfaces :shared-properties :value-types :repositories
                         :projects :actions]))
        sec  (first secs)]
    (cond
      rid?
      (let [[sc rid spec] rid?]
        {:kind :resource :section section
         :view (assoc spec :rid rid :kind (clojure.core/name sc)
                      :name (or (some-> (some spec name-fields) kw-name) rid))})
      ot?
      {:kind :object-type :section section
       :view (merge (type-view body section env)
                    (try {:count (count-of section)}
                         (catch InterruptedException e (throw e))
                         (catch Exception e {:count nil :count-error (ex-message e)})))}
      tf?
      {:kind :transform :section section :view (transform-view body section)}
      lt?
      {:kind :link-type :section section :view (link-type-view body section env)}
      ds?
      {:kind :dataset :section section :view (dataset-view body section)}
      (< 1 (count secs))
      {:kind :ambiguous :section section
       :view (vec (for [s secs
                        :let [[rid _] (section-entry (get body s) section)]]
                    {:kind (clojure.core/name s) :rid rid}))}
      sec
      {:kind ({:interfaces :interface :shared-properties :shared-property
               :value-types :value-type :actions :action-type} sec :resource)
       :section section :view (assoc (schema-entry-view body sec section)
                                     :kind (clojure.core/name sec))}
      (= section :llm-models)
      {:kind :llm-models :section section :view (:llm-models body)}
      (sections section)
      (let [m0 (get body section)
            m1 (if (= section :transforms)
                 (into {} (remove (comp :function-name val)) m0)
                 m0)
            m  (if (and project (map? m1)) (in-project (:projects body) project m1) m1)]
        {:kind :section :section section
         :names (vec (when (map? m) (entity-names m)))})
      :else
      {:kind :unknown :section section})))

(defn described
  [body tokens project env]
  (let [secset (set (concat schema-section-order (keys body)))]
    (cond
      (and project (not (some #{project} (entity-names (:projects body)))))
      {:no-project project}
      (empty? tokens)
      {:shape :overview :data (schema-overview-result body)}
      :else
      (let [views (mapv #(describe-token-view body secset project % env) tokens)]
        (if (= 1 (count views))
          {:shape :one :data (describe-token->json (first views))}
          {:shape :many
           :data {:described (into {} (map (juxt :section describe-token->json) views))}})))))

(defn problem-line
  [it]
  (if (map? it)
    (let [subj (some it [:transform :object-type :link-type :action :application
                         :app :name :attribute :entity-class])
          why  (some it [:reason :section-error :shape-error :error :problem])
          why* (if (coll? why) (str/join "; " (map str why)) why)]
      (if why*
        (str (when subj (str subj " — ")) why*)
        (pr-str it)))
    (str it)))

(defn clip
  ([s] (clip s 80))
  ([s n] (let [s (str/trim (str/replace s #"\s+" " "))]
           (if (> (count s) n) (str (subs s 0 n) "…") s))))

(defn- val-summary
  [values]
  (let [named (some-> (or (:name values) (get values "name")) str not-empty)
        scals (->> (vals values)
                   (filter #(or (string? %) (number? %) (keyword? %) (boolean? %)))
                   (map str)
                   (remove str/blank?)
                   distinct)]
    (->> (cons named scals) (remove nil?) distinct (take 4) (map clip) (str/join "  "))))

(defn objects->rows
  [objects names]
  (vec (for [[type-ref rows] objects
             {:keys [id values]} rows
             :let [tn (get names type-ref (str type-ref))]]
         {:id    (str id)
          :type  tn
          :label (str/trim (str tn "  " (val-summary values)))})))

(def ^:private lineage-glyph
  {:object-type "o" :link-type "lt" :dataset "d" :language-model "llm"
   :interface "i" :datasource "ds"})

(defn node-label
  [{:keys [kind name table] q? :query?}]
  (str (get lineage-glyph (keyword kind) (kw-name kind)) ":" name
       (cond table (str "." table)
             q?    "(query)"
             :else "")))

(defn ref-tokens
  [& xs]
  (->> xs
       (mapcat (fn [x] (cond (nil? x) nil
                             (sequential? x) x
                             :else [x])))
       (mapcat #(str/split (clojure.core/name %) #","))
       (remove str/blank?)))

(defn graph-node-refs
  [refs]
  (let [bucket {"datasource"  :datasources  "dataset"     :datasets
                "transform"   :transforms   "object-type" :object-types
                "link-type"   :link-types   "action"      :actions
                "function"    :functions    "application" :applications}]
    (reduce (fn [m r]
              (let [s (clojure.core/name r)
                    [kind nm] (if (str/includes? s "/")
                                (str/split s #"/" 2)
                                ["object-type" s])
                    k (or (bucket kind)
                          (refusal/refuse! :refusal.cli/unknown-node-kind {:ref r}
                                           {:kind  kind
                                            :kinds (str/join ", " (sort (keys bucket)))}))]
                (update m k (fnil conj #{}) (keyword nm))))
            {} refs)))

(defn scope-count [vt] (reduce + 0 (map count (vals vt))))

(def busy-graph 40)
