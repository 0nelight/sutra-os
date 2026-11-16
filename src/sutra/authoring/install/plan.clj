(ns sutra.authoring.install.plan
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.function-contract :as contract]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.outputs :as outputs]
            [sutra.kit.coerce :refer [drop-empty ->name-kw]]
            [sutra.types.shape :as tshape]
            [sutra.catalog.publishing :as publishing]
            [clojure.string :as str]
            [sutra.types.attribute :as attribute]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.findings :as fnd]
            [sutra.authoring.install.findings.actions :as actions]
            [sutra.authoring.install.findings.apps :as apps]
            [sutra.authoring.install.findings.functions :as function-findings]
            [sutra.authoring.install.findings.backing :as backing-findings]
            [sutra.authoring.install.findings.derive :as derive-findings]
            [sutra.authoring.install.findings.exports :as export-findings]
            [sutra.authoring.install.findings.security :as security-findings]
            [sutra.authoring.install.findings.sources :as source-findings]
            [sutra.authoring.install.findings.loops :as loop-findings]
            [sutra.authoring.install.findings.retire :as retire]
            [sutra.authoring.install.findings.sql :as sql-findings]
            [sutra.authoring.install.migration :as migration]
            [sutra.catalog.program :as prog]
            [sutra.parser.sugar :as sugar]
            [sutra.types.backing :as backing]
            [sutra.types.entities :as ents]
            [sutra.types.versions :as vers]
            [sutra.types.finding :as finding]
            [sutra.types.languages :as langs]
            [sutra.types.link-side :as ls]
            [sutra.types.paths :as paths]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.semver :as semver]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(def ^:private verb-of {:created "create-" :updated "update-" :deleted "delete-"})

(defn- ->edit
  ([kind entity outcome] {:kind kind :entity entity :outcome outcome})
  ([kind entity outcome args]
   (assoc (->edit kind entity outcome)
          :action (keyword (str (verb-of outcome) (name kind)))
          :args args)))

(defn- diff-edit
  [kind entity exists? same? args]
  (cond (not exists?) (->edit kind entity :created args)
        (same?)       (->edit kind entity :skipped)
        :else         (->edit kind entity :updated args)))

(defn- storable
  [kind want have]
  (let [known (into #{:name :id} (keys (get ents/fields (ents/tag->section kind))))]
    (into {} (filter (fn [[k _]] (or (known k) (contains? have k)))) want)))

(defn- plan-diff
  [kind ent-kw want current canonical->args]
  (let [existing (live/lookup current ent-kw)]
    (diff-edit kind ent-kw (some? existing)
               #(let [have (canonical->args ent-kw existing)]
                  (= (storable kind (drop-empty want) have) have))
               want)))

(defn- kebab->snake [s] (str/replace (name s) #"-" "_"))

(defn- ->wire
  [t]
  (-> t (dissoc :transform-name) (assoc :name (:transform-name t))))

(defn- attr->author
  [{:keys [attribute-name attribute-type opts]}]
  (if (seq opts)
    [attribute-name attribute-type opts]
    [attribute-name attribute-type]))

(defn- shape-projection
  [section]
  (fn [k spec] (drop-empty (tshape/->author section spec {:name k}))))

(defn- author-args
  [section spec base]
  (tshape/project section spec base {:from :author :to :author}))

(defn- canonical-conn->wire-args [c-kw c]
  (drop-empty (tshape/->author :link-types c {:name c-kw})))

(defn- canonical-interface->wire-args [iface-kw c]
  (drop-empty
   (tshape/->author :interfaces c
                    {:name             iface-kw
                     :property-list    (mapv attr->author (:property-list c))
                     :link-constraints (mapv ls/normal-constraint (:link-constraints c))})))

(defn- oc->pairs
  [oc]
  (mapv (fn [a] (if (map? a) [(:attribute-name a) (:attribute-type a)] (vec a)))
        oc))

(defn- tx->wire-args [t-kw c]
  (drop-empty
   (cond-> (tshape/->author :transforms c {:name t-kw})
     (seq (:output-columns c)) (assoc :output-columns (oc->pairs (:output-columns c)))
     (langs/query-language? (:language c)) (assoc :query (:query c)))))

(defn- dataset->wire-args [k c]
  (drop-empty (tshape/->author :datasets c {:name k})))

(defn- compat-refusal
  [kind version snapshot live]
  (let [major (:major (semver/parse version))]
    (when-not (or (semver/initial-development? version) (semver/prerelease? version))
      (let [sig    (memoize (fn [v] (contract/signature kind (if (= v version)
                                                               snapshot
                                                               (get (functions/releases live) v)))))
            line   (filter #(and (= major (:major (semver/parse %))) (not (semver/prerelease? %)))
                           (functions/released-versions live))
            broken (for [r line
                         :let [[lo hi] (if (neg? (semver/compare-versions r version))
                                         [r version] [version r])
                               old (sig lo) new (sig hi)
                               breaks (when (and old new) (contract/breaks old new))]
                         :when (seq breaks)]
                     [lo hi breaks])]
        (when (seq broken)
          {:major  major
           :breaks (str/join " | " (for [[lo hi breaks] broken]
                                     (str lo " → " hi ": " (str/join "; " breaks))))
           :next   (inc (apply max major (keep #(:major (semver/parse %))
                                               (functions/released-versions live))))})))))

(def ^:private release-wording
  {:edit     {:noun "function"}
   :query    {:noun "function"}
   :template {:noun   "template"
              :advice " A body that is still being written needs no release: `render-template --file <x.entt> --preview` renders the checkout's draft without installing it."}})

(defn- with-release
  [kind f-kw args live]
  (let [history (merge (functions/releases args) (functions/releases live))]
    (if-let [version (:version args)]
      (let [snapshot (cond-> (vers/snapshot (dissoc args ::libraries))
                       (seq (::libraries args)) (assoc :libraries (::libraries args)))
            args     (dissoc args ::libraries)]
        (when-let [[code _] (functions/release-conflict live version snapshot)]
          (let [{:keys [noun advice]} (release-wording kind)]
            (refusal/refuse! code {:function f-kw :version version}
                             {:noun noun :advice (or advice "")})))
        (when-not (contains? history version)
          (when-let [params (compat-refusal kind version snapshot live)]
            (refusal/refuse! :refusal.authoring/release-breaks-line
                             {:function f-kw :version version} params)))
        (assoc args :releases (assoc history version snapshot)))
      (cond-> (dissoc args ::libraries) (seq history) (assoc :releases history)))))

(defn- with-first-publish
  [section k args live]
  (if (vers/publishes? section)
    (publishing/versioned section k args live)
    (cond-> args
      (and (nil? live) (nil? (:status args))) (assoc :status :published))))

(defn- live-link-ends
  [cur]
  (let [name-of (fn [rid]
                  (or (some-> (get-in cur [:object-types (str rid) :object-type-name]) keyword)
                      (some-> (get-in cur [:interfaces (str rid) :interface-name]) keyword)))]
    (into {} (for [[_ lt] (:link-types cur)
                   :let  [nm (:link-type-name lt)]
                   :when nm]
               [nm {:from        (name-of (:from-object-type lt))
                    :to          (name-of (:to-object-type lt))
                    :cardinality (:cardinality lt)
                    :opts        (select-keys lt [:forward-api-name :reverse-api-name])}]))))

(defn live-ref-symbols
  [cur]
  (let [fn-names (into (set (keep :function-name (vals (:functions cur))))
                       (keep :function-name) (vals (:transforms cur)))]
    {:links           (live-link-ends cur)
     :object-names    (set (keep :object-type-name (vals (:object-types cur))))
     :link-names      (set (keep :link-type-name (vals (:link-types cur))))
     :interface-names (set (keep :interface-name (vals (:interfaces cur))))
     :iface-links     (into {} (map (fn [i] [(:interface-name i)
                                             (set (map :link-name (:link-constraints i)))]))
                            (vals (:interfaces cur)))
     :function-names  fn-names
     :action-names    (set (keys (:actions cur)))
     :application-names (set (keep :application-name (vals (:applications cur))))
     :object-set-names (set (keep :object-set-name (vals (:object-sets cur))))
     :llm-models      (set (keys (:llm-models cur)))
     :project-names   (set (keep :project-name (vals (:projects cur))))
     :space-names     (set (keep :space-name (vals (:spaces cur))))
     :organization-names (set (keep :organization-name (vals (:organizations cur))))
     :value-type-names (set (keep :value-type-name (vals (:value-types cur))))
     :ss-types        (set (keep (fn [o] (when (:semantic-search o) (:object-type-name o)))
                                 (vals (:object-types cur))))}))

(defn- scope-name
  [m spec]
  (some-> (sch/scope-of m spec)
          (as-> s (or (some-> (get-in m [:spaces s :space-name]) name) (name s)))))

(defn- spaced
  [ctx kinds-key want at]
  (let [cur   (:current ctx)
        dsr   (:schema ctx)
        rn    (sch/rid-name-index cur)
        shown (fn [e] (rid/rewrite-refs kinds-key e (fn [_ v] (get rn v v))))]
    (reduce-kv
     (fn [ops n spec]
       (let [n     (name n)
             scope (scope-name dsr spec)
             [k own hits :as found] (at n scope)]
         (cond
           (nil? found)  (reduced nil)
           (empty? hits) ops
           own           (assoc ops k [:put (shown (val own))])
           (and (= 1 (count hits)) (nil? (scope-name cur (val (first hits))))) ops
           (and (= 1 (count hits)) (nil? scope)) ops
           :else         (assoc ops k [:drop]))))
     {} want)))

(defn- in-space-whole
  [ctx kinds-key name-key base want]
  (let [base    (live/whole base)
        cur     (:current ctx)
        by-name (group-by #(some-> (get (val %) name-key) name) (get cur kinds-key))
        key-of  (into {} (map (fn [k] [(name k) k])) (keys base))
        at      (fn [n scope]
                  (let [hits (get by-name n)]
                    [(get key-of n (keyword n))
                     (some #(when (= scope (scope-name cur (val %))) %) hits)
                     hits]))]
    (reduce-kv (fn [m k [op v]] (if (= :put op) (assoc m k v) (dissoc m k)))
               base (spaced ctx kinds-key want at))))

(defn- in-space-named
  [{cur :current names :names :as ctx} kinds-key name-key base want]
  (let [es    (get cur kinds-key)
        at    (fn [n scope]
                (let [ks   (distinct (map first (live/named cur names kinds-key name-key
                                                           base n)))
                      hits (vec (for [r (distinct (names kinds-key n))
                                      :let [e (find es r)]
                                      :when (and e (= n (some-> (get (val e) name-key) name)))]
                                  e))
                      owns (filter #(= scope (scope-name cur (val %))) hits)]
                  (when-not (or (< 1 (count ks)) (< 1 (count owns)))
                    [(or (first ks) (keyword n)) (first owns) hits])))]
    (spaced ctx kinds-key want at)))

(defn- in-space
  [ctx kinds-key name-key base]
  (let [cur  (:current ctx)
        dsr  (:schema ctx)
        want (get dsr (ents/section->author kinds-key))]
    (if-not (and (map? want) (or (seq (:spaces cur)) (seq (:spaces dsr))))
      base
      (let [whole (delay (in-space-whole ctx kinds-key name-key base want))]
        (if-let [ops (when (live/looked? base)
                       (in-space-named ctx kinds-key name-key base want))]
          (live/patched base ops whole)
          @whole)))))

(defn- cur
  [ctx kinds-key name-key]
  (in-space ctx kinds-key name-key ((:live ctx) kinds-key name-key)))

(defn- live-index
  [ctx live section]
  (let [idx (if (vector? live)
              (cur ctx (first live) (second live))
              (live ctx))]
    (if (contains? ents/normalized-name-sections section)
      (let [full (delay (live/by-name (for [[k v] (live/whole idx)]
                                        [(keyword (rid/->name k)) k v])))]
        (if (vector? live)
          (live/normalized (:current ctx) (:names ctx) (first live) (second live) idx full)
          @full))
      idx)))

(defn- hide-absent
  [canonical->args silent want]
  (let [drop-ks (into [] (remove #(some? (get want %))) silent)]
    (if (seq drop-ks)
      (fn [k c] (apply dissoc (canonical->args k c) drop-ks))
      canonical->args)))

(def ^:private minted-output-keys
  [:dataset :name])

(defn- hide-minted-outputs
  [canonical->args want]
  (let [outs    (remove nil? (cons (:output want) (:outputs want)))
        stated  (into #{} (mapcat (fn [o] (filter #(some? (get o %)) minted-output-keys))) outs)
        drop-ks (into [] (remove stated) minted-output-keys)]
    (if (empty? drop-ks)
      canonical->args
      (fn [k c]
        (let [args  (canonical->args k c)
              clean #(apply dissoc % drop-ks)]
          (cond-> args
            (:output args)        (update :output clean)
            (seq (:outputs args)) (update :outputs #(mapv clean %))))))))

(defn- plan-transform-diff
  [current want]
  (plan-diff :transform (:name want) want current (hide-minted-outputs tx->wire-args want)))

(defn- merge-lists
  [key-fn live incoming]
  (let [inc-by  (coll/index-by key-fn incoming)
        live-ks (into #{} (map key-fn) live)
        kept    (mapv (fn [x] (get inc-by (key-fn x) x)) live)
        added   (filterv #(not (live-ks (key-fn %))) incoming)]
    (into kept added)))

(defn- merge-additive
  [{:keys [mode canon live key-fn]} incoming]
  (if (and (= mode :merge) canon (seq live))
    (merge-lists key-fn (vec live) (or incoming []))
    incoming))

(defn- ident-str [x] (if (keyword? x) (name x) (str x)))

(defn- with-entity
  [kind ent-kw f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e (throw e))
       (catch InterruptedException e (throw e))
       (catch Exception e
         (anomaly/raise! :fault (str (ident-str kind) " " (ident-str ent-kw) ": "
                                     (or (ex-message e) (.getName (class e))))
                         {:kind kind :entity ent-kw :class (.getName (class e)) ::escaped true}
                         e))))

(defn- versioning
  ([section] (versioning section nil))
  ([section kind]
   (let [{:keys [mode compat first-publish]} (vers/of section)
         compat (if (map? compat) (get compat kind (:default compat)) compat)]
     (when (or mode first-publish)
       (fn [want k live]
         (cond-> (case mode
                   :release (with-release compat k want live)
                   :publish (vers/carry want live)
                   want)
           first-publish (#(with-first-publish section k % live))))))))

(defn- with-revision
  [want k live]
  (if (nil? live)
    (assoc want :revision 1)
    (let [canon (hide-absent (shape-projection :actions) [:edits] want)
          have  (dissoc (canon k live) :revision)
          same? (= (storable :action (drop-empty (dissoc want :revision)) have) have)
          rev   (:revision live)]
      (cond same? (cond-> (dissoc want :revision) rev (assoc :revision rev))
            :else (assoc want :revision (inc (or rev 1)))))))

(defn- entity-phase
  [{:keys [shape section kind live desired canonical only silent-when-absent
           silent-nested publish]}]
  (let [projected (when shape (shape-projection shape))
        section   (or section shape)
        publish   (or publish (versioning section))
        desired   (or desired
                      (when projected (fn [_ k spec] (projected k spec))))
        canonical (or canonical projected)]
    (fn [ctx]
      (let [current (live-index ctx live section)]
        (vec
         (for [[ent-kw spec] (get (:schema ctx) section)
               :when (or (nil? only) (only spec))]
           (with-entity kind ent-kw
             #(let [want (cond-> (desired ctx ent-kw spec)
                           publish (publish ent-kw (live/lookup current ent-kw)))]
                (plan-diff kind ent-kw want current
                           (cond-> (hide-absent canonical silent-when-absent want)
                             silent-nested (silent-nested want)))))))))))

(defn- edit-function-args
  [ctx k spec]
  (let [{:keys [language source]} (if (:library spec) {} spec)]
    (when (and source (or (= :python language)
                          (and (contains? #{nil :clojure} language)
                               (str/includes? source ":sutra/function"))))
      (try (prog/parse-source {:language (or language :clojure) :source source})
           (catch clojure.lang.ExceptionInfo e
             (refusal/refuse! :refusal.authoring/function-invalid
                              (assoc (ex-data e) :function k)
                              {:why (ex-message e)}
                              e))))
    (when (and source (contains? #{nil :clojure} language))
      (when-let [steps (try (:steps (prog/parse-source {:language (or language :clojure)
                                                         :source source}))
                            (catch clojure.lang.ExceptionInfo e
                              (if (anomaly/client-error? (ex-data e))
                                (refusal/refuse! :refusal.authoring/function-invalid
                                                 (assoc (ex-data e) :function k)
                                                 {:why (ex-message e)}
                                                 e)
                                nil))
                            (catch Exception e
                              (attempt/rethrow-fatal! e)
                              nil))]
        (when-not (prog/writes-an-edit? steps)
          (refusal/refuse! :refusal.authoring/edit-function-no-edit
                           {:function k :steps (mapv :op steps)}))))
    (cond-> ((shape-projection :functions) k spec)
      (seq (:uses spec))
      (assoc ::libraries (functions/bound-libraries
                          (functions/library-lookup (:functions (:schema ctx)) (:current ctx))
                          k spec)))))

(defn- plan-query-functions
  [ctx]
  (let [current (cur ctx :transforms :transform-name)]
    (vec
     (for [[f-kw spec] (:functions (:schema ctx))
           :when (not (ents/edit-function? spec))]
       (with-entity :function f-kw
         #(let [[enriched failure] ((:introspect ctx) (:code ctx)
                                                      (assoc spec :name f-kw :function-name f-kw
                                                             :sutra.code/section :functions))]
            (when-not (langs/code-language? (:language enriched))
              (refusal/refuse! :refusal.authoring/function-language
                               {:function f-kw :language (:language enriched)}
                               {:languages (set (filter langs/code-language?
                                                        langs/compute-language-names))}))
            (when (or (:output enriched) (seq (:outputs enriched)))
              (refusal/refuse! :refusal.authoring/function-output
                               {:function f-kw
                                :output (or (:output enriched) (vec (:outputs enriched)))}))
            (let [want ((versioning :functions :query) (tx->wire-args f-kw enriched) f-kw
                                                       (live/lookup current f-kw))]
              (cond-> (plan-transform-diff current want)
                failure (assoc :introspection-failure failure)))))))))

(defn- auto->object-tx
  [ds type-kw attrs]
  {:name        (backing/transform-name :object-type type-kw)
   :input       {:kind :datasource :datasource ds}
   :source      {:table (kebab->snake type-kw)}
   :primary-key (ffirst attrs)
   :column-map  (into {} (for [[a _] attrs] [a a]))
   :output      {:kind :object-type :object-type type-kw}})

(defn- auto->conn-tx [ds conn-kw from-kw to-kw]
  {:name            (backing/transform-name :link-type conn-kw)
   :input           {:kind :datasource :datasource ds}
   :source          {:table (str "c_" (kebab->snake conn-kw))}
   :from-key-column (keyword (str (kebab->snake from-kw) "_key"))
   :to-key-column   (keyword (str (kebab->snake to-kw) "_key"))
   :output          {:kind :link-type :link-type conn-kw}})

(defn- dataset-lifts
  [datasets]
  (concat
   (for [[ds-kw spec] datasets
         :let [ext (:external-source spec)]
         :when ext]
     [ds-kw #(backing/in-project-of spec (->wire (backing/dataset-transform ds-kw ext)))])
   (for [[ds-kw spec] datasets
         :when (and (= :materialization (:kind spec)) (:object-type spec))]
     [ds-kw #(backing/in-project-of spec (->wire (backing/materialization-transform
                                                  ds-kw (:object-type spec))))])))

(defn- object-lifts
  [auto-ds type-kw {:keys [attributes external-source external-sources reconcile-mode] :as spec}]
  (let [primary (cond
                  external-source (->wire (backing/object-transform type-kw external-source))
                  (= reconcile-mode :merge) nil
                  auto-ds         (auto->object-tx auto-ds type-kw attributes))
        extras  (map-indexed (fn [i ext]
                               (->wire (backing/object-transform type-kw ext (str "-src" (inc i)))))
                             external-sources)]
    (mapv (partial backing/in-project-of spec) (keep identity (cons primary extras)))))

(defn- lifted-link-args
  [auto-ds [c-kw from to _card opts]]
  (some->> (if-let [ext (:external-source opts)]
             (->wire (backing/link-transform c-kw from to ext))
             (when auto-ds (auto->conn-tx auto-ds c-kw from to)))
           (backing/in-project-of opts)))

(defn- plan-dataset-source-transforms
  [ctx]
  (let [current (cur ctx :transforms :transform-name)]
    (vec (for [[ds-kw lift] (dataset-lifts (:datasets (:schema ctx)))]
           (with-entity :dataset ds-kw #(plan-transform-diff current (lift)))))))

(defn- media-set-lifts
  [media-sets]
  (for [[set-kw spec] media-sets
        :when (and (:sync spec) (:source spec))]
    [set-kw #(backing/in-project-of spec (->wire (backing/media-set-transform set-kw spec)))]))

(defn- plan-media-set-source-transforms
  [ctx]
  (let [current (cur ctx :transforms :transform-name)]
    (vec (for [[set-kw lift] (media-set-lifts (:media-sets (:schema ctx)))]
           (with-entity :media-set set-kw #(plan-transform-diff current (lift)))))))

(defn- plan-export-transforms
  [ctx]
  (let [current (cur ctx :transforms :transform-name)]
    (vec (for [[exp-kw spec] (:exports (:schema ctx))
               :when (:dataset spec)]
           (with-entity :export exp-kw
             #(plan-transform-diff current
                                   (backing/in-project-of
                                    spec (->wire (backing/export-transform exp-kw spec)))))))))

(defn- plan-object-source-transforms
  [ctx]
  (let [current (cur ctx :transforms :transform-name)]
    (into [] (mapcat (fn [[type-kw spec]]
                       (with-entity :object-type type-kw
                         #(mapv (partial plan-transform-diff current)
                                (object-lifts (:auto-ds ctx) type-kw spec)))))
          (:objs ctx))))

(defn- plan-link-source-transforms
  [ctx]
  (let [current (cur ctx :transforms :transform-name)]
    (into [] (keep (fn [c]
                     (with-entity :link-type (first c)
                       #(some->> (lifted-link-args (:auto-ds ctx) c)
                                 (plan-transform-diff current)))))
          (:conns ctx))))

(defn- object-type-args
  [ctx type-kw spec implements?]
  (let [spec (sugar/lower-display-name spec)
        {:keys [attributes implements reconcile-mode]} spec
        shared (:shared ctx)
        canon  (live/lookup (cur ctx :object-types :object-type-name) type-kw)
        attrs* (merge-additive {:mode reconcile-mode :canon canon :key-fn first
                                :live (map attr->author (:attribute-list canon))}
                               (mapv #(sugar/lower-shared-attr shared %) attributes))]
    (cond-> (author-args :object-types spec
                         {:name type-kw :attributes attrs* :whole-declaration? true})
      implements? (assoc :implements (vec implements))
      (= :merge reconcile-mode)
      (as-> args (merge (select-keys (tshape/->author :object-types canon {})
                                     (concat tshape/presentation-axis tshape/binding-axis))
                        args)))))

(defn- iface-attr-names
  [ctx implements]
  (let [authored (:ifaces ctx)
        live     (cur ctx :interfaces :interface-name)
        props-of (fn [k]
                   (concat (map #(if (keyword? %) % (first %)) (:property-list (get authored k)))
                           (map :attribute-name (:property-list (live/lookup live k)))))
        ext-of   (fn [k] (concat (:extends (get authored k)) (:extends (live/lookup live k))))]
    (loop [q (vec implements) seen #{} acc #{}]
      (if-let [k (first q)]
        (if (seen k)
          (recur (subvec q 1) seen acc)
          (recur (into (subvec q 1) (ext-of k)) (conj seen k) (into acc (props-of k))))
        acc))))

(defn- object-satisfied?
  [shared iface-attrs canon {:keys [attributes implements project ignore-inherited] :as args}
   check-impl?]
  (and (some? canon)
       (or (nil? project) (= project (:project canon)))
       (or (nil? ignore-inherited)
           (= (boolean ignore-inherited) (boolean (:ignore-inherited canon))))
       (tshape/scalars-satisfied? :object-types args canon)
       (let [proj       (fn [n t o] [n t (not-empty o)])
             have-attrs (set (map (fn [{:keys [attribute-name attribute-type opts]}]
                                    (proj attribute-name attribute-type opts))
                                  (:attribute-list canon)))
             want       (map (fn [a] (sugar/lower-shared-attr shared a)) attributes)
             want-attrs (map (fn [[n t o]] (proj n t o)) want)
             want-names (into #{} (map first) want)
             dropped    (into [] (comp (map :attribute-name)
                                       (remove want-names)
                                       (remove iface-attrs))
                              (:attribute-list canon))]
         (and (every? have-attrs want-attrs)
              (empty? dropped)))
       (or (not check-impl?)
           (= (set (map keyword (:implements canon))) (set (map keyword implements))))))

(defn- plan-object-pass
  [ctx check-impl?]
  (let [cur-objs (cur ctx :object-types :object-type-name)]
    (vec
     (for [[type-kw spec] (:objs ctx)
           :when (or (not check-impl?)
                     (seq (:implements spec))
                     (and (contains? spec :implements)
                          (seq (:implements (live/lookup cur-objs type-kw)))))]
       (with-entity :object-type type-kw
         #(let [canon (live/lookup cur-objs type-kw)
                args  (object-type-args ctx type-kw spec check-impl?)
                ifa
                (iface-attr-names ctx (distinct (concat (:implements spec) (:implements canon))))]
            (diff-edit :object-type type-kw (or (some? canon) check-impl?)
                       (fn [] (object-satisfied? (:shared ctx) ifa canon args check-impl?))
                       (cond-> args check-impl? (dissoc :status :deprecation)))))))))

(defn- plan-interfaces
  [ctx]
  (let [cur-ifaces (cur ctx :interfaces :interface-name)
        shared     (:shared ctx)]
    (vec
     (for [[iface-kw
            {:keys [property-list link-constraints reconcile-mode] :as spec}] (:ifaces ctx)]
       (with-entity :interface iface-kw
         #(let [canon  (live/lookup cur-ifaces iface-kw)
                plist* (merge-additive {:mode reconcile-mode :canon canon :key-fn first
                                        :live (map attr->author (:property-list canon))}
                                       (mapv (fn [a] (sugar/lower-shared-attr shared a))
                                             property-list))
                args (cond-> (author-args :interfaces spec {:name iface-kw})
                       (seq plist*)           (assoc :property-list plist*)
                       (seq link-constraints)
                       (assoc :link-constraints (mapv ls/normal-constraint link-constraints)))]
            (plan-diff :interface iface-kw args cur-ifaces canonical-interface->wire-args)))))))

(defn- plan-link-types
  [ctx]
  (let [cur-conns (cur ctx :link-types :link-type-name)]
    (vec
     (for [[c-kw from to card opts] (:conns ctx)]
       (with-entity :link-type c-kw
         #(let [canon (live/lookup cur-conns c-kw)
                sat (when-let [s (:satisfies opts)] (if (sequential? s) (vec s) [s]))
                sat (merge-additive {:mode (:reconcile-mode opts) :canon canon
                                     :key-fn identity :live (:satisfies canon)}
                                    sat)
                args (author-args :link-types (assoc opts :satisfies sat)
                                  {:name c-kw :from from :to to :cardinality card})]
            (plan-diff :link-type c-kw args cur-conns canonical-conn->wire-args)))))))

(defn- desired-transform-names
  [{:keys [schema objs conns auto-ds]}]
  (set (concat
        (for [[_ lift] (dataset-lifts (:datasets schema))] (:name (lift)))
        (for [[_ lift] (media-set-lifts (:media-sets schema))] (:name (lift)))
        (for [[type-kw spec] objs
              args (object-lifts auto-ds type-kw spec)]
          (:name args))
        (for [[type-kw {:keys [external-source reconcile-mode]}] objs
              :when (and auto-ds (not external-source) (= :merge reconcile-mode))]
          (backing/transform-name :object-type type-kw))
        (keep #(:name (lifted-link-args auto-ds %)) conns)
        (keys (:transforms schema))
        (keys (:functions schema)))))

(defn- in-repository?
  [{:keys [strict-repository current]} spec]
  (or (nil? strict-repository)
      (let [repo-ids (into #{} (keep rid/->name)
                           [strict-repository
                            (get-in current [:repositories (str strict-repository)
                                             :repository-name])])]
        (contains? repo-ids (rid/->name (:repository spec))))))

(defn- sweepable?
  [{:keys [strict-repository strict-projected]} author k spec]
  (if-let [projected (some-> strict-projected force)]
    (let [n (rid/->name k)]
      (or (contains? (get projected author) n)
          (and (= :transforms author)
               (contains? (get projected :functions) n))))
    (case (ents/sweep-only author)
      :standalone-or-virtual (contains? #{:standalone :virtual} (->name-kw (:kind spec)))
      :not-object-view       (or (nil? strict-repository) (not (tshape/object-view? spec)))
      :projected             (backing/projected-transform? spec)
      true)))

(defn- companions
  [ctx kind k]
  (let [tx (cur ctx :transforms :transform-name)]
    (for [t-kw (sort-by str (backing/generated kind k))
          :let [t (live/lookup tx t-kw)]
          :when (and t
                     (or (nil? (:repository t))
                         (backing/endpoint-fk-producer? t))
                     (not (contains? (:transforms (:schema ctx)) t-kw))
                     (some #(= k (get % (:kind %))) (tio/outputs-of t)))]
      t-kw)))

(defn- swept
  [{:keys [strict-projected names current]} live-of author]
  (let [section   (ents/author->section author)
        m         (live-of section)
        projected (when names (some-> strict-projected force))]
    (if-not projected
      (sort-by (comp str key) (live/whole m))
      (let [ns (cond-> (set (get projected author))
                 (= :transforms author) (into (get projected :functions)))]
        (sort-by (comp str first)
                 (distinct (mapcat #(live/named current names section
                                                (rid/name-field-of section) m %)
                                   (filter string? ns))))))))

(defn- strict-deletes
  [{:keys [schema conns strict? strict-sections] :as ctx}]
  (when strict?
    (let [desired-tx (desired-transform-names ctx)
          desired-of (fn [author]
                       (case author
                         :links      (set (map first conns))
                         :transforms desired-tx
                         (set (keys (get schema author)))))
          live-of    (fn [section]
                       (if-let [nf (rid/name-field-of section)]
                         (cur ctx section nf)
                         (or (get (:current ctx) section) {})))]
      (for [[kind author] ents/sweep-order
            :when (or (nil? strict-sections) (contains? strict-sections author))
            :let [desired (desired-of author)]
            [k spec] (swept ctx live-of author)
            :when (and (not (desired k)) (in-repository? ctx spec) (sweepable? ctx author k spec))
            e (concat (map #(->edit :transform % :deleted {:name %}) (companions ctx kind k))
                      [(->edit kind k :deleted {:name k})])]
        e))))

(defn- producing
  [{:keys [names producing current]} tx links]
  (if-not (and names producing (live/looked? tx))
    (sort-by (comp str key) tx)
    (sort-by (comp str first)
             (distinct
              (for [l links
                    v (live/keys-named names l)
                    r (distinct (producing :link-type v))
                    :let [t (get-in current [:transforms r])
                          k (or (:transform-name t) r)
                          e (live/entry-of tx k)]
                    :when e]
                [k (first e)])))))

(defn- superseded-deletes
  [{:keys [schema] :as ctx}]
  (let [desired-tx   (:transforms schema)
        backed-links (reduce (fn [m [t-kw t]]
                               (reduce (fn [m l] (update m l (fnil conj #{}) t-kw))
                                       m (diff/link-outputs-of t)))
                             {} desired-tx)]
    (for [[old-kw old-t] (when (seq backed-links)
                           (producing ctx (cur ctx :transforms :transform-name)
                                      (keys backed-links)))
          l (diff/link-outputs-of old-t)
          :let [producers (get backed-links l)]
          :when (and (seq producers)
                     (not (contains? producers old-kw))
                     (not (contains? desired-tx old-kw)))]
      (->edit :transform old-kw :deleted {:name old-kw}))))

(defn- distinct-by
  [f xs]
  (first (reduce (fn [[acc seen] x]
                   (let [k (f x)]
                     (if (seen k) [acc seen] [(conj acc x) (conj seen k)])))
                 [[] #{}] xs)))

(defn- plan-deletes [ctx]
  (distinct-by (juxt :kind :entity) (concat (strict-deletes ctx) (superseded-deletes ctx))))

(defn- materialization-columns
  [shared objs cur-objs {:keys [object-type properties]}]
  (let [want     (some-> (seq properties) set)
        attrs    (or (seq (get-in objs [object-type :attributes]))
                     (seq (:attribute-list (live/lookup cur-objs object-type))))
        derived? (fn [a]
                   (:derived (if (map? a) (:opts a) (when (sequential? a) (nth a 2 nil)))))
        bad      (when want
                   (filter want (keep #(when (derived? %) (attribute/attr-name %)) attrs)))]
    (when (seq bad)
      (refusal/refuse! :refusal.authoring/materialization-derived
                       {:object-type object-type :properties (vec bad)}
                       {:names (str/join ", " (map name bad))}))
    (vec (for [a     (remove derived? attrs)
               :let  [[nm ty] (if (map? a)
                                [(:attribute-name a) (:attribute-type a)]
                                (sugar/lower-shared-attr shared a))]
               :when (and nm (or (nil? want) (want nm)))]
           {:attribute-name nm :attribute-type ty}))))

(defn- custom-application-args
  [_ctx k spec]
  (let [known paths/api-namespaces
        want  (into #{} (map #(keyword (name %))) (get-in spec [:scope :operations]))
        bad   (sort (remove known want))]
    (when (seq bad)
      (refusal/refuse! :refusal.authoring/unknown-operations
                       {:custom-application k :unknown (vec bad)}))
    ((shape-projection :custom-applications) k spec)))

(def ^:private phases
  [{:phase :organizations
    :why "the outermost containers: a space names its organization,
          so organizations exist before the spaces that stand in them. `:grants` is not
          carried, as with projects"
    :plan (entity-phase
           {:shape :organizations :kind :organization
            :live [:organizations :organization-name]})}

   {:phase :spaces
    :why "before the projects that stand in them: a project's `:space` resolves
          name→RID at finalize"
    :plan (entity-phase
           {:shape :spaces :kind :space
            :live [:spaces :space-name]})}

   {:phase :projects
    :why "FIRST — nearly every other section may carry a `:project` ref that resolves
          name→RID at finalize, so the container has to exist before anything names
          it. It belongs here because the grammar already ACCEPTS the block and
          `project-export` writes it back: without this phase a push reported
          `:projects/<name>` green and normalization then dropped the declaration,
          leaving every resource that named it owned by nothing — a green push that
          broke main. `:grants` is not carried, and that is deliberate: it is a
          `:custom` field the exporter never writes, so the file cannot claim it and
          an install FROM the file cannot clobber it (the write half merges)"
    :plan (entity-phase
           {:shape :projects :kind :project
            :live [:projects :project-name]})}

   {:phase :repositories
    :why "right after the projects it is nested in (Project ⊃ Repository ⊃ Resource) and
          before every code-bearing section, whose `:repository` back-ref resolves
          name→RID at finalize. Same hole as `:projects` and the same reason: the
          grammar accepts the block and `project-export` writes it back, so without a
          phase the declaration was reported green and then dropped"
    :plan (entity-phase
           {:shape :repositories :kind :repository
            :live [:repositories :repository-name]})}

   {:phase :roles
    :why "the role table a security text declares: named sets of
          operations, over the standard roles; a grant names one, so they precede nothing
          and reference nothing but each other"
    :plan (entity-phase {:shape :roles :kind :role
                         :live (fn [ctx] (or (:roles (:current ctx)) {}))})}

   {:phase :markings
    :why "the markings a security text declares: category, rank and the
          groups cleared for each; resources carry them by name, so nothing waits on them"
    :plan (entity-phase {:shape :markings :kind :marking-definition
                         :live (fn [ctx] (or (:markings (:current ctx)) {}))})}

   {:phase :projections
    :why "the declared lenses over the graph — the kinds of text a projection is
          written in, which nodes it shows, what it may change: plain data that
          references nothing, so it can go early"
    :plan (entity-phase {:shape :projections :kind :projection
                         :live (fn [ctx] (or (:projections (:current ctx)) {}))})}

   {:phase :eval-suites
    :why "the tests of the targets the rest declares — a suite names a transform, function,
          agent or action by name and is checked when a run reads it, so it references
          nothing an install must order"
    :plan (entity-phase {:shape :eval-suites :kind :eval-suite
                         :live (fn [ctx] (or (:eval-suites (:current ctx)) {}))})}

   {:phase :media-sets
    :why "before every section whose attributes may name one: an object type's media
          property and a dataset's media column carry `:media-set`, and the gate
          `:media-sets-declared?` refuses a name no entry declares, so the set has to
          exist before the type that lands into it. It is its own resource and not a
          trait of the referrer because the referrer governs nothing — holding the
          dataset that points at an item does not grant its bytes; the project named here is what decides,
          and `:retention` is what lets go. Carried for the same reason `:projects` is:
          the grammar accepts the block and `project-export` writes it back, so without
          a phase a declaration reports green and is then dropped"
    :plan (entity-phase
           {:shape :media-sets :kind :media-set
            :live [:media-sets :media-set-name]})}

   {:phase :value-types
    :why "first of the ontology proper — a named base-type + reusable constraint/format metadata
    with no
          dependency of its own; attribute TYPE-slot references are lowered into
          concrete types by the parser, so this phase only persists the definition
"
    :plan (entity-phase {:shape :value-types :kind :value-type
                         :live [:value-types :value-type-name]})}

   {:phase :shared-properties
    :why "pure metadata (a name, a value type and the display metadata it lends to
          every property naming it) with no dependency of its own, and
          object-type/interface attribute lists resolve bare shared-property
          references against it (sugar/lower-shared-attr)"
    :plan (let [projected (shape-projection :shared-properties)]
            (entity-phase {:shape :shared-properties :kind :shared-property
                           :live [:shared-properties :shared-property-name]
                           :desired (fn [_ k spec]
                                      (projected k (sugar/lower-display-name spec)))}))}

   {:phase :sources
    :why "before transforms, so a transform's :datasource ref resolves name→RID"
    :plan (entity-phase {:shape :sources :kind :source
                         :live [:sources :source-name]
                         :silent-when-absent [:secret-cipher]})}

   {:phase :datasets
    :why "before transforms, so a transform's :dataset input/output ref resolves"
    :plan (entity-phase {:section :datasets :kind :dataset
                         :live [:datasets :dataset-name]
                         :desired (fn [ctx k spec]
                                    (dataset->wire-args
                                     k (if (= :materialization (:kind spec))
                                         (assoc spec :columns
                                                (materialization-columns
                                                 (:shared ctx)
                                                 (:objs ctx)
                                                 (cur ctx :object-types :object-type-name)
                                                 spec))
                                         spec)))
                         :canonical dataset->wire-args
                         :silent-when-absent [:project :ignore-inherited]})}

   {:phase :exports
    :why "after :sources and :datasets, whose RIDs an export's refs resolve to, and
          before the transforms, since the export's own build node is lifted from it.
          An export is its own resource and not a field of the dataset: it
          is configured ON THE SOURCE, with its own schedule and its own export
          control — on the dataset the direction
          out would be a property of the rows instead of something the source is asked
          to accept"
    :plan (entity-phase {:shape :exports :kind :export
                         :live [:exports :export-name]})}

   {:phase :transforms
    :why "before the backing transforms that reference them via :inputs"
    :plan (entity-phase {:section :transforms :kind :transform
                         :live [:transforms :transform-name]
                         :desired (fn [ctx k spec]
                                    (when (:version spec)
                                      (refusal/refuse! :refusal.authoring/transform-version
                                                       {:transform k :version (:version spec)}))
                                    (tx->wire-args
                                     k (->> (assoc spec :name k :sutra.code/section :transforms)
                                            (#(first ((:introspect ctx) (:code ctx) %)))
                                            (outputs/recompile-board (:current ctx)))))
                         :canonical tx->wire-args
                         :silent-nested hide-minted-outputs})}

   {:phase :functions
    :why "QUERY functions (no :source) — stored THROUGH the transform path; same
          machinery, distinct authoring name. Edit functions (a :source program)
          from the SAME `:functions` slice stage later, at :edit-functions"
    :plan plan-query-functions}

   {:phase :dataset-source-transforms
    :why "Source→Dataset syncs lifted from a DATASET's :external-source — before
          the object/link ones, since a shaping transform reads the dataset they
          land and the whole point is that the ontology sits downstream of it"
    :plan plan-dataset-source-transforms}

   {:phase :media-set-source-transforms
    :why "Source→Media-Set syncs lifted from a set's :sync, for the same reason as a dataset's"
    :plan plan-media-set-source-transforms}

   {:phase :export-transforms
    :why "the build node lifted from each declared export — after the syncs, so an
          export of a synced dataset is ordered behind the sync that fills it"
    :plan plan-export-transforms}

   {:phase :object-source-transforms
    :why "transforms lifted from an object's :external-source"
    :plan plan-object-source-transforms}

   {:phase :link-source-transforms
    :why "transforms lifted from a link's :external-source"
    :plan plan-link-source-transforms}

   {:phase :object-types
    :why "WITHOUT :implements (deferred to :object-implements, breaking the
          cyclic link-constraint dependency), and BEFORE interfaces so
          implementer tables carry every projected column before the v_<iface>
          UNION view that SELECTs them is rebuilt"
    :plan #(plan-object-pass % false)}

   {:phase :interfaces
    :why "after object-types (so a view rebuild finds every column), before
          link-types that endpoint on them"
    :plan plan-interfaces}

   {:phase :link-types
    :why "after the interfaces its endpoints may name"
    :plan plan-link-types}

   {:phase :object-sets
    :why "AFTER object-types and link-types, whose names its :root and :include
          resolve against; BEFORE actions and templates, which NAME a saved set
          (:options-from, a template parameter of type :object-set) and whose
          reference gate closes when the set is declared. The phase was missing
          entirely: the grammar took an :object-sets entry, nothing staged it,
          and the section came back empty — so every consumer of a saved set
          was inert, and prod held none"
    :plan (entity-phase {:shape :object-sets :kind :object-set
                         :live [:object-sets :object-set-name]})}

   {:phase :object-type-groups
    :why "after object-types, so each :members ref resolves name→RID"
    :plan (entity-phase {:shape :object-type-groups :kind :object-type-group
                         :live [:object-type-groups :object-type-group-name]})}

   {:phase :object-implements
    :why "re-stage object-types that declare :implements, now that every
          link-type named by their interface link-constraints exists"
    :plan #(plan-object-pass % true)}

   {:phase :edit-functions
    :why "EDIT functions (a :source program) from the `:functions` slice — after
          object-types, since :target-type and :param-types reference them"
    :plan (entity-phase {:shape :functions :kind :function
                         :live [:functions :function-name]
                         :only ents/edit-function?
                         :desired edit-function-args})}

   {:phase :actions
    :why "after the object/link types their rules reference, for the name→RID
          resolution ensure-actions does at finalize"
    :plan (entity-phase {:section :actions :kind :action
                         :publish with-revision
                         :live [:actions :action-name]
                         :canonical (shape-projection :actions)
                         :desired (fn [ctx k spec]
                                    (drop-empty
                                     (rid/rewrite-action-refs
                                      (fn [targets v]
                                        (or
                                         (rid/resolve-name->rid (:action-rid-index ctx) targets v)
                                         v))
                                      (assoc spec :name k))))
                         :silent-when-absent [:edits]})}

   {:phase :collections
    :why "BEFORE the applications that name it: an app's :collection back-ref resolves
          name→RID at finalize, so the section it points at must already exist. Same hole
          as :projects and :repositories above — without a phase the grammar accepts the
          block, project-export writes it back, and the declaration is reported green and
          then dropped"
    :plan (entity-phase
           {:shape :collections :kind :collection
            :live [:collections :collection-name]})}

   {:phase :applications
    :why "after object-types/actions, so an app's :object-type and :project
          refs resolve at finalize"
    :plan (entity-phase {:shape :applications :kind :application
                         :live [:applications :application-name]
                         :silent-when-absent [:status]})}

   {:phase :custom-applications
    :why "AFTER object-types, actions, projects and repositories: a custom
          application is nothing BUT references — its :scope names the object
          types and actions a token may reach, and :project/:repository say
          where it belongs — so every one of them must exist before name→RID
          resolution at finalize. No :publish default either, unlike an
          :application: this one is an OAuth door, and a door that opens itself
          on first install is not a door. It stays :draft until someone says
          otherwise"
    :plan (entity-phase {:shape :custom-applications :kind :custom-application
                         :live [:custom-applications :custom-application-name]
                         :desired custom-application-args
                         :silent-when-absent [:status]})}

   {:phase :workspaces
    :why "AFTER the applications, the other way round from :collections — a workspace
          names a :landing app and an :apps list, so every one of them must exist before
          name→RID resolution at finalize"
    :plan (entity-phase
           {:shape :workspaces :kind :workspace
            :live [:workspaces :workspace-name]})}

   {:phase :pipelines
    :why "after transforms AND object-types/datasets: a pipeline is a container of
          refs — :members are transforms, :imports/:outputs datasets or types — so
          every one of them must exist before name→RID resolution at finalize"
    :plan (entity-phase {:shape :pipelines :kind :pipeline
                         :live [:pipelines :pipeline-name]})}

   {:phase :templates
    :why "AFTER object-types and object-sets: a template parameter of type
          :object / :object-set NAMES the type it binds, and that ref resolves
          name→RID at finalize. Versioned like an edit Function rather than
          published like an app, because a template HAS pinned consumers — a
          document already generated from 1.2.0 must keep meaning what 1.2.0
          meant, which is exactly the immutable-release rule. It still
          self-publishes on first install: unlike an OAuth door, a template
          that nobody may generate from is not a safer template, it is an
          unusable one"
    :plan (entity-phase {:shape :templates :kind :template
                         :live [:templates :template-name]
                         :silent-when-absent [:status]})}

   {:phase :agents
    :why "DDL-less, staged like an application; after object-types/actions so
          an agent's :project ref and its tool bindings resolve at finalize"
    :plan (entity-phase {:shape :agents :kind :agent
                         :live [:agents :agent-name]})}

   {:phase :deletes
    :why "strict mode only, in REVERSE dependency order — link-types and
          transforms before the object-types they reference"
    :plan plan-deletes}])

(def staging-order
  (mapv #(select-keys % [:phase :why]) phases))

(defn- live-of
  [current names]
  (if names
    (memoize (fn [kk nk] (live/section current names kk nk #(live/whole-section current kk nk))))
    (memoize (diff/live-by-name current))))

(defn- shared-of
  [authored current names live]
  (let [sh  (live :shared-properties :shared-property-name)
        kws (delay (live/by-name (for [[k v] (live/whole sh)] [(keyword k) k v])))]
    (if names
      (live/merged (live/normalized current names :shared-properties :shared-property-name
                                    keyword sh kws)
                   authored)
      (merge @kws authored))))

(defn- plan-context
  [schema current
   {:keys [auto-ds strict? strict-sections strict-repository strict-projected code introspect
           names producing]}]
  (let [live (live-of current names)]
    {:schema  schema
     :names   names
     :producing producing
     :code    code
     :introspect introspect
     :strict-projected strict-projected
     :current current
     :auto-ds auto-ds
     :strict? strict?
     :strict-sections strict-sections
     :strict-repository strict-repository
     :objs    (:objects schema)
     :ifaces  (:interfaces schema)
     :conns   (:links schema)
     :shared  (live/as-lookup (shared-of (:shared-properties schema) current names live))
     :live    live
     :action-rid-index (sch/name-index current)}))

(defn- plan-phase
  [ctx {:keys [phase plan]}]
  (let [escaped (fn [msg data cause]
                  (ex-info (str "install plan, phase " (ident-str phase) " — " msg)
                           (-> data (dissoc ::escaped) (assoc :phase phase)) cause))]
    (try (mapv #(assoc % :phase phase) (plan ctx))
         (catch clojure.lang.ExceptionInfo e
           (throw (if (::escaped (ex-data e)) (escaped (ex-message e) (ex-data e) (ex-cause e)) e)))
         (catch InterruptedException e (throw e))
         (catch Exception e
           (throw (escaped (or (ex-message e) (.getName (class e)))
                           {:class (.getName (class e))} e))))))

(defn plan-schema
  [schema current {:keys [only] :as opts}]
  (let [ctx (plan-context schema current opts)]
    (into [] (mapcat #(plan-phase ctx %)) (cond->> phases only (filter (comp only :phase))))))

(def ontology-phases
  (conj (ents/sections-in :ontology-phase) :object-implements))

(defn plan-tally
  [plan]
  (reduce (fn [t {:keys [outcome]}] (update t outcome (fnil inc 0)))
          {:created 0 :updated 0 :skipped 0}
          plan))

(defn planned-rows
  [plan]
  (vec (for [{:keys [action kind entity]} plan
             :when action
             :let [[verb type-s] (str/split (name action) #"-" 2)]]
         {:action    (name action)
          :verb      verb
          :type      (or (some-> kind name) type-s)
          :type-name (some-> entity name)})))

(defn plan-retirements
  [schema current opts]
  (let [opts  (if (:referring opts) opts (merge opts (live/world-index current)))
        plan  (vec (plan-deletes (plan-context schema current opts)))
        going (retire/retiring current (retire/resolver current opts) plan)]
    {:retiring (mapv #(select-keys % [:kind :entity :status]) going)
     :problems (vec (retire/retire-problems current plan opts))
     :orphaned (retire/orphaning current going opts)}))

(def checks
  [retire/retire
   apps/template-search-around
   backing-findings/binding-dropped
   backing-findings/producer-removed
   backing-findings/sourceless-root
   backing-findings/virtual-dataset
   migration/identity-changes
   migration/stranded-edits
   actions/expressions
   derive-findings/derived-cycles
   sql-findings/sql-traps
   actions/rule-values
   actions/rule-targets
   actions/webhooks
   actions/source-webhooks
   source-findings/external-systems
   export-findings/exports
   security-findings/security
   loop-findings/loops
   actions/writeback-pk
   backing-findings/primary-key-dropped
   backing-findings/edit-only
   backing-findings/decimal-precision
   backing-findings/supplement-unmapped
   actions/action-log
   actions/derived-link-writes
   actions/edit-functions
   function-findings/function-contract
   actions/side-effect-edits
   apps/app-functions
   apps/app-binds-unknown-type
   apps/app-lint
   apps/app-param-bindings
   apps/app-hand-made-hops
   apps/view-in-repository])

(defn with-severity [findings]
  (map #(finding/ranked % :error) findings))

(defn diff-findings [d]
  (fnd/scan-all checks d))

(defn plan-findings
  ([schema current plan] (plan-findings schema current plan {}))
  ([schema current plan edits]
   (->> (diff/diff schema current plan edits (live/world-index current))
        diff-findings
        with-severity
        vec)))

(defn touched-names
  [plan]
  (into #{} (for [{:keys [kind entity outcome]} plan
                  :when (and entity (#{:created :updated} outcome))]
              [kind (rid/->name entity)])))

(def ^:private self-contained-rules
  (into #{} (mapcat :self) checks))

(def ^:private tag-of
  (into {} (for [e ents/entities k [(:tag e) (:section e) (:author e)] :when k] [k (:tag e)])))

(defn standing?
  [touched finding]
  (let [n    (some-> (:entity finding) rid/->name)
        kind (some-> (:entity-class finding) keyword tag-of)]
    (boolean (and (self-contained-rules (:rule finding))
                  n
                  (if kind
                    (not (touched [kind n]))
                    (not-any? #(= n (second %)) touched))))))

(defn plan-problems
  ([schema current plan] (plan-problems schema current plan {}))
  ([schema current plan edits]
   (let [touched (touched-names plan)]
     (filterv #(and (finding/blocking? %) (not (standing? touched %)))
              (plan-findings schema current plan edits)))))

(defn plan-warnings
  [schema current plan]
  (let [touched (touched-names plan)]
    (vec (remove #(and (finding/blocking? %) (not (standing? touched %)))
                 (plan-findings schema current plan)))))

(defn- say-already-staged
  [{kind :entity-class :keys [entity] s :seq}]
  (str (name kind) " " (pr-str entity) " is ALREADY staged for creation"
       " (seq " s "). A batch carrying two creates of one name cannot"
       " commit — and it blocks every author sharing it, not just this"
       " one. Commit the pending batch, or drop the stale row:"
       " `changeset drop " s "`."))

(defn already-staged-problems
  [staged plan]
  (for [{:keys [kind entity outcome]} plan
        :when (= :created outcome)
        :let [s (get staged [kind (keyword (rid/->name entity))])]
        :when s]
    (fnd/render {:already-staged say-already-staged}
                {:rule :already-staged :entity-class kind :entity entity :seq s})))

(def ^:private refusal-headlines
  [[#{:migration-required}
    "install would orphan user edits — a migration is required"]
   [#{:backing-replaced}
    #(str "install would permanently replace the storage of " (count %) " object type(s)")]
   [#{:identity-inert :identity-locked :identity-unretractable :migration-required}
    "install would change how object ids are minted"]
   [#{:unknown-expression-op}
    "install carries an expression this platform cannot evaluate"]
   [#{:object-param-into-attribute}
    "install carries an action rule that writes an object where a scalar belongs"]
   [#{:rule-target-not-object-param}
    "install carries an action rule whose target is not an object-reference parameter"]
   [#{:root-transform-sourceless}
    "install carries a root transform that names no table or query"]
   [#{:introspection-refused}
    "install carries a python transform whose module does not import"]
   [#{:introspection-unavailable}
    "install could not verify a python transform — introspection is unavailable"]
   [#{:introspection-refused :introspection-unavailable}
    "install carries a python transform that was not verified"]
   [#{:sql-refused :sql-char-without-length}
    "install carries a SQL transform that Postgres does not take as written"]
   [#{:app-function-not-listed :app-function-unknown :app-function-type-outside
      :app-function-scalar-unbound :app-function-arg-unresolved}
    "install carries an app whose Function call cannot work"]
   [#{:producer-removed}
    "install would leave ontology entities with no producer"]
   [#{:template-search-around}
    "install carries a template whose :search-around does not walk the ontology"]
   [#{:retire-protected :retire-referenced}
    "install would remove by omission what may not be removed yet"]
   [#{:already-staged}
    "install would stage a second create for a name the pending batch already creates"]])

(defn refusal-headline [probs]
  (let [ps (set (map :rule probs))]
    (or (some (fn [[rules h]] (when (every? rules ps) (if (fn? h) (h probs) h))) refusal-headlines)
        "install is refused by the checks below")))
