(ns sutra.ref.install
  (:require [sutra.kit.coll :as coll]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.ref.err :refer [fail!]]
            [sutra.ref.sides :as sides]
            [sutra.ref.sigma :as s]
            [sutra.ref.value :as v]
            [sutra.types.entities :as ents]
            [sutra.types.kinds :as kinds]
            [sutra.types.refs :as refs]
            [sutra.types.table :as tbl]))

(set! *warn-on-reflection* true)

(def rid-kinds (set (keys ents/section->tag)))
(def name-field (into {} (map (juxt identity ents/name-field)) ents/rid-sections))
(def tag-of ents/section->tag)
(def refs-of ents/refs)
(def fields-of ents/fields)
(def gates (-> "gates.edn" io/resource slurp edn/read-string))
(def class-by-tag (coll/index-by :tag ents/mutable))

(defn kw [x] (cond (keyword? x) x (nil? x) nil (symbol? x) (keyword x) :else (keyword (str x))))

(def ^:private rid-re #"ri\.sutra\.main\.([a-z-]+)\.([0-9A-Za-z_-]+)")
(defn rid? [x] (boolean (and (string? x) (re-matches rid-re x))))
(defn rid-kind [r] (some-> (re-matches rid-re (str r)) second keyword))
(defn loc [r] (some-> (re-matches rid-re (str r)) (nth 2)))
(defn rid [tag locator] (str "ri.sutra.main." (name tag) "." locator))
(defn well-formed-for? [sec x] (and (rid? x) (= (tag-of sec) (rid-kind x))))
(defn mint [cx sec] (rid (tag-of sec) (s/call cx :fresh-uuid)))
(defn dsrid [r] (rid :dataset (loc r)))
(defn- uuid3 [^String s] (str (java.util.UUID/nameUUIDFromBytes (.getBytes s "UTF-8"))))

(defn- slug [x]
  (when (some? x)
    (let [t (-> (name x) str/lower-case str/trim
                (str/replace #"[^a-z0-9]+" "-") (str/replace #"^-+|-+$" ""))]
      (keyword (if (= t "") "graph" t)))))

(defn- present? [rule v]
  (case rule
    :seq (boolean (seq v)) :truthy (boolean v) :map (map? v) :number (number? v)
    (some? v)))

(defn- carry [rule v]
  (case rule
    :kw (keyword v) :vec (vec v) :kws (mapv keyword v)
    :kw-map (when (map? v) (into {} (map (fn [[k x]] [(kw k) (kw x)])) v))
    :bool (boolean v) :str (str v) :long (long v) :sorted-strs (vec (sort (map str v)))
    v))

(defn canonical
  [sec body base]
  (reduce-kv (fn [acc f {:keys [as present coerce in custom author-only default]}]
               (let [v (let [raw (get body f)] (if (some? raw) raw default))]
                 (if (and (not custom) (not author-only) (present? present v))
                   (assoc acc (or as f) (cond->> v true (carry coerce) in (carry in)))
                   acc)))
             base (get fields-of sec)))

(defn- attr-row [x]
  (select-keys (cond (map? x) x
                     (keyword? x) {:attribute-name x}
                     (sequential? x) (let [[n t opts] x]
                                       (cond-> {:attribute-name n :attribute-type t}
                                         (map? opts) (assoc :opts opts))))
               [:attribute-name :attribute-type :opts]))

(defn- attrs [xs] (mapv attr-row xs))

(defn- some-kws [m es ks]
  (reduce (fn [m k] (if-let [x (get es k)] (assoc m k (kw x)) m)) m ks))

(defn- ext-source [es]
  (when (and (map? es) (seq es))
    (let [strategy (some-> (:strategy es) kw)]
      (if (= :endpoint-fk strategy)
        (some-kws {:strategy :endpoint-fk :fk-side (kw (:fk-side es))
                   :fk-attribute (kw (:fk-attribute es))}
                  es [:on-null :on-unresolved :on-duplicate])
        (cond-> (some-kws {:datasource (some-> (:datasource es) kw)} es
                          [:dataset :primary-key :from-key-column :to-key-column])
          strategy               (assoc :strategy strategy)
          (seq (:table es))      (assoc :table (:table es))
          (seq (:query es))      (assoc :query (:query es))
          (seq (:column-map es)) (assoc :column-map (carry :kw-map (:column-map es)))
          (seq (:transforms es)) (assoc :transforms (vec (:transforms es))))))))

(defn- require-link-ext! [k es]
  (case (or (:strategy es) :join-table)
    :join-table (let [{:keys [datasource dataset table query from-key-column
                              to-key-column]} es]
                  (when-not (and (or dataset (and datasource (or (seq table) (seq query))))
                                 from-key-column to-key-column)
                    (fail! :incomplete-external-source
                           (str "link-type " k " is missing a complete :external-source mapping. "
                                "Required for :join-table: :dataset, or :datasource + (:table or :query), "
                                "+ :from-key-column + :to-key-column.")
                           {:link-type k :external-source es})))
    :endpoint-fk (let [{:keys [fk-side fk-attribute]} es]
                   (when-not (and (#{:from :to} fk-side) (keyword? fk-attribute))
                     (fail! :incomplete-external-source
                            (str "link-type " k " is missing a complete :external-source mapping. "
                                 "Required for :endpoint-fk: :fk-side (:from|:to) + :fk-attribute.")
                            {:link-type k :external-source es}))
                   (doseq [k' [:on-null :on-unresolved :on-duplicate]
                           :let [v (get es k')] :when (and (some? v) (not= :skip v))]
                     (fail! :fk-on-option
                            (str "link-type " k " :external-source " k' " is " (pr-str v)
                                 ". An FK link has no moment at which it could fail: it is the "
                                 "join of"
                                 " its endpoints, so a row whose FK is null or matches nothing "
                                 "simply"
                                 " does not link. Only :skip is still accepted, and it is a no-op.")
                            {:link-type k :key k' :value v})))
    (fail! :unknown-strategy (str "link-type " k " has unknown :external-source :strategy "
                                  (pr-str (:strategy es)))
           {:link-type k :external-source es})))

(def compute-languages #{:sql :pg-sql :sqlite-sql :mssql-sql :python :clojure})

(def ^:private placed-elsewhere
  #{:language :inputs :project :repository :pipeline :ignore-inherited})

(defn- transform-field [f {:keys [present coerce custom compute-only]} compute? x]
  (when (and (not (placed-elsewhere f))
             (or compute? (not compute-only))
             (if custom (if (coll? x) (seq x) x) (present? present x)))
    (if (= :output-columns f) (attrs x) (carry coerce x))))

(defn- transform-spec [k body]
  (let [lang (some-> (:language body) kw) compute? (contains? compute-languages lang)]
    (reduce-kv (fn [m f row]
                 (let [v (transform-field f row compute? (get body f))]
                   (cond-> m (some? v) (assoc f v))))
               (cond-> {:transform-name k} compute? (assoc :language lang :inputs (:inputs body)))
               (fields-of :transforms))))

(defn spec
  [cx tag k body]
  (let [sec (:section (class-by-tag tag))]
    (case tag
      :object-type (let [es (ext-source (:external-source body))]
                     (canonical sec body
                                (cond-> {:object-type-name k
                                         :attribute-list (attrs (:attributes body))}
                                  (some? (:implements body))
                                  (assoc :implements (mapv kw (:implements body)))
                                  (some? es) (assoc :external-source es))))
      :link-type (let [es  (ext-source (:external-source body))
                       sat (when-let [x (:satisfies body)]
                             (mapv (fn [{:keys [interface constraint]}]
                                     {:interface (kw interface) :constraint (kw constraint)})
                                   (if (sequential? x) x [x])))]
                   (when es (require-link-ext! k es))
                   (canonical sec (dissoc body :satisfies)
                              (cond-> {:link-type-name k} es (assoc :external-source es)
                                      sat (assoc :satisfies sat))))
      :interface (canonical sec body
                            {:interface-name k
                             :extends (mapv kw (or (:extends body) []))
                             :property-list (attrs (or (:property-list body) (:properties body) []))
                             :link-constraints
                             (mapv (fn [{:keys [link-name to-object-type to-interface cardinality
                                                required] :as lc}]
                                     (cond-> {:link-name (kw link-name)
                                              :cardinality (sides/constraint-card cardinality)}
                                       to-object-type (assoc :to-object-type (kw to-object-type))
                                       to-interface (assoc :to-interface (kw to-interface))
                                       (true? required) (assoc :required true)
                                       (not (or to-object-type
                                                to-interface))
                                       (merge (select-keys lc [:to :to-iface]))))
                                   (or (:link-constraints body) (:links body) []))})
      :project (canonical sec body
                          (cond-> {:project-name k}
                            (some? (:grants body))
                            (assoc :grants
                                   (mapv #(cond-> % (some? (:principal %)) (update :principal str))
                                         (:grants body)))))
      :dataset (let [es (ext-source (:external-source body))
                     stream (s/call cx :stream (:stream body))]
                 (cond-> (canonical sec body {:dataset-name k})
                   (some? es) (assoc :external-source es)
                   (some? stream) (assoc :stream stream)))
      :transform (let [v (transform-spec k body)]
                   (s/call cx :transform-contract v)
                   v)
      (canonical sec body (if (contains? rid-kinds sec) {(name-field sec) k} {})))))

(defn- key-of [tag x]
  (case tag :llm-model (str x) :graph (slug x) (kw x)))

(defn with-project [step body]
  (if-not (map? (:value step))
    step
    (reduce (fn [acc k]
              (let [v (get body k)]
                (cond (= :none v) (assoc-in acc [:value k] nil)
                      (some? v)   (assoc-in acc [:value k] v)
                      :else       acc)))
            (cond-> step
              (some? (:ignore-inherited body))
              (assoc-in [:value :ignore-inherited] (when (:ignore-inherited body) true)))
            [:project :repository :pipeline])))

(defn build
  [cx verb tag body]
  (let [{sec :section marks :step-marks} (class-by-tag tag)
        enrich (fn [b] (if (= :transform tag) (s/call cx :introspect b) b))]
    (with-meta
      (case verb
        :create (let [b (cond-> body
                          (= :graph tag) (update :title #(or % (some-> (:name body) name))))
                      b (enrich b)
                      k (key-of tag (:name b))]
                  (with-project {:op :set :path [sec k] :value (spec cx tag k b)} b))
        :update (let [b (enrich body)
                      k (key-of tag (:name body))]
                  (with-project (into {:op :set :path [sec k] :value (spec cx tag k b)}
                                      (map (fn [m] [m (boolean (get body m))]))
                                      marks)
                    b))
        :delete {:op :dissoc :path [sec (key-of tag (:name body))]})
      {:verb verb})))

(defn- scope-of
  [W e]
  (when-let [p (:project e)]
    (let [by (fn [sec nf x] (when x (if (contains? (get W sec) x) x
                                        (some (fn [[r v]]
                                                (when (= (name x) (some-> (get v nf) name)) r))
                                              (get W sec)))))
          pr (by :projects :project-name p)
          sp (get-in W [:projects pr :space])]
      (or (by :spaces :space-name sp) :space/default))))

(defn name-index
  [W]
  (let [plain (into {} (for [sec rid-kinds :let [es (get W sec)] :when es]
                         [sec
                          (into {}
                                (for [[_ e] es :let [n (get e (name-field sec))] :when n]
                                  [n (:id e)]))]))]
    (assoc plain
           ::scoped (into {} (for [sec rid-kinds :let [es (get W sec)] :when es]
                               [sec
                                (into {} (for [[_ e] es :let [n (get e (name-field sec))] :when n]
                                           [[n (scope-of W e)] (:id e)]))]))
           ::count  (into {} (for [sec rid-kinds :let [es (get W sec)] :when es]
                               [sec (frequencies (keep #(get % (name-field sec)) (vals es)))])))))

(defn ref-of
  ([idx targets x] (ref-of idx targets x nil))
  ([idx targets x scope]
   (cond (rid? x) x
         (nil? x) nil
         :else
         (let [k (if (string? x) (keyword x) x)]
           (some (fn [t]
                   (let [n (get-in idx [::count t k] 0)]
                     (cond
                       (<= n 1) (get-in idx [t k])
                       :else (or (when scope (get-in idx [::scoped t [k scope]]))
                                 (get-in idx [::scoped t [k nil]])
                                 (throw
                                  (ex-info
                                   (str "the name " k
                                        " is given in several spaces — name the space's project")
                                   {:name k :section t}))))))
                 targets)))))

(defn own-entry
  [W idx sec ident value]
  (let [scope (scope-of W value)
        found (ref-of idx #{sec} ident scope)
        other (some->> found (vector sec) (get-in W) (scope-of W))]
    (when-not (and scope other (not= scope other) (seq (:spaces W))) found)))

(defn- resolver-of
  ([idx] (resolver-of idx nil))
  ([idx scope] (fn [t x] (or (ref-of idx t x scope) x))))

(defn- resolver [W] (resolver-of (name-index W)))

(defn rewrite [sec m resolve]
  (refs/rewrite-refs (get refs-of sec) m resolve))

(defn resolve-step [cx W {:keys [op path value] :as step}]
  (let [[sec ident] path]
    (if-not (contains? rid-kinds sec)
      step
      (let [idx      (name-index W)
            existing (own-entry W idx sec ident value)
            resolve  (resolver-of idx (scope-of W value))]
        (case op
          :set    (let [r (or existing (mint cx sec))]
                    (assoc step :path [sec r] :value (rewrite sec (assoc value :id r) resolve)))
          :dissoc (assoc step :path [sec (or existing ident)]))))))

(def ^:private mode-keys [:language :code :query :inputs :input :source :ops :strategy :sources])

(def ^:private output-forms
  {:output [:output :primary-key :column-map :from-key-column :to-key-column :fk-attribute]
   :outputs [:outputs]
   :output-columns [:output-columns :primary-key]})

(defn- mode-flip? [{:keys [value]}]
  (boolean (or (:language value) (:input value) (:source value) (:strategy value))))

(def clear-table
  (into
   [{:section :sources :clears [:secret-ref] :restatable? false}
    {:section :actions :when #(contains? (:value %) :rules) :clears [:edits]}
    {:section :transforms :when mode-flip? :clears mode-keys}
    {:section :transforms :clears [:primary-key]
     :when #(and (mode-flip? %) (= :dataset (get-in (:merged %) [:output :kind])))}
    {:section :applications :when #(contains? (:value %) :layout) :clears [:source :object-type]}]
   (concat
    (for [[section ks] ents/whole-declaration]
      {:section section :when #(:whole-declaration? (:step %)) :clears ks})
    (for [[form ks] output-forms]
      {:section :transforms :when #(contains? (:value %) form)
       :clears (remove (set ks) (mapcat val (dissoc output-forms form)))}))))

(defn merge-existing [W {:keys [op path value] :as step}]
  (if-let [old (and (= op :set) value (get-in W path))]
    (let [merged (merge old value)
          c {:section (first path) :value value :merged merged :step step}]
      (assoc step :value
             (reduce (fn [m {:keys [section clears restatable?] pred :when :or {restatable? true}}]
                       (if (and (= section (:section c)) (or (nil? pred) (pred c)))
                         (reduce (fn [m k]
                                   (if (and restatable? (contains? value k)) m (dissoc m k))) m
                                 clears)
                         m))
                     merged clear-table)))
    step))

(defn drop-empty-implements [{:keys [path] :as step}]
  (if (and (= :object-types (first path)) (= [] (get-in step [:value :implements])))
    (update step :value dissoc :implements)
    step))

(defn- reach [succ seeds]
  (loop [order [] seen #{} todo (vec seeds) i 0]
    (if (< i (count todo))
      (let [x (nth todo i)]
        (if (contains? seen x)
          (recur order seen todo (inc i))
          (recur (conj order x) (conj seen x) (into todo (succ x)) (inc i))))
      order)))

(defn- dedup-by [f xs]
  (first (reduce (fn [[acc seen] x]
                   (let [k (f x)] (if (contains? seen k) [acc seen] [(conj acc x) (conj seen k)])))
                 [[] #{}] xs)))

(defn ancestors-of
  [W]
  (let [ext (update-vals (:interfaces W) :extends)]
    (into {}
          (for [[k _] ext]
            [k (into #{} (filter #(contains? ext %)) (reach #(get ext %) (get ext k)))]))))

(defn effective-interface [W anc i]
  (let [ifs (:interfaces W) chain (cons i (get anc i))]
    (assoc (get ifs i)
           :property-list (dedup-by :attribute-name (mapcat #(:property-list (get ifs %)) chain))
           :link-constraints (dedup-by :link-name
                                       (mapcat #(or (:link-constraints (get ifs %)) []) chain)))))

(defn effective-implements [W anc ot]
  (let [known (:interfaces W)]
    (into [] (comp (filter #(contains? known %)) (mapcat #(cons % (get anc %))) (distinct))
          (or (:implements ot) []))))

(defn contribute-props
  ([W impls attr-list] (contribute-props W (ancestors-of W) impls attr-list))
  ([W anc impls attr-list]
   (let [own (into #{} (map :attribute-name) attr-list)]
     (into (vec attr-list)
           (dedup-by :attribute-name (for [i (effective-implements W anc {:implements impls})
                                           p (:property-list (effective-interface W anc i))
                                           :when (not (own (:attribute-name p)))]
                                       p))))))

(defn- contribute-step [W {:keys [op path value] :as step}]
  (if (and (= op :set) (= :object-types (first path)) (seq (:implements value)))
    (assoc-in step [:value :attribute-list]
              (contribute-props W (:implements value) (:attribute-list value)))
    step))

(defn prepare [cx W step]
  (->> (resolve-step cx W step) (merge-existing W) drop-empty-implements (contribute-step W)))

(defn authorize-step!
  [cx op sec r]
  (doseq [id [r nil]]
    (s/call cx :authorize op (when (or sec id) {:type sec :id id}))))

(defn eval-step [cx W {:keys [op path value]}]
  (let [[sec r] path]
    (authorize-step! cx op sec r)
    (if (= op :set) (assoc-in W path value) (v/dissoc-in W path))))

(defn prune [W-pre W {:keys [op path]}]
  (let [[sec r] path]
    (if (not= op :dissoc)
      W
      (cond-> W
        (= sec :object-types)
        (update :link-types
                #(into {}
                       (remove (fn [[_ lt]]
                                 (or (= r (:from-object-type lt)) (= r (:to-object-type lt))))) %))
        (ents/in-role? :carries-datasets sec)
        (update :datasets
                #(apply dissoc %
                        (into #{(dsrid r)} (keep :dataset)
                              (get-in W-pre [:transforms r :outputs]))))))))

(def ^:private folds
  {"ä" "ae" "ö" "oe" "ü" "ue" "Ä" "Ae" "Ö" "Oe" "Ü" "Ue" "ß" "ss" "ø" "oe" "Ø" "Oe"})

(defn fold
  [a]
  (let [t (name a)]
    (keyword (if (re-find #"[äöüÄÖÜßøØ?!-]" t)
               (-> (reduce-kv str/replace t folds) (str/replace #"[?!]" "") (str/replace "-" "_"))
               t))))

(defn sql-name [k]
  (if-let [l (and (string? k) (loc k))] (str/replace l "-" "_") (name (fold k))))

(defn live [cx W]
  (let [d #(str "d_" (sql-name %))
        bulk (set (map d (concat (keys (:transforms W)) (keys (:datasets W)))))
        fk? (partial s/call cx :fk-link?)]
    {:bulk bulk
     :datasets (into bulk (map d) (keys (:object-types W)))
     :links (into #{} (comp (remove #(fk? W %)) (map #(str "c_" (sql-name %))))
                  (keys (:link-types W)))}))

(defn drops [cx W-pre W']
  (let [o (live cx W-pre) n (live cx W')
        gone (fn [k] (remove (get n k) (get o k)))
        gone-bulk (set (gone :bulk))]
    (into (vec (for [g (keys (:graphs W-pre)) :when (not (contains? (:graphs W') g))]
                 {:op :sweep-graph :graph (or (get-in W-pre [:graphs g :id]) g)}))
          (map (fn [t] {:op :drop-table :table t}))
          (concat gone-bulk (gone :links) (remove gone-bulk (gone :datasets))))))

(defn- installed-rids
  [installed sec]
  (reduce (fn [idx [r ie]]
            (let [n (get ie (name-field sec))]
              (if (not (well-formed-for? sec r))
                idx
                (cond-> (update idx ::scoped assoc [n (scope-of installed ie)] r)
                  (not (contains? idx n)) (assoc n r)
                  true (update-in [::count n] (fnil inc 0))))))
          {} (get installed sec)))

(defn legacy-rid
  [sec nm]
  (rid (tag-of sec) (uuid3 (str (name (tag-of sec)) "/" (name nm)))))

(defn- legacy-names
  [installed sec]
  (into #{} (comp (map key) (filter keyword?)) (get installed sec)))

(defn rekey-legacy
  [W]
  (reduce (fn [W sec]
            (let [es (get W sec)]
              (if-not (some keyword? (keys es))
                W
                (assoc W sec
                       (into {} (for [[k e] es]
                                  (if (keyword? k)
                                    (let [r (legacy-rid sec k)]
                                      [r (cond-> (assoc e :id r)
                                           (nil? (get e (name-field sec)))
                                           (assoc (name-field sec) k))])
                                    [k e])))))))
          W rid-kinds))

(defn ensure-rids
  [cx W installed]
  (reduce
   (fn [W sec]
     (if-let [es (get W sec)]
       (let [known  (installed-rids installed sec)
             legacy (legacy-names installed sec)]
         (assoc W sec (reduce (fn [m [k e]]
                                (let [nm (or (get e (name-field sec)) (when (keyword? k) k))
                                      mine (get-in known [::scoped [nm (scope-of W e)]])
                                      r  (cond (well-formed-for? sec (:id e)) (:id e)
                                               (well-formed-for? sec k)      k
                                               mine                          mine
                                               (contains? known nm)
                                               (let [scope (scope-of W e)]
                                                 (cond
                                                   (not (or (seq (:spaces W))
                                                            (seq (:spaces installed))))
                                                   (get known nm)
                                                   (nil? scope)
                                                   (if (= 1 (get-in known [::count nm]))
                                                     (get known nm)
                                                     (throw
                                                      (ex-info
                                                       (str "the name " nm
                                                            " is given in several spaces — "
                                                            "name the project of the one you mean")
                                                       {:name nm :section sec})))
                                                   :else (mint cx sec)))
                                               (contains? legacy nm)  (legacy-rid sec nm)
                                               :else                         (mint cx sec))]
                                  (assoc m r (cond-> (assoc e :id r)
                                               (and (keyword? k) (nil? (get e (name-field sec))))
                                               (assoc (name-field sec) k)))))
                              {} (sort-by (fn [[k _]] (not (well-formed-for? sec k))) es))))
       W))
   W rid-kinds))

(defn- put-dataset [W r nm kind cols & [extra]]
  (update-in W [:datasets r]
             #(merge (assoc extra :id r :dataset-name nm :kind kind :columns (vec cols)) %)))

(defn- named-outputs
  [cx [W known] t]
  (reduce (fn [[m known] {:keys [kind dataset]}]
            (if (and (= :dataset kind) dataset (not (rid? dataset)) (not (known (name dataset))))
              [(put-dataset m (mint cx :datasets) (keyword (name dataset))
                            :transform-output [] (when-let [p (:project t)] {:project p}))
               (conj known (name dataset))]
              [m known]))
          [W known] (concat (some-> (:output t) vector) (:outputs t))))

(defn- output-name
  [W d nm]
  (if (some (fn [[r ds]] (and (not= r d) (= (some-> (:dataset-name ds) name) (name nm))))
            (:datasets W))
    (keyword (str (name nm) "-output"))
    nm))

(defn- transform-outputs [W tr t]
  (let [back (fn [m paths d nm cols]
               (put-dataset (reduce #(assoc-in %1 %2 d) m paths) d (output-name m d nm)
                            :transform-output cols
                            (when-let [p (:project t)] {:project p})))
        nm   (:transform-name t)]
    (cond
      (not (rid? tr)) W
      (:output-columns t) (cond-> W (not (:dataset t))
                                  (back [[:transforms tr :dataset]] (dsrid tr) nm
                                        (:output-columns t)))
      (:output t) (cond-> W (not (get-in t [:output :dataset]))
                          (back [[:transforms tr :output :dataset] [:transforms tr :dataset]]
                                (dsrid tr) nm []))
      (seq (:outputs t))
      (let [n (count (:outputs t))]
        (reduce (fn [m i]
                  (let [out (get-in m [:transforms tr :outputs i])]
                    (if (:dataset out)
                      m
                      (back m
                            (cond-> [[:transforms tr :outputs i :dataset]]
                              (zero? i) (conj [:transforms tr :dataset]))
                            (if (= 1 n) (dsrid tr)
                                (rid :dataset (uuid3 (str (loc tr) "/" (name (:name out))))))
                            (:name out) []))))
                W (range n)))
      :else W)))

(defn ensure-datasets [cx W]
  (let [known (into #{} (keep #(some-> (:dataset-name %) name)) (vals (:datasets W)))
        W (first (reduce (partial named-outputs cx) [W known] (vals (:transforms W))))
        W (reduce-kv (fn [m r ot]
                       (if (and (rid? r) (nil? (:dataset ot)))
                         (-> (assoc-in m [:object-types r :dataset] (dsrid r))
                             (put-dataset (dsrid r) (:object-type-name ot) :object-backing
                                          (:attribute-list ot [])
                                          (when-let [p (:project ot)] {:project p})))
                         m))
                     W (:object-types W))]
    (reduce-kv transform-outputs W (:transforms W))))

(defn refs->rids [W]
  (let [idx (name-index W)]
    (reduce (fn [W' sec]
              (cond-> W' (get W' sec)
                      (update sec
                              (fn [es]
                                (reduce-kv (fn [m r e]
                                             (assoc m
                                                    r (rewrite sec e
                                                               (resolver-of idx (scope-of W e)))))
                                           {} es)))))
            W (keys refs-of))))

(defn ensure-actions [W]
  (let [resolve (resolver W)]
    (update W :actions
            (fn [as]
              (update-vals (or as {})
                           (fn [a]
                             (refs/rewrite-refs ents/action-refs a resolve)))))))

(defn derived
  ([cx W] (derived cx W nil))
  ([cx W installed] (->> (ensure-rids cx W installed) (ensure-datasets cx) refs->rids)))

(defn mint-names
  [cx W old]
  (reduce (fn [W sec]
            (let [es    (get W sec)
                  named (into {} (remove (fn [[k _]] (well-formed-for? sec k))) es)]
              (if (empty? named)
                W
                (let [kept (apply dissoc es (keys named))]
                  (assoc W
                         sec (merge kept
                                    (get (ensure-rids cx {sec named} (update old sec merge kept))
                                         sec)))))))
          W rid-kinds))

(defn- has-external-source? [W]
  (some (fn [sec] (some :external-source (vals (get W sec))))
        [:object-types :link-types :datasets]))

(defn normalise
  [cx W installed]
  (let [W (if (or (contains? W :objects) (contains? W :links))
            (s/call cx :unpack W)
            W)
        W (if (has-external-source? W)
            (s/call cx :migrate-external W)
            (reduce (fn [W k] (update W k #(or % {}))) W
                    [:transforms :object-types :link-types :datasets]))
        W (ensure-actions (derived cx W installed))]
    (update W :datasets #(merge % (s/call cx :system-datasets)))))

(defn- named [sec k e] (or (get e (name-field sec)) k))

(defn- duplicate-names [W]
  (for [sec rid-kinds
        :let [nf (name-field sec)
              key-of (if (= sec :link-types)
                       (fn [e]
                         [(get e nf) (:from-object-type e) (:to-object-type e) (scope-of W e)])
                       (fn [e] [(get e nf) (scope-of W e)]))]
        [k es] (group-by key-of (filter #(get % nf) (vals (get W sec))))
        :when (> (count es) 1)]
    {:entity-class sec :entity (if (vector? k) (first k) k)
     :rids (vec (sort-by str (map :id es)))}))

(defn- shared-property-value-types [W]
  (let [by-name (into {} (map (fn [[k vt]] [(or (:value-type-name vt) k) vt])) (:value-types W))]
    (for [[k sp] (:shared-properties W)
          :let [ref (some-> (:value-type sp) kw)]
          :when ref
          :let [vt (get by-name ref)]
          :when (or (nil? vt) (not= (kw (:type vt)) (kw (:type sp))))]
      (cond-> {:entity-class :shared-properties :entity (or (:shared-property-name sp) k)
               :value-type ref :type (:type sp)}
        vt (assoc :base (kw (:type vt)))))))

(defn- title-keys [W anc]
  (for [[k ot] (:object-types W)
        :let [tk (:title-key ot)]
        :when tk
        :let [available (into #{} (map :attribute-name)
                              (contribute-props W anc (:implements ot) (:attribute-list ot)))]
        :when (not (contains? available tk))]
    {:entity-class :object-types :entity (named :object-types k ot)
     :title-key tk :available (vec (sort available))}))

(defn- columns [W anc]
  (for [[k ot] (:object-types W)
        :let [names (distinct (keep :attribute-name
                                    (contribute-props
                                     W anc (:implements ot) (:attribute-list ot))))]
        [col group] (group-by fold names)
        :let [problem (cond (> (count group) 1) :fold-collision
                            (not (re-matches #"[A-Za-z0-9_]+" (name col))) :illegal-column)]
        :when problem]
    {:entity-class :object-types :entity (named :object-types k ot)
     :problem problem :column col :attributes (vec (sort group))}))

(defn- implements-known [W]
  (for [[k ot] (:object-types W) i (or (:implements ot) [])
        :when (not (contains? (:interfaces W) i))]
    {:entity-class :object-types :entity k :interface i}))

(defn- property-superset [W anc]
  (for [[k ot] (:object-types W)
        :let [types (into {} (map (juxt :attribute-name :attribute-type))
                          (map attr-row (:attribute-list ot)))]
        i (effective-implements W anc ot)
        {:keys [attribute-name attribute-type]} (:property-list (get (:interfaces W) i))
        :let [got (get types attribute-name ::missing)]
        :when (not= got attribute-type)]
    (cond-> {:entity-class :object-types :entity k :interface i
             :attribute attribute-name :want attribute-type}
      (not= got ::missing) (assoc :got got))))

(defn- interfaces-agree [W anc member per-iface]
  (for [[k ot] (:object-types W)
        :let [mine (effective-implements W anc ot)]
        :when (>= (count mine) 2)
        [n entries] (group-by first
                              (mapcat (fn [i]
                                        (for [[n x] (per-iface (get (:interfaces W) i))] [n [i x]]))
                                      mine))
        :let [by-iface (into {} (map second) entries)]
        :when (> (count (set (vals by-iface))) 1)]
    {:entity-class :object-types :entity k member n :by-interface by-iface}))

(defn- extends-known [W]
  (for [[k i] (:interfaces W) p (or (:extends i) []) :when (not (contains? (:interfaces W) p))]
    {:entity-class :interfaces :entity k :parent p}))

(defn- extends-acyclic [W anc]
  (for [k (keys (:interfaces W)) :when (contains? (get anc k) k)]
    {:entity-class :interfaces :entity k}))

(defn- type-outputs-stripped [t]
  (let [strip #(cond-> % (kinds/is? :typed-output? (:kind %)) (dissoc :object-type :link-type))]
    (cond-> t
      (map? (:output t))         (update :output strip)
      (sequential? (:outputs t)) (update :outputs #(mapv strip %)))))

(defn ref-hits
  [W]
  (for [[in ent [targets v]] (concat
                              (for [sec rid-kinds
                                    [r e] (get W sec)
                                    :let [e (cond-> e (= :transforms sec) type-outputs-stripped)]
                                    spec (get refs-of sec)
                                    rv (refs/ref-values [spec] e)]
                                [sec r rv])
                              (for [[k a] (:actions W)
                                    rv    (refs/ref-values ents/action-refs a)]
                                [:actions k rv]))
        :when (rid? v)]
    {:in in :entity ent :ref v :targets targets}))

(defn- dangling-refs
  [W]
  (let [live? (fn [targets v] (some #(contains? (get W %) v) targets))]
    (vec (remove #(live? (:targets %) (:ref %)) (ref-hits W)))))

(defn- unregistered-models
  [W]
  (let [reg (into #{} (map #(if (keyword? %) (subs (str %) 1) (str %))) (keys (:llm-models W)))]
    (vec (for [[_ t] (:transforms W)
               [_ in] (:inputs t)
               :when (= :language-model (:kind in))
               :when (not (contains? reg (str (:model in))))]
           {:entity-class :transforms :entity (:transform-name t) :model (:model in)}))))

(defn- outputs-of [t]
  (cond (seq (:outputs t)) (vec (:outputs t))
        (some? (:output t)) [(:output t)]
        :else []))

(defn- inputs-of [t]
  (cond (map? (:inputs t)) (vec (vals (:inputs t)))
        (seq (:inputs t))  (vec (:inputs t))
        (:input t)         [(:input t)]
        :else              []))

(defn kahn
  [deps]
  (loop [remaining deps levels []]
    (let [ready (into [] (filter #(empty? (get remaining %))) (keys remaining))]
      (cond
        (empty? remaining) {:levels levels}
        (empty? ready)     {:levels levels :cycle (set (keys remaining))}
        :else (recur (into {} (map (fn [[k d]] [k (apply disj d ready)]))
                           (apply dissoc remaining ready))
                     (conj levels ready))))))

(defn- writes?
  [t kind k]
  (some #(and (= kind (:kind %)) (= k (get % kind))
              (or (not= :object-type kind) (not= :supplement (:role %))))
        (outputs-of t)))

(def ^:private unresolved-input
  (tbl/per-key "install gate 8 (unresolved-input)" kinds/kinds
               {:transform   (fn [W _ in] (not (contains? (:transforms W) (:transform in))))
                :object-type (fn [W t in] (or (not (contains? (:object-types W) (:object-type in)))
                                              (writes? t :object-type (:object-type in))))
                :dataset     (fn [_ t in] (writes? t :dataset (:dataset in)))
                #{:link-type :media-set :media-set-items :media :language-model :datasource
                  :export :scalar}
                nil}))

(defn- compute-graph
  [W]
  (let [ts  (or (:transforms W) {})
        deps (update-vals ts
                          (fn [t]
                            (into #{} (comp (filter #(= :transform (:kind %))) (map :transform)
                                            (filter #(contains? ts %)))
                                  (vals (:inputs t)))))]
    (concat
     (for [[r t] ts
           in (inputs-of t)
           :when (tbl/on unresolved-input (:kind in) W t in)]
       {:entity-class :transforms :entity (named :transforms r t) :input in})
     (when-let [c (:cycle (kahn deps))]
       [{:entity-class :transforms :entity (vec (sort (map #(named :transforms % (get ts %)) c)))
         :cycle? true}]))))

(defn- satisfies [lt] (when-let [s (:satisfies lt)] (if (sequential? s) s [s])))

(defn- link-constraints
  [W anc]
  (let [named (into {} (map (fn [[_ lt]] [(:link-type-name lt) lt])) (:link-types W))]
    (for [[k ot] (:object-types W)
          i (effective-implements W anc ot)
          {:keys [link-name to-object-type to-interface cardinality
                  required]} (or (:link-constraints (get (:interfaces W) i)) [])
          :when (true? required)
          :when (empty? (sides/constraint-sides W k i link-name))
          :let [by-name (get named link-name)
                any?    (or (some? by-name)
                            (some (fn [[_ lt]]
                                    (some #(and (= i (:interface %))
                                                (= link-name (kw (:constraint %))))
                                          (satisfies lt)))
                                  (:link-types W)))]]
      {:object-type k :interface i :link-name link-name :missing? (not any?)
       :reason (if any? :unsatisfied-constraint :no-such-link-type)
       :expected {:from k :to (or to-object-type to-interface)
                  :cardinality (sides/constraint-card cardinality)}
       :actual by-name})))

(defn- ignoring-anc [f] (fn [W _] (f W)))

(def ^:private gate-impls
  {:names-unique?                 (ignoring-anc duplicate-names)
   :no-dangling-refs?             (ignoring-anc dangling-refs)
   :models-registered?            (ignoring-anc unregistered-models)
   :compute-graph-valid?          (ignoring-anc compute-graph)
   :link-constraints-met?         link-constraints
   :shared-property-value-types?  (ignoring-anc shared-property-value-types)
   :title-keys-valid?             title-keys
   :columns-valid?                columns
   :implements-known?             (ignoring-anc implements-known)
   :property-superset?            property-superset
   :interface-properties-agree?
   #(interfaces-agree %1 %2 :attribute
                      (fn [i]
                        (for [p (:property-list i)] [(:attribute-name p) (:attribute-type p)])))
   :interface-links-agree?
   #(interfaces-agree %1 %2 :link-name
                      (fn [i] (for [c (or (:link-constraints i) [])]
                                [(:link-name c)
                                 {:to (or (:to-object-type c) (:to-interface c))
                                  :cardinality (sides/constraint-card (:cardinality c))}])))
   :extends-known?                (ignoring-anc extends-known)
   :extends-acyclic?              extends-acyclic})

(def stated-gates
  (update-vals gate-impls (fn [f] #(f % (ancestors-of %)))))

(defn- problems [cx id W anc]
  (cond
    (= id :lint-gate-clean?) (s/call cx :lint W)
    (contains? gate-impls id) ((gate-impls id) W @anc)
    :else (if-let [f (get (s/value cx :gate-problems) id)] (f W) [])))

(defn- lint-key [f] [(:rule f) (str (:entity f))])

(defn- evaluate
  [cx W]
  (let [W         (s/call cx :index W)
        anc       (delay (ancestors-of W))
        structure (delay (s/call cx :structure W))]
    (into {::structure     structure
           ::structure-ok? (delay (if (s/provides? cx :structure-ok?)
                                    (s/call cx :structure-ok? W)
                                    (empty? @structure)))}
          (for [{:keys [id]} gates] [id (delay (vec (problems cx id W anc)))]))))

(defn- inherited-lint
  [cx old]
  (delay (when old (set (map lint-key (s/call cx :lint (s/call cx :index old)))))))

(defn- valid? [ev skip-lint?]
  (and @(::structure-ok? ev)
       (every? #(empty? @(ev (:id %)))
               (cond->> gates skip-lint? (remove #(= :lint-gate-clean? (:id %)))))))

(defn- accepted? [ev inherited]
  (or (valid? ev false)
      (boolean (and @inherited (valid? ev true)
                    (every? @inherited (map lint-key @(ev :lint-gate-clean?)))))))

(defn- report-of [ev inherited]
  (let [inherited (or @inherited #{})]
    (vec (concat (for [f @(::structure ev)] (assoc f :rule :schema/structure :severity :error))
                 (for [{:keys [id rule]} gates
                       p (try @(ev id) (catch RuntimeException _ []))
                       :let [f (assoc (if (= id :lint-gate-clean?) p (assoc p :rule rule))
                                      :gate id)]
                       :when (not (and (= id :lint-gate-clean?)
                                       (contains? inherited (lint-key f))))]
                   (assoc f :severity (:severity f :error)))))))

(defn accept? [cx W old]
  (accepted? (evaluate cx W) (inherited-lint cx old)))

(defn report
  [cx W old]
  (report-of (evaluate cx W) (inherited-lint cx old)))

(defn- refuse! [fs]
  (let [errors (filterv #(= :error (:severity %)) fs)]
    (cond
      (seq errors) (fail! :plan-problems
                          (str "Invalid schema map: " (count errors) " problem(s)")
                          {:sutra/gate :plan-problems
                           :problems errors})
      (seq fs)     (fail! :invalid-schema "Invalid schema map" {:reasons fs})
      :else        nil)))

(defn- normalised-old [cx o]
  (some->> (not-empty o) (#(normalise cx % nil))))

(defn finalize
  [cx W' installed]
  (let [N  (normalise cx W' (normalised-old cx (or installed (s/call cx :persisted))))
        ev (evaluate cx N)]
    (if @(::structure-ok? ev)
      N
      (refuse! (vec (for [f @(::structure ev)]
                      (assoc f :rule :schema/structure :severity :error)))))))

(defn judge
  [cx N old]
  (let [old       (normalised-old cx old)
        ev        (evaluate cx N)
        inherited (inherited-lint cx old)]
    (if (accepted? ev inherited)
      N
      (refuse! (report-of ev inherited)))))

(def ^:private protected-phases
  #{:active :endorsed})

(defn- check-edit-conflict!
  ([W k] (check-edit-conflict! W k nil))
  ([W k edit-conflict]
   (let [ot (some->> (ref-of (name-index W) #{:object-types} k) (vector :object-types)
                     (get-in W))]
     (check-edit-conflict! W k ot (or edit-conflict (:edit-conflict ot)))))
  ([_ k ot {:keys [strategy timestamp-attr]}]
   (when (= :apply-most-recent (some-> strategy keyword))
     (let [a (some #(when (= (some-> timestamp-attr name) (some-> (:attribute-name %) name)) %)
                   (:attribute-list ot))]
       (when-not (= :timestamp (some-> (:attribute-type a) keyword))
         (fail! :edit-conflict-timestamp
                (str "Object type " (pr-str k) ": :apply-most-recent needs :timestamp-attr naming"
                     " one of its :timestamp attributes"
                     (when timestamp-attr (str ", not " (pr-str timestamp-attr))))
                {:name k :timestamp-attr timestamp-attr}))))))

(defn apply-step
  [cx W step]
  (let [verb     (:verb (meta step))
        [sec k]  (:path step)
        existing (when (#{:create :delete} verb)
                   (some->> (own-entry W (name-index W) sec k (:value step))
                            (vector sec) (get-in W)))]
    (when (and (= :create verb) existing)
      (if (= :transforms sec)
        (fail! :transform-exists (str "Cannot create transform " (pr-str k))
               {:name k :existing existing})
        (fail! :name-taken (str "Cannot create " (name sec) " " (pr-str k)
                                ": the name is taken — update it instead")
               {:name k :section sec})))
    (when (and (= :delete verb) (contains? protected-phases (:status existing)))
      (fail! :protected-retire (str "Cannot delete " (name sec) " " (pr-str k) ": it is "
                                    (:status existing) " — set its status to :deprecated first")
             {:name k :section sec :status (:status existing)})))
  (let [s  (prepare cx W step)
        W1 (prune W (eval-step cx W s) s)]
    (case (:tag (ents/by-section (first (:path step))))
      :object-type (check-edit-conflict! W1 (second (:path s)))
      :transform   (let [t (get-in W1 (:path s))]
                     (doseq [o (or (seq (:outputs t)) (some-> (:output t) vector))
                             :when (and (:edit-conflict o) (:object-type o))]
                       (check-edit-conflict! W1 (:object-type o) (:edit-conflict o))))
      nil)
    {:world W1 :ddl (drops cx W W1) :step s}))

(defn install
  [run-state cx verb tag body]
  (let [W    (rekey-legacy (:world run-state))
        step (build cx verb tag body)
        {W1 :world ddl :ddl s :step} (apply-step cx W step)
        W1   (s/call cx :place W W1)
        [sec r] (:path s)
        W2   (finalize cx W1 (some-> (:installed run-state) rekey-legacy))]
    (when (= tag :transform)
      (when-let [fs (seq (s/call cx :sql-gate W W2 (second (:path step))))]
        (fail! :sql-gate "SQL gate" {:sutra/gate :sql :problems (vec fs)})))
    (-> run-state
        (assoc :world W2 :installed W2)
        (update :ddl (fnil into []) ddl)
        (v/emit {:op (keyword (str (name (:op s)) "-schema")) :target {:type sec :id r}}))))
