(ns sutra.cli.assemble
  (:require [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [sutra.types.languages :as langs]
            [sutra.types.scaffold :as scaffold]))

(def ^:private where-clause-re #"(?U)^([\w.?+-]+)(<>|>=|<=|!=|\^=|~|@|=|>|<)(.*)$")

(def ^:private where-op->kw
  {">=" :>= "<=" :<= "=" := ">" :> "<" :<
   "!=" :not= "^=" :starts-with "~" :contains "@" :geo})

(def ^:private where-op-aliases
  {"<>" "!="})

(defn- where-val [op raw]
  (if (#{:> :< :>= :<=} op) (or (parse-long raw) (parse-double raw) raw) raw))

(defn- geo-field [s]
  (let [parts (str/split s #"\+")]
    (if (= 2 (count parts)) (mapv keyword parts) (keyword s))))

(defn- geo-filter
  [clause raw]
  (let [ps (str/split raw #"/")
        n  (fn [x] (or (parse-double x)
                       (throw (ex-info (str "bad --where clause " (pr-str clause) " — " (pr-str x)
                                            " is not a number")
                                       {:clause clause}))))]
    (case (count ps)
      3 (let [[lat lon d] ps
              [_ dist unit] (or (re-matches #"([0-9.]+)([a-z]*)" (str/lower-case d))
                                (throw
                                 (ex-info (str "bad --where clause " (pr-str clause) " — distance "
                                               (pr-str d)
                                               " is a number with an optional unit, e.g. 300km")
                                          {:clause clause})))]
          {:within-distance {:center [(n lat) (n lon)] :distance (n dist)
                             :unit (if (str/blank? unit) :km (keyword unit))}})
      4 (let [[north west south east] (map n ps)]
          {:within-bounding-box {:top-left [north west] :bottom-right [south east]}})
      (throw
       (ex-info (str "bad --where clause " (pr-str clause)
                     " — @ takes lat/lon/<distance>[unit] "
                     "(within a distance, e.g. lage@46.61/13.85/300km) or north/west/south/east "
                     "(within a box); the field is a geopoint attribute or lat+lon")
                {:clause clause})))))

(defn- parse-where-clause [s]
  (if-let [[_ lhs op-str v] (re-matches where-clause-re s)]
    (let [_  (when-let [use (where-op-aliases op-str)]
               (throw (ex-info (str "bad --where clause " (pr-str s) " — " op-str
                                    " is SQL's spelling; this filter language writes it "
                                    use " (" lhs use v ")")
                               {:clause s :op op-str :use use})))
          op (where-op->kw op-str)
          [field-of f] (if (= :geo op)
                         [geo-field (fn [] (geo-filter s v))]
                         [keyword (fn [] {op (where-val op v)})])]
      (if (str/includes? lhs ".")
        (let [[lk attr] (str/split lhs #"\." 2)] [(keyword lk) {(field-of attr) (f)}])
        [(field-of lhs) (f)]))
    (throw (ex-info (str "bad --where clause " (pr-str s)
                         " — expected attr<op>val, op one of = != < > <= >= ^= ~ @ "
                         "(e.g. status=neu, status!=neu, wo-art-id>=8, "
                         "name^=Mül for starts-with, name~müller for contains, "
                         "side.attr=x for a linked object, side = the link side's API name, "
                         "lage@46.61/13.85/300km or lat+lon@47/12/46/14 for a geo filter)")
                    {:clause s}))))

(defn parse-where
  [s]
  (let [link? (fn [v] (map? (val (first v))))]
    (reduce (fn [m [k v]]
              (if (and (link? v) (some-> (get m k) link?))
                (update m k #(merge-with merge % v))
                (assoc m k v)))
            {}
            (map parse-where-clause (remove str/blank? (str/split s #","))))))

(defn parse-show
  [s]
  (let [items (remove str/blank? (map str/trim (str/split (str s) #",")))]
    (reduce (fn [out item]
              (if-let [[_ side attr] (re-matches #"([^.]+)\.(.+)" item)]
                (let [side (keyword side) attr (keyword attr)
                      i
                      (first (keep-indexed #(when (and (map? %2) (contains? %2 side)) %1) out))]
                  (if i
                    (update-in out [i side] conj attr)
                    (conj out {side [attr]})))
                (conj out (keyword item))))
            []
            items)))

(defn parse-order
  [s]
  (let [toks (str/split (str/trim s) #"[:\s]+")
        bad! (fn [] (throw (ex-info (str "bad --order " (pr-str s) " — expected "
                                         "<column> or <column>:asc|desc "
                                         "(e.g. gesendet-am:desc)")
                                    {:order s})))]
    (cond
      (= 1 (count toks))
      (if (str/blank? (first toks)) (bad!) {:order-by (keyword (first toks)) :sort-dir :asc})

      (and (= 2 (count toks)) (not (str/blank? (first toks)))
           (#{"asc" "desc"} (str/lower-case (last toks))))
      {:order-by (keyword (first toks)) :sort-dir (keyword (str/lower-case (last toks)))}

      :else (bad!))))

(defn parse-field
  [s]
  (let [[nm ty] (str/split (str s) #":" 2)]
    (when (or (str/blank? nm) (str/blank? (str ty)))
      (throw (ex-info (str "bad --field " (pr-str s) " — expected name:type (e.g. bnr:string)")
                      {:field s})))
    [(keyword nm) (keyword ty)]))

(defn- held!
  [problem data]
  (when-let [[code params] problem]
    (refusal/refuse! code data params)))

(defn object-type
  [{:keys [name fields pk display plural symbol implements]}]
  (let [attrs    (when-not (str/blank? (str fields)) (mapv parse-field (str/split fields #",")))
        pk-field (or pk (ffirst attrs))]
    (held! (scaffold/object-type-problem {:name name :plural plural :title pk-field
                                          :properties (map first attrs)}
                                         true)
           {:name name})
    {:schema {:objects
              {name (cond-> {:attributes attrs :title pk-field :plural plural}
                      display          (assoc :name display)
                      symbol           (assoc :symbol symbol)
                      (seq implements) (assoc :implements
                                              (mapv keyword (str/split implements #","))))}}}))

(defn set-status
  [{:keys [name status reason deadline replacement]}]
  (let [dep (cond-> {}
              reason      (assoc :reason reason)
              deadline    (assoc :deadline deadline)
              replacement (assoc :replacement (keyword replacement)))]
    (cond-> {:name name :status (keyword status)}
      (seq dep) (assoc :deprecation dep))))

(defn link-type
  [{:keys [name from to cardinality display fk fk-side join from-key to-key]}]
  (let [card (or cardinality :many-to-one)
        es   (cond
               fk   {:strategy :endpoint-fk :fk-attribute fk
                     :fk-side (or fk-side (scaffold/inferred-fk-side card))}
               join {:strategy :join-table :dataset join
                     :from-key-column from-key :to-key-column to-key})
        opts (cond-> {} display (assoc :name display) es (assoc :external-source es))]
    (held! (scaffold/link-type-problem {:name name :from from :to to :cardinality card
                                        :external-source es}
                                       true)
           {:name name})
    {:schema {:links [(cond-> [name from to card] (seq opts) (conj opts))]}}))

(defn shared-property
  [{:keys [name type display]}]
  (held! (scaffold/shared-property-problem {:type type}) {:name name})
  {:schema {:shared-properties {name (cond-> {:type type} display (assoc :name display))}}})

(defn- value-type-constraints [{:keys [one-of regex format min max]}]
  (cond-> {}
    one-of (assoc :one-of (str/split one-of #","))
    regex  (assoc :regex regex)
    format (assoc :format (keyword format))
    min    (assoc :min (or (parse-long min) (parse-double min) min))
    max    (assoc :max (or (parse-long max) (parse-double max) max))))

(defn value-type
  [{:keys [name type display] :as a}]
  (when (nil? type)
    (throw (ex-info "value-type create needs --type <base-type> (e.g. string)" {:name name})))
  (let [cs (value-type-constraints a)]
    {:schema {:value-types {name (cond-> {:type type}
                                   (seq cs) (assoc :constraints cs)
                                   display  (assoc :name display))}}}))

(defn interface
  [{:keys [name fields extends display]}]
  (when (str/blank? (str fields))
    (throw (ex-info "interface create needs at least one --field name:type" {:name name})))
  {:schema {:interfaces
            {name (cond-> {:property-list (mapv parse-field (str/split fields #","))}
                    extends (assoc :extends (mapv keyword (str/split extends #",")))
                    display (assoc :display-name display))}}})

(defn- parse-column
  [s]
  (let [[nm ty] (str/split (str s) #":" 2)]
    (when (or (str/blank? nm) (str/blank? (str ty)))
      (throw (ex-info (str "bad --column " (pr-str s) " — expected name:type (e.g. att_id:string)")
                      {:column s})))
    {:attribute-name (keyword nm) :attribute-type (keyword ty)}))

(defn dataset
  [{:keys [name columns kind display description]}]
  (when (str/blank? (str columns))
    (throw (ex-info "dataset create needs at least one --column name:type" {:name name})))
  {:schema {:datasets
            {name (cond-> {:columns (mapv parse-column (str/split columns #","))
                           :kind    (or kind :standalone)}
                    display     (assoc :title display)
                    description (assoc :description description))}}})

(def ^:private py-stub
  "def compute(ctx):\n    # scaffold — fill in via `transform edit`\n    return []\n")
(def ^:private sql-stub "SELECT * FROM input")
(def ^:private clj-stub
  ";; scaffold — fill in via `transform edit`\n(fn [{:keys [input]}]\n  (for [row input]\n    {:id (:id row)}))\n")

(defn- parse-transform-input
  [s]
  (let [parts (str/split (str s) #":")
        [label kind ref table] parts]
    (when (or (str/blank? label)
              (and (not= "scalar" kind) (or (< (count parts) 3) (str/blank? (str ref)))))
      (throw
       (ex-info (str "bad --input " (pr-str s)
                     " — expected label:kind:ref (kind = object-type|transform|dataset|source)"
                     " or label:scalar[:type] for a value the caller passes")
                {:input s})))
    [(keyword label)
     (case kind
       "scalar"      (cond-> {:kind :scalar}
                       (not (str/blank? (str ref))) (assoc :type (keyword ref)))
       "object-type" {:kind :object-type :object-type (keyword ref)}
       "transform" {:kind :transform :transform (keyword ref)}
       "dataset"   {:kind :dataset   :dataset   (keyword ref)}
       "source"    (do (when (str/blank? (str table))
                         (throw
                          (ex-info (str "--input " (pr-str s)
                                        " — a source input needs label:source:<datasource>:<table>")
                                   {:input s})))
                       {:kind :datasource :datasource (keyword ref) :table table})
       (throw (ex-info (str "unknown --input kind " (pr-str kind)
                            " in " (pr-str s)
                            " — use object-type|transform|dataset|source|scalar")
                       {:input s})))]))

(defn- parse-llm-input
  [s]
  (let [[label source model] (str/split (str s) #":" 3)]
    (when (or (str/blank? label) (str/blank? (str source)) (str/blank? (str model)))
      (throw (ex-info (str "bad --llm " (pr-str s)
                           " — expected label:source:model (e.g. llm:xai:grok-4)")
                      {:llm s})))
    [(keyword label) {:kind :language-model :source (keyword source) :model model}]))

(defn- compute-transform-spec
  ([who args] (compute-transform-spec who args {}))
  ([who {:keys [language inputs llm code query name
                output-type output-dataset output-link from-key to-key
                code-file query-file columns pk incremental semantic-version]}
    {:keys [lifecycle] :or {lifecycle :transform}}]
   (let [lang  (or language :python)
         code  (or code  (when code-file  (slurp code-file)))
         query (or query (when query-file (slurp query-file)))
         std  (into {} (map parse-transform-input)
                    (remove str/blank? (str/split (str inputs) #",")))
         llms (into {} (map parse-llm-input)
                    (remove str/blank? (str/split (str llm) #",")))
         all  (merge std llms)
         _    (held! (scaffold/transform-problem
                      {:lifecycle lifecycle :language lang :inputs all
                       :outputs (cond-> []
                                  output-type    (conj :object-type)
                                  output-dataset (conj :dataset)
                                  output-link    (conj :link-type))
                       :primary-key pk :from-key from-key :to-key to-key})
                     {:name name :verb who :lifecycle lifecycle})
         egress (into [] (comp (map val)
                               (keep (fn [{:keys [kind] :as in}]
                                       (case kind
                                         :datasource     (:datasource in)
                                         :language-model (:source in)
                                         nil)))
                               (distinct))
                      all)
         out  (cond
                (= :function lifecycle) nil
                output-type    {:kind :object-type :object-type output-type}
                output-dataset {:kind :dataset :dataset output-dataset}
                output-link    {:kind            :link-type
                                :link-type       output-link
                                :from-key-column from-key
                                :to-key-column   to-key})
         body (case (langs/body-field lang)
                :code  {:code (or code ({:python py-stub :clojure clj-stub} lang))}
                :query {:query (or query sql-stub)})
         cols (when (seq (str columns)) (mapv parse-field (str/split columns #",")))]
     (cond-> (merge {:language lang :inputs all} body)
       out                          (assoc :output out)
       (seq egress)                 (assoc :external-systems egress)
       (and output-type pk)         (assoc :primary-key pk)
       (and output-type (seq cols)) (assoc :output-columns cols)
       incremental                  (assoc :incremental true)
       semantic-version             (assoc :semantic-version semantic-version)))))

(defn transform
  [{:keys [name] :as args}]
  {:schema {:transforms {name (compute-transform-spec "transform create" args)}}})

(defn function
  [{:keys [name kind target-type language source params] :as args}]
  (let [k (or kind :query)]
    (case k
      :query {:schema {:functions {name (compute-transform-spec "function create" args
                                                                {:lifecycle :function})}}}
      :edit  (do
               (held! (scaffold/edit-function-problem {:target-type target-type})
                      {:name name})
               {:schema {:functions
                         {name (scaffold/edit-function
                                {:target-type target-type :language language :source source
                                 :param-types (when (seq (str params))
                                                (into {} (map parse-field)
                                                      (str/split params #",")))})}}})
      (throw (ex-info "function create --kind must be query or edit" {:kind k})))))

(defn describe [a]
  (assoc (dissoc a :sections) :sections (mapv keyword (:sections a))))

(defn query [a]
  (when-not (or (:find a) (:object-set a))
    (throw (ex-info "query needs a <type> or --object-set <name|rid>"
                    {:args a})))
  (when (and (:freeze? a) (not (:save? a)))
    (throw
     (ex-info "--freeze materializes a saved handle — pair it with --save"
              {:args a})))
  (cond-> {:count? true}
    (:find a)     (assoc :find (:find a))
    (:object-set a) (assoc :object-set (:object-set a))
    (:show a)     (assoc :show (parse-show (:show a)))
    (:order-by a) (merge (parse-order (:order-by a)))
    (:count-only? a) (assoc :count-only? true :limit (or (:limit a) 1))
    (:latest a)   (assoc :latest (:latest a)
                         :limit (or (:limit a) (:latest a)))
    (:limit a)    (assoc :limit (:limit a))
    (:offset a)   (assoc :offset (:offset a))
    (:around a)   (assoc :around (mapv keyword (str/split (:around a) #",")))
    (:explain? a) (assoc :explain? true :count? false)
    (:save? a)    (assoc :save? true :count? false)
    (:freeze? a)  (assoc :freeze? true)
    (:engine a)   (assoc :engine (:engine a))
    (:accuracy a) (assoc :accuracy (:accuracy a))
    (:where a)    (assoc :where (parse-where (:where a)))))

(defn series [a]
  (when-not (:property a)
    (throw (ex-info (str "series needs --property <name> — the "
                         "time-series property to read "
                         "(`describe <type>` lists them)")
                    {:args a})))
  (when (and (:rollup a) (not (:bucket a)))
    (throw (ex-info (str "--rollup folds points into BUCKETS — add "
                         "--bucket week|month|… or drop it")
                    {:args a})))
  (cond-> {:find (:find a) :property (:property a)}
    (:from a)   (assoc :from (:from a))
    (:until a)  (assoc :until (:until a))
    (:bucket a) (assoc :bucket (:bucket a))
    (:rollup a) (assoc :rollup (:rollup a))
    (:limit a)  (assoc :limit (:limit a))
    (:where a)  (assoc :where (parse-where (:where a)))))

(defn force-snapshot [a]
  (cond-> {:name (:dataset a)} (:message a) (assoc :message (:message a))))

(defn- node-names
  [s]
  (mapv #(keyword (str/replace % "_" "-")) (str/split s #",")))

(defn upload [a]
  (cond-> {:file (:file a) :dataset (:dataset a)}
    (:media-column a) (assoc :media-column (:media-column a))
    (:set a)   (assoc :set (into {} (for [kv  (str/split (:set a) #",")
                                          :let [[k v] (str/split kv #"=" 2)]
                                          :when v]
                                      [(keyword k) v])))
    (:on-upload a) (assoc :on-upload (:on-upload a))
    (:arg a)   (assoc :args (into {} (for [kv  (str/split (:arg a) #",")
                                           :let [[k v] (str/split kv #"=" 2)]
                                           :when v]
                                       [(keyword k) v])))
    (:build a) (assoc :build (node-names (:build a)))))

(defn run-import [a]
  (cond-> {}
    (:only a)     (assoc :only (node-names (:only a)))
    (:from a)     (assoc :from (node-names (:from a)))
    (:strategy a) (assoc :strategy (:strategy a))
    (:trace? a)   (assoc :trace true)
    (:profile? a) (assoc :profile true)
    (:abort-on-failure? a) (assoc :abort-on-failure true)
    (:retry-count a)
    (assoc :retry-count (or (parse-long (:retry-count a)) (:retry-count a)))
    (:retry-backoff a) (assoc :retry-backoff (:retry-backoff a))
    (:full? a)    (assoc :full true)
    (:force? a)   (assoc :force true)
    (:detach? a)  (assoc :detach true)
    (:wait? a)    (assoc :wait true)
    (:again? a)   (assoc :again true)))

(defn build-estimate [a]
  (cond-> {}
    (:only a)     (assoc :only (node-names (:only a)))
    (:from a)     (assoc :from (node-names (:from a)))
    (:strategy a) (assoc :strategy (:strategy a))
    (:days a)     (assoc :days (:days a))
    (:calls-per-row a)
    (assoc :calls-per-row (parse-double (str (:calls-per-row a))))))

(defn diff-schema [a] (cond-> a (empty? (:presets a)) (dissoc :presets)))

(defn register [a]
  {:users [(cond-> {:username (:username a)}
             (:external a)       (assoc :external (:external a)
                                        :auth-source
                                        (or (:auth-source a) :spnego))
             (not (:external a)) (assoc :password (or (:password a) :auto))
             (:user-id a)        (assoc :user-id (:user-id a)))]})
