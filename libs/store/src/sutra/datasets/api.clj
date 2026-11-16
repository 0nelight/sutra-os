(ns sutra.datasets.api
  (:refer-clojure :exclude [read count])
  (:require [sutra.datasets.landing :as landing]
            [sutra.kit.anomaly :as anomaly]
            [sutra.port :as port]
            [sutra.store.values :as values]
            [sutra.types.value-types :as vt]))

(set! *warn-on-reflection* true)

(def addr-schema [:map [:branch :string] [:dataset some?] [:scratch? {:optional true} :boolean]
                  [:pk {:optional true} :string] [:at {:optional true} int?]
                  [:rid {:optional true} some?] [:ids {:optional true} [:sequential some?]]
                  [:control {:optional true} [:map {:closed true} [:model [:enum :event :state]]
                                              [:key {:optional true} [:sequential keyword?]]]]])

(def meta-schema [:map-of :keyword any?])

(def column-schema
  [:map [:attribute-name [:or :keyword :string]]
   [:attribute-type [:fn {:error/message "a storable value type"} vt/storable?]]])

(def decls-schema
  [:map-of :string [:sequential column-schema]])

(def layout-schema
  [:map {:closed true} [:old decls-schema] [:new decls-schema]
   [:drop-unclaimed? {:optional true} :boolean]
   [:head {:optional true} fn?]])

(def profile-schema
  [:map {:closed true}
   [:query {:optional true} [:map {:closed true} [:dialects [:set :keyword]]]]
   [:history-kinds? {:optional true} :boolean]
   [:landing {:optional true} [:set (into [:enum] landing/capabilities)]]])

(def write-schema
  [:multi {:dispatch :kind}
   [:snapshot
    [:map [:kind [:= :snapshot]] [:pk :string] [:rows [:sequential map?]]
     [:whole-row? {:optional true} :boolean]
     [:meta {:optional true} meta-schema]
     [:columns {:optional true} [:sequential column-schema]]]]
   [:update
    [:map [:kind [:= :update]] [:pk :string] [:rows [:sequential map?]]
     [:meta {:optional true} meta-schema]]]
   [:delete   [:map [:kind [:= :delete]] [:pk :string] [:meta {:optional true} meta-schema]
               [:ids {:optional true} [:sequential some?]] [:all? {:optional true} :boolean]]]])

(def read-schema
  [:map {:closed true}
   [:at {:optional true} int?]
   [:ids {:optional true} [:sequential some?]]
   [:columns {:optional true} [:sequential :string]]
   [:limit {:optional true} nat-int?]])

(def shape-schema
  [:map {:closed true} [:columns [:sequential column-schema]]])

(defn shape?
  [in]
  (not (contains? in :dataset)))

(def plan-schema
  [:map [:inputs [:map-of :string [:or addr-schema shape-schema]]] [:sql :string]
   [:params {:optional true} [:sequential any?]] [:explain? {:optional true} :boolean]])

(defprotocol Datasets
  (-profile [d])
  (-layout! [d branch change])
  (-list [d branch opts])
  (-inspect [d branch decls])
  (-write! [d addr tx])
  (-read [d addr opts])
  (-columns [d addr])
  (-count [d addr opts])
  (-version [d addr])
  (-delta [d addr lo hi])
  (-horizon [d addr])
  (-retire-below! [d addr v])
  (-sweep! [d addr opts])
  (-expire! [d addr through])
  (-history [d addr opts])
  (-query [d plan])
  (-restore! [d addr v])
  (-fork! [d from to])
  (-drop! [d branch])
  (-drop-dataset! [d addr]))

(defn- check! [schema x what] (port/asked! :datasets schema x what))

(def ^:private rows-answer [:sequential map?])
(def ^:private version-answer [:map [:version int?]])

(defn- answered [what schema x] (port/answered! :datasets schema x what))

(def ^:private inspect-answer
  [:map [:missing [:sequential :string]]
   [:orphans [:sequential [:map [:name :string] [:rows nat-int?]]]]])

(defn profile [d] (answered :profile profile-schema (-profile d)))
(defn layout! [d branch change]
  (-layout! d (check! :string branch :branch) (check! layout-schema change :layout)))
(defn inspect [d branch decls]
  (answered :inspect inspect-answer
            (-inspect d (check! :string branch :branch) (check! decls-schema decls :decls))))
(defn list-datasets ([d branch] (list-datasets d branch {}))
  ([d branch opts] (answered :list [:sequential string?] (-list d branch opts))))
(defn- malformed! [addr tx row col p]
  (let [pk (or (:pk tx) "id")
        id (when row (or (get row pk) (get row (keyword pk))))]
    (anomaly/raise! :incorrect
                    (str "dataset " (:dataset addr) (when col (str ", column " (name col)))
                         (when row (str ", row " pk "=" (pr-str id))) ": " (values/explain p)
                         (when-let [by (get-in tx [:meta :producer])] (str " — written by " by))
                         "; no store keeps it unchanged")
                    (cond-> {:store/refusal :malformed :dataset (:dataset addr)
                             :value-class (:class p)}
                      (:rid addr) (assoc :rid (:rid addr))
                      col (assoc :column (name col))
                      row (assoc :pk id)
                      (get-in tx
                              [:meta
                               :producer]) (assoc :producer (get-in tx [:meta :producer]))))))

(defn- held
  [addr tx]
  (doseq [{n :attribute-name} (:columns tx)]
    (when-let [p (values/ident-problem n)] (malformed! addr tx nil n p)))
  (if-not (:rows tx)
    tx
    (let [named (volatile! #{})
          pk    (or (:pk tx) "id")]
      (update tx :rows
              (fn [rows]
                (mapv (fn [row]
                        (reduce-kv
                         (fn [m col v]
                           (when-not (contains? @named col)
                             (when-let [p (values/ident-problem col)]
                               (malformed! addr tx row col p))
                             (vswap! named conj col))
                           (let [h (values/dataset-value v)]
                             (when-let [p (if (= pk (name col)) (values/key-problem h)
                                              (values/problem h))]
                               (malformed! addr tx row col p))
                             (if (identical? h v) m (assoc m col h))))
                         row row))
                      rows))))))

(defn- land
  [d addr tx]
  (let [o (landing/offer (-profile d))]
    (if (or (= :delete (:kind tx)) (empty? (landing/residual o)))
      (-write! d addr tx)
      (let [before  (when (landing/reads-before? o tx) (-read d addr {}))
            exists? (or (seq before) (seq (-history d addr {:limit 1})))
            p       (landing/pushed tx o before exists?)]
        (if-let [pushed (:tx p)]
          (-write! d addr pushed)
          {:version (-version d addr) :rows (:rows p)})))))

(defn write! [d addr tx]
  (let [addr (check! addr-schema addr :address)
        tx   (held addr (check! write-schema tx :write))]
    (answered :write version-answer (land d addr tx))))
(defn- answerable!
  [d addr opts]
  (let [at (or (:at opts) (:at addr))]
    (when (and (some? at) (< at (answered :horizon int? (-horizon d (dissoc addr :at)))))
      (anomaly/raise! :incorrect
                      "below the horizon"
                      {:store/refusal :as-of-unsupported :version at})))
  addr)

(defn read ([d addr] (read d addr {}))
  ([d addr opts]
   (answered :read rows-answer
             (-read d (answerable! d (check! addr-schema addr :address) opts)
                    (check! read-schema opts :read)))))
(defn columns [d addr]
  (answered :columns [:map-of :string any?] (-columns d (check! addr-schema addr :address))))
(defn count ([d addr] (count d addr {}))
  ([d addr opts] (answered :count nat-int?
                           (-count d (answerable! d (check! addr-schema addr :address) opts)
                                   opts))))
(defn version [d addr] (answered :version int? (-version d (check! addr-schema addr :address))))
(defn delta [d addr lo hi]
  (let [addr (check! addr-schema addr :address)]
    (when (< (check! int? lo :lo) (answered :horizon int? (-horizon d (dissoc addr :at))))
      (anomaly/raise! :incorrect
                      "below the horizon"
                      {:store/refusal :as-of-unsupported :version lo}))
    (answered :delta [:map [:added set?] [:removed set?] [:changed set?] [:replaced? :boolean]]
              (-delta d addr lo (check! int? hi :hi)))))
(defn horizon [d addr] (answered :horizon int? (-horizon d (check! addr-schema addr :address))))
(def ^:private history-opts
  [:map {:closed true} [:before {:optional true} int?] [:limit {:optional true} nat-int?]
   [:typed? {:optional true} :boolean]])
(def ^:private history-answer [:sequential [:map [:version int?]]])
(defn history ([d addr] (history d addr {}))
  ([d addr opts] (answered :history history-answer
                           (-history d (check! addr-schema addr :address)
                                     (check! history-opts opts :history)))))
(defn retire-below! [d addr v]
  (answered :retire [:map [:horizon int?]]
            (-retire-below! d (check! addr-schema addr :address) (check! int? v :version))))
(defn sweep!
  ([d addr] (sweep! d addr {}))
  ([d addr opts]
   (answered :sweep [:map [:swept int?] [:rows nat-int?]]
             (-sweep! d (check! addr-schema addr :address)
                      (check! [:map {:closed true} [:grace-ms {:optional true} nat-int?]]
                              opts :sweep)))))
(defn drop-dataset! [d addr] (-drop-dataset! d (check! addr-schema addr :address)))
(defn query
  [d plan]
  (check! plan-schema plan :plan)
  (when-not (:query (-profile d))
    (throw (ex-info "this datasets driver has no transform-language engine"
                    {:datasets/unsupported :query})))
  (when-let [[alias addr] (first (filter (comp some? :at val) (:inputs plan)))]
    (anomaly/raise! :incorrect
                    "a query reads its inputs at their head"
                    {:store/refusal :as-of-unsupported :input alias :at (:at addr)}))
  (answered :query rows-answer (-query d plan)))
(defn restore! [d addr v]
  (let [addr (check! addr-schema addr :address)
        v    (check! int? v :version)]
    (answerable! d addr {:at v})
    (answered :restore version-answer (-restore! d addr v))))
(defn expire! [d addr through]
  (answered :expire [:map [:version int?] [:rows nat-int?]]
            (-expire! d (check! addr-schema addr :address) (check! int? through :version))))
(defn fork! [d from to] (-fork! d (check! :string from :from) (check! :string to :to)))
(defn drop! [d branch] (-drop! d (check! :string branch :branch)))
