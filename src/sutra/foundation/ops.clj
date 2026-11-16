(ns sutra.foundation.ops
  (:refer-clojure :exclude [bound? run!])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [sutra.foundation.contracts :as c]
            [sutra.foundation.ports :as ports]
            [sutra.foundation.rpc :as rpc]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(def kinds #{:query :command :event :reaction})

(def entry-schema
  [:and
   [:map {:closed true}
    [:service :keyword]
    [:kind (into [:enum] kinds)]
    [:doc :string]
    [:domain {:optional true} :keyword]
    [:in {:optional true} :any]
    [:out {:optional true} :any]
    [:payload {:optional true} :any]
    [:key {:optional true} [:vector :keyword]]
    [:errors {:optional true} [:set :keyword]]
    [:uses {:optional true} [:set :keyword]]
    [:emits {:optional true} [:set :keyword]]
    [:on {:optional true} :keyword]
    [:consistency {:optional true} [:enum :same-commit :after-commit]]
    [:auth {:optional true} [:enum :public :user :admin]]
    [:branch? {:optional true} :boolean]
    [:many {:optional true} :boolean]
    [:egress {:optional true} :boolean]
    [:observed-only {:optional true} :boolean]
    [:emitted-by {:optional true} [:set :symbol]]
    [:partition-by {:optional true} [:vector :keyword]]
    [:shape {:optional true} [:map-of [:enum :batch :by-ref :pushdown :stream] :boolean]]
    [:http {:optional true} :map]]
   [:fn {:error/message "a :command or :reaction names its :domain"}
    #(or (not (#{:command :reaction} (:kind %))) (:domain %))]
   [:fn {:error/message "a :reaction names the event it is :on"}
    #(or (not= :reaction (:kind %)) (:on %))]])

(defn load-table
  [path]
  (let [t   (edn/read-string (slurp (io/resource path)))
        bad (into {} (keep (fn [[k v]]
                             (when-let [p (or (when-not (qualified-keyword? k) ["not a name"])
                                              (some-> (m/explain entry-schema v) me/humanize))]
                               [k p])))
                  t)]
    (when (seq bad)
      (anomaly/raise! :fault (str "malformed operations: " (pr-str bad)) {:problems bad}))
    t))

(def table (load-table "sutra/ops.edn"))

(def ^:dynamic *extra*
  {})

(defn entries
  []
  (if (empty? *extra*) table (merge table *extra*)))

(defn entry
  [op]
  (or (get *extra* op) (get table op)
      (anomaly/raise! :fault (str op " is not an operation of sutra/ops.edn") {:op op})))

(defonce ^:private bodies (atom {}))

(defn- module-of
  [ns-sym]
  (let [[root m] (str/split (str ns-sym) #"\.")]
    (when (and (= "sutra" root) m) (keyword m))))

(defn register!
  [op service ns-sym calls f]
  (let [{:keys [many] :as e} (entry op)]
    (when (and many (nil? service))
      (anomaly/raise! :fault (str op " is :many: bind a body per service, (defop [" op " :svc] …)")
                             {:op op}))
    (when (and service (not many))
      (anomaly/raise! :fault (str op " is not :many and has one body under " (:service e))
                             {:op op :service service}))
    (swap! bodies assoc (if many [op service] op) {:f f :ns ns-sym :calls calls})
    op))

(defn unregister!
  ([op] (swap! bodies dissoc op) nil)
  ([op service] (swap! bodies dissoc [op service]) nil))

(def ^:private callers #{`ask `plan})

(defn- called-ops
  [env-ns body]
  (->> (tree-seq coll? seq body)
       (keep (fn [form]
               (when (and (seq? form) (symbol? (first form)))
                 (let [v (ns-resolve env-ns (first form))]
                   (when (and (var? v) (callers (symbol v)) (keyword? (nth form 2 nil)))
                     (nth form 2))))))
       set))

(defmacro defop
  [op-or-pair args & body]
  (let [[op service] (if (vector? op-or-pair) op-or-pair [op-or-pair nil])
        fname        (symbol (str (namespace op) "-" (name op)))]
    `(register! ~op ~service '~(ns-name *ns*) '~(called-ops *ns* body)
                (fn ~fname ~args ~@body))))

(defn bound?
  [op]
  (if (:many (entry op))
    (boolean (some (fn [k] (and (vector? k) (= op (first k)))) (keys @bodies)))
    (contains? @bodies op)))

(def ^:private guard? (some? (System/getenv "SUTRA_OPS_GUARD")))

(defn- guarded? [] (or guard? (c/checking?)))

(defonce ^:private meter (atom {}))

(defonce ^:private undeclared (atom #{}))

(defn stats
  []
  @meter)

(defn- conform! [op side schema x]
  (when-not (c/valid? schema x)
    (anomaly/raise! :fault
                    (str op " " (name side) " breaks its contract: "
                         (pr-str (c/explain schema x)))
                    {:op op :side side :problem (c/explain schema x)}))
  x)

(def fresh-schema [:or [:enum :snapshot :committed] [:tuple [:= :lag] nat-int?]])

(defn- run-body [op {:keys [f]} snap in]
  (let [t0 (System/nanoTime)]
    (try
      (f (assoc snap ::caller op) in)
      (catch clojure.lang.ExceptionInfo e
        (let [d (ex-data e) code (:op/error d)]
          (cond
            (and code (not (contains? (:errors (entry op)) code)))
            (swap! undeclared conj [op code])
            (and (not code) (anomaly/client-error? d))
            (swap! undeclared conj [op (anomaly/category d)])))
        (throw e))
      (finally
        (let [dt (- (System/nanoTime) t0)]
          (swap! meter update op (fn [{:keys [calls nanos] :or {calls 0 nanos 0}}]
                                   {:calls (inc calls) :nanos (+ nanos dt)})))))))

(defn- body-of [op]
  (or (get @bodies op)
      (anomaly/raise! :fault (str "no body for " op " in this process") {:op op})))

(defn- refuse-kind! [op kind want]
  (when-not (= want kind)
    (anomaly/raise! :fault (str op " is a " kind ", not a " want) {:op op})))

(def topology-schema
  [:map {:closed true}
   [:profile :keyword]
   [:self :keyword]
   [:processes [:map-of :keyword
                [:map {:closed true}
                 [:services [:or [:= :all] [:set :keyword]]]
                 [:url {:optional true} :string]]]]])

(def solo {:profile :solo :self :main :processes {:main {:services :all}}})

(defn read-topology
  [s]
  (let [t (if (str/blank? s) solo (edn/read-string s))]
    (when-let [problem (some-> (m/explain topology-schema t) me/humanize)]
      (anomaly/raise! :fault (str "malformed topology: " (pr-str problem)) {:topology t}))
    (when-not (get-in t [:processes (:self t)])
      (anomaly/raise! :fault (str "topology names no process " (:self t) " for :self")
                      {:topology t}))
    t))

(def ^:dynamic *topology* (read-topology (System/getenv "SUTRA_OPS_TOPOLOGY")))

(defn- serves? [{:keys [services]} svc] (or (= :all services) (contains? services svc)))

(defn placement
  [topology op]
  (let [{:keys [service many]} (entry op)
        {:keys [self processes]} topology]
    (if (or many (serves? (get processes self) service))
      :local
      (let [[p {:keys [url]}] (first (filter #(serves? (val %) service)
                                             (sort-by key processes)))]
        (cond
          (nil? p)   (anomaly/raise! :fault (str "no process of the topology serves " service)
                                     {:op op :service service})
          (nil? url) (anomaly/raise! :fault
                                     (str "process " p " serves " service " but names no :url")
                                     {:op op :process p})
          :else      {:process p :url (str/replace url #"/+$" "")})))))

(defn topology-problems
  [topology]
  (let [ps (vals (:processes topology))]
    (sort (for [svc (into (sorted-set) (map (comp :service val)) (entries))
                :when (not-any? #(serves? % svc) ps)]
            svc))))

(def peer
  "operations")

(defonce ^:private peer-secret* (atom nil))

(defn set-peer-secret!
  [s]
  (reset! peer-secret* (not-empty s)))

(defn peer-secret
  []
  (or @peer-secret*
      (anomaly/raise! :fault "no operations secret: the process booted without one" {})))

(defn accepts-remote?
  [topology]
  (< 1 (count (:processes topology))))

(def remote-timeout-ms 18000)

(def signature-window-ms
  60000)

(def op-path "/api/ops")

(defn- portable
  [ctx]
  (let [store (:store ctx)]
    (cond-> {:schema? (contains? ctx :schema)}
      store (assoc :branch (:branch store) :authority (:authority store)))))

(defn signature
  [secret ts nonce text]
  (rpc/token-for secret (str ts "\n" nonce "\n" text)))

(defn sign
  [secret now-ms payload]
  (let [text  (pr-str payload)
        nonce (str (random-uuid))]
    {:ts now-ms :nonce nonce :payload text :sig (signature secret now-ms nonce text)}))

(defonce ^:private seen-nonces (atom {}))

(defn- fresh-nonce!
  [nonce now-ms]
  (let [[old _] (swap-vals! seen-nonces
                            (fn [m]
                              (assoc (into {} (filter #(< (- now-ms (val %))
                                                          (* 2 signature-window-ms)))
                                           m)
                                     nonce now-ms)))]
    (not (contains? old nonce))))

(defn open
  [secret now-ms {:keys [ts nonce payload sig]}]
  (cond
    (not (and (int? ts) (string? nonce) (string? payload) (string? sig)))
    (refusal/refuse! :refusal.foundation/op-unsigned {})

    (not (rpc/same-token? (signature secret ts nonce payload) sig))
    (refusal/refuse! :refusal.foundation/op-bad-signature {})

    (< signature-window-ms (abs (- now-ms ts)))
    (refusal/refuse! :refusal.foundation/op-outside-window {:ts ts})

    (not (fresh-nonce! nonce now-ms))
    (refusal/refuse! :refusal.foundation/op-replayed {:nonce nonce})

    :else
    (edn/read-string payload)))

(defn peer-token [] (rpc/token-for (peer-secret) peer))

(defn- remote!
  [{:keys [process url]} ctx op kind in]
  (rpc/call! {:url (str url op-path) :peer (str "operation host " (name process))
              :timeout remote-timeout-ms :token (peer-token)}
             (sign (peer-secret) (System/currentTimeMillis)
                   (assoc (portable ctx) :op op :kind kind :in in))))

(defn- call [snap op in edge?]
  (let [{:keys [kind many] in-s :in out-s :out} (entry op)
        where (placement *topology* op)]
    (refuse-kind! op kind :query)
    (when edge? (conform! op :in in-s in))
    (let [out (cond
                (map? where) (remote! where snap op :query in)
                many         (into {} (for [[k b] @bodies
                                            :when (and (vector? k) (= op (first k)))]
                                        [(second k) (run-body op b snap in)]))
                :else        (run-body op (body-of op) snap in))]
      (when (or edge? (guarded?))
        (if many
          (doseq [o (vals out)] (conform! op :out out-s o))
          (conform! op :out out-s out)))
      out)))

(defn- check-uses! [snap op]
  (let [caller (::caller snap)]
    (when (and caller (not (contains? (:uses (entry caller)) op)))
      (anomaly/raise! :fault (str caller " calls " op ", which is not in its :uses")
                             {:op caller :calls op}))
    caller))

(defn ask
  ([snap op in] (ask snap op in nil))
  ([snap op in {:keys [fresh]}]
   (check-uses! snap op)
   (when (and fresh (not (m/validate fresh-schema fresh)))
     (anomaly/raise! :fault (str "unknown freshness " (pr-str fresh)) {:op op :fresh fresh}))
   (call snap op in (guarded?))))

(defn query
  [ctx op in]
  (call (dissoc ctx ::caller) op in true))

(def ^:private tx-vectors
  [:ops :log :records :fences :retires :touched :forget-edits :adopt-edits :parents])

(defn- record-clash [a b]
  (let [by-key (into {} (map (juxt (juxt :coll :key) :value)) (:records a))]
    (first (for [{:keys [coll key value]} (:records b)
                 :let [prior (get by-key [coll key] ::none)]
                 :when (and (not= ::none prior) (not= prior value))]
             [coll key]))))

(defn merge-tx
  [& txs]
  (reduce
   (fn [a b]
     (when-let [k (record-clash a b)]
       (anomaly/raise! :fault (str "two decisions write record " (pr-str k) " differently")
                              {:record k}))
     (when (and (:schema a) (:schema b))
       (anomaly/raise! :fault "two decisions each change the schema" {}))
     (reduce (fn [m k]
               (if (contains? b k) (update m k #(into (vec %) (get b k))) m))
             (merge (apply dissoc b tx-vectors) (apply dissoc a tx-vectors)
                    (select-keys a tx-vectors))
             tx-vectors))
   {}
   (remove nil? txs)))

(defn- emitted
  [op emit]
  (vec (for [[ev payload] emit]
         (do (when-not (contains? (:emits (entry op)) ev)
               (anomaly/raise! :fault (str op " emits " ev ", which is not in its :emits")
                                      {:op op :emits ev}))
             (when (guarded?) (conform! ev :payload (:payload (entry ev)) payload))
             {:kind ev :payload payload :outbox? true}))))

(defn reactions
  [ev c]
  (sort (for [[op {:keys [kind on consistency]}] (entries)
              :when (and (= :reaction kind) (= ev on) (= c (or consistency :after-commit)))]
          op)))

(declare react)

(defn- decide [op snap in]
  (let [{out-s :out} (entry op)
        {:keys [tx out emit] :as d} (run-body op (body-of op) snap in)]
    (when-not (and (map? d) (or (contains? d :tx) (contains? d :out)))
      (anomaly/raise! :fault (str op " decided no {:tx :out :emit}") {:op op}))
    (when (and (guarded?) out-s) (conform! op :out out-s out))
    {:tx  (apply merge-tx
                 (cond-> (or tx {}) (seq emit) (update :log (fnil into []) (emitted op emit)))
                 (for [[ev payload] emit
                       r (reactions ev :same-commit)]
                   (:tx (react snap r payload))))
     :out out}))

(defn react
  [snap op payload]
  (refuse-kind! op (:kind (entry op)) :reaction)
  (decide op (assoc snap ::caller op) payload))

(defmacro emission
  [ev payload]
  (let [e (entry ev)]
    (when-not (= :event (:kind e))
      (anomaly/raise! :fault (str ev " is no :event") {:op ev}))
    (when-not (contains? (:emitted-by e) (ns-name *ns*))
      (anomaly/raise! :fault (str (ns-name *ns*) " emits " ev ", but is not in its :emitted-by")
                             {:op ev :ns (ns-name *ns*)}))
    `(let [p# ~payload]
       (when ((var guarded?)) ((var conform!) ~ev :payload (:payload (entry ~ev)) p#))
       {:kind ~ev :payload p# :outbox? true})))

(defn after-commit?
  [tx]
  (some #(and (:outbox? %) (seq (reactions (:kind %) :after-commit))) (:log tx)))

(defn plan
  [snap op in]
  (let [{:keys [kind domain]} (entry op)
        caller (check-uses! snap op)]
    (refuse-kind! op kind :command)
    (when (and caller (not= domain (:domain (entry caller))))
      (anomaly/raise! :fault (str caller " plans " op " across domains " (:domain (entry caller))
                                  " → " domain)
                             {:op caller :plans op}))
    (when (guarded?) (conform! op :in (:in (entry op)) in))
    (decide op snap in)))

(defn- writes? [tx] (some #(and (coll? %) (seq %)) (vals tx)))

(defn- run-here!
  [ctx op in]
  (let [{out-s :out} (entry op)
        commit  (ports/commit (:store ctx))
        branch  (str (or (:branch in) (:branch ctx) (:branch (:store ctx)) "main"))
        events? (volatile! false)
        out     (commit (:store ctx)
                        {:branch branch :auth (:auth ctx)}
                        (fn [snapshot]
                          (let [{:keys [tx out]} (decide op (assoc (dissoc ctx ::caller)
                                                                   :store snapshot)
                                                         in)]
                            (vreset! events? (after-commit? tx))
                            (cond-> {:result out} (writes? tx) (assoc :tx tx)))))]
    (when @events?
      (when-let [dispatch (ports/dispatch (:store ctx))]
        (dispatch (:store ctx) branch)))
    (conform! op :out out-s out)))

(defn run!
  [ctx op in]
  (let [{:keys [kind] in-s :in out-s :out} (entry op)
        where (placement *topology* op)]
    (refuse-kind! op kind :command)
    (conform! op :in in-s in)
    (if (map? where)
      (conform! op :out out-s (remote! where ctx op :command in))
      (run-here! ctx op in))))

(defn serve
  [ctx op kind in]
  (let [e (entry op)]
    (when-not (= kind (:kind e))
      (refusal/refuse! :refusal.foundation/op-wrong-kind {:op op} {:kind-is (:kind e) :kind kind}))
    (when (map? (placement *topology* op))
      (refusal/refuse! :refusal.foundation/op-not-served {:op op}))
    (case kind
      :query   (query ctx op in)
      :command (run! ctx op in))))

(defn- schema-problem [s]
  (try (m/schema s c/opts) nil
       (catch Exception e (ex-message e))))

(defn problems
  []
  (let [bs @bodies]
    (vec
     (concat
      (for [[op {:keys [kind] :as e}] (entries)
            [field need?] [[:in (not= :event kind)]
                           [:out (#{:query :command} kind)]
                           [:payload (= :event kind)]]
            :let [s (get e field)]
            :when (or (and need? (nil? s)) (and s (schema-problem s)))]
        {:rule 1 :op op :msg (if s
                               (str field " " (pr-str s) " is no schema: " (schema-problem s))
                               (str "a " kind " needs " field))})
      (for [[k {:keys [calls]}] bs
            :let [op (if (vector? k) (first k) k) uses (:uses (entry op) #{})]
            msg (concat (for [c (sort calls) :when (not (uses c))]
                          (str "asks " c ", not in :uses"))
                        (for [u (sort uses) :when (not (calls u))]
                          (str ":uses " u ", never asked")))]
        {:rule 2 :op op :msg msg})
      (for [[k {:keys [calls]}] bs
            :let [op (if (vector? k) (first k) k) d (:domain (entry op))]
            c (sort calls)
            :let [ce (get (entries) c)]
            :when (and (= :command (:kind ce)) (not= d (:domain ce)))]
        {:rule 5 :op op :msg (str "plans " c " of domain " (:domain ce) " from domain " d)})
      (for [[k {:keys [ns]}] bs
            :let [[op svc] (if (vector? k) k [k (:service (entry k))])]
            :when (not= svc (module-of ns))]
        {:rule 4 :op op :msg (str "body in " ns ", outside module " (name svc))})
      (for [[op {:keys [emits domain]}] (entries)
            ev (sort emits)
            r  (reactions ev :same-commit)
            :when (not= domain (:domain (entry r)))]
        {:rule 5 :op r :msg (str "reacts in the same commit to " ev " of " op
                                 " across domains " domain " → " (:domain (entry r)))})
      (for [[op {:keys [egress consistency]}] (entries)
            :when (and egress (= :same-commit consistency))]
        {:rule 10 :op op :msg "an :egress reaction runs :after-commit"})
      (for [[op {:keys [kind observed-only]}] (entries)
            :when (and (= :event kind) (not observed-only)
                       (empty? (concat (reactions op :same-commit)
                                       (reactions op :after-commit))))]
        {:rule 14 :op op :msg "no reaction answers it and it is not :observed-only"})
      (for [[op {:keys [kind on]}] (entries)
            :when (and (= :reaction kind) (not= :event (:kind (get (entries) on))))]
        {:rule 1 :op op :msg (str ":on " on " is no event")})
      (for [[op code] (sort-by str @undeclared)]
        {:rule 12 :op op :msg (if (namespace code)
                               (str "threw undeclared :op/error " code)
                               (str "refused (" (name code) ") without an :op/error"))})))))

(defn unbound
  []
  (sort (remove #(or (= :event (:kind (entry %))) (bound? %)) (keys (entries)))))
