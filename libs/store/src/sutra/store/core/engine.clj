(ns sutra.store.core.engine
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [sutra.kit.anomaly :as anomaly]
            [malli.core :as m]
            [sutra.store.api :as api]
            [sutra.store.lib :as lib]
            [sutra.store.plan :as plan]
            [sutra.store.scan :as scan]
            [sutra.store.semantics :as sem]
            [sutra.store.shapes :as shapes]
            [sutra.store.spi :as spi]
            [sutra.store.values :as values]
            [sutra.store.core :as-alias core]))

(set! *warn-on-reflection* true)

(defn engine-failure
  [op ^Throwable e]
  (let [d (ex-data e)]
    (cond
      (:store/refusal d) e
      (anomaly/client-error? d) (api/refusal :malformed (ex-message e) (assoc d :store/engine-op op)
                                             e)
      :else (api/refusal :unavailable (str "store engine failed in " (name op) ": " (ex-message e))
                         {:store/engine-op op :cause-data d} e))))

(defmacro ^:private eng
  [op & body]
  `(try (api/tally! ~op (do ~@body))
        (catch Exception e# (throw (engine-failure ~op e#)))))

(def ^:private contracts
  (into {}
        (for [[what schema] {:row spi/row-schema :link spi/link-schema :layout spi/layout-schema}]
          [what [schema (m/validator schema)]])))

(defn- checked
  [check? what xs]
  (when check?
    (let [[schema valid?] (contracts what)]
      (doseq [x xs :when (not (valid? x))]
        (throw (api/refusal :unavailable (str "store engine returned a malformed " (name what))
                            {:store/engine-contract what :explain (m/explain schema x)
                             :value x})))))
  xs)

(defn ecx [c]
  (dissoc c :W :base :commit?))

(declare ^:dynamic *verify*)

(defn- in-domain
  [T rows]
  (when *verify*
    (doseq [{:keys [id values]} rows
            [a v] values
            :let [p (values/problem v)] :when p]
      (throw (api/refusal :unavailable
                          (str "store engine answered " id "." (name a)
                               " outside the value domain: "
                               (values/explain p))
                          {:store/engine-contract :driver/value :type T :id id :attribute a
                           :value-class (:class p)}))))
  rows)

(defn- checked-select
  [check? denied rows]
  (when (and check? (not= spi/decline rows) (not-every? (partial lib/visible? denied) rows))
    (throw (api/refusal :unavailable "store engine returned a row the authority may not see"
                        {:store/engine-contract :select})))
  rows)

(defn bare
  [autos coll v]
  (if-let [a (get autos coll)] (if (map? v) (dissoc v a) v) v))

(defn- checked-keys
  [check? coll kvs ks]
  (when check?
    (doseq [[[k _] assigned] (map vector kvs ks)
            :when (and (= api/auto k) (not (int? assigned)))]
      (throw (api/refusal :unavailable "store engine assigned a key that is not an integer"
                          {:store/engine-contract :record-key :coll coll :key assigned}))))
  ks)

(defn- bare-scan
  [check? autos coll agg answer]
  (cond
    agg answer
    (and check? (not (and (sequential? answer) (every? #(and (vector? %) (= 2 (count %))) answer))))
    (throw (api/refusal :unavailable
                        "store engine answered a record scan in another shape than [[k v] …]"
                        {:store/engine-contract :record-scan :coll coll}))
    :else (mapv (fn [[k v]] [k (bare autos coll v)]) answer)))

(defn- crossing
  [check? k by x]
  (when (and check? (not (shapes/valid? k x)))
    (throw (api/refusal :unavailable
                        (str (if (= :engine by) "store engine returned"
                                 "store core handed the engine")
                             " a malformed " (name k))
                        {(if (= :engine by) :store/engine-contract :store/core-contract) k
                         :explain (shapes/explain k x)})))
  x)

(defn- read-cx! [check? cx] (crossing check? :driver/read-cx :core (ecx cx)))
(defn- write-cx! [check? cx] (crossing check? :driver/write-cx :core (ecx cx)))
(defn- records-cx! [check? cx] (crossing check? :driver/records-cx :core (ecx cx)))

(defn- pushed-scan
  [x check? autos profile h cx {[kind coll] :source :keys [agg] :as s}]
  (let [answer (eng :scan
                    (spi/-scan x h (read-cx! check? cx) (crossing check? :driver/scan :core s)))]
    (crossing check?
              (if (and agg (= :partial (get-in profile [:scan :agg-mode]))) :driver/agg-states
                  :driver/answer)
              :engine answer)
    (if (= :records kind) (bare-scan check? autos coll agg answer)
        (bare-scan check? {} nil agg answer))))

(defn- pushed-execute
  [x check? h cx {:keys [denied] :as fragment}]
  (->> (eng :execute
            (spi/-execute x h (read-cx! check? cx)
                          (crossing check? :driver/fragment :core fragment)))
       (crossing check? :driver/fragment-answer :engine)
       (checked-select check? denied)))

(defn- pushed-explain
  [x check? h cx fragment]
  (eng :explain
       (spi/-explain x h (read-cx! check? cx) (crossing check? :driver/fragment :core fragment))))

(defrecord ^:private Checked [engine check? colls autos profile]
  spi/Engine
  (-profile [_] profile)
  (-begin [_ mode lane] (eng :begin (spi/-begin engine mode lane)))
  (-lock! [_ h ks] (when (seq ks) (eng :lock (spi/-lock! engine h ks))))
  (-next-rev [_ h] (eng :next-rev (spi/-next-rev engine h)))
  (-commit! [_ h] (eng :commit (spi/-commit! engine h)))
  (-rollback! [_ h] (eng :rollback (spi/-rollback! engine h)))
  (-kv-read [_ h ks] (if (seq ks) (eng :kv-read (spi/-kv-read engine h ks)) {}))
  (-kv-write! [_ h writes] (when (seq writes) (eng :kv-put (spi/-kv-write! engine h writes))))
  (-kv-scan [_ h space part opts] (eng :kv-scan (spi/-kv-scan engine h space part opts)))
  (-kv-parts [_ h space] (eng :kv-parts (spi/-kv-parts engine h space)))
  (-ns-exists? [_ h ns] (eng :ns-exists? (spi/-ns-exists? engine h ns)))
  (-ns-fork! [_ h from to cx]
    (eng :ns-fork (spi/-ns-fork! engine h from to (crossing check? :driver/fork-cx :core cx))))
  (-ns-drop! [_ h ns] (eng :ns-drop (spi/-ns-drop! engine h ns)))
  (-layout! [_ h cx change] (eng :layout (spi/-layout! engine h (write-cx! check? cx) change)))
  (-inspect [_ h cx]
    (first (checked check? :layout [(eng :inspect (spi/-inspect engine h (read-cx! check? cx)))])))
  (-rows [_ h cx T ids]
    (in-domain T
               (checked check? :row
                        (eng :rows (spi/-rows engine h (read-cx! check? cx) T ids)))))
  (-links [_ h cx L where]
    (checked check? :link (eng :links (spi/-links engine h (read-cx! check? cx) L where))))
  (-write-rows! [_ h cx T rows]
    (eng :write-rows (spi/-write-rows! engine h (write-cx! check? cx) T rows)))
  (-delete-rows! [_ h cx T ids]
    (eng :delete-rows (spi/-delete-rows! engine h (write-cx! check? cx) T ids)))
  (-write-links! [_ h cx L links]
    (eng :write-links (spi/-write-links! engine h (write-cx! check? cx) L links)))
  (-delete-links! [_ h cx L ids]
    (eng :delete-links (spi/-delete-links! engine h (write-cx! check? cx) L ids)))

  spi/Pushdown
  (-scan [_ h cx s] (pushed-scan engine check? autos profile h cx s))
  (-execute [_ h cx fragment] (pushed-execute engine check? h cx fragment))
  (-explain [_ h cx fragment] (pushed-explain engine check? h cx fragment))

  spi/Records
  (-rec-colls [_] colls)
  (-rec-get [_ h cx coll k]
    (bare autos coll (eng :rec-get (spi/-rec-get engine h (records-cx! check? cx) coll k))))
  (-rec-put! [_ h cx coll kvs]
    (let [kvs (mapv (fn [[k v]] [k (bare autos coll v)]) kvs)]
      (checked-keys check? coll kvs
                    (eng :rec-put (spi/-rec-put! engine h (records-cx! check? cx) coll kvs)))))
  (-rec-scan [_ h cx coll]
    (bare-scan check? autos coll nil
               (eng :rec-scan (spi/-rec-scan engine h (records-cx! check? cx) coll))))
  (-rec-copy! [_ h coll from to] (eng :rec-copy (spi/-rec-copy! engine h coll from to)))

  spi/RecordsClock
  (-rec-clock [_ h coll]
    (when (and (satisfies? spi/RecordsClock engine) (contains? colls coll))
      (eng :rec-clock (spi/-rec-clock engine h coll)))))

(defrecord CheckedExecutor [pushdown check? autos profile]
  spi/Pushdown
  (-scan [_ h cx s] (pushed-scan pushdown check? autos profile h cx s))
  (-execute [_ h cx fragment] (pushed-execute pushdown check? h cx fragment))
  (-explain [_ h cx fragment] (pushed-explain pushdown check? h cx fragment)))

(defn check-profile!
  [check? profile]
  (when (and check? (not (m/validate spi/profile-schema profile)))
    (throw (api/refusal :unavailable "store engine has a malformed profile"
                        {:store/engine-contract :profile
                         :explain (m/explain spi/profile-schema profile)})))
  (try (sem/check-overrides! (:semantics-overrides profile))
       (catch clojure.lang.ExceptionInfo e
         (throw (api/refusal :unavailable (ex-message e)
                             (assoc (ex-data e) :store/engine-contract :profile)))))
  profile)

(def certificates
  (edn/read-string (slurp (io/resource "sutra/store/certificates.edn"))))

(def claim-keys [:history? :consistency :scan :plan :explain?])

(defn uncertified
  [{:keys [driver] :as profile}]
  (let [cert (get certificates driver {})]
    (into {} (keep (fn [k] (when (and (contains? profile k) (not= (get profile k) (get cert k)))
                             [k (get profile k)])))
          claim-keys)))

(def ^:dynamic *require-certificate* true)

(defn- check-certified! [check? profile]
  (when (and check? *require-certificate*)
    (when-let [claims (not-empty (uncertified profile))]
      (throw (api/refusal :unavailable
                          (str "store engine " (pr-str (:driver profile))
                               " claims what no certificate proves: " (pr-str (keys claims)))
                          {:store/engine-contract :duty/profile :driver (:driver profile)
                           :claims (vec (keys claims))}))))
  profile)

(defn checked-engine
  [engine check? autos]
  (let [profile (eng :profile (spi/-profile engine))]
    (check-profile! check? profile)
    (check-certified! check? profile)
    (->Checked engine check? (if (satisfies? spi/Records engine) (set (spi/-rec-colls engine)) #{})
               autos profile)))

(def ^:dynamic *one-handle-per-lane*
  false)

(def ^:private ^java.util.concurrent.ConcurrentHashMap held
  (java.util.concurrent.ConcurrentHashMap.))

(defn begin [{:keys [engine]} mode lane]
  (if-not *one-handle-per-lane*
    (spi/-begin engine mode lane)
    (let [t (Thread/currentThread)]
      (when-let [[_ _ m] (some (fn [[th l :as e]] (when (and (identical? th t) (= l lane)) e))
                               (.values held))]
        (throw (ex-info (str "a " (name mode) " handle on lane " (pr-str lane)
                             " opened while this thread"
                             " holds a " (name m) " handle there (a nested connection acquisition)")
                        {:lane lane :mode mode :held m})))
      (let [h (spi/-begin engine mode lane)
            k (Object.)]
        (.put held k [t lane mode])
        (assoc h ::core/lease k)))))

(defn- released [h] (some->> (::core/lease h) (.remove held)))
(defn commit! [{:keys [engine]} h] (try (spi/-commit! engine h) (finally (released h))))
(defn rollback! [{:keys [engine]} h] (try (spi/-rollback! engine h) (finally (released h))))
(defn lock! [{:keys [engine]} h ks] (spi/-lock! engine h ks))
(defn engine-can? [{:keys [engine]} cap] (contains? (spi/capabilities (spi/-profile engine)) cap))

(def ^:dynamic *verify*
  false)

(defn candidates
  [{:keys [engine executors]}]
  (cons [engine (spi/-profile engine)] (map (juxt :pushdown :profile) executors)))

(defn- binding-of
  [{:keys [engine]} h {:keys [as-of basis commit?]}]
  (let [raw (:engine engine) basis (or as-of basis)]
    (when (and (not commit?) basis (satisfies? spi/Snapshots raw))
      (some->> (spi/-storage-snapshot raw h basis) (plan/snapshot-binding basis)))))

(defn question [db h c kind]
  (let [b (binding-of db h c)]
    [{:kind kind :commit? (boolean (:commit? c)) :binding? (some? b)}
     (cond-> (ecx c) b (assoc :binding b))]))

(defn scan
  [db h c s entries]
  (when (:check? db) (scan/check! s))
  (let [[q c'] (question db h c :scan)
        [x p]  (plan/executor-for (candidates db) q #(scan/scans? (:scan %) s))]
    (if x
      (let [o (scan/offer (:scan p) s)
            a (scan/answer s o (spi/-scan x h c' (scan/pushed s o)))]
        (when *verify*
          (let [r (scan/reference (entries) s)]
            (when-not (scan/same-answer? s a r)
              (throw (api/refusal :unavailable
                                  "store engine answered a scan otherwise than the core's reference"
                                  (scan/divergence :scan s o a r 20))))))
        a)
      (scan/reference (entries) s))))
