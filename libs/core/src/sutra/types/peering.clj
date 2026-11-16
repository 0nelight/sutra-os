(ns sutra.types.peering
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(def data-types
  [:source-data :action-data :media :datasets])

(def directions [:none :export :import :both])

(defn exports? [dir] (contains? #{:export :both} (keyword (name (or dir :none)))))
(defn imports? [dir] (contains? #{:import :both} (keyword (name (or dir :none)))))

(defn mirrored
  [dir]
  (case (keyword (name (or dir :none))) :export :import :import :export :both :both :none))

(defn data-type-problems
  [dts]
  (vec (concat
        (for [[dt _] dts
              :when (not (contains? (set data-types) (keyword (name dt))))]
          {:problem :unknown-data-type :data-type dt :known data-types})
        (for [[dt dir] dts
              :when (not (contains? (set directions) (keyword (name (or dir :none)))))]
          {:problem :unknown-direction :data-type dt :direction dir :known directions})
        (when (contains? #{:both} (keyword (name (or (get dts :source-data) :none))))
          [{:problem :source-data-one-way
            :why "source data peers in one direction only"}])
        (when (contains? #{:both} (keyword (name (or (get dts :media) :none))))
          [{:problem :media-one-way
            :why "a media set's items peer in one direction only"}]))))

(defn normal-data-types
  [dts]
  (into {} (for [dt data-types]
             [dt (keyword (name (or (get dts dt) (get dts (name dt)) :none)))])))

(def lifecycle
  {:requested {:accepted :accepted}
   :accepted  {:activate :active :activated :active}
   :active    {}})

(defn next-status
  [status event]
  (get-in lifecycle [(keyword (name status)) event]))

(defn flowing?
  [conn]
  (and (= :active (keyword (name (or (:status conn) :requested))))
       (not (:disabled-local conn))
       (not (:disabled-remote conn))))

(defn may-flow?
  [conn mapping data-type way]
  (let [ok? (if (= :out way) exports? imports?)]
    (and (flowing? conn)
         (ok? (get-in conn [:data-types data-type]))
         (ok? (get mapping data-type)))))

(defn invite-payload
  [{:keys [connection-id space data-types ceiling name]} {:keys [fingerprint]}]
  {:v 1 :connection-id connection-id :from fingerprint :space space :name name
   :data-types (normal-data-types data-types) :ceiling ceiling})

(defn invite-problems
  [payload {:keys [trust existing]}]
  (vec (concat
        (when-not (and (map? payload) (= 1 (:v payload)) (string? (:connection-id payload)))
          [{:problem :malformed-invite}])
        (when (nil? trust)
          [{:problem :untrusted-instance :fingerprint (:from payload)
            :why "declare a trust for the inviting instance first (trust set … --inbound true)"}])
        (when existing [{:problem :connection-exists :connection-id (:connection-id payload)}])
        (data-type-problems (:data-types payload)))))

(defn accepted-connection
  [payload {:keys [space trust actor]}]
  {:connection-id   (:connection-id payload)
   :name            (or (:name payload) (:connection-id payload))
   :role            :acceptor
   :status          :accepted
   :trust           trust
   :remote-fingerprint (:from payload)
   :space           space
   :remote-space    (:space payload)
   :data-types      (into {} (for [[dt dir] (normal-data-types (:data-types payload))]
                               [dt (mirrored dir)]))
   :ceiling         (:ceiling payload)
   :disabled-local  false
   :disabled-remote false
   :created-by      actor})

(defn ceiling-problems
  [c]
  (let [{:keys [classification markings]} c]
    (vec (concat
          (when-not (map? c) [{:problem :malformed-ceiling}])
          (when (and classification
                     (not (and (string? (:name classification))
                               (string? (:category classification))
                               (int? (:rank classification)))))
            [{:problem :malformed-classification :classification classification}])
          (when-not (and (sequential? (or markings [])) (every? string? markings))
            [{:problem :malformed-markings :markings markings}])))))

(defn admitted?
  [{:keys [classification markings]} ms]
  (let [allowed (set markings)]
    (every? (fn [{nm :name :keys [category rank]}]
              (if category
                (and classification
                     (= (str category) (str (:category classification)))
                     (<= (long rank) (long (:rank classification))))
                (contains? allowed (str nm))))
            ms)))

(defn held-back
  [ceiling ms]
  (vec (remove #(admitted? ceiling [%]) ms)))

(defn propose-ceiling
  [conn ceiling side]
  (assoc conn :pending-ceiling {:ceiling ceiling :proposed-by side}))

(defn approve-ceiling
  [conn side]
  (let [{:keys [ceiling proposed-by]} (:pending-ceiling conn)]
    (cond
      (nil? (:pending-ceiling conn)) {:refused "no ceiling change waits for approval"}
      (= proposed-by side)
      {:refused "a ceiling change is approved by the other side of the connection"}
      :else (assoc conn :ceiling ceiling :pending-ceiling nil))))

(defn wire-markings
  [ms classification marking-map]
  (let [out (into {} (map (fn [[wire local]] [(str local) wire])) marking-map)]
    (vec (for [m ms
               :let [m (name m) c (get classification (keyword m))]]
           (cond-> {:name (get out m m)}
             c (assoc :category (:category c) :rank (:rank c)))))))

(defn local-name
  [connection-id wire-name]
  (str "peer-" (subs (str connection-id) 0 (min 8 (count (str connection-id)))) "-"
       (str/replace (str/lower-case (str wire-name)) #"[^a-z0-9-]+" "-")))

(defn marking-plan
  [connection-id ms marking-map]
  (reduce (fn [acc {nm :name}]
            (if-let [l (get marking-map nm)]
              (update acc :local conj (keyword l))
              (let [l (local-name connection-id nm)]
                (-> acc (update :local conj (keyword l)) (assoc-in [:created nm] l)))))
          {:local [] :created {}}
          ms))

(def unpeerable-types
  #{:cipher :cipher-text :ciphertext :vector :embedding :time-series :timeseries})

(defn- type-of [t] (some-> t (#(if (vector? %) (first %) %)) name keyword))

(defn share-refusal
  [c shared? ds markings]
  (cond (not (flowing? c)) "the connection is not active"
        (not (exports? (get-in c [:data-types :datasets])))
        "the connection does not share datasets in this direction"
        (not shared?) (str "dataset " ds " is not shared over the connection (peer share)")
        (not (admitted? (:ceiling c) markings))
        (str "dataset " ds " carries markings the connection's ceiling does not let through")))

(defn datasource-kind
  [backing entry]
  (cond (or (:direct backing) (:direct entry)
            (= "direct" (some-> (or (:kind backing) (:kind entry)) name)))
        :direct
        (:stream entry) :stream
        :else :dataset))

(defn- flows-at-all? [dir] (not= :none (keyword (name (or dir :none)))))

(defn datasource-problems
  [mapping kind]
  (when (= :stream kind)
    (for [dt [:source-data :action-data] :when (flows-at-all? (get mapping dt))]
      {:problem :stream-backed :data-type dt
       :why (str "a stream-backed object type does not peer " (name dt) ": back it with a"
                 " direct datasource the stream feeds, or sync the stream (peer sync)")})))

(defn datasource-notes
  [mapping kind]
  (when (and (= :dataset kind) (flows-at-all? (:source-data mapping)))
    [{:note :dataset-source-data
      :why  (str "source data of a dataset-backed type peers object by object; syncing the"
                 " dataset (peer sync) keeps it whole and lets the other side build on it")}]))

(defn mapping-problems
  [{:keys [properties] :as mapping} {:keys [local-attrs remote-attrs local-product
                                             remote-product datasource]}]
  (let [props (seq properties)]
    (vec (concat
          (when (nil? local-attrs) [{:problem :no-local-type :type (:local-type mapping)}])
          (when (nil? remote-attrs) [{:problem :no-remote-type :type (:remote-type mapping)}])
          (when-not props [{:problem :no-properties
                            :why "map at least the properties that should peer"}])
          (when (and local-attrs remote-attrs)
            (for [[l r] props
                  :let [lt (type-of (get local-attrs (keyword (name l))))
                        rt (type-of (get remote-attrs (keyword (name r))))]
                  p (cond
                      (nil? lt) [{:problem :no-local-property :property l}]
                      (nil? rt) [{:problem :no-remote-property :property r}]
                      (not= lt rt) [{:problem :type-mismatch :property l :local lt :remote rt}]
                      (unpeerable-types lt) [{:problem :unpeerable-type :property l :type lt}]
                      :else nil)]
              p))
          (for [[r n] (frequencies (map (comp name val) props)) :when (> n 1)]
            {:problem :mapped-twice :remote-property r})
          (cond
            (nil? local-product)
            [{:problem :not-from-a-product :side :local
              :why "publish or install the type through a product registry product first"}]
            (nil? remote-product)
            [{:problem :not-from-a-product :side :remote}]
            (not= local-product remote-product)
            [{:problem :different-products :local local-product :remote remote-product}])
          (datasource-problems mapping datasource)
          (data-type-problems (select-keys mapping data-types))))))

(defn outbound-values
  [{:keys [properties]} values]
  (into {} (for [[l _] properties
                 :let [k (keyword (name l))]
                 :when (contains? values k)]
             [(name l) (get values k)])))

(defn inbound-values
  [{:keys [properties]} values]
  (let [back (into {} (map (fn [[l r]] [(name r) (keyword (name l))])) properties)]
    (into {} (for [[r v] values
                   :let [l (get back (name r))]
                   :when l]
               [l v]))))

(defn peered-id
  [connection-id remote-id]
  (let [md (MessageDigest/getInstance "SHA-256")
        bs (.digest md (.getBytes (str "peer|" connection-id "|" remote-id)
                                  StandardCharsets/UTF_8))]
    (str (UUID/nameUUIDFromBytes bs))))

(defn outbound-ref
  [crosswalk local-id]
  (cond-> {:id (str local-id)}
    (contains? crosswalk (str local-id)) (assoc :yours (get crosswalk (str local-id)))))

(defn inbound-id
  [connection-id reverse-crosswalk {:keys [id yours]}]
  (or (some-> yours str)
      (get reverse-crosswalk (str id))
      (peered-id connection-id id)))

(defn edit-stands?
  [{:keys [at from]} {local-at :at local-from :from}]
  (or (nil? local-at)
      (> (long at) (long local-at))
      (and (= (long at) (long local-at)) (pos? (compare (str from) (str local-from))))))

(defn later-rev
  [a b]
  (cond (nil? a) b (nil? b) a (pos? (compare a b)) a :else b))

(defn oldest-change
  [ats]
  (some->> (seq (filter #(and (number? %) (pos? (long %))) ats)) (apply min)))

(defn lag
  [since now]
  (when since (max 0 (- (long now) (long since)))))

(defn backoff-ms
  [attempts]
  (min 300000 (* 1000 (bit-shift-left 1 (min 20 (max 0 (long attempts)))))))

(def stale-link-ms
  (* 60 60 1000))

(defn- millis [t]
  (cond (instance? java.util.Date t) (.getTime ^java.util.Date t)
        (instance? java.time.Instant t) (.toEpochMilli ^java.time.Instant t)
        (number? t) (long t)
        (string? t) (try (.toEpochMilli (java.time.Instant/parse t)) (catch Exception _ nil))
        :else nil))

(defn pending-links
  [rows now]
  (let [ages (keep #(some->> (millis (:since %)) (- (long now))) rows)]
    {:links     (count rows)
     :oldest-ms (when (seq ages) (apply max ages))
     :stale     (count (filter #(> % stale-link-ms) ages))}))

(defn health
  ([jobs] (health jobs nil))
  ([jobs pending]
   (let [last-by (reduce (fn [m j] (update m [(:data-type j) (:way j)] #(or % j))) {} jobs)]
     (cond-> {:state (cond (some #(= :failed (keyword (name (:state %)))) (vals last-by))
                           :failing
                           (pos? (long (:stale pending 0))) :degraded
                           (empty? last-by) :idle
                           :else :healthy)
              :jobs  (into (sorted-map)
                           (for [[[dt w] j] last-by]
                             [(str (name dt) "/" (name w))
                              (select-keys j [:state :items :held :superseded :error
                                              :latency-ms :at])]))}
       (pos? (long (:links pending 0))) (assoc :pending-links pending)))))

(def strategies
  #{:all :instance :mesh :labels})

(defn- mirror-mapping [{:keys [local-type remote-type properties links] :as m}]
  (cond-> {:local-type  remote-type
           :remote-type local-type
           :properties  (into {} (map (fn [[l r]] [(name r) (name l)])) properties)
           :source-data (mirrored (:source-data m))
           :action-data (mirrored (:action-data m))}
    (seq links) (assoc :links (into {} (map (fn [[l r]] [(name r) (name l)])) links))))

(defn counterpart
  [{:keys [pair side name ceiling data-types types auto-connect]}]
  {:pair         pair
   :side         (if (= :hub (keyword (clojure.core/name side))) :spoke :hub)
   :name         (str name " (counterpart)")
   :ceiling      ceiling
   :data-types   (into {} (for [[dt dir] (normal-data-types data-types)] [dt (mirrored dir)]))
   :types        (mapv mirror-mapping types)
   :auto-connect (boolean auto-connect)
   :strategy     {:kind :all}})

(defn profile-problems
  [{:keys [pair name side strategy data-types types]}]
  (vec (concat
        (when (str/blank? (str pair)) [{:problem :no-pair}])
        (when (str/blank? (str name)) [{:problem :no-name}])
        (when-not (contains? #{:hub :spoke} (keyword (clojure.core/name (or side :none))))
          [{:problem :unknown-side :side side :known [:hub :spoke]}])
        (let [k (keyword (clojure.core/name (or (:kind strategy) :none)))]
          (cond
            (not (strategies k))
            [{:problem :unknown-strategy :strategy (:kind strategy) :known (sort strategies)}]
            (and (= :instance k) (str/blank? (str (:instance strategy))))
            [{:problem :strategy-names-no-instance}]
            (and (= :labels k) (empty? (:labels strategy)))
            [{:problem :strategy-names-no-label}]))
        (data-type-problems (normal-data-types data-types))
        (for [{:keys [local-type remote-type properties] :as t} types
              p (cond
                  (or (str/blank? (str local-type)) (str/blank? (str remote-type)))
                  [{:problem :type-mapping-without-types :mapping t}]
                  (empty? properties)
                  [{:problem :type-mapping-without-properties :local-type local-type}])]
          p))))

(defn targets
  [{{:keys [kind instance labels]} :strategy} trusts mesh]
  (let [trusts (set trusts)]
    (sort (case (keyword (name (or kind :none)))
            :all      trusts
            :instance (filter #{(str instance)} trusts)
            :mesh     (filter trusts (keys mesh))
            :labels   (for [[t ls] mesh
                            :when (and (trusts t) (some (set (map str ls)) (map str labels)))]
                        t)
            []))))

(defn profile-offer-stands?
  [profile pair trust trusts mesh]
  (and (= (str pair) (str (:pair profile)))
       (:auto-connect profile)
       (some #{(str trust)} (targets profile trusts mesh))
       true))
