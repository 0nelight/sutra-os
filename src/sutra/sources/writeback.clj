(ns sutra.sources.writeback
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.foundation.side-effect :as fx]
            [sutra.types.decimal :as d]
            [sutra.sources.protocols :as api]
            [sutra.sources.datasources :as ds]
            [sutra.sources.policy :as policy]))

(set! *warn-on-reflection* true)

(defn- same-value?
  [a b]
  (cond
    (and (nil? a) (nil? b))       true
    (or  (nil? a) (nil? b))       false
    (and (number? a) (number? b)) (= 0 (d/cmp a b))
    :else                         (= a b)))

(defn- marshal-row
  [{:keys [table pk-attr column-map writable]} values live only seen]
  (let [pk-val (get values pk-attr)]
    (when (nil? pk-val)
      (refusal/refuse! :refusal.foreign/object-no-pk {:pk-attr pk-attr :values values}))
    (let [cols (for [[attr col] column-map
                     :let [c (name col)]
                     :when (and (not= attr pk-attr)
                                (contains? writable c)
                                (contains? values attr)
                                (or (nil? only) (contains? only attr))
                                (not (same-value? (get values attr) (get live attr))))]
                 [attr c])]
      {:table    table
       :pk-value pk-val
       :set      (into {} (for [[attr c] cols] [c (get values attr)]))
       :expect   (into {}
                       (for [[attr c] cols]
                         [c (get (if (contains? seen attr) seen live) attr)]))})))

(defn- no-export-transform!
  [{:keys [export-transform]} shape]
  (when export-transform
    (refusal/refuse! :refusal.foreign/export-transform-unimplemented
                     {:export-transform export-transform}
                     {:shape shape})))

(defn reshape
  ([tgt values live only] (reshape tgt values live only nil))
  ([tgt values live only seen]
   (no-export-transform! tgt "write-spec")
   (marshal-row tgt values live only seen)))

(defn- marshal-delete
  [{:keys [table pk-attr column-map delete-guard]} values live]
  (let [pk-val (get values pk-attr)
        guard  (set (map name (or delete-guard [])))]
    (when (nil? pk-val)
      (refusal/refuse! :refusal.foreign/object-no-pk-delete {:pk-attr pk-attr :values values}))
    {:table    table
     :pk-value pk-val
     :delete   true
     :expect   (into {} (for [[attr col] column-map
                              :let [c (name col)]
                              :when (contains? guard c)]
                          [c (get live attr)]))}))

(defn reshape-delete
  [tgt values live]
  (marshal-delete tgt values live))

(defn- marshal-insert
  [{:keys [table column-map insertable identity-col]} values]
  (let [id-col (some-> identity-col name)
        ins    (into {}
                     (for [[attr col] column-map
                           :let [c (name col)]
                           :when (and (contains? insertable c)
                                      (not= c id-col)
                                      (contains? values attr))]
                       [c (get values attr)]))]
    (when (empty? ins)
      (refusal/refuse! :refusal.foreign/nothing-insertable
                       {:table table :insertable insertable :values (vec (keys values))}))
    (cond-> {:table table :insert ins}
      id-col (assoc :returning id-col))))

(defn reshape-insert
  [tgt values]
  (no-export-transform! tgt "insert write-spec")
  (marshal-insert tgt values))

(defn- pk-literal
  [pk-val]
  (let [s (str pk-val)]
    (cond
      (re-matches #"\d+" s) s
      (re-matches #"[A-Za-z0-9_.:-]+" s) (str "'" s "'")
      :else (refusal/refuse! :refusal.foreign/unsafe-pk-value {:pk pk-val}))))

(defn- cell
  [row col]
  (let [c (name col)]
    (if-let [e (or (find row (keyword c)) (find row (keyword (str/lower-case c))))]
      (val e)
      (some (fn [[k v]] (when (.equalsIgnoreCase (name k) c) v)) row))))

(defn- via-agent
  [{:keys [connect authority source write-session]}]
  (connect authority source :agent write-session))

(defn- scan-row-where
  [{:keys [table query column-map] :as tgt} cols values]
  (when-not (str/blank? query)
    (refusal/refuse! :refusal.foreign/reread-query-needs-sql {:table table}))
  (let [col (fn [c] (keyword (str/lower-case (name c))))]
    (api/with-source [s via-agent tgt]
      (api/scan s table (cond-> {:where (mapv (fn [c v] [:= (col c) v]) cols values) :limit 2}
                          (seq column-map)
                            (assoc :project
                                   (into (set (map col cols)) (map col (vals column-map)))))))))

(defn- read-row-where
  ([tgt cols vals] (read-row-where tgt cols vals false))
  ([{:keys [table query column-map source] :as tgt} cols vals bind?]
   (let [rows  (if (policy/capable? source :sql)
                 (let [from  (if (str/blank? query)
                               (policy/safe-ident table)
                               (str "(" query ") AS _wb"))
                       where (str/join " AND "
                                       (map (fn [c v]
                                              (str (policy/safe-ident c) " = "
                                                   (if bind? "?" (pk-literal v))))
                                            cols vals))]
                   (api/with-source [s via-agent tgt]
                     (api/read s (str "SELECT * FROM " from " WHERE " where)
                               (when bind? (vec vals)))))
                 (scan-row-where tgt cols vals))]
     (when-let [r (first rows)]
       (reduce-kv (fn [m attr c] (assoc m attr (cell r c))) {} column-map)))))

(defn- read-row-by
  [{:keys [table query] :as tgt} col value]
  (when (and (str/blank? query) (str/blank? (str table)))
    (refusal/refuse! :refusal.foreign/reread-needs-table {}))
  (read-row-where tgt [col] [value]))

(defn read-live-row
  [{:keys [pk-result pk-columns] :as tgt} pk-val]
  (if (seq pk-columns)
    (read-row-where tgt pk-columns (policy/pk-parts pk-columns pk-val))
    (read-row-by tgt pk-result pk-val)))

(defn find-by-match-key
  [{:keys [match-key table query] :as tgt} match-vals]
  (when (and (seq match-key) (seq match-vals))
    (when (and (str/blank? query) (str/blank? (str table)))
      (refusal/refuse! :refusal.foreign/reread-needs-table {}))
    (read-row-where tgt match-key match-vals true)))

(defn conflict-verdict
  [strategy {:keys [timestamp-attr ts-live ts-known]}]
  (case strategy
    :apply-most-recent
    (cond
      (nil? timestamp-attr)
      {:allow? false
       :reason "apply-most-recent requires a :timestamp-attr on the source's :writeback config"}
      (and ts-live ts-known (pos? (compare (str ts-live) (str ts-known))))
      {:allow? false
       :reason (str "blocked: live source advanced to " (pr-str ts-live)
                    " past last-synced " (pr-str ts-known)
                    " — source value is most recent (apply-most-recent)")}
      :else
      {:allow? true
       :reason "apply-most-recent: edit is at least as recent as the source — push"})
    {:allow? true
     :reason "apply-user-edits: ontology edit wins — push"}))

(defn- pushable
  [auth source spec gate]
  (ds/assert-source-write-cleared! auth source)
  (policy/assert-export-allowed! source)
  (fx/side-effect gate :external-calls {:source source :table (:table spec)}))

(defn push!
  [{:keys [authority source] :as tgt} write-spec gate]
  (let [fx (pushable authority source write-spec gate)]
    (if (fx/admit! fx)
      (api/with-source [s via-agent tgt] (api/write! s write-spec))
      {:deferred? true :side-effect fx})))

(defn push-create-webhook!
  [{:keys [authority source webhook-id identity-path table]} insert-spec gate]
  (if-let [held (some #(when-not (fx/admit! %) %)
                      [(pushable authority source insert-spec gate)
                       (fx/side-effect gate :webhooks
                                                  {:source source :webhook webhook-id})])]
    {:identity nil :deferred? true :side-effect held}
    (let [res (ds/invoke-webhook! source webhook-id (:insert insert-spec)
                                 {:event-type "com.sutra.sources.writeback-create"})]
      (when-not (<= 200 (or (:status res) 0) 299)
        (refusal/refuse! :refusal.sources/writeback-create-rejected
                         {:webhook webhook-id :table table :result res}
                         {:status (:status res)}))
      (let [id (get-in res identity-path)]
        (when (nil? id)
          (refusal/refuse! :refusal.sources/writeback-no-identity
                           {:webhook webhook-id :table table
                            :identity-path identity-path :result res}))
        {:identity id :result res}))))
