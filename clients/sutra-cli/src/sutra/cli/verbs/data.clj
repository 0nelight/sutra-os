(ns sutra.cli.verbs.data
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [ok? req!]]
            [sutra.cli.live :refer [schema-sections short-commit]]
            [sutra.cli.out :refer [emit! emit-rows! paint prompt-yes? render rows-lines rpad say!]]
            [sutra.cli.json :as json]
            [sutra.cli.udp :as udp]
            [sutra.cli.view :refer [dataset-entry kw-name]]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]))

(defmethod step! :streams [ctx [_ _]]
  (let [{:keys [streams]} (req! "streams" (api/get! :streams))]
    (emit-rows! ctx (vec (for [s streams]
                           (into {:id (str (:stream s))}
                                 (select-keys s [:partitions :cdc? :buffered :archived-rows
                                                 :subscribers]))))
                "no streams — a dataset becomes one by declaring `:stream` in its .ent")))

(defmethod step! :stream-offsets [ctx [_ {:keys [name]}]]
  (let [nm (clojure.core/name name)
        {:keys [end-offsets]} (req! (str "stream offsets " nm)
                                    (api/get! :stream-offsets {:name nm}))]
    (emit-rows! ctx (vec (for [[p o] (sort end-offsets)]
                           {:id (str p) :partition p :next-offset o}))
                "no partitions")))

(defn- stream-records-arg
  [{:keys [record file]}]
  (let [parsed (cond
                 record (json/read-str record :key-fn keyword)
                 file   (json/read-str (slurp file) :key-fn keyword)
                 :else  (throw (ex-info "publish needs --record '{…}' or --file <rows.json>" {})))]
    (cond
      (map? parsed)        [parsed]
      (sequential? parsed) (vec parsed)
      :else (throw (ex-info (str "a record is a JSON object or an array of them, got "
                                 (pr-str (type parsed))) {})))))

(defmethod step! :stream-publish [ctx [_ {:keys [name] :as a}]]
  (let [nm      (clojure.core/name name)
        records (stream-records-arg a)
        r       (api/post! :stream-records {:name nm} {:records (vec records)})]
    (emit! ctx (:body r))
    (when-not (ok? r)
      (throw (ex-info (i18n/t :cli/publish-to-refused {:stream nm}) {:desc "stream publish"
                                                                     :status (:status r)
                                                                     :body (:body r)})))))

(defmethod step! :stream-listen-udp [ctx [_ {:keys [name port bind batch flush-ms]}]]
  (let [nm (clojure.core/name name)]
    (req! (str "stream " nm) (api/get! :stream-by {:name nm}))
    (emit! ctx (udp/forward!
                (cond-> {:port     port
                         :publish  #(api/post! :stream-records {:name nm} {:records (vec %)})
                         :report   #(say! ctx (str "[listen-udp " nm "] " %))
                         :on-ready #(say! ctx (str "[listen-udp " nm "] listening on UDP port " %))}
                  bind     (assoc :bind bind)
                  batch    (assoc :batch batch)
                  flush-ms (assoc :flush-ms flush-ms))))))

(defmethod step! :stream-tail [ctx [_ {:keys [name partition from limit]}]]
  (let [nm (clojure.core/name name)
        {:keys [records]} (req! (str "stream tail " nm)
                                (api/stream-records nm {:partition (or partition 0)
                                                        :start-offset from
                                                        :limit limit}))
        rows (vec (for [r records]
                    (merge {:id (str (:partition r) ":" (:offset r))}
                           (select-keys r [:partition :offset :key :record]))))]
    (emit! ctx {:rows rows :partition (or partition 0) :from (or from 0)} ::stream-tail)
    {:kind :objects :rows rows}))

(defmethod render ::stream-tail [_ {:keys [rows partition from]}]
  (if (empty? rows)
    (str "  nothing at partition " partition " from offset " from)
    (for [r rows]
      (str "  " (paint :cyan (rpad (:id r) 8)) (paint :dim (rpad (str (:key r)) 14))
           (pr-str (:record r))))))

(defmethod step! :stream-view [ctx [_ {:keys [name limit]}]]
  (let [nm (clojure.core/name name)
        {:keys [rows hot cold]} (req! (str "stream view " nm)
                                      (api/get! :stream-view {:name (name nm) :limit limit}))]
    (emit! ctx {:rows rows :hot hot :cold cold} ::stream-view)
    {:kind :objects :rows (vec (for [r rows] {:id (str (hash r)) :record r}))}))

(defmethod render ::stream-view [_ {:keys [rows hot cold]}]
  [(str "  " (paint :dim (i18n/t :cli/stream-view-summary {:archiviert cold :gepuffert hot})))
   (if (empty? rows)
     (str "  " (i18n/t :cli/view-empty))
     (for [r rows] (str "  " (pr-str r))))])

(defmethod step! :stream-reset [ctx [_ {:keys [name yes?]}]]
  (let [nm (clojure.core/name name)
        s  (req! (str "stream " nm) (api/get! :stream-by {:name nm}))]
    (if-not (or yes?
                (prompt-yes? ctx (i18n/t :cli/clear-buffer-confirm {:n (:buffered s) :stream nm})))
      (say! ctx (str "  " (paint :dim (i18n/t :cli/cancelled))))
      (let [b (req! (str "stream reset " nm)
                    (api/post! :stream-reset {:name nm} (or nil {})))]
        (emit! ctx b)))))

(defmethod step! :transactions [ctx [_ {:keys [dataset limit]}]]
  (let [{:keys [transactions via]}
        (req! (str "transactions " (clojure.core/name dataset))
              (api/get! :catalog-transactions
                        (cond-> {:dataset (clojure.core/name dataset)} limit (assoc :limit limit))))
        rows (vec (for [t transactions]
                    {:id         (str (:txn_rid t))
                     :type       (str (:type t))
                     :row-delta  (:row_delta t)
                     :row-count  (:row_count t)
                     :snapshot   (short-commit (:snapshot_id t))
                     :started-at (str (:started_at t))}))]
    (emit! ctx (cond-> {:dataset (clojure.core/name dataset) :rows rows} via (assoc :via via))
           ::transactions)
    {:kind :objects :rows rows}))

(defmethod render ::transactions [_ {:keys [dataset rows via]}]
  [(when-let [{:keys [object-type producer]} via]
     (paint :dim (str "  ↳ " (i18n/t :cli/object-type-producer-chain
                                     {:typ object-type :producer (paint :cyan producer)}))))
   (rows-lines rows (i18n/t :cli/no-transactions-for {:dataset dataset}))])

(defmethod step! :dataset-preview [ctx [_ {:keys [dataset limit]}]]
  (let [schema (schema-sections :datasets)
        entry  (dataset-entry (:datasets schema) dataset)]
    (when-not entry
      (throw (ex-info (str "no dataset named " (clojure.core/name dataset)) {:dataset dataset})))
    (let [[rid _spec] entry
          {:keys [kind columns rows]}
          (req! (str "dataset preview " (clojure.core/name dataset))
                (api/get! :dataset-preview {:id rid}))
          rows     (vec rows)
          shown    (if limit (vec (take limit rows)) rows)
          declared (mapv #(kw-name (:attribute-name %)) columns)
          physical (vec (into (sorted-set) (comp (mapcat keys) (map kw-name)) rows))]
      (emit! ctx {:dataset (clojure.core/name dataset)
                  :kind (kw-name kind)
                  :declared-columns declared
                  :physical-columns physical
                  :count (clojure.core/count rows)
                  :rows shown}))))

(defmethod step! :consumption [ctx [_ {:keys [transform]}]]
  (let [{:keys [consumption]}
        (req! (str "consumption " (clojure.core/name transform))
              (api/get! :catalog-consumption {:transform (clojure.core/name transform)}))
        rows (vec (for [w consumption]
                    {:id         (str (:input_dataset_rid w))
                     :snapshot   (short-commit (:last_snapshot_id w))
                     :updated-at (str (:updated_at w))}))]
    (emit-rows! ctx rows (str "no consumption recorded for " (clojure.core/name transform)))))

(defmethod step! :delta [ctx [_ {:keys [transform input]}]]
  (let [{:keys [delta]}
        (req! (str "delta " (clojure.core/name transform) " " (clojure.core/name input))
              (api/get! :catalog-delta
                        {:transform (clojure.core/name transform)
                         :input (clojure.core/name input)}))]
    (emit! ctx delta))
  nil)

(defmethod step! :edit-history [ctx [_ {:keys [object-type] :as args}]]
  (let [{:keys [transactions provenance history total next-page-token]}
        (req! (str "edit-history " (clojure.core/name object-type))
              (api/edit-history (clojure.core/name object-type) (dissoc args :object-type)))]
    (emit! ctx {:transactions transactions :provenance provenance :history history
                :total total :next-page-token next-page-token})
    nil))
