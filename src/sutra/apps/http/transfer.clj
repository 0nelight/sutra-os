(ns sutra.apps.http.transfer
  (:require [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.types.upload :as upload]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.blobs.api :as blobs]
            [sutra.builds.api :as builds]
            [sutra.schedules.api :as schedules]
            [sutra.compute.api :as compute]
            [sutra.foundation.encoding :as enc]
            [sutra.kit.coerce :refer [->kw ->num0]]
            [sutra.kit.digest :as digest]
            [sutra.kit.tree :as tree]
            [sutra.kit.time :as time]
            [sutra.apps.http.delivery :refer [app-bound-objects]]
            [sutra.media.http.blobs :as blob-api]
            [sutra.builds.http :as conductor]
            [sutra.media.http.sets :as media]
            [sutra.http.responses :refer [fault json-response]]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.value-types :as vt]
            [sutra.objects.api :as objects]
            [sutra.kit.anomaly :as anomaly]
            [sutra.catalog.project-scope :as pscope]
            [sutra.apps.http.actions :refer [submission-refusal submit-action!]]
            [sutra.apps.http.session :refer [with-app]]
            [sutra.apps.http.view :refer [with-view-reach]]))

(set! *warn-on-reflection* true)

(defn- type-holds? [store schema rid hash]
  (let [media (into {} (filter (comp vt/media? val)) (lookup/attr-type-map schema rid))]
    (when (and (seq media)
               (grants/allows-read? (grants/read-rights store)
                                    (get-in schema [:object-types rid]) rid))
      (some (fn [[nm t]]
              (seq (objects/objects-matching
                    store schema rid
                    {:where {nm {(if (sequential? t) :has :=) hash}} :limit 1} {})))
            media))))

(defn- app-holds-hash? [store schema scope hash]
  (boolean (some #(type-holds? store schema % hash) (:bound-type scope))))

(defn my-application-blob
  [{{:keys [hash]} :path-params :keys [headers] :as req}]
  (with-app req {:preview? true}
    (fn [{:keys [schema branch scope store auth]}]
      (cond
        (not (blobs/valid-address? hash)) (refusal/refused :refusal.http/bad-blob-hash {:hash hash})
        (app-holds-hash? (ctx/at store branch) schema
                         (update scope :bound-type into
                                 (:type (with-view-reach auth schema scope)))
                         hash)
        (blob-api/stream-blob (ctx/at store branch) hash headers)
        :else (refusal/refused :refusal.http/no-such-blob {:hash hash})))))

(defn- epoch-ms [t]
  (if (inst? t) (inst-ms t) (time/->epoch-ms t)))

(defn my-application-upload-status
  [req]
  (with-app req {:json? true :branch? true}
    (fn [{:keys [a branch store]}]
      (let [since  (some-> (get-in req [:query-params "since"]) parse-long)
            names  (set (some->> (get-in a [:layout :upload :dataset])
                                 (schedules/watching store branch)))
            fired  (->> (schedules/list-all store branch)
                        (filter #(contains? names (:name %)))
                        (filter #(when-let [t (epoch-ms (:last-fired-at %))]
                                   (and since (>= t since)))))
            rs     (keep #(some->> (:last-run-id %) (builds/get-run (ctx/at store branch))) fired)
            states (map #(some-> (:status %) name) rs)]
        (json-response
         200
         (cond
           (empty? names)                         {:done true :status "unbuilt"}
           (empty? rs)                            {:pending true}
           (some #(not (#{"succeeded" "failed" "aborted"} %)) states) {:pending true}
           (every? #{"succeeded"} states)         {:done true :status "succeeded"}
           :else {:done true :status "failed"
                  :error (some #(some-> (or (:error %) (:message %)) str) rs)}))))))

(def ^:private max-app-upload-bytes (* 20 1024 1024))

(defn- coerce-col
  [attr-type v]
  (if (and (= attr-type :integer) (string? v))
    (let [t (str/trim v)] (if (re-matches #"-?\d+" t) (parse-long t) v))
    v))

(defn- throwable-msg
  [^Throwable e]
  (let [top   (or (ex-message e) (str e))
        cause (some-> (ex-cause e) ex-message)]
    (if (and cause (not= cause top) (not (str/includes? top cause)))
      (str top " — " cause)
      top)))

(defn- upload-refusal
  [up b64 bytes]
  (cond
    (nil? up)
    [400 {:error "diese App erlaubt keinen Upload (:layout :upload fehlt)"}]

    (not (and (string? b64) bytes))
    [400 {:error "leerer oder ungültiger Upload (bytes fehlt/kein base64)"}]

    (> (alength ^bytes bytes) max-app-upload-bytes)
    [413 {:error "Datei zu groß" :max max-app-upload-bytes}]))

(defn- upload-values
  [up ds-cols body username ^bytes bytes at]
  (let [{:keys [fields key-field name-field stamp-field user-field size-field mime-field]} up
        meta (into {} (for [k fields
                            :let [v (get body (->kw k))]
                            :when (some? v)]
                        [(->kw k) (coerce-col (get ds-cols (->kw k)) v)]))]
    (fn [h]
      (cond-> meta
        key-field   (assoc (->kw key-field) (subs h 0 12))
        name-field  (assoc (->kw name-field) (str (:dateiname body)))
        stamp-field (assoc (->kw stamp-field) at)
        user-field  (assoc (->kw user-field) (or username ""))
        size-field  (assoc (->kw size-field) (alength bytes))
        mime-field  (assoc (->kw mime-field) (str (:mime body)))))))

(defn- already-uploaded?
  [branch-store {:keys [dataset key-field]} ds-rid bytes]
  (when key-field
    (let [h  (digest/sha256-hex bytes)
          id (str (enc/pk-uuid (name (->kw dataset)) (subs h 0 12)))]
      (some #(= id (str (:id %))) (dsets/rows branch-store ds-rid)))))

(defn- built!
  [store schema branch build]
  (try (conductor/run-import {:body-params {:only (vec build)} :sutra/store store
                              :sutra/auth grants/system-authority
                              :sutra/schema schema :sutra/branch branch})
       (catch InterruptedException e (throw e))
       (catch Exception e
         (if (anomaly/of e) (fault e) {:status 500 :body {:error (throwable-msg e)}}))))

(defn- upload-args
  [{:keys [dataset on-upload]} hash filename]
  (upload/resolve-args (:params on-upload)
                       {:hash hash :filename filename :dataset (name (->kw dataset))}))

(defn- upload-acted!
  [branch-store edge auth branch schema up hash filename]
  (let [nm (->kw (get-in up [:on-upload :action]))]
    (try (submit-action! branch-store edge auth branch schema nm
                         (upload-args up hash filename))
         {:ok true :name nm}
         (catch InterruptedException e (throw e))
         (catch Exception e {:ok false :name nm :error (throwable-msg e)}))))

(defn- upload-project
  [schema a ds-rid]
  (when-let [p (or (get-in schema [:datasets ds-rid :project]) (:project a))]
    (if (string? p) p (lookup/rid-of schema :projects (->kw p)))))

(defn- upload-denial
  [branch-store edge auth branch schema a up ds-rid hash filename]
  (if (:on-upload up)
    (when-let [r (submission-refusal branch-store edge auth branch schema
                                     (->kw (get-in up [:on-upload :action]))
                                     (upload-args up hash filename))]
      [(or (anomaly/status r) 403)
       {:error (anomaly/message r) :category (anomaly/category r)}])
    (try (pscope/require! auth :update (upload-project schema a ds-rid) "Der Upload")
         nil
         (catch clojure.lang.ExceptionInfo e
           (let [r (pscope/named schema (anomaly/of e))]
             [403 {:error (anomaly/message r) :sutra/acl-denied true}])))))

(def ^:private queued-build-note
  (str "Die Datei ist gespeichert. Gerade läuft ein anderer Import, deshalb wird sie mit "
       "dem nächsten Build verarbeitet — kein erneuter Upload nötig."))

(defn- built-answer
  [landed built acted data]
  (let [ok?     (= 200 (:status built))
        queued? (= 409 (:status built))]
    (cond-> (assoc landed :build {:ok ok?})
      acted     (assoc :action acted)
      ok?       (assoc :counts (get-in built [:body :counts]) :data data)
      queued?   (assoc-in [:build :queued] true)
      (not ok?) (assoc-in [:build :error]
                          (if queued?
                            queued-build-note
                            (or (get-in built [:body :error])
                                (str "Verarbeitung fehlgeschlagen — wird beim nächsten Build "
                                     "erneut versucht")))))))

(defn- land-upload!
  [{:keys [schema branch a body store auth edge username]} up ds-rid bytes filename]
  (let [{:keys [dataset media-column build key-field on-upload]} up
        branch-store (ctx/at store branch)
        ds-cols  (into {} (map (juxt :attribute-name :attribute-type))
                       (get-in schema [:datasets ds-rid :columns]))
        already? (already-uploaded? branch-store up ds-rid bytes)
        since    (ctl/millis store)
        {:keys [hash write triggered-by]}
        (media/land! store schema branch
                     {:bytes        bytes
                      :dataset      dataset
                      :media-column (->kw media-column)
                      :key-field    (some-> key-field ->kw)
                      :values-fn    (upload-values up ds-cols body username bytes (ctl/stamp store))
                      :filename     (not-empty filename)
                      :mime         (not-empty (str (:mime body)))
                      :message      (str "app-upload " (:dateiname body))})
        landed   (cond-> {:ok true :hash hash :dateiname (:dateiname body)}
                   already? (assoc :already true))]
    (cond
      (anomaly/refusal? write)
      write

      (not= 200 (:status write))
      (json-response (:status write) (:body write))

      (empty? build)
      (json-response 200 (assoc landed :build {:scheduled true :since since
                                               :by (vec triggered-by)}))

      :else
      (let [built (built! store schema branch build)
            ok?   (= 200 (:status built))
            acted (when (and ok? on-upload)
                    (upload-acted! branch-store edge auth branch schema up hash filename))
            data  (when ok?
                    (into {} (map (fn [t] [t (app-bound-objects branch-store schema t)]))
                          (->> (get-in a [:layout :object-types]) (keep ->kw) distinct)))]
        (json-response 200 (built-answer landed built acted data))))))

(defn- chosen-destination?
  [a]
  (some #(and (= :media-uploader (some-> (:widget %) ->kw))
              (or (:destination %) (:destination-var %)))
        (tree/nodes (dissoc (:layout a) :vars))))

(defn- upload-target
  [a {:keys [destination media-column]}]
  (let [up (get-in a [:layout :upload])]
    (if (and (seq (str destination)) media-column (chosen-destination? a))
      {:dataset destination :media-column media-column}
      up)))

(defn my-application-upload
  [req]
  (with-app req {:json? true :branch? true}
    (fn [{:keys [schema branch a body store auth edge] :as app}]
      (let [up       (upload-target a body)
            b64      (:bytes body)
            bytes    (blob-api/b64-bytes b64)
            ds-rid   (some->> (:dataset up) ->kw (lookup/rid-of schema :datasets))
            filename (str (:dateiname body))]
        (if-let [[status answer]
                 (or (upload-refusal up b64 bytes)
                     (upload-denial (ctx/at store branch) edge auth branch schema a up ds-rid
                                    (blobs/address-of bytes) filename))]
          (json-response status answer)
          (land-upload! (assoc app :username (ep/signed-in-user req))
                        up ds-rid bytes filename))))))

(defn my-application-export
  [req]
  (with-app req {:json? true :branch? true}
    (fn [{:keys [schema branch a body store]}]
      (let [ex     (get-in a [:layout :export])]
        (cond
          (nil? ex) (json-response 400
                                   {:error
                                    "diese App erlaubt keinen Export (:layout :export fehlt)"})
          (not (and (:template-b64 ex) (:code ex)))
          (json-response 400
                         {:error "Export unvollständig konfiguriert (:template-b64/:code fehlt)"})
          :else
          (let [{:keys [object-type key-field value-field period-fields cumulative-field
                        template-b64 code python-deps filename profile]} ex
                period (into {} (for [k period-fields
                                      :let [v (get body (->kw k))]
                                      :when (some? v)]
                                  [(->kw k) v]))
                objs   (app-bound-objects (ctx/at store branch) schema
                                          (->kw object-type))
                cum    (some-> cumulative-field ->kw)
                in-period? (fn [o] (every? (fn [[k v]]
                                             (if (= k cum)
                                               (<= (->num0 (get (:values o) k)) (->num0 v))
                                               (= (str v) (str (get (:values o) k)))))
                                           period))
                values (reduce (fn [m o]
                                 (if (in-period? o)
                                   (let [k (some-> (get (:values o) (->kw key-field)) str str/trim)
                                         v (->num0 (get (:values o) (->kw value-field)))]
                                     (if (str/blank? k) m (update m k (fnil + 0.0) v)))
                                   m))
                               {} objs)
                job    {:template_b64 template-b64
                        :values values
                        :params (into {} (map (fn [[k v]] [k v])) period)}
                out    (try (compute/run-python!
                             {:label       :app-export
                              :code        code
                              :python-deps python-deps
                              :profile     (some-> profile ->kw)
                              :stdin       (json/write-str job)})
                            (catch InterruptedException e (throw e))
                            (catch Exception e {::error (.getMessage e)}))]
            (if (and (map? out) (::error out))
              (json-response 500 {:error (str "Export fehlgeschlagen: " (::error out))})
              (let [res (enc/json-or-nil out :key-fn keyword)]
                (if-not (:ok res)
                  (json-response 500
                                 {:error "Export lieferte kein Ergebnis"
                                  :raw (some-> out (subs 0 (min 400 (count out))))})
                  (json-response 200 {:ok true
                                      :filename (str (or filename "export") ".ods")
                                      :file_b64 (:ods_b64 res)
                                      :stats (dissoc res :ods_b64 :ok)}))))))))))
