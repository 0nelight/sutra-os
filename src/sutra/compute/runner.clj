(ns sutra.compute.runner
  (:require [clojure.java.io :as io]
            [sutra.blobs.api :as blobs]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.digest :as digest]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.compute.external :as ext]
            [sutra.compute.clojure :as cljt]
            [sutra.compute.images :as images]
            [sutra.store.access.scope :as ctx]
            [sutra.compute.sql :as sql]
            [sutra.types.kinds :as kinds]
            [sutra.types.table :as tbl])
  (:import [java.util Base64]))

(set! *warn-on-reflection* true)

(defn- rows-entry [{:keys [kind rows]}] {:kind kind :rows rows})

(def ^:private manifest-entries
  (tbl/per-key "compute/runner manifest-entries" kinds/compute-input-kinds
               {:datasource     (fn [in] {:path (:path in)})
                :language-model (fn [in] (dissoc in :rows :path :media-refs))
                :scalar         (fn [in] {:kind :scalar :value (ext/->wire (:value in))})
                :media          (fn [in] {:kind :media :address (:address in)})
                #{:transform :object-type :dataset :media-set :media-set-items} rows-entry}))

(defn- python-inputs
  [resolved prev-rows]
  (reduce-kv
   (fn [acc alias {:keys [kind media-refs aliases] :as in}]
     (let [entry (cond-> ((get manifest-entries kind rows-entry) in)
                   media-refs (assoc :media-refs media-refs)
                   aliases    (assoc :aliases aliases))]
       (cond-> acc
         (:path entry)      (update :input-paths conj (:path entry))
         true               (assoc-in [:inputs-manifest alias] entry))))
   (cond-> {:input-paths [] :inputs-manifest {}}
     prev-rows (assoc :previous-rows prev-rows))
   resolved))

(defn- clojure-inputs
  [resolved prev-rows]
  {:inputs resolved :prev-rows prev-rows})

(def runners
  {:python  {:exec           ext/exec!
             :prepare-inputs python-inputs
             :introspect     (fn [{:keys [name code]}]
                               (ext/discover-transform-spec {:transform-name name :code code}))
             :egress?        true
             :media-by-path? true
             :media-spool?   true}
   :clojure {:guard          cljt/refuse-external-systems!
             :exec           cljt/exec!
             :media-in-run?  true
             :prepare-inputs clojure-inputs
             :introspect     (fn [{:keys [code]}] (cljt/declared-transform-spec code))}})

(defn- runner
  [language]
  (or (runners language)
      (refusal/refuse! :refusal.foreign/no-runner {:language language})))

(defn prepare-inputs
  [language resolved prev-rows]
  ((:prepare-inputs (runner language)) resolved prev-rows))

(defn egress?
  [language]
  (boolean (:egress? (runners language))))

(defn- resolve-lm-service!
  [compute lm-service]
  (let [uses-lm? (some llm-models/language-model-input? (vals (:inputs compute)))
        lm       (when uses-lm? (force lm-service))]
    (when (and uses-lm? (nil? lm))
      (refusal/refuse! :refusal.foreign/lm-service-unconfigured
                       {:transform (:transform-name compute) :reason :lm-service-unconfigured}))
    lm))

(defn- land-media-dir!
  [blobs refs]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "sutra_media_" (into-array java.nio.file.attribute.FileAttribute [])))]
    (try
      (doseq [r refs]
        (when-let [is (blobs/open blobs r)]
          (with-open [in is
                      out (io/output-stream (io/file dir (str r ".bin")))]
            (io/copy in out))))
      (.getAbsolutePath dir)
      (catch Throwable t (ext/delete-tree! dir) (throw t)))))

(defn- media-reach
  [media-by-path? opts landed]
  (if (or (not media-by-path?) (ext/media-by-url?))
    opts
    (reduce-kv (fn [opts alias {:keys [media-refs] :as e}]
                 (if (seq media-refs)
                   (let [d (land-media-dir! (force (:blobs opts)) media-refs)]
                     (swap! landed conj d)
                     (-> opts
                         (assoc-in [:inputs-manifest alias]
                                   (-> e (dissoc :media-refs) (assoc :media-dir d)))
                         (update :input-paths (fnil conj []) d)))
                   opts))
               opts (:inputs-manifest opts))))

(defn- spooled-file
  ^java.io.File [spool name]
  (let [f (io/file spool (str name))]
    (when-not (and spool (= (.getParentFile (.getCanonicalFile f))
                            (.getCanonicalFile (io/file spool)))
                   (.isFile f))
      (throw (ex-info "spooled media item outside its spool" {:spool spool :name name})))
    f))

(defn- land-media! [{:keys [media-spool] :as opts} media]
  (doseq [{:keys [hash mime b64 spool]} media]
    (let [h (if spool
              (with-open [in (io/input-stream (spooled-file media-spool spool))]
                (ext/record-media! opts in mime))
              (ext/record-media! opts (.decode (Base64/getDecoder) ^String b64) mime))]
      (when (and hash (not= hash h))
        (throw (ex-info "media item content-address mismatch (corrupt transport?)"
                        {:declared hash :actual h}))))))

(defn payload [{:keys [rows outputs trace pending-observed log media-set-items] :as result}]
  (cond-> (if outputs {:outputs outputs} {:rows rows})
    (seq trace)                                    (assoc :trace trace)
    (seq media-set-items)                          (assoc :media-set-items media-set-items)
    (not (get result :incremental-complete true))  (assoc :incremental-complete false)
    (some? pending-observed)                       (assoc :pending-observed pending-observed)
    (seq log)                                      (assoc :log log)))

(defn- row-count [{:keys [rows outputs]}]
  (if outputs (reduce + 0 (map count (vals outputs))) (count rows)))

(defn- in-image
  [{:keys [transform-name container] :as compute} opts]
  (if container
    [(assoc compute :image (images/for-run! (ctx/system-store) transform-name container))
     (assoc opts :open-layer (fn [address] (blobs/open (force (:blobs opts)) address)))]
    [compute opts]))

(defn run-code!
  [language {:keys [transform-name code] :as compute} {:keys [hook] :as opts}]
  (let [landed (atom [])]
    (try
      (let [{:keys [guard exec media-in-run? media-by-path? media-spool?]}
            (or (runners language)
                (refusal/refuse! :refusal.foreign/no-runner
                                 {:transform transform-name :language language}))]
        (when guard (guard compute))
        (when-let [b (:before hook)] (b transform-name))
        (let [t0     (System/nanoTime)
              lm     (resolve-lm-service! compute (:lm-service opts))
              [compute opts] (in-image compute opts)
              opts   (media-reach media-by-path? opts landed)
              spool  (when media-spool? (ext/media-spool!))
              _      (some->> spool (swap! landed conj))
              opts   (cond-> opts spool (assoc :media-spool spool))
              result (exec compute
                           (cond-> (dissoc opts :lm-service)
                             (not media-in-run?) (dissoc :blobs :media-sink)
                             (some? lm)          (assoc :lm-service lm)))]
          (when-let [a (:after hook)]
            (a transform-name
               (Math/round (double (/ (- (System/nanoTime) t0) 1e6)))
               (row-count result)
               (some-> code digest/sha256-hex)))
          (land-media! opts (:media result))
          (payload result)))
      (finally (run! ext/delete-tree! @landed)))))

(def module
  {:name     :compute-runner
   :provides {:compute/runner {:run-code!      run-code!
                               :run-query!     sql/run-query!
                               :query-runnable? sql/runnable?
                               :egress?        egress?
                               :prepare-inputs prepare-inputs
                               :spans          ext/spans}}})
