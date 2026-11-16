(ns sutra.compute.external
  (:require [sutra.foundation.periodic :as periodic]
            [sutra.kit.coll :as coll]
            [sutra.kit.noted :as noted]
            [sutra.foundation.settings :as settings]
            [sutra.foundation.egress :as egress-agents]
            [sutra.kit.effects :as effects]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.digest :as digest]
            [sutra.kit.time :as time]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [sutra.types.expectations :as xp]
            [sutra.types.kinds :as kinds]
            [sutra.types.rid :as rid]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [sutra.blobs.api :as blobs]
            [sutra.foundation.encoding :as enc]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.foundation.rpc :as rpc]
            [sutra.foundation.media-grant :as grant]
            [sutra.compute.cores :as cores]
            [sutra.compute.container :as ctr]
            [sutra.compute.protocol :as protocol]
            [sutra.compute.sandbox :as sandbox]
            [sutra.foundation.confine :as confine]
            [sutra.foundation.env :as env])
  (:import [java.util Base64]))

(set! *warn-on-reflection* true)

(declare xfer-dir parameter-values)

(def ^:private decimal-tag "~f")

(defn- to-wire
  [v]
  (cond
    (instance? java.math.BigDecimal v)
    (str decimal-tag (.toPlainString ^java.math.BigDecimal v))

    (string? v)
    (if (str/starts-with? ^String v "~") (str "~" v) v)

    (instance? java.sql.Date v)        (str (time/sql-date->local-date v))
    (instance? java.time.LocalDate v)  (str v)
    (instance? java.sql.Timestamp v)   (str (time/stored-timestamp->instant v))
    (instance? java.time.Instant v)    (str v)
    (instance? java.time.OffsetDateTime v) (str (.toInstant ^java.time.OffsetDateTime v))
    (instance? java.time.ZonedDateTime v)  (str (.toInstant ^java.time.ZonedDateTime v))
    (instance? java.time.LocalDateTime v)  (str v)

    :else v))

(defn- from-wire
  [v]
  (if (and (string? v) (str/starts-with? ^String v "~"))
    (if (str/starts-with? ^String v decimal-tag)
      (bigdec (subs v (count decimal-tag)))
      (subs v 1))
    v))

(defn- wire-values
  [f x]
  (cond
    (map? x)        (persistent! (reduce-kv (fn [m k v] (assoc! m k (wire-values f v)))
                                            (transient {}) x))
    (sequential? x) (mapv #(wire-values f %) x)
    :else           (f x)))

(defn- rows->wire [rows] (mapv #(wire-values to-wire %) rows))

(defn ->wire [x] (wire-values to-wire x))
(defn <-wire [x] (wire-values from-wire x))

(defn- temp-file!
  (^java.io.File [prefix suffix write!] (temp-file! prefix suffix nil write!))
  (^java.io.File [prefix suffix dir write!]
   (let [f (if dir
             (java.io.File/createTempFile prefix suffix (io/file dir))
             (java.io.File/createTempFile prefix suffix))]
     (write! f)
     f)))

(defn delete-tree!
  [f]
  (when f
    (let [f (io/file f)]
      (when (.isDirectory f)
        (doseq [c (.listFiles f)] (delete-tree! c)))
      (io/delete-file f true))))

(defn export-rows-json!
  (^String [rows] (export-rows-json! rows nil))
  (^String [rows dir]
   (.getAbsolutePath
    (temp-file! "sutra_in_" ".json" dir
                (fn [f] (with-open [w (io/writer f)] (json/write (rows->wire rows) w)))))))

(defn record-media!
  [{:keys [blobs media-sink]} in mime]
  (let [{h :hash sz :size} (blobs/put! (force blobs) in)]
    (when media-sink
      (swap! media-sink conj {:hash h
                              :mime (or mime "application/octet-stream")
                              :size sz}))
    h))

(def ^:private python-lib-dir
  (delay (-> (io/resource "python/sutra_transforms.py")
             io/file .getParentFile .getAbsolutePath)))

(defn- write-temp-module! ^java.io.File [t-name code]
  (temp-file! (str "sutra_t_" (name t-name) "_") ".py" #(spit % code)))

(defn- write-temp-siblings!
  [t-name modules]
  (into {} (for [[m src] modules]
             [m (temp-file! (str "sutra_t_" (name t-name) "_m_" m "_") ".py" #(spit % src))])))

(defn- sibling-paths [files] (update-vals files #(.getAbsolutePath ^java.io.File %)))

(defn- write-temp-manifest! ^java.io.File [t-name manifest]
  (temp-file! (str "sutra_t_" (name t-name) "_inputs_") ".json"
              #(spit % (json/write-str manifest))))

(defn- write-temp-input!
  ^java.io.File [t-name alias content]
  (temp-file! (str "sutra_t_" (name t-name) "_in_" (name alias) "_") ".dat"
              #(spit % content :encoding "UTF-8")))

(defn- write-temp-media-dir!
  ^java.io.File [t-name blobs]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      (str "sutra_media_" (name t-name) "_")
                      (into-array java.nio.file.attribute.FileAttribute [])))]
    (doseq [{blob-name :name b64 :b64} blobs]
      (with-open [out (io/output-stream (confine/confine-path dir blob-name))]
        (.write out ^bytes (.decode (Base64/getDecoder) ^String b64))))
    dir))

(defn- write-temp-secrets!
  ^java.io.File [t-name secrets]
  (temp-file! (str "sutra_t_" (name t-name) "_secrets_") ".json"
              (fn [^java.io.File f]
                (doto f
                  (.setReadable false false) (.setReadable true true)
                  (.setWritable false false) (.setWritable true true))
                (spit f (json/write-str secrets)))))

(defn- kw-rows
  [rows]
  (mapv (fn [row] (into {} (for [[k v] row] [(keyword k) (wire-values from-wire v)]))) rows))

(defn- kill-tree!
  [^Process p]
  (run! (fn [^java.lang.ProcessHandle h] (.destroyForcibly h))
        (iterator-seq (.iterator (.descendants (.toHandle p)))))
  (.destroyForcibly p))

(defn- run-process!*
  [argv log-path work]
  (let [f    (if log-path (io/file log-path) (java.io.File/createTempFile "sutra-xerr-" ".log"))
        p    (.start (doto (ProcessBuilder. ^java.util.List (mapv str argv))
                       (.redirectError (java.lang.ProcessBuilder$Redirect/appendTo f))))
        _    (.close (.getOutputStream p))
        work (or work effects/*work*)
        stop (when work
               (periodic/beating "sandbox-watch" 200
                                 (fn []
                                   (cond
                                     (not (.isAlive p))        nil
                                     (effects/cancelled? work) (do (kill-tree! p) nil)
                                     :else                     true))))
        out  (slurp (.getInputStream p) :encoding "UTF-8")
        exit (.waitFor p)
        _    (when stop (stop))
        err  (slurp f :encoding "UTF-8")]
    (when-not log-path (io/delete-file f true))
    {:exit exit :out out :err err}))

(defn- run-process!
  [argv log-path work]
  (effects/observed (str "EXEC " (first argv))
                    #(run-process!* argv log-path work)))

(defn- python-failure
  [err]
  (some (fn [line]
          (let [p (enc/json-or-nil line :key-fn keyword)]
            (when (and (map? p) (:error p))
              (str (some-> (:type p) (str ": ")) (:error p)
                   (some->> (:where p) (str " @ "))))))
        (reverse (remove str/blank? (str/split-lines (str err))))))

(defn spans
  [err]
  (let [lines  (remove str/blank? (str/split-lines (str err)))
        parsed (map (fn [l]
                      (if (or (str/starts-with? l "{\"sutra_span\"")
                              (str/starts-with? l "{\"sutra_intermediate\""))
                        (try [l (json/read-str l :key-fn keyword)]
                             (catch Exception e
                               [(str l "  [unreadable span: " (ex-message e) "]") nil]))
                        [l nil]))
                    lines)
        ints   (filter :sutra_intermediate (keep second parsed))
        events (remove :sutra_intermediate (keep second parsed))
        closed (coll/index-by :id (filter #(= "close" (:sutra_span %)) events))]
    {:spans (vec (for [o events :when (= "open" (:sutra_span o))
                       :let [c (get closed (:id o))]]
                   (-> (merge o c)
                       (dissoc :sutra_span :id :at_ms)
                       (assoc :at-ms (:at_ms o))
                       (cond-> (nil? c) (assoc :open true)))))
     :intermediates (into {} (map (juxt (comp keyword :sutra_intermediate) :value)) ints)
     :log   (vec (keep (fn [[l p]] (when-not p l)) parsed))}))

(defn- open-span
  [err]
  (when-let [{:keys [name at-ms] :as s} (last (filter :open (:spans (spans err))))]
    (str "still in " name
         (some->> (or (:model s) (:ref s)) (str " "))
         (when at-ms (str ", opened " (quot at-ms 1000) " s in")))))

(def ^:private timed-out-exit
  124)

(defn- log-file
  ^java.io.File []
  (if @xfer-dir
    (io/file @xfer-dir (str "xlog-" (java.util.UUID/randomUUID) ".log"))
    (java.io.File/createTempFile "sutra-xlog-" ".log")))

(defn- tail!
  [^java.io.File f on-line]
  (let [pos   (atom 0)
        buf   (java.io.ByteArrayOutputStream.)
        drain (fn []
                (when (and (.exists f) (> (.length f) (long @pos)))
                  (with-open [raf (java.io.RandomAccessFile. f "r")]
                    (.seek raf (long @pos))
                    (let [bs (byte-array (- (.length raf) (long @pos)))]
                      (.readFully raf bs)
                      (swap! pos + (alength bs))
                      (.write buf bs)))
                  (let [all (.toByteArray buf)
                        cut (long (loop [i (dec (alength all))]
                                    (cond (neg? i) -1
                                          (= 10 (aget all i)) i
                                          :else (recur (dec i)))))]
                    (when (>= cut 0)
                      (.reset buf)
                      (.write buf all (inc cut) (- (alength all) (inc cut)))
                      (doseq [line (str/split-lines (String. all 0 (inc cut) "UTF-8"))
                              :when (not (str/blank? line))]
                        (noted/quietly "python output line" (on-line line)))))))
        stop  (promise)
        fut   (future (loop [] (noted/quietly "python output" (drain))
                            (when-not (deref stop 500 false) (recur)))
                      (noted/quietly "python output" (drain)))]
    (fn [] (deliver stop true) (deref fut 5000 nil))))

(defn- sandbox-env
  [{:keys [transform-name] :as compute}
   {:keys [secrets-path previous-path trace? output-enums run-id lm-service media-spool]}
   has-lm?]
  (cond-> {"SUTRA_PROTOCOL_VERSION" protocol/version}
    media-spool   (assoc "SUTRA_MEDIA_SPOOL" media-spool)
    (seq (:parameters compute))
    (assoc "SUTRA_PARAMETERS" (json/write-str (parameter-values (:parameters compute))))
    secrets-path  (assoc "SUTRA_SECRETS" secrets-path)
    previous-path (assoc "SUTRA_PREVIOUS_OUTPUT" previous-path)
    (:refuse-cap? compute) (assoc "SUTRA_REFUSE_CAP" "1")
    (:incremental-run? compute) (assoc "SUTRA_INCREMENTAL_RUN" "1")
    trace?        (assoc "SUTRA_TRACE" "1")
    (seq output-enums) (assoc "SUTRA_OUTPUT_ENUMS" (json/write-str output-enums))
    has-lm?   (assoc "SUTRA_TRANSFORM_NAME" (name transform-name))
    (and has-lm? run-id) (assoc "SUTRA_RUN_ID" (str run-id))
    (and has-lm? lm-service) (assoc "SUTRA_LM_SERVICE" (json/write-str lm-service))))

(defn- process-spec
  [compute {:keys [input-paths profile wheelhouse secrets-path previous-path media-spool]}
   {:keys [lib-dir module-path sibling-paths manifest-path network? env]}]
  (let [wheelhouse (or wheelhouse (:wheelhouse compute))]
    (cond-> {:inner-cmd   (cond-> ["python3" "-m" "sutra_transforms" "--run" "/work/mod.py"]
                            manifest-path (conj "--inputs" manifest-path))
             :lib-dir     lib-dir
             :module-path module-path
             :sibling-paths sibling-paths
             :input-paths (cond-> (vec input-paths)
                            manifest-path (conj manifest-path)
                            secrets-path  (conj secrets-path)
                            previous-path (conj previous-path))
             :limits      (sandbox/limits-of compute profile)
             :network     network?}
      wheelhouse  (assoc :wheelhouse wheelhouse)
      media-spool (assoc :rw-paths [media-spool])
      (seq env)   (assoc :env env))))

(defn- process-result
  [transform-name {:keys [wall-seconds]} result]
  (when (and wall-seconds (= timed-out-exit (:exit result)))
    (throw (ex-info (str transform-name " timed out after " wall-seconds " s"
                         (some->> (open-span (:err result)) (str " — "))
                         " (@configure(allowed_run_duration=…) on a transform, :timeout-ms on a Function)")
                    {:transform transform-name
                     :timeout-ms (* 1000 wall-seconds)
                     :exit (:exit result)
                     :stderr (:err result)})))
  (when-not (zero? (:exit result))
    (throw (ex-info (str "python transform " transform-name " failed"
                         (some->> (python-failure (:err result)) (str " — ")))
                    {:transform transform-name
                     :exit (:exit result)
                     :stderr (:err result)
                     :stdout (:out result)})))
  (let [out    (str/trim (:out result))
        parsed (try (json/read-str out :key-fn keyword)
                    (catch Exception e
                      (throw (ex-info "python runner returned non-JSON stdout"
                                      {:transform transform-name
                                       :stdout out
                                       :stderr (:err result)} e))))]
    (when-not (protocol/compatible? (:protocol_version parsed))
      (throw (ex-info (str "python transform " transform-name " speaks runtime protocol "
                           (:protocol_version parsed) ", the host " protocol/version)
                      {:transform transform-name
                       :protocol (:protocol_version parsed)
                       :host-protocol protocol/version})))
    {:rows (some-> (:rows parsed) kw-rows)
     :outputs (some->> (:outputs parsed)
                       (reduce-kv (fn [m k v] (assoc m k (kw-rows v))) {}))
     :media (:media parsed)
     :media-set-items (:media_set_items parsed)
     :trace (:trace parsed)
     :incremental-complete (get parsed :incremental-complete true)
     :pending-observed (:pending-observed parsed)
     :log (some-> (:err result) str str/trim not-empty)}))

(defn manifest-paths
  [manifest]
  (into [] (comp (mapcat (juxt :path :media-dir)) (remove nil?)) (vals manifest)))

(defn- container-of
  [transform-name image open]
  (when-not open
    (refusal/refuse! :refusal.foreign/image-unreachable {:transform transform-name}))
  (let [rootfs (ctr/rootfs! image open)]
    {:rootfs rootfs :entries (ctr/root-entries rootfs) :resolv? (ctr/resolv? rootfs)
     :image image :driver @ctr/host-driver}))

(defn run-transform-local!
  ([compute] (run-transform-local! compute nil))
  ([{:keys [transform-name image] :as compute} {:keys [inputs-manifest] :as opts}]
   (run-transform-local!
    compute
    (cond-> opts
      image (-> (assoc :container (container-of transform-name image (:open-layer opts)))
                (update :input-paths (comp vec distinct concat) (manifest-paths inputs-manifest))
                (update :inputs-manifest ctr/remap-manifest)))
    :prepared))
  ([{:keys [transform-name code modules] :as compute}
    {:keys [inputs-manifest secrets-path lm-service log-path container] :as opts} _]
   (sandbox/require-sandbox! :refusal.compute/transform-sandbox-missing
                            {:transform transform-name})
   (let [has-lm?  (boolean (some llm-models/language-model-input? (vals inputs-manifest)))
         _        (when (and has-lm? (nil? lm-service))
                    (refusal/refuse! :refusal.foreign/sandbox-lm-unconfigured
                                     {:transform transform-name :reason :lm-service-unconfigured}))
         module   (write-temp-module! transform-name code)
         siblings (write-temp-siblings! transform-name modules)
         manifest (when (seq inputs-manifest)
                    (write-temp-manifest! transform-name inputs-manifest))
         spec     (process-spec compute opts
                                {:lib-dir       @python-lib-dir
                                 :module-path   (.getAbsolutePath module)
                                 :sibling-paths (sibling-paths siblings)
                                 :manifest-path (some-> manifest .getAbsolutePath)
                                 :network?      (boolean (or secrets-path (:network compute)
                                                             has-lm?
                                                             (some :media-url
                                                                   (vals inputs-manifest))))
                                 :env           (sandbox-env compute opts has-lm?)})
         spec     (cond-> spec container (assoc :container container))
         result   (try (cores/with-cores [cpus (get-in compute [:resources :cpu-cores])]
                         (run-process! (sandbox/wrap-argv (-> spec
                                                      (assoc-in [:limits :cpus] cpus)
                                                      (assoc-in [:env "SUTRA_CPU_CORES"]
                                                                (str (count cpus)))))
                                       log-path (:work opts)))
                       (finally (delete-tree! module) (delete-tree! manifest)
                                (run! delete-tree! (vals siblings))))]
     (process-result transform-name (sandbox/limits-of compute (:profile opts)) result))))

(def ^:private job-transport-ms (* 7 24 60 60 1000))

(defn- post-edn
  [url payload work & {:keys [timeout-ms]}]
  (rpc/call! {:work       work
              :url        url
              :peer       "transform worker"
              :timeout    (or timeout-ms
                              (* 1000 (long (:wall-seconds (sandbox/limits-for :xlarge)))))
              :retries    (settings/value :transform-retries)
              :backoff-ms (settings/value :transform-backoff-ms)}
             payload))

(def ^:private xfer-dir
  (delay (let [d (env/getenv "SUTRA_XFER_DIR")]
           (when (and (seq d) (.isDirectory (io/file d))) d))))

(def ^:private worker-api-url
  (delay (some-> (env/getenv "SUTRA_WORKER_API_URL") str/trim not-empty)))

(defn- remote-xfer?
  []
  (boolean (and (env/getenv "SUTRA_TRANSFORM_URL") @xfer-dir)))

(defn media-by-url?
  []
  (boolean (and (remote-xfer?) @worker-api-url)))

(defn sweep-xfer!
  [max-age-ms]
  (if-let [d @xfer-dir]
    (let [cut (- (System/currentTimeMillis) (long max-age-ms))
          old (filter #(< (.lastModified ^java.io.File %) cut) (.listFiles (io/file d)))]
      (run! delete-tree! old)
      (count old))
    0))

(defn media-spool!
  []
  (when (or (not (env/getenv "SUTRA_TRANSFORM_URL")) (remote-xfer?))
    (let [d (if (remote-xfer?)
              (doto (io/file @xfer-dir (str "xfs-" (java.util.UUID/randomUUID))) .mkdirs)
              (.toFile (java.nio.file.Files/createTempDirectory
                        "sutra_spool_" (into-array java.nio.file.attribute.FileAttribute []))))]
      (.getAbsolutePath ^java.io.File d))))

(defn- in-xfer? [p]
  (confine/within? (io/file @xfer-dir) (io/file p)))

(def ^:private spill-threshold
  (delay (or (some-> (env/getenv "SUTRA_XFER_SPILL_BYTES") Long/parseLong)
             (* 1 1024 1024))))

(defn- render
  [v]
  (if-not @xfer-dir
    [:inline (pr-str v)]
    (let [f (io/file @xfer-dir (str "xf-" (java.util.UUID/randomUUID) ".dat"))]
      (with-open [w (io/writer f :encoding "UTF-8")]
        (binding [*out* w] (pr v)))
      (if (> (.length f) ^long @spill-threshold)
        [:ref (.getAbsolutePath f)]
        (let [s (slurp f :encoding "UTF-8")]
          (io/delete-file f true)
          [:inline s])))))

(defn- read-ref
  [path]
  (slurp path :encoding "UTF-8"))

(defn- slurp-ref!
  [path]
  (let [s (slurp path :encoding "UTF-8")]
    (io/delete-file path true)
    s))

(defn- spill-result
  [result]
  (let [ref-of   (fn [k] (when (contains? result k)
                           (let [[kind v] (render (get result k))]
                             (when (= :ref kind) v))))
        rows-ref (ref-of :rows)
        outs-ref (ref-of :outputs)]
    (cond-> result
      rows-ref (-> (dissoc :rows) (assoc :rows-ref rows-ref))
      outs-ref (-> (dissoc :outputs) (assoc :outputs-ref outs-ref)))))

(defn- read-result-refs!
  [result]
  (cond-> result
    (:rows-ref result)    (-> (dissoc :rows-ref)
                              (assoc :rows (edn/read-string (slurp-ref! (:rows-ref result)))))
    (:outputs-ref result) (-> (dissoc :outputs-ref)
                              (assoc :outputs
                                     (edn/read-string (slurp-ref! (:outputs-ref result)))))))

(defn- read-media-blobs
  [dir]
  (into []
        (for [^java.io.File f (.listFiles (io/file dir))
              :when (.isFile f)]
          {:name (.getName f)
           :b64  (.encodeToString (Base64/getEncoder)
                                  (java.nio.file.Files/readAllBytes (.toPath f)))})))

(defn- xfer-copy!
  ^java.io.File [^java.io.File src suffix]
  (let [dst (io/file @xfer-dir (str "xfi-" (java.util.UUID/randomUUID) "-" suffix))]
    (io/copy src dst)
    dst))

(defn- xfer-copy-dir!
  ^java.io.File [dir]
  (let [dst (io/file @xfer-dir (str "xfd-" (java.util.UUID/randomUUID)))]
    (.mkdirs dst)
    (doseq [^java.io.File f (.listFiles (io/file dir)) :when (.isFile f)]
      (io/copy f (io/file dst (.getName f))))
    dst))

(defn- ship-inputs
  [manifest]
  (reduce-kv
   (fn [acc alias {:keys [path kind media-dir media-refs] :as entry}]
     (cond
       (kinds/is? :passed-as-is? kind) (assoc acc alias entry)
       (= :media kind)
       (assoc acc alias
              (cond-> entry
                (and media-dir @xfer-dir)
                (assoc :media-dir (.getAbsolutePath (xfer-copy-dir! media-dir)))
                (and media-dir (not @xfer-dir))
                (-> (dissoc :media-dir) (assoc :media-blobs (read-media-blobs media-dir)))
                media-refs
                (-> (dissoc :media-refs)
                    (assoc :media-url (str @worker-api-url "/api/xfer/media")
                           :media-grants (grant/issue media-refs (System/currentTimeMillis))))))
       (not path)
       (refusal/refuse! :refusal.foreign/remote-input-no-path {:alias alias :entry entry})
       @xfer-dir
       (assoc acc alias
              (cond-> (assoc entry :path
                             (if (in-xfer? path)
                               path
                               (.getAbsolutePath
                                (xfer-copy! (io/file path) (str (name alias) ".dat")))))
                media-dir  (assoc :media-dir (.getAbsolutePath (xfer-copy-dir! media-dir)))
                media-refs (-> (dissoc :media-refs)
                               (assoc :media-url (str @worker-api-url "/api/xfer/media")
                                      :media-grants (grant/issue media-refs
                                                                 (System/currentTimeMillis))))))
       :else
       (assoc acc alias
              (cond-> (-> entry (dissoc :path)
                          (assoc :content (slurp path :encoding "UTF-8")))
                media-dir (-> (dissoc :media-dir)
                              (assoc :media-blobs (read-media-blobs media-dir)))))))
   {} manifest))

(defn- fetch-secrets!
  [external-systems]
  (when (seq external-systems)
    (reduce (fn [acc {:keys [source-id] :as ref}]
              (assoc acc (name source-id)
                     (let [agent (egress-agents/agent-of (:spec ref))]
                       (:fields (rpc/call! {:url     (str (egress-agents/agent-url agent) "/secret")
                                            :peer    (str "egress agent " agent)
                                            :timeout 30000
                                            :token   (egress-agents/agent-token agent)}
                                           ref)))))
            {} external-systems)))

(defn- model-source-names
  [inputs]
  (into #{} (comp (filter llm-models/language-model-input?) (keep :source) (map name))
        (vals inputs)))

(defn- external-systems-for-secrets
  [external-systems inputs]
  (let [model-names (model-source-names inputs)]
    (into []
          (comp (map #(if (keyword? %) {:source-id %} %))
                (remove #(contains? model-names (name (:source-id %)))))
          external-systems)))

(defn- introspect-local!
  [{:keys [transform-name code modules]}]
  (sandbox/require-sandbox! :refusal.compute/transform-sandbox-missing
                           {:transform transform-name})
  (let [module   (write-temp-module! transform-name code)
        siblings (write-temp-siblings! transform-name modules)
        argv     (sandbox/wrap-argv {:inner-cmd ["python3" "-m" "sutra_transforms"
                                         "--introspect" "/work/mod.py"]
                             :lib-dir @python-lib-dir
                             :module-path (.getAbsolutePath module)
                             :sibling-paths (sibling-paths siblings)})
        result   (try (apply sh/sh argv)
                      (finally (delete-tree! module) (run! delete-tree! (vals siblings))))]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "introspect of transform " transform-name " failed"
                           (some->> (python-failure (:err result)) (str " — ")))
                      {:transform transform-name :exit (:exit result)
                       :stderr (:err result)})))
    (json/read-str (str/trim (:out result)) :key-fn keyword)))

(declare routes)

(defn- on-worker
  [route payload]
  (if-let [url (env/getenv "SUTRA_TRANSFORM_URL")]
    (post-edn (str url route) payload nil)
    ((get routes route) payload)))

(defn- entered
  [{:keys [entry-point] :as compute}]
  (cond-> compute
    entry-point (update :code str "\n__sutra_entry__ = " (json/write-str entry-point) "\n")))

(defn- introspect!
  [compute]
  (on-worker "/introspect" (select-keys (entered compute) [:transform-name :code :modules])))

(def ^:private kebab-key enc/kebab-key)

(def ^:private input-ref-fields
  (into #{} (keep #(:at (:input-ref %))) (vals kinds/kinds)))

(def ^:private input-edn-keys
  (into input-ref-fields [:kind :table :query :source :base-url]))

(def ^:private input-kw-vals
  (into #{:kind :source} (disj input-ref-fields :model)))

(def ^:private output-edn-keys
  (into #{:kind} (keep #(:at (:output-ref %))) (vals kinds/kinds)))
(def ^:private output-kw-vals output-edn-keys)

(defn- map-entry->edn [m keep kw-vals]
  (into {} (for [[k v] m
                 :let  [kk (kebab-key k)]
                 :when (and (some? v) (contains? keep kk))]
             [kk (if (and (contains? kw-vals kk) (not (rid/rid? v))) (keyword v) v)])))

(defn- incremental-value->edn
  [v]
  (cond
    (boolean? v)    v
    (sequential? v) (mapv keyword v)
    :else           (keyword v)))

(defn- incremental->edn
  [inc]
  (cond
    (true? inc) true
    (map? inc)  (not-empty (into {} (map (fn [[k v]] [(kebab-key k) (incremental-value->edn v)]))
                                 inc))
    :else       nil))

(defn- with-checks
  [entry spec]
  (cond-> entry (seq (:checks spec)) (assoc :checks (xp/checks-from-wire (:checks spec)))))

(defn- with-stops
  [entry spec]
  (cond-> entry
    (seq (:stop_propagating spec)) (assoc :stop-propagating (mapv keyword (:stop_propagating spec)))
    (seq (:stop_requiring spec))   (assoc :stop-requiring (mapv keyword (:stop_requiring spec)))))

(defn- output->edn-entry
  [nm spec]
  (cond-> (-> (map-entry->edn spec output-edn-keys output-kw-vals)
              (with-checks spec)
              (assoc :name (keyword (name nm))))
    (:primary_key spec)      (assoc :primary-key (keyword (:primary_key spec)))
    (seq (:column_map spec)) (assoc :column-map
                                    (into {} (map (fn [[k v]] [(keyword k) (keyword v)]))
                                          (:column_map spec)))))

(defn- outputs->edn
  [outputs order]
  (let [ks (or (seq order) (keys outputs))]
    (into [] (keep (fn [k]
                     (when-let [spec (get outputs (keyword (name k)))]
                       (output->edn-entry k spec))))
          ks)))

(defn done-key->edn
  [s]
  (let [[alias col] (str/split (str s) #"\." 2)]
    (when-not (and (seq alias) (seq col))
      (refusal/refuse! :refusal.foreign/done-key-shape {:done-key s}))
    {:input (keyword alias)
     :key   (keyword (str/replace col "_" "-"))}))

(defn prompt-key->edn
  [ks column]
  (let [parsed (for [s ks
                     :let [[alias col] (str/split (str s) #"\." 2)]]
                 (do (when-not (and (seq alias) (seq col))
                       (refusal/refuse! :refusal.foreign/prompt-key-shape {:prompt-key ks}
                                        {:entry s}))
                     [(keyword alias) (keyword (str/replace col "_" "-"))]))
        aliases (distinct (map first parsed))]
    (when (< 1 (count aliases))
      (refusal/refuse! :refusal.foreign/prompt-key-inputs {:prompt-key ks}
                       {:aliases (vec aliases)}))
    {:input   (first aliases)
     :columns (mapv second parsed)
     :column  (keyword (str/replace (or (some-> column str not-empty) "prompt_hash")
                                    "_" "-"))}))

(defn- parameters->edn
  [parameters]
  (update-vals parameters (fn [{:keys [type default]}]
                            {:type (keyword type) :default default})))

(defn parameter-values
  [parameters]
  (update-vals parameters :default))

(defn introspect->spec
  [{:keys [entry inputs output outputs output_order media_outputs external_systems parameters
           incremental semantic_version done_key max_per_run allowed_run_seconds
           prompt_key prompt_hash_column container]}]
  (let [out  (with-checks (map-entry->edn output output-edn-keys output-kw-vals) output)
        outs (outputs->edn outputs output_order)
        inc  (incremental->edn incremental)]
    (cond-> {:sutra.code/entry (keyword (or entry "transform"))
             :inputs (into {} (map (fn [[alias spec]]
                                     [alias (-> (map-entry->edn spec input-edn-keys input-kw-vals)
                                                (with-checks spec)
                                                (with-stops spec))]))
                           inputs)}
      (seq out)                     (assoc :output out)
      (seq outs)                    (assoc :outputs outs)
      (seq media_outputs)           (assoc :media-outputs
                                           (into {} (map (fn [[alias nm]]
                                                           [(keyword alias)
                                                            {:media-set (keyword nm)}]))
                                                 media_outputs))
      (:primary_key output)         (assoc :primary-key (keyword (:primary_key output)))
      (seq (:column_map output))    (assoc :column-map
                                           (into {} (map (fn [[k v]] [(keyword k) (keyword v)]))
                                                 (:column_map output)))
      (seq external_systems)        (assoc :external-systems (mapv keyword external_systems))
      (seq parameters)              (assoc :parameters (parameters->edn parameters))
      (some? inc)                   (assoc :incremental inc)
      (int? semantic_version)       (assoc :semantic-version semantic_version)
      (seq (str done_key))          (assoc :done-key (done-key->edn done_key))
      (int? max_per_run)            (assoc :max-per-run max_per_run)
      (int? allowed_run_seconds)    (assoc :allowed-run-seconds allowed_run_seconds)
      (seq prompt_key)              (assoc :prompt-key
                                           (prompt-key->edn prompt_key prompt_hash_column))
      (map? container)              (assoc :container (select-keys container [:image :tag])))))

(defn discover-transform-spec
  [compute]
  (introspect->spec (introspect! compute)))

(defn layer-reach
  [image api-url now-ms]
  (assoc image
         :layer-url (str api-url "/api/xfer/media")
         :layer-grants (grant/issue (:layers image) now-ms)))

(defn- run-transform-remote!
  [url {:keys [transform-name code modules profile resources wheelhouse external-systems
               parameters refuse-cap? incremental-run? build-job? allowed-run-seconds timeout-ms
               image]}
   {:keys [inputs-manifest previous-path output-enums lm-service trace? run-id log-path work
           media-spool]}]
  (when wheelhouse
    (refusal/refuse! :refusal.foreign/remote-wheelhouse {:transform transform-name}))
  (when (and image (not @worker-api-url))
    (refusal/refuse! :refusal.foreign/image-unreachable {:transform transform-name}))
  (let [shipped (ship-inputs inputs-manifest)
        prev-ref (when (and previous-path @xfer-dir)
                   (.getAbsolutePath (xfer-copy! (io/file previous-path) "previous.dat")))
        xfer-copies (when @xfer-dir
                      (cond-> (into [] (comp (mapcat (juxt :path :media-dir)) (remove nil?))
                                    (vals shipped))
                        prev-ref (conj prev-ref)))
        job (cond-> {:transform-name transform-name
                     :code code :profile profile
                     :inputs shipped}
              (seq modules) (assoc :modules modules)
              (seq resources) (assoc :resources resources)
              (seq external-systems) (assoc :external-systems external-systems)
              (seq parameters) (assoc :parameters parameters)
              refuse-cap? (assoc :refuse-cap? true)
              incremental-run? (assoc :incremental-run? true)
              build-job? (assoc :build-job? true)
              allowed-run-seconds (assoc :allowed-run-seconds allowed-run-seconds)
              timeout-ms (assoc :timeout-ms timeout-ms)
              image (assoc :image (layer-reach image @worker-api-url (System/currentTimeMillis)))
              (some? lm-service) (assoc :lm-service lm-service)
              (some? run-id) (assoc :run-id run-id)
              log-path (assoc :log-path log-path)
              media-spool (assoc :media-spool media-spool)
              (seq output-enums) (assoc :output-enums output-enums)
              trace? (assoc :trace? true)
              prev-ref (assoc :previous-path prev-ref)
              (and previous-path (not prev-ref))
              (assoc :previous-content (slurp previous-path :encoding "UTF-8")))
        [job-kind rendered] (render job)
        job-ref (when (= :ref job-kind) rendered)
        payload (if job-ref {:job-ref job-ref} job)]
    (try
      (read-result-refs! (post-edn (str url "/run-transform") payload work
                                   :timeout-ms (when build-job? job-transport-ms)))
      (finally
        (when job-ref (io/delete-file job-ref true))
        (doseq [p xfer-copies]
          (let [f (io/file p)]
            (when (.isDirectory f)
              (doseq [^java.io.File c (.listFiles f)] (io/delete-file c true)))
            (io/delete-file f true)))))))

(defn- run-transform-from-content!
  [{:keys [transform-name code modules profile resources inputs external-systems parameters
           refuse-cap? incremental-run? build-job? allowed-run-seconds timeout-ms
           previous-content image
           previous-path media-spool
           output-enums lm-service trace? run-id log-path]}]
  (let [written (atom [])
        own!    (fn [f] (swap! written conj f) f)]
    (try
      (let [manifest (reduce-kv
                      (fn [acc alias {:keys [content path kind media-blobs] :as entry}]
                        (cond
                          (kinds/is? :passed-as-is? kind) (assoc acc alias entry)
                          (= :media kind)
                          (assoc acc alias
                                 (cond-> entry
                                   (seq media-blobs)
                                   (-> (dissoc :media-blobs)
                                       (assoc :media-dir
                                              (.getAbsolutePath
                                               ^java.io.File
                                               (own! (write-temp-media-dir!
                                                      transform-name media-blobs)))))))
                          path (assoc acc alias entry)
                          :else
                          (let [^java.io.File f
                                (own! (write-temp-input! transform-name alias content))]
                            (assoc acc alias
                                   (cond-> (-> entry (dissoc :content)
                                               (assoc :path (.getAbsolutePath f)))
                                     (seq media-blobs)
                                     (-> (dissoc :media-blobs)
                                         (assoc :media-dir
                                                (.getAbsolutePath
                                                 ^java.io.File
                                                 (own!
                                                  (write-temp-media-dir!
                                                   transform-name media-blobs))))))))))
                      {} inputs)
            input-paths (into [] (comp (mapcat (juxt :path :media-dir)) (remove nil?))
                              (vals manifest))
            secrets (fetch-secrets! (external-systems-for-secrets external-systems inputs))
            ^java.io.File sf (when (seq secrets)
                               (own! (write-temp-secrets! transform-name secrets)))
            ^java.io.File pf (when previous-content
                               (own! (write-temp-input! transform-name :--previous--
                                                        previous-content)))
            prev (or previous-path (some-> pf .getAbsolutePath))]
        (spill-result
         (run-transform-local! {:transform-name transform-name :code code :modules modules
                                :profile profile :resources resources
                                :parameters parameters
                                :refuse-cap? refuse-cap? :incremental-run? incremental-run?
                                :build-job? build-job?
                                :allowed-run-seconds allowed-run-seconds
                                :timeout-ms timeout-ms
                                :image (some-> image (dissoc :layer-url :layer-grants))}
                               {:input-paths input-paths :inputs-manifest manifest
                                :secrets-path (some-> sf .getAbsolutePath)
                                :previous-path prev
                                :output-enums output-enums
                                :lm-service lm-service
                                :run-id run-id
                                :log-path log-path
                                :media-spool media-spool
                                :trace? trace?
                                :open-layer (when-let [u (:layer-url image)]
                                              (ctr/url-opener u (:layer-grants image)))})))
      (finally (run! delete-tree! @written)))))

(declare exec*)

(defn- exec-logged!
  [{:keys [transform-name] :as compute} opts]
  (if-let [sink (:log-line opts)]
    (let [f    (log-file)
          stop (tail! f #(sink transform-name %))]
      (try (exec* compute (assoc opts :log-path (.getAbsolutePath f)))
           (finally (stop) (io/delete-file f true))))
    (exec* compute opts)))

(defn exec!
  [compute {:keys [inputs-manifest previous-rows] :as opts}]
  (let [written (atom [])
        dir     (when (remote-xfer?) @xfer-dir)
        export! (fn [rows] (let [p (export-rows-json! rows dir)] (swap! written conj p) p))]
    (try
      (let [manifest (reduce-kv (fn [m alias e]
                                  (assoc m alias (if (contains? e :rows)
                                                   (-> e (dissoc :rows)
                                                       (assoc :path (export! (:rows e))))
                                                   e)))
                                {} inputs-manifest)]
        (exec-logged! compute
                      (cond-> (-> opts
                                  (dissoc :previous-rows)
                                  (assoc :inputs-manifest manifest)
                                  (update :input-paths (fnil into []) @written))
                        (some? previous-rows) (assoc :previous-path (export! previous-rows)))))
      (finally (run! delete-tree! @written)))))

(defn- exec* [compute opts]
  (if-let [url (env/getenv "SUTRA_TRANSFORM_URL")]
    (run-transform-remote! url (entered compute) opts)
    (let [{:keys [transform-name] :as compute} (entered compute)
          secrets (fetch-secrets! (external-systems-for-secrets
                                   (:external-systems compute) (:inputs compute)))
          sf (when (seq secrets) (write-temp-secrets! (or transform-name "xf") secrets))]
      (try
        (run-transform-local! compute (assoc opts :secrets-path (some-> sf .getAbsolutePath)))
        (finally (some-> sf .delete))))))

(def ^:private wheelhouse-root
  (or (env/getenv "SUTRA_WHEELHOUSE_DIR")
      (str (System/getProperty "java.io.tmpdir") "/sutra-wheelhouses")))

(def ^:private pip-index-url
  (env/getenv "SUTRA_PIP_INDEX_URL"))

(defn- deps-key
  [deps]
  (subs (digest/sha256-hex (str/join "\n" (sort (map str deps)))) 0 24))

(defn- provision-argv
  [target deps limits]
  (sandbox/jail-argv limits true ["/etc/pip.conf"]
             ["--bind" target target "--chdir" "/tmp"]
             (cond-> [["HOME" "/tmp"] ["TMPDIR" "/tmp"]]
               pip-index-url (conj ["PIP_INDEX_URL" pip-index-url]))
             (concat ["python3" "-m" "pip" "install"
                      "--target" target "--no-cache-dir" "--no-input" "--disable-pip-version-check"]
                     (map str deps))))

(defonce ^:private wheelhouse-locks (java.util.concurrent.ConcurrentHashMap.))

(defn- wheelhouse-lock
  [k]
  (.computeIfAbsent ^java.util.concurrent.ConcurrentHashMap wheelhouse-locks (str k)
                    (reify java.util.function.Function
                      (apply [_ _] (Object.)))))

(defn- ensure-wheelhouse!
  [deps]
  (when (seq deps)
    (sandbox/require-sandbox! :refusal.compute/deps-sandbox-missing {:deps deps})
    (let [k    (deps-key deps)
          dir  (io/file wheelhouse-root k)
          done (io/file dir ".sutra-wheelhouse-done")
          lock (wheelhouse-lock k)]
      (locking lock
        (when-not (.exists done)
          (.mkdirs dir)
          (let [limits (assoc (sandbox/limits-for :large) :wall-seconds 300 :cpu-seconds 240)
                r (apply sh/sh (provision-argv (.getAbsolutePath dir) deps limits))]
            (when-not (zero? (:exit r))
              (throw (ex-info "python dependency provisioning failed"
                              {:deps deps :exit (:exit r)
                               :stderr (:err r) :stdout (:out r)})))
            (spit done (str/join "\n" (sort (map str deps)))))))
      (.getAbsolutePath dir))))

(defn- run-python-local!
  [{:keys [label code python-deps stdin profile]}]
  (sandbox/require-sandbox! :refusal.compute/python-sandbox-missing {:label label})
  (println (str "[transform-worker] ad-hoc python " (some-> label name)))
  (let [wh     (ensure-wheelhouse! python-deps)
        module (write-temp-module! (or label :adhoc-python) code)
        argv   (sandbox/wrap-argv (cond-> {:inner-cmd   ["python3" "/work/mod.py"]
                                   :lib-dir     @python-lib-dir
                                   :module-path (.getAbsolutePath module)
                                   :limits      (sandbox/limits-for profile)
                                   :network     false}
                            wh (assoc :wheelhouse wh)))
        result (try (apply sh/sh (concat argv [:in (or stdin "")]))
                    (finally (delete-tree! module)))]
    (if (zero? (:exit result))
      (:out result)
      (throw (ex-info (str "ad-hoc python " (some-> label name) " failed")
                      {:exit (:exit result) :stderr (:err result)
                       :stdout (:out result)})))))

(defn run-python!
  [job]
  (on-worker "/run-python" job))

(defonce ^:private server* (atom nil))

(def ^:private routes
  {"/run-transform" (fn [body]
                      (let [job (if-let [ref (:job-ref body)]
                                  (edn/read-string (read-ref ref))
                                  body)
                            tag (str (:transform-name job) " run " (:run-id job))
                            t0  (System/nanoTime)]
                        (println (str "[transform-worker] start " tag))
                        (try (run-transform-from-content! job)
                             (finally
                               (println (str "[transform-worker] end " tag " after "
                                             (quot (- (System/nanoTime) t0) 1000000)
                                             " ms"))))))
   "/run-python"    #'run-python-local!
   "/introspect"    #'introspect-local!})

(defn handler [req] (rpc/handler routes req))

(defn start!
  [{:keys [port ip] :or {port 8732 ip "127.0.0.1"}}]
  (rpc/serve! server* #'handler {:port port :ip ip :max-body (* 64 1024 1024)})
  (println (str "transform-worker on " ip ":" port))
  @server*)

(defn -main [& _args]
  (let [port (Integer/parseInt (or (env/getenv "SUTRA_TRANSFORM_PORT") "8732"))
        ip   (or (env/getenv "SUTRA_TRANSFORM_IP") "127.0.0.1")]
    (start! {:port port :ip ip})
    @(promise)))

(def module
  {:name     :python-transforms
   :provides {:code/introspect-python
              (fn [body]
                (discover-transform-spec {:transform-name (:name body) :code (:code body)
                                          :modules (:modules body)
                                          :entry-point (:entry-point body)}))}})
