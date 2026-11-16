(ns sutra.compute.clojure
  (:require [sutra.foundation.encoding :as enc]
            [sutra.foundation.ops :as op]
            [sutra.kit.effects :as effects]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.coerce :refer [->num0]]
            [sutra.kit.err :as err]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.edn]
            [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as str]
            [clojure.walk]
            [sutra.blobs.api :as blobs]
            [sutra.types.kinds :as kinds]
            [sutra.types.naming :as naming]
            [sutra.types.table :as tbl]
            [sutra.parser.fn-params :as fn-params]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.compute.bounded :as bounded]
            [sutra.compute.external :as ext]
            [sutra.compute.sandbox :as sandbox]
            [sutra.foundation.outbound :as out]
            [sci.core :as sci]))

(set! *warn-on-reflection* true)

(def ^:dynamic ^:private *run* nil)

(defn- run-state
  [{:keys [prev-rows output-enums trace? media-dirs]}]
  {:prev-rows     (vec prev-rows)
   :output-enums  (or output-enums {})
   :trace?        (boolean trace?)
   :media-dirs    (or media-dirs {})
   :trace         (atom [])
   :more-remaining (atom false)
   :pending-observed (atom nil)
   :chat-calls    (atom 0)})

(defn- normalize-value
  [v]
  (cond
    (keyword? v)   (name v)
    (symbol? v)    (name v)
    (ratio? v)     (double v)
    (char? v)      (str v)
    (instance? java.time.temporal.TemporalAccessor v) (str v)
    (instance? java.time.temporal.TemporalAmount v)   (str v)
    (set? v)       (mapv normalize-value v)
    (sequential? v) (mapv normalize-value v)
    :else          v))

(defn- normalize-rows
  [rows where]
  (when-not (or (nil? rows) (sequential? rows))
    (refusal/refuse! :refusal.foreign/rows-not-sequence {:got (type rows)} {:where where}))
  (mapv (fn [row]
          (when-not (map? row)
            (refusal/refuse! :refusal.foreign/row-not-map {:row row}
                             {:where where :type (pr-str (type row))}))
          (reduce-kv (fn [m k v] (assoc m (keyword k) (normalize-value v))) {} row))
        rows))

(defn- trace!
  [event & {:as fields}]
  (when (:trace? *run*)
    (swap! (:trace *run*) conj (merge {:event (str event)} fields))))

(defn- is-incremental
  []
  (boolean (:incremental-run? *run*)))

(defn- previous-output
  []
  (let [rows (:prev-rows *run*)]
    (trace! "previous_output" :rows (count rows))
    rows))

(defn- output-enum
  [attr]
  (get (:output-enums *run*) (name attr)))

(defn- cap-batch
  [items n]
  (let [items (vec items)]
    (reset! (:pending-observed *run*) (count items))
    (when (> (count items) n)
      (when (:refuse-cap? *run*)
        (refusal/refuse! :refusal.compute/cap-batch-over {:items (count items) :cap n}))
      (reset! (:more-remaining *run*) true)
      (trace! "cap_batch" :took n :total (count items)))
    (vec (take n items))))

(defn- ->bytes
  ^bytes [data]
  (cond
    (bytes? data)  data
    (string? data) (.getBytes ^String data "UTF-8")
    :else (refusal/refuse! :refusal.foreign/media-not-bytes {:type (type data)})))

(defn- put-media-item
  ([data] (put-media-item data "application/octet-stream"))
  ([data mime]
   (let [h (ext/record-media! *run* (->bytes data) mime)]
     (trace! "put_media_item" :hash h :mime mime)
     h)))

(defn- media-bytes
  ^bytes [ref]
  (when (and (string? ref) (blobs/valid-address? ref))
    (or (some (fn [dir]
                (let [f (io/file dir (str ref ".bin"))]
                  (when (.exists f)
                    (with-open [in (io/input-stream f)] (.readAllBytes in)))))
              (vals (:media-dirs *run*)))
        (when-let [is (blobs/open (force (:blobs *run*)) ref)]
          (with-open [in is] (.readAllBytes in))))))

(defn- media-text
  [ref]
  (some-> (media-bytes ref) (String. "UTF-8")))

(defn- model-input
  [alias]
  (let [ins (:model-inputs *run*)]
    (or (get ins (keyword alias))
        (refusal/refuse! :refusal.foreign/no-model-input {:alias alias :have (vec (keys ins))}))))

(defn- chat
  ([alias messages] (chat alias messages nil))
  ([alias messages opts]
   (let [{:keys [source model]} (model-input alias)
         {:keys [url token]}    (:lm-service *run*)
         payload (merge {:model model
                         :max_tokens (or (:max-tokens opts) 1024)
                         :messages messages}
                        (when-let [t (:tools opts)] {:tools t})
                        (when-let [tc (:tool-choice opts)] {:tool_choice tc})
                        (dissoc opts :tools :tool-choice :max-tokens))
         body    {:source source :model model
                  :caller (name (:transform-name *run*))
                  :run_id (:run-id *run*)
                  :request payload}
         resp    (effects/call! (:work *run*)
                                #(do (swap! (:chat-calls *run*) inc)
                                     (trace! "chat" :model model)
                                     (out/post! url
                                                {:headers
                                                 (cond-> {"content-type" "application/json"}
                                                   token
                                                   (assoc "authorization"
                                                          (str "Bearer " token)))
                                                 :body    (json/write-str body)
                                                 :timeout 300000})))]
     (when (:error resp)
       (refusal/refuse! :refusal.compute/model-call-failed {:alias alias :model model}
                        {:why (str (:error resp))} (:error resp)))
     (when (>= (:status resp) 400)
       (refusal/refuse! :refusal.compute/model-call-status
                        {:alias alias :model model :status (:status resp)}
                        {:body (some-> (:body resp) (subs 0 (min 500 (count (:body resp)))))}))
     (json/read-str (:body resp) :key-fn keyword))))

(defn- tool-args
  [response tool-name]
  (some (fn [choice]
          (some (fn [tc]
                  (let [f (:function tc)]
                    (when (= (name tool-name) (:name f))
                      (enc/json-or-nil (or (:arguments f) "{}") :key-fn keyword))))
                (get-in choice [:message :tool_calls])))
        (:choices response)))

(defn- tool-call
  ([alias tool messages] (tool-call alias tool messages nil))
  ([alias tool messages opts]
   (let [nm (get-in tool [:function :name])]
     (-> (chat alias messages
               (assoc opts :tools [tool]
                      :tool-choice {:type "function" :function {:name nm}}))
         (tool-args nm)))))

(def ^:private denied
  '[eval load-string load-file load-reader read-string requiring-resolve
    resolve intern alter-var-root with-redefs with-redefs-fn
    slurp spit future future-call pmap agent send send-off shutdown-agents
    add-tap tap> System])

(def ^:private spec-meta-key
  :sutra/transform)

(def declared-spec-keys
  [:inputs :output :outputs :output-columns :primary-key :column-map
   :incremental :profile :semantic-version :external-systems])

(defn- declare-transform
  [spec f]
  (when-not (map? spec)
    (refusal/refuse! :refusal.foreign/transform-spec-not-map {:spec spec}
                     {:type (pr-str (type spec))}))
  (when-not (fn? f)
    (refusal/refuse! :refusal.foreign/transform-fn-not-fn {:got (type f)}))
  (with-meta f {spec-meta-key spec}))

(defn- round-scale
  [x scale]
  (-> (bigdec x) (.setScale (int scale) java.math.RoundingMode/HALF_UP) double))

(defn- ->dec
  ([x] (cond
         (decimal? x) x
         (string? x)  (bigdec (str/trim x))
         (number? x)  (bigdec x)
         :else        (refusal/refuse! :refusal.foreign/dec-not-number {:got (type x)})))
  ([x scale] (.setScale ^java.math.BigDecimal (->dec x) (int scale)
                        java.math.RoundingMode/HALF_UP)))

(defn- call-with-math-context
  [precision rounding f]
  (binding [*math-context* (java.math.MathContext.
                            (int precision)
                            (java.math.RoundingMode/valueOf (name (or rounding 'HALF_UP))))]
    (f)))

(defn- with-precision-macro
  [_form _env precision & exprs]
  (let [[rounding body] (if (= :rounding (first exprs))
                          [(second exprs) (nnext exprs)]
                          [nil exprs])]
    `(sutra/-call-with-math-context ~precision '~rounding (fn [] ~@body))))

(def ^:private value-classes
  '[java.math.BigDecimal java.math.BigInteger java.math.RoundingMode java.math.MathContext
    java.time.LocalDate java.time.LocalDateTime java.time.LocalTime java.time.YearMonth
    java.time.Year java.time.MonthDay java.time.DayOfWeek java.time.Month java.time.Instant
    java.time.Duration java.time.Period java.time.ZonedDateTime java.time.OffsetDateTime
    java.time.ZoneId java.time.ZoneOffset java.time.temporal.ChronoUnit
    java.time.temporal.ChronoField java.time.temporal.TemporalAdjusters
    java.time.temporal.IsoFields java.time.format.DateTimeFormatter java.util.Locale])

(def ^:private sci-classes
  (into {} (map (juxt identity resolve)) value-classes))

(def ^:private sci-imports
  (into {} (map (juxt #(symbol (peek (str/split (name %) #"\."))) identity)) value-classes))

(defn- sci-context
  []
  (sci/init
   {:namespaces {'clojure.core {'parse-double parse-double
                                'parse-long   parse-long
                                'abs          abs
                                'with-precision (with-meta with-precision-macro {:sci/macro true})}
                 'clojure.string (sci/copy-ns clojure.string (sci/create-ns 'clojure.string))
                 'str            (sci/copy-ns clojure.string (sci/create-ns 'str))
                 'clojure.set    (sci/copy-ns clojure.set (sci/create-ns 'clojure.set))
                 'set            (sci/copy-ns clojure.set (sci/create-ns 'set))
                 'clojure.walk   (sci/copy-ns clojure.walk (sci/create-ns 'clojure.walk))
                 'walk           (sci/copy-ns clojure.walk (sci/create-ns 'walk))
                 'clojure.edn    {'read-string clojure.edn/read-string}
                 'edn            {'read-string clojure.edn/read-string}
                 'json           {'read-str  (fn [s & opts]
                                               (apply json/read-str s :key-fn keyword opts))
                                  'write-str json/write-str}
                 'sutra          {'transform       declare-transform
                                  'num             ->num0
                                  'round           round-scale
                                  'dec             ->dec
                                  '-call-with-math-context call-with-math-context
                                  'previous-output previous-output
                                  'is-incremental is-incremental
                                  'output-enum     output-enum
                                  'trace           trace!
                                  'cap-batch       cap-batch
                                  'put-media-item  put-media-item
                                  'media-bytes     media-bytes
                                  'media-text      media-text
                                  'chat            chat
                                  'tool-call       tool-call
                                  'tool-args       tool-args}}
    :classes sci-classes
    :imports sci-imports
    :deny denied}))

(defn- stack-trace
  [e]
  (some->> (sci/stacktrace e) seq sci/format-stacktrace (str/join "\n")))

(def compile-ms
  10000)

(defn- compiled
  [ctx code]
  (let [r (try [:ok (bounded/call "compile" compile-ms #(sci/eval-string* ctx code))]
               (catch InterruptedException e (throw e))
               (catch Exception e [:err e]))]
    (if (= [:ok ::bounded/timeout] r)
      [:err (ex-info (str "no result within " (quot compile-ms 1000) " s") {:reason :timeout})]
      r)))

(defn introspect
  [code]
  (let [[k v] (compiled (sci-context) code)
        v     (if (= :ok k)
                v
                (refusal/refuse! :refusal.compute/compile-failed
                                 {:reason :compile}
                                 {:error (ex-message v)
                                  :trace (if-let [trace (stack-trace v)] (str "\n" trace) "")}
                                 v))
        var?  (instance? sci.lang.Var v)
        f     (if var? @v v)
        spec  (get (meta v) spec-meta-key)]
    (when-not (fn? f)
      (refusal/refuse! :refusal.foreign/last-form-not-fn {:reason :not-a-fn :type (str (type v))}
                       {:type (pr-str (type v))}))
    (when (and spec (not (map? spec)))
      (refusal/refuse! :refusal.foreign/spec-decl-not-map {:reason :bad-spec :spec spec}
                       {:key spec-meta-key :type (pr-str (type spec))}))
    (when-let [unknown (seq (remove (set declared-spec-keys) (keys spec)))]
      (refusal/refuse! :refusal.foreign/unknown-spec-keys
                       {:reason :unknown-spec-keys :unknown (vec unknown)}
                       {:key spec-meta-key :allowed declared-spec-keys}))
    {:fn f :spec (not-empty (select-keys spec declared-spec-keys))}))

(defn compile-transform
  [code]
  (:fn (introspect code)))

(defn declared-transform-spec
  [code]
  (:spec (introspect code)))

(defn- csv-rows
  [path]
  (with-open [r (io/reader path)]
    (let [[header & rows] (doall (csv/read-csv r))
          ks (mapv #(keyword (str/trim (str %))) header)]
      (mapv #(zipmap ks %) rows))))

(defn- refuse-folded-key!
  [m k input]
  (when (keyword? k)
    (let [col (naming/read-key k)]
      (when (and (not= col k) (contains? m col))
        (refusal/refuse! :refusal.foreign/folded-column {:input input :key k :column col})))))

(deftype InputRow [^clojure.lang.IPersistentMap m aliases input]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [_ k nf]
    (cond
      (contains? m k)       (get m k)
      (contains? aliases k) (get m (get aliases k) nf)
      :else                 (do (refuse-folded-key! m k input) nf)))
  clojure.lang.IPersistentMap
  (assoc [_ k v] (assoc m k v))
  (assocEx [_ k v] (.assocEx m k v))
  (without [_ k] (dissoc m k))
  (containsKey [_ k] (or (contains? m k)
                         (and (contains? aliases k) (contains? m (get aliases k)))))
  (entryAt [this k] (when (.containsKey this k) (clojure.lang.MapEntry. k (.valAt this k))))
  (count [_] (count m))
  (cons [_ o] (conj m o))
  (empty [_] {})
  (equiv [_ o] (= m o))
  (seq [_] (seq m))
  (iterator [_] (.iterator ^Iterable m))
  clojure.lang.MapEquivalence
  java.util.Map
  (size [_] (count m))
  (isEmpty [_] (empty? m))
  (containsValue [_ v] (.containsValue ^java.util.Map m v))
  (get [this k] (.valAt this k))
  (keySet [_] (.keySet ^java.util.Map m))
  (values [_] (.values ^java.util.Map m))
  (entrySet [_] (.entrySet ^java.util.Map m))
  (put [_ _ _] (throw (UnsupportedOperationException.)))
  (remove [_ _] (throw (UnsupportedOperationException.)))
  (putAll [_ _] (throw (UnsupportedOperationException.)))
  (clear [_] (throw (UnsupportedOperationException.)))
  clojure.lang.IFn
  (invoke [this k] (.valAt this k))
  (invoke [this k nf] (.valAt this k nf))
  clojure.lang.IKVReduce
  (kvreduce [_ f init] (reduce-kv f init m))
  clojure.lang.IObj
  (meta [_] (meta m))
  (withMeta [_ mm] (InputRow. (with-meta m mm) aliases input))
  clojure.lang.IHashEq
  (hasheq [_] (hash m))
  Object
  (hashCode [_] (.hashCode ^Object m))
  (equals [_ o] (.equals ^Object m o))
  (toString [_] (str m)))

(defmethod print-method InputRow [^InputRow r ^java.io.Writer w]
  (print-method (.-m r) w))

(defn- input-rows
  [alias aliases rows]
  (let [aliases (or aliases {})]
    (mapv #(if (map? %) (InputRow. % aliases alias) %) rows)))

(def ^:private bindings
  (tbl/per-key "compute/clojure bindings" kinds/compute-input-kinds
               {#{:transform :object-type :dataset :media-set}
                (fn [alias {:keys [rows aliases]}] (input-rows alias aliases rows))
                :scalar         (fn [_ in] (:value in))
                :language-model (fn [_ in] in)
                :datasource
                (fn [alias {:keys [path]}]
                  (if (and path (str/ends-with? (str/lower-case path) ".csv"))
                    (csv-rows path)
                    (refusal/refuse! :refusal.foreign/datasource-not-csv
                                     {:alias alias :path path})))
                #{:media-set-items :media} nil}))

(defn- bind-input
  [alias {:keys [kind] :as in}]
  (if-let [bind (get bindings kind)]
    (bind alias in)
    (refusal/refuse! :refusal.foreign/unknown-input-kind {:alias alias :input in} {:kind kind})))

(defn- select-outputs
  [result compute]
  (let [tname (:transform-name compute)]
    (if (map? result)
      (do
        (when (and (:output compute) (empty? (:outputs compute)))
          (refusal/refuse! :refusal.foreign/single-output-map
                           {:transform tname :got-keys (vec (keys result))}))
        {:outputs (reduce-kv (fn [m k rows]
                               (assoc m (keyword k)
                                      (normalize-rows rows (str "transform " tname
                                                                " output " (pr-str k)))))
                             {} result)})
      {:rows (normalize-rows result (str "transform " tname))})))

(defn- wall-ms
  [compute]
  (* 1000 (:wall-seconds (sandbox/limits-for (:profile compute)))))

(defn refuse-external-systems! [{:keys [transform-name external-systems] :as compute}]
  (when (seq (remove (set (keep :source (filter llm-models/language-model-input?
                                                (vals (:inputs compute)))))
                     external-systems))
    (refusal/refuse! :refusal.foreign/clojure-external-systems
                     {:transform transform-name :external-systems external-systems})))

(defn exec!
  [compute
   {:keys [inputs prev-rows output-enums media-dirs trace? run-id lm-service blobs media-sink]
    build-work :work}]
  (let [tname (:transform-name compute)
        f     (compile-transform (:code compute))
        work  (cond-> (effects/work-token (+ (System/currentTimeMillis) (wall-ms compute)))
                build-work (assoc :cancelled (:cancelled build-work)))
        state (assoc (run-state {:prev-rows prev-rows :output-enums output-enums
                                 :trace? trace? :media-dirs media-dirs})
                     :transform-name tname
                     :refuse-cap? (:refuse-cap? compute)
                     :incremental-run? (:incremental-run? compute)
                     :blobs blobs
                     :media-sink media-sink
                     :run-id run-id
                     :lm-service lm-service
                     :work work
                     :model-inputs (into {} (filter (comp llm-models/language-model-input? val))
                                         (:inputs compute)))
        args  (reduce-kv (fn [m alias in] (assoc m alias (bind-input alias in)))
                         {} inputs)
        run   (bounded/start
               tname
               #(binding [*run* state]
                  (try (select-outputs (f args) compute)
                       (catch Exception e
                         (throw (ex-info (str "clojure transform " (pr-str tname)
                                              ": " (ex-message e)
                                              (when-let [trace (stack-trace e)]
                                                (str "\n" trace)))
                                         {:transform tname} e))))))
        res   (deref (:result run) (wall-ms compute) ::timeout)]
    (when (= ::timeout res)
      (effects/cancel! work)
      (bounded/abandon! run)
      (refusal/refuse! :refusal.foreign/transform-wall-clock {:transform tname :reason :wall-clock}
                       {:seconds (quot (wall-ms compute) 1000)
                        :profile (or (:profile compute) :default)}))
    (assoc (bounded/outcome res)
           :trace @(:trace state)
           :incremental-complete (not @(:more-remaining state))
           :pending-observed @(:pending-observed state))))

(def ^:dynamic ^:private *edit* nil)

(defn- edit-state []
  (or *edit* (refusal/refuse! :refusal.foreign/outside-edit {})))

(defn- obj-type [o]
  (or (:sutra/type (meta o))
      (refusal/refuse! :refusal.foreign/not-an-object {} {:object (pr-str o)})))

(defn- obj-pk [o]
  (let [t (obj-type o)]
    (if-let [p (get-in (edit-state) [:pk-attrs t])] (get o p) (:sutra/id (meta o)))))

(defn- obj-ref [o]
  {:type (obj-type o) :pk (obj-pk o) :id (:sutra/id (meta o))})

(defn- ask [req] ((:host (edit-state)) req))

(defn- record! [r] (swap! (:edits (edit-state)) conj r) nil)

(defn- onto-get [t pk] (ask {:op :get :type t :pk pk}))

(defn- onto-search
  ([t] (onto-search t {}))
  ([t opts] (ask (merge {:op :search :type t} (select-keys opts [:where :order-by :limit])))))

(defn- onto-links
  ([o side] (onto-links o side {}))
  ([o side opts]
   (if (:sutra/new (meta o))
     []
     (ask (merge {:op :links :source (obj-ref o) :side side}
                 (select-keys opts [:where :order-by :limit]))))))

(defn- onto-link [o side] (first (onto-links o side {:limit 1})))

(defn- edit-create
  ([t pk] (edit-create t pk {}))
  ([t pk vals]
   (let [p (get-in (edit-state) [:pk-attrs t])
         v (cond-> vals p (assoc p pk))]
     (record! {:kind :create :type t :pk pk :values v})
     (with-meta v {:sutra/type t :sutra/new true}))))

(defn- edit-modify [o vals]
  (let [p (get-in (edit-state) [:pk-attrs (obj-type o)])]
    (when (and p (contains? vals p) (not= (get vals p) (get o p)) (not (:sutra/new (meta o))))
      (refusal/refuse! :refusal.foreign/pk-immutable {:user-facing? true}
                       {:pk (name p) :type (name (obj-type o))}))
    (record! (assoc (obj-ref o) :kind :modify :values vals))
    (with-meta (merge o vals) (meta o))))

(defn- edit-delete [o] (record! (assoc (obj-ref o) :kind :delete)))

(defn- link-edit [op]
  (fn
    ([o side] (record! {:kind :link :op op :side side :source (obj-ref o)}))
    ([o side target] (record! {:kind :link :op op :side side :source (obj-ref o)
                               :target (obj-ref target)}))))

(defn- user-facing-error [msg]
  (refusal/refuse! :refusal.compute/user-message {:user-facing? true} {:msg (str msg)}))

(defn- edit-context []
  (sci/merge-opts
   (sci-context)
   {:namespaces {'ontology {'get onto-get 'search onto-search 'links onto-links 'link onto-link
                            'type-of obj-type 'pk obj-pk}
                 'edits    {'create edit-create 'modify edit-modify 'delete edit-delete
                            'add-link (link-edit :add) 'remove-link (link-edit :remove)
                            'set-link (link-edit :set) 'clear-link (link-edit :clear)}
                 'uuid     {'random #(str (random-uuid))}
                 'sutra    {'user-facing-error user-facing-error}}}))

(defn- user-facing [^Throwable e]
  (err/cause (comp :user-facing? ex-data) e))

(defn run-edit-function!
  [{:keys [code entry args pk-attrs wall-ms]} host]
  (let [queue (java.util.concurrent.LinkedBlockingQueue.)
        ask*  (fn [req]
                (let [p (promise)]
                  (.put queue [req p])
                  (let [[ok v] @p] (if ok v (throw v)))))
        state {:host ask* :edits (atom []) :pk-attrs pk-attrs}
        run   (bounded/start
               (str "edit-" entry)
               #(binding [*edit* state]
                  (let [ctx (edit-context)]
                    (sci/eval-string* ctx code)
                    (apply (sci/eval-string* ctx entry) args))))
        until (+ (System/currentTimeMillis) (or wall-ms 60000))]
    (loop []
      (cond
        (realized? (:result run))
        (try (bounded/outcome @(:result run))
             {:edits @(:edits state)}
             (catch Throwable e
               (if-let [uf (user-facing e)]
                 (throw (ex-info (ex-message uf) (ex-data uf)))
                 (refusal/refuse! :refusal.compute/edit-function-failed
                                  {:function entry}
                                  {:error (ex-message e)
                                   :trace (if-let [trace (stack-trace e)] (str "\n" trace) "")}
                                  e))))

        (> (System/currentTimeMillis) until)
        (do (bounded/abandon! run)
            (refusal/refuse! :refusal.foreign/edit-wall-clock {:function entry :reason :wall-clock}
                             {:seconds (quot (or wall-ms 60000) 1000)}))

        :else
        (do (when-let [[req p] (.poll queue 20 java.util.concurrent.TimeUnit/MILLISECONDS)]
              (deliver p (try [true (host req)] (catch Throwable t [false t]))))
            (recur))))))

(defn compile-edit-function
  [code]
  (if-let [{:keys [params] fname :name} (first (fn-params/over-limit code))]
    (str (if fname (str "`" fname "`") "a fn") " declares " params
         " parameters; Clojure takes at most " fn-params/limit
         " — drop the ones it can derive, or move the logic into a :library"
         " function that takes a map, and call that from the body")
    (let [[k v] (compiled (edit-context) code)]
      (when (= :err k) (ex-message v)))))

(op/defop :compute/check-edit-function [_ {:keys [code]}]
  (compile-edit-function code))

(def module
  {:name     :clojure-transforms
   :provides {:code/introspect-clojure (fn [body] (declared-transform-spec (:code body)))}})
