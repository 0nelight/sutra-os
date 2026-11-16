(ns sutra.cli.verbs.maintenance
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.out :refer [emit! mark paint prompt-yes? render say! warn!]]
            [sutra.i18n.core :as i18n]
            [clojure.string :as str]
            [sutra.wire.api :as api]))

(declare health-overview! sql-health!)

(defmethod step! :reindex [ctx [_ {:keys [object-type force?]}]]
  (let [r (api/reindex-object-type object-type {:force? force?})]
    (if (and (= 409 (:status r)) (:would-delete (:body r)))
      (let [{:keys [would-delete sample]} (:body r)]
        (warn! (mark :red "✋"
                     (str "reindex " (paint :bold (str object-type)) " REFUSED — would delete "
                          would-delete " object(s) with no source backing and no edit "
                          "provenance (likely writeback). Re-run with --force to proceed.")))
        (when (seq sample)
          (warn! (paint :dim
                        (str "  " (i18n/t :cli/sample) ": " (str/join ", " (take 5 sample))))))
        nil)
      (let [{:keys [deleted]} (req! "reindex" r)]
        (say! ctx (mark :green "✓"
                        (i18n/t :cli/reindexed
                                {:typ (paint :bold (str object-type)) :n (or deleted 0)})))
        nil))))

(defn- mib [b]
  (let [b (or b 0)]
    (cond (< b 1024)          (str b " B")
          (< b (* 1024 1024)) (format "%.1f KiB" (/ (double b) 1024))
          :else               (format "%.1f MiB" (/ (double b) 1024 1024)))))

(defn- report-then-apply!
  [ctx {:keys [apply? yes?]} {:keys [read render skip? nothing confirm cancelled]}]
  (let [report (read false)]
    (render report)
    (cond
      (not apply?)  nil
      (skip? report) (do (when nothing (say! ctx (mark :dim "•" nothing))) nil)
      (not (prompt-yes? (assoc ctx :yes? (or yes? (:yes? ctx))) (confirm report)))
      (do (say! ctx (mark :yellow "⏸" cancelled)) nil)
      :else (do (render (read true)) nil))))

(defmethod step! :retention-run [ctx [_ {:keys [dataset apply? grace-days]}]]
  (emit! ctx (req! "retention run"
                   (api/post! :dataset-retention (cond-> {:apply? (boolean apply?)}
                                                   dataset    (assoc :dataset dataset)
                                                   grace-days (assoc :grace-days grace-days)))))
  nil)

(defmethod step! :retention-policy-set
  [ctx [_ {:keys [name datasets older-than-days view-count transaction-count
                  views-older-than-days latest-view?]}]]
  (let [sels (cond-> []
               older-than-days       (conj ["older-than-days" older-than-days])
               view-count            (conj ["view-count" view-count])
               transaction-count     (conj ["transaction-count" transaction-count])
               views-older-than-days (conj ["only-in-views-older-than-days"
                                            views-older-than-days]))]
    (emit! ctx (req! "retention policy set"
                     (api/post! :retention-policies
                                {:name name
                                 :datasets (if (or (nil? datasets) (= "all" datasets))
                                             "all"
                                             (mapv str/trim (str/split datasets #",")))
                                 :selectors sels
                                 :allow-latest-view? (boolean latest-view?)}))))
  nil)

(defmethod step! :media-gc [ctx [_ {:keys [grace-days] :as a}]]
  (report-then-apply!
   ctx a
   {:read   (fn [go?] (req! (if go? "media gc apply" "media gc")
                            (api/post! :media-gc
                                       (cond-> {:grace-days grace-days} go? (assoc :apply? true)))))
    :render #(emit! ctx %)
    :skip?  (fn [r] (or (:blocked r) (zero? (:count (:unreferenced r) 0))))
    :confirm (fn [r] (i18n/t :cli/gc-confirm {:n (:count (:unreferenced r))
                                              :bytes (mib (:bytes (:unreferenced r)))}))
    :cancelled (i18n/t :cli/cancelled-nothing-deleted)}))

(defmethod step! :schema-code-blobs [ctx [_ {:keys [limit] :as a}]]
  (report-then-apply!
   ctx a
   {:read   (fn [go?] (req! (if go? "schema code-blobs apply" "schema code-blobs")
                            (api/post! :schema-code-blobs
                                       (cond-> {:limit limit} go? (assoc :apply? true)))))
    :render #(emit! ctx %)
    :skip?  (fn [r] (and (zero? (:bytes-freed (:code-blobs r) 0))
                         (zero? (:rows (:history-entities r) 0))
                         (zero? (:dropped (:compaction r) 0))))
    :nothing "nichts zu migrieren"
    :confirm (fn [r] (str "SCHREIBE " (:rows (:history-entities r) 0)
                          " Historienzeilen um? Die Bytes liegen vorher in der CAS, "
                          "jede Zeile wird einzeln konvertiert — ein Abbruch hinterlässt "
                          "eine Mischung, keine Lücke."))
    :cancelled "abgebrochen — nichts geschrieben"}))

(defmethod step! :prune-resolution [ctx [_ a]]
  (let [swept-n (fn [r] (let [{:keys [crosswalk provenance]} (:swept r)]
                          (+ (or crosswalk 0) (or provenance 0))))]
    (report-then-apply!
     ctx a
     {:read   (fn [go?] (req! (if go? "prune-resolution apply" "prune-resolution")
                              (api/post! :prune-resolution (if go? {:apply? true} {}))))
      :render #(emit! ctx %)
      :skip?  (fn [r] (zero? (swept-n r)))
      :confirm (fn [r] (str "DELETE these " (swept-n r) " row(s)? Entity resolution is the only "
                            "thing that ever read them, and no type on this branch "
                            "resolves any more — but this is a permanent delete."))
      :cancelled "cancelled — nothing deleted"})))

(defmethod step! :replay-history
  [ctx [_ {:keys [start? apply? verify? repair? drop-texts? dedupe? threads yes?]}]]
  (cond
    (and apply?
         (not (prompt-yes? (assoc ctx :yes? (or yes? (:yes? ctx)))
                           (str "Write the replayed history into every branch that still keeps"
                                " whole-world texts? Run --start first and read its report."))))
    (say! ctx (mark :yellow "⏸" "cancelled — nothing written"))

    (and repair?
         (not (prompt-yes? (assoc ctx :yes? (or yes? (:yes? ctx)))
                           (str "Reset and replay again every branch whose resource history a"
                                " replay alone wrote, then drop the versions nothing holds?"))))
    (say! ctx (mark :yellow "⏸" "cancelled — nothing written"))

    (and drop-texts?
         (not (prompt-yes? (assoc ctx :yes? (or yes? (:yes? ctx)))
                           (str "Delete the whole-world texts of every branch whose resources"
                                " now hold all of them (each verified first)?"))))
    (say! ctx (mark :yellow "⏸" "cancelled — nothing deleted"))

    (or start? apply? verify? repair? drop-texts? dedupe?)
    (emit! ctx (req! "replay history" (api/post! :replay-history {}
                                                 (cond-> {:apply? (boolean apply?)
                                                          :verify? (boolean verify?)
                                                          :repair? (boolean repair?)
                                                          :drop-texts? (boolean drop-texts?)
                                                          :dedupe? (boolean dedupe?)}
                                                   threads (assoc :threads threads)))))

    :else
    (emit! ctx (req! "replay history" (api/get! :replay-history)))))

(defmethod step! :restore-point [ctx [_ {:keys [label to yes?]}]]
  (cond
    to
    (let [{:keys [points]} (req! "restore points" (api/get!  :restore-point))
          point (first (filter #(= (long to) (long (:id %))) points))]
      (cond
        (not point) (say! ctx (mark :yellow "⚠" (str "no restore point " to)))
        (not (prompt-yes? (assoc ctx :yes? (or yes? (:yes? ctx)))
                          (i18n/t :cli/rollback-data-plane-confirm {:punkt to :label (:label point)
                                                                    :snapshot (:snapshot point)})))
        (say! ctx (mark :yellow "⏸" (i18n/t :cli/rollback-cancelled)))
        :else (emit! ctx (req! "rollback" (api/post! :restore-point {:to to} nil)))))

    label
    (emit! ctx (req! "restore point" (api/post! :restore-point {} {:label label})))

    :else
    (emit! ctx (req! "restore points" (api/get!  :restore-point))))
  nil)

(defmethod step! :prune-orphans [ctx [_ {:keys [yes?]}]]
  (let [{:keys [orphans]} (req! "orphan preview"
                                (api/post! :schema-prune-orphans {:dry-run (when true true)} nil))]
    (emit! ctx {:orphans orphans})
    (when (seq orphans)
      (if-not (prompt-yes? (assoc ctx :yes? (or yes? (:yes? ctx)))
                           (i18n/t :cli/drop-orphans-confirm {:n (count orphans)}))
        (say! ctx (mark :yellow "⏸" (i18n/t :cli/prune-cancelled)))
        (emit! ctx (req! "prune" (api/post! :schema-prune-orphans
                                            {:dry-run (when false true)} nil)))))
    nil))

(defmethod step! :health [ctx [_ {:keys [sql]}]]
  (if sql (sql-health! ctx) (health-overview! ctx)))

(defn- sql-health!
  [ctx]
  (let [{:keys [checked compiled? findings
                counts]} (req! "sql-check" (api/post! :schema-sql-check {}))
        {:keys [refused unknown]} counts
        bad? (or (not compiled?) (pos? (or refused 0)) (pos? (or unknown 0)))]
    (emit! ctx {:ok (not bad?) :compiled? compiled? :counts counts
                :checked checked :findings findings})
    (when bad? :abort)))

(defn- health-overview!
  [ctx]
  (let [{:keys [verdict checks build schedules]} (req! "health" (api/get! :health-overview))
        vk (keyword (or verdict "yellow"))]
    (emit! ctx {:ok (= vk :green) :verdict (clojure.core/name vk)
                :checks checks :build build :schedules schedules})
    nil))

(defn- mb [b] (if b (str (quot (long b) 1048576)) "–"))

(defn- node-line
  [{:keys [role node age-ms jvm process cgroup pools]}]
  (str (paint :bold (str (name (or role "?")))) " " node
       (when (pos? (or age-ms 0)) (paint :dim (str " (" (quot age-ms 1000) " s ago)")))
       "\n    heap " (mb (:heap-used jvm)) "/" (mb (:heap-max jvm)) " MB"
       " · rss " (mb (:rss process)) " MB · swap " (mb (:swap process)) " MB"
       (when (:memory cgroup)
         (str " · cgroup " (mb (:memory cgroup)) "/" (mb (:memory-max cgroup)) " MB"))
       (when-let [l (:cpu-load process)] (format " · cpu %.0f %%" (* 100 (double l))))
       " · threads " (:threads jvm)
       (when (seq pools)
         (str "\n    pools "
              (str/join ", " (for [[lane {:keys [active idle waiting max]}] pools]
                               (str (name lane) " " active "+" idle "/" max
                                    (when (pos? (or waiting 0)) (str " WAIT " waiting)))))))))

(defn- store-lines
  [{:keys [database-bytes settings connections catalog branch-schemas]}]
  (let [conns (reduce + (map :n connections))]
    (concat
     [(str (paint :bold "postgres") " db " (mb database-bytes) " MB · shared_buffers "
           (mb (:shared-buffers settings)) " MB · connections " conns "/"
           (:max-connections settings) " · relations " (reduce + (vals catalog))
           " (" (:tables catalog 0) " tables, " (:indexes catalog 0) " indexes)")]
     (for [[state {:keys [schemas tables empty bytes]}] (:by-state branch-schemas)]
       (str "    " (name state) ": " schemas " schemas · " tables " tables (" empty
            " empty) · " (mb bytes) " MB")))))

(defmethod render ::telemetry [_ {:keys [nodes agents store findings]}]
  [(map node-line nodes)
   (for [{:keys [agent heap]} agents]
     (str (paint :bold "egress") " " agent " heap " (:used-mb heap) "/" (:max-mb heap) " MB"))
   (when store (store-lines store))
   (if (seq findings)
     (for [{:keys [kind node says] :as f} findings]
       (mark :yellow "⚠" (str (name kind) (when node (str " " node)) " — " says
                             (when-let [n (:schemas f)]
                               (str " (" n " schemas, " (:tables f) " tables)")))))
     (mark :green "✓" "no findings"))])

(defmethod step! :telemetry [ctx [_ _]]
  (emit! ctx (req! "telemetry" (api/get! :telemetry)) ::telemetry))

(def ^:private error-levels "render-error,uncaught,promise-rejection,console-error,api-error")

(defmethod step! :client-log [ctx [_ {:keys [errors? level user since q limit stack?]}]]
  (let [params  (cond-> {}
                  errors?             (assoc :level error-levels)
                  (not-empty level)   (assoc :level level)
                  (not-empty user)    (assoc :user user)
                  (not-empty since)   (assoc :since since)
                  (not-empty q)       (assoc :q q)
                  limit               (assoc :limit limit))
        entries (vec (:entries (req! "client log" (api/get! :client-log-entries params))))]
    (emit! ctx {:count (count entries) :stack? (boolean stack?) :entries entries}
           ::client-log)))

(defn- client-log-entry-lines
  [stack? {:keys [at level user url message ua stack mapped]}]
  [(str (paint :dim (subs (str at) 0 (min 19 (count (str at))))) " "
        (paint (if (#{"render-error" "uncaught" "promise-rejection"} level) :red :yellow) level)
        " " user " " (paint :dim url))
   (str "  " message)
   (when ua (paint :dim (str "  " ua)))
   (when (not-empty mapped) (str/join "\n" (take (if stack? 200 8) (str/split-lines mapped))))
   (when (and stack? (not-empty stack)) (paint :dim stack))])

(defmethod render ::client-log [_ {n :count :keys [entries stack?]}]
  (if (zero? n)
    [(mark :green "✓" "no entries")]
    (into [(str n " entries, newest first")]
          (mapcat #(conj (client-log-entry-lines stack? %) "")) entries)))

(defn- setting-line
  [{:keys [key value source by at]}]
  (str "  " (paint :bold (name key)) " = " (pr-str value)
       (paint :dim (str "  (" (name source)
                        (when by (str " · " by (when at (str " " (subs (str at) 0 16)))))
                        ")"))))

(defmethod render ::setting [_ s]
  (when s
    (let [{:keys [doc default env type min]} s]
      [(setting-line s)
       (paint :dim (str "  " doc " · " (name type) (when min (str " ≥ " min))
                        " · default " (pr-str default)
                        (when env (str " · falls back to " env))))])))

(defmethod render ::settings [_ {:keys [groups settings]}]
  (for [{:keys [group doc]} groups
        :let [in (filterv #(= (name group) (name (:group %))) settings)]
        :when (seq in)]
    [(str (paint :bold (str/upper-case (name group))) " — " doc)
     (map setting-line in)]))

(defmethod step! :settings [ctx [_ {:keys [key value reset?]}]]
  (when (and reset? value)
    (throw (ex-info "`settings <key> --reset` takes no value" {:key key})))
  (when (and reset? (not key))
    (throw (ex-info "`settings --reset` needs the key it resets" {})))
  (let [{:keys [settings] :as r}
        (cond
          reset? (req! (str "reset " key) (api/post! :instance-settings {:reset [key]}))
          value  (req! (str "set " key) (api/post! :instance-settings {:set {key value}}))
          :else  (req! "settings" (api/get! :instance-settings)))
        shown (cond->> settings key (filterv #(= key (name (:key %)))))]
    (when (and key (empty? shown))
      (warn! (mark :red "✗" (str "no instance setting " key))))
    (if key
      (emit! ctx (first shown) ::setting)
      (emit! ctx r ::settings))))

(defmethod step! :egress-policy-propose [ctx [_ {:keys [address port why agents]}]]
  (emit! ctx (req! "propose" (api/post! :egress-policies
                                        (cond-> {:address address :port port :justification why}
                                          agents (assoc :agents (str/split agents #",")))))))

(defn- cidr-entry
  [s]
  (let [[_ cidr from to] (re-matches #"(.+):(\d+)(?:-(\d+))?" s)]
    (cond-> {:cidr-block cidr :port (parse-long from)}
      to (assoc :end-port (parse-long to)))))

(defmethod step! :agent-proxy-allowlist [ctx [_ {:keys [agent cidrs]}]]
  (emit! ctx (req! "agent proxy allowlist"
                   (api/post! :agent-proxy-allowlist
                              {:agent agent
                               :allow-listed-cidrs (mapv cidr-entry (str/split cidrs #","))}))))

(defmethod step! :egress-policy-adopt [ctx [_ {:keys [except]}]]
  (emit! ctx (req! "adopt" (api/post! :egress-policies-adopt
                                      {:except (when except (str/split except #","))}))))

(defmethod step! :egress [ctx [_ _]]
  (let [{:keys [egress agents xf]} (req! "egress" (api/get! :egress-allowlist))
        status (:agents (req! "egress status" (api/get! :egress-status)))
        allow  (assoc agents "default" egress)
        xf-set (set xf)
        probe  (into {} (for [{:keys [agent endpoints]} status
                              {:keys [endpoint] :as p} endpoints]
                          [[agent endpoint] p]))
        rows   (vec (for [{:keys [agent]} status
                          e (get allow agent)
                          :let [{:keys [open ms error]} (get probe [agent e])]]
                      (cond-> {:agent agent :endpoint e
                               :scope (if (xf-set e) "agent+worker" "agent")}
                        (true? open)  (assoc :connect (str "open " ms "ms"))
                        (false? open) (assoc :connect "BLOCKED" :error error))))
        blocked (filter #(= "BLOCKED" (:connect %)) rows)]
    (emit! ctx {:agents (mapv #(dissoc % :endpoints) status) :rows rows})
    (doseq [{:keys [agent endpoint error]} blocked]
      (warn! (mark :red "✗" (str "egress " agent " → " endpoint " BLOCKED — " error))))
    {:kind :objects :rows rows}))
