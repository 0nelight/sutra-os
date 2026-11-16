(ns sutra.cli.verbs.observe
  (:require [clojure.string :as str]
            [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.live :refer [short-commit transform-name-index]]
            [sutra.cli.out :refer [emit! emit-rows! mark paint render say! short-id show-lines
                                   warn!]]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]))

(defn- ->instant
  [ts]
  (cond
    (nil? ts)                     nil
    (instance? java.time.Instant ts) ts
    (instance? java.util.Date ts) (.toInstant ^java.util.Date ts)
    :else (let [s (str/trim (str ts))]
            (when (seq s)
              (try (java.time.Instant/parse
                    (cond-> s (not (str/ends-with? s "Z")) (-> (str/replace " " "T") (str "Z"))))
                   (catch java.time.DateTimeException _ nil))))))

(defmethod step! :runs [ctx [_ {:keys [kind hours limit]}]]
  (let [{:keys [runs]} (req! "runs" (api/list-runs
                                     (cond-> {:brief true}
                                       kind  (assoc :kind (clojure.core/name kind))
                                       hours (assoc :hours hours)
                                       limit (assoc :limit limit))))
        dur   (fn [{:keys [started-at finished-at]}]
                (let [a (->instant started-at) b (->instant finished-at)]
                  (when (and a b)
                    (let [ms (- (.toEpochMilli ^java.time.Instant b)
                                (.toEpochMilli ^java.time.Instant a))]
                      (cond (< ms 1000)  (str ms "ms")
                            (< ms 60000) (format "%.1fs" (/ ms 1000.0))
                            :else        (format "%.1fmin" (/ ms 60000.0)))))))
        rows  (vec (for [r runs]
                     {:id         (str (:id r))
                      :kind       (clojure.core/name (or (:kind r) :run))
                      :status     (str (:status r))
                      :started-at (str (:started-at r))
                      :duration   (or (dur r) "")}))]
    (emit-rows! ctx rows (if hours (str "no runs in the last " hours "h") "no runs"))))

(defmethod step! :stop [ctx [_ {:keys [id all force]}]]
  (letfn [(one [run-id]
            (let [r (req! "stop"
                          (if force
                            (api/post! :run-cancel (merge {:id run-id} (when true {:force true}))
                                       nil) (api/post! :run-cancel {:id run-id} nil)))]
              (assoc r :run-id run-id)))
          (resolve-run-id [given]
            (if (>= (count (str given)) 36)
              given
              (let [builds (:builds (req! "stop" (api/get! :builds-active)))
                    hit    (first (filter #(str/starts-with? (str (:run-id %)) (str given))
                                          builds))]
                (or (:run-id hit) given))))]
    (cond
      all (let [builds (:builds (req! "stop" (api/get! :builds-active)))]
            (if (empty? builds)
              (say! ctx (paint :dim (str "  " (i18n/t :cli/no-live-build))))
              (emit! ctx {:stopped (mapv (comp one :run-id) builds)} ::stop)))
      id  (emit! ctx {:stopped [(one (resolve-run-id id))]} ::stop)
      :else (warn! (paint :yellow (str "  " (i18n/t :cli/stop-needs-id)))))))

(defmethod render ::stop [_ {:keys [stopped]}]
  (for [{:keys [run-id] :as r} stopped]
    (mark (if (or (:cancelled r) (:forced r)) :yellow :dim) "⏹"
          (str (short-id run-id) " → "
               (cond (:forced r)     (if (:reaped r)
                                       (i18n/t :cli/force-reaped)
                                       (i18n/t :cli/force-no-row))
                     (:cancelled r) (i18n/t :cli/cancel-requested-boundary)
                     (:reaped r)    (i18n/t :cli/reaped-orphan-row)
                     :else          (i18n/t :cli/not-found-not-running))))))

(defmethod step! :run [ctx [_ {:keys [id trace?]}]]
  (let [run (dissoc (req! (str "run " id) (api/get! :run {:id id})) :inputs)]
    (emit! ctx run (if trace? :default ::run)))
  nil)

(defmethod render ::run [_ run]
  (show-lines (cond-> run (:summary run) (update :summary dissoc :trace))))

(defmethod render ::awaited [_ {:keys [read]}]
  (when read (paint :dim (str "  details: `" read "`"))))

(def ^:private await-poll-ms 5000)
(def ^:private await-default-s 1800)
(def ^:private await-max-s 3600)
(def ^:private run-terminal #{"succeeded" "failed" "aborted" "cancelled" "canceled"
                              "stopped" "skipped"})

(defn- finished-eval
  [{:keys [experiment runs summary run-id suite]}]
  (if experiment
    (let [suite (:suite (first runs))
          nm    (:name experiment)]
      {:body {:suite suite :experiment nm
              :runs  (mapv (fn [{:keys [run-id summary]}]
                             {:run-id run-id :params (get-in summary [:experiment :params])
                              :passed (:passed summary) :total (:total summary)})
                           runs)
              :read  (str "eval history " suite " --experiment " nm)}
       :line (str "eval job done — experiment " nm " · " (count runs) " run(s): "
                  (str/join ", " (for [{s :summary} runs]
                                   (str (pr-str (get-in s [:experiment :params])) " "
                                        (:passed s) "/" (:total s)))))})
    {:body {:run-id run-id :suite suite :passed (:passed summary) :total (:total summary)
            :read (str "eval cases " run-id " --failing")}
     :line (str "eval job done — " (:passed summary) "/" (:total summary)
                " cases pass · run " run-id)}))

(defn- await-probe
  [handle]
  (let [j (api/get! :eval-job {:id handle})]
    (if (= 200 (:status j))
      (let [{:keys [status result error]} (:body j)
            s (str status)]
        {:kind :eval-job :status s :done? (not= "running" s) :ok? (= "done" s)
         :body (cond-> {:handle handle :kind "eval-job" :status s}
                 (= "done" s) (merge (:body (finished-eval result))))
         :line (case s
                 "running" "eval job running"
                 "done"    (:line (finished-eval result))
                 (str "eval job " s (when error (str " — " error))))})
      (let [r (api/get! :run {:id handle})]
        (when (= 200 (:status r))
          (let [{:keys [status phase error-message queued-behind objects-count progress]} (:body r)
                s (str status)
                done? (contains? run-terminal s)]
            {:kind :run :status s :done? done? :ok? (= "succeeded" s)
             :body (cond-> {:handle handle :kind "run" :status s}
                     error-message (assoc :error error-message)
                     (and done? objects-count) (assoc :objects objects-count)
                     (and done? (seq (:phases progress))) (assoc :phases (:phases progress))
                     (and done? (seq (:steps progress))) (assoc :steps (:steps progress))
                     true (assoc :read (str "build show " handle)))
             :line (str "run " s (when phase (str " · " (name phase)))
                        (when (seq queued-behind)
                          (str " behind " (str/join ", " queued-behind)))
                        (when (and error-message (not= "succeeded" s))
                          (str " — " error-message)))}))))))

(defmethod step! :await [ctx [_ {:keys [handle timeout-s]}]]
  (let [limit (* 1000 (min await-max-s (or timeout-s await-default-s)))
        t0    (System/currentTimeMillis)
        over? #(> (- (System/currentTimeMillis) t0) limit)]
    (loop [prev nil, seen? false]
      (let [p (try (await-probe handle) (catch Exception e {:error (ex-message e)}))]
        (cond
          (:error p)
          (if (and seen? (not (over?)))
            (do (Thread/sleep await-poll-ms) (recur prev seen?))
            (throw (ex-info (str "await " handle ": " (:error p))
                            {:handle handle :timeout seen?})))

          (nil? p)
          (throw (ex-info (str "await " handle ": neither an eval job nor a run on this branch"
                               (if seen? " any more" "")
                               " — wait for the handle a --detach printed (`await` line), "
                               "not a run id from a history or list")
                          {:refused :unknown-handle :handle handle}))

          :else
          (do (when (not= (:line p) prev) (say! ctx (str "  " (:line p))))
              (cond
                (and (:done? p) (:ok? p))
                (do (emit! ctx (:body p) ::awaited) nil)

                (:done? p)
                (throw (ex-info (str "await " handle ": " (:line p))
                                {:handle handle :status (:status p)}))

                (over?)
                (throw (ex-info (str "await " handle ": still " (:status p) " after "
                                     (quot limit 1000) " s — re-arm `await " handle "`")
                                {:handle handle :status (:status p) :timeout true}))

                :else
                (do (Thread/sleep await-poll-ms) (recur (:line p) true)))))))))

(defn- spans-brief
  [spans]
  (when (seq spans)
    (let [open (filter :open spans)
          by   (->> (remove :open spans)
                    (group-by :name)
                    (map (fn [[n ss]] [n (count ss) (reduce + 0 (keep :ms ss))]))
                    (sort-by #(- (long (nth % 2)))))]
      (str/join " · "
                (concat (for [{:keys [name at-ms]} open]
                          (str "OFFEN " name (when at-ms (str " ab " at-ms "ms"))))
                        (for [[n c ms] (take 4 by)]
                          (format "%s %d× %.1fs" n c (/ (double ms) 1000))))))))

(defn- job-seconds
  [{:keys [started_at ended_at]}]
  (when (and started_at ended_at)
    (try (/ (.toMillis (java.time.Duration/between
                        (java.time.Instant/parse (str started_at))
                        (java.time.Instant/parse (str ended_at))))
            1000.0)
         (catch Exception _ nil))))

(defmethod step! :jobs [ctx [_ {:keys [id]}]]
  (let [{:keys [jobs]} (req! (str "jobs " id) (api/get! :run-jobs {:id id}))
        tname (transform-name-index)
        label-for
        (fn [rid] (or (get tname rid)
                      (let [s (str rid)] (subs s (max 0 (- (count s) 8))))))
        rows  (vec (for [j jobs]
                     {:id        (str (:job_rid j))
                      :status    (some-> (:status j) str)
                      :transform (label-for (:transform_rid j))
                      :rows_in   (:rows_in j)
                      :rows_out  (:rows_out j)
                      :attempt   (:attempt j)
                      :commit    (short-commit (:snapshot_id j))
                      :error     (not-empty (str (:error j)))
                      :log       (not-empty (str (:job_log j)))
                      :spans     (spans-brief (:job_spans j))
                      :started   (some-> (:started_at j) str)
                      :seconds   (job-seconds j)}))]
    (emit-rows! ctx rows (str "no jobs for run " id
                              " — unknown run-id, or a build predating the Job log"))))
