(ns sutra.cli.verbs.watch
  (:require [clojure.string :as str]
            [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :as errors :refer [req!]]
            [sutra.cli.live :refer [transform-name-index]]
            [sutra.cli.out :refer [mark paint rpad say! short-id warn!]]
            [sutra.cli.view :refer [clip]]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]))

(defn- job-status-color
  [status]
  (case (str status)
    "SUCCEEDED" :green "COMPLETED" :green
    "RUNNING"   :cyan
    "FAILED"    :red
    "WAITING"   :dim
    "RUN_PENDING" :yellow "ABORT_PENDING" :yellow
    "ABORTED"   :yellow "DID_NOT_RUN" :yellow "SKIPPED" :yellow
    :yellow))

(def ^:private job-state-order
  {"RUNNING" 0 "ABORT_PENDING" 0 "RUN_PENDING" 0
   "WAITING" 1 "FAILED" 2 "ABORTED" 3 "DID_NOT_RUN" 4 "SKIPPED" 4
   "SUCCEEDED" 5 "COMPLETED" 5})

(defn- watch-job-line
  [tname j]
  (let [status (str (:status j))
        nm (or (get tname (:transform_rid j))
               (let [s (str (:transform_rid j))] (subs s (max 0 (- (count s) 8)))))
        io (str (rpad (str (or (:rows_in j) "—")) 5) "→ " (rpad (str (or (:rows_out j) "—")) 6))]
    (str "    " (paint (job-status-color status) (rpad status 9)) " "
         (paint :bold (rpad nm 26)) " "
         (paint :dim io)
         (when (> (or (:attempt j) 1) 1) (paint :yellow (str " ×" (:attempt j))))
         (when (not-empty (str (:error j))) (str "  " (paint :red (str "✗ " (:error j))))))))

(defn- watch-roster!
  [ctx tname prev]
  (let [builds (:builds (req! "watch" (api/get! :builds-active)))]
    (if (empty? builds)
      prev
      (let [snaps (for [{:keys [run-id] :as b} builds]
                    (assoc b :jobs (:jobs (try (api/get! :run-jobs {:id (str run-id)})
                                               (catch Exception e
                                                 (errors/rethrow-fatal! e)
                                                 nil)))))
            sig   (pr-str (for [{:keys [run-id phase jobs]} snaps]
                            [run-id phase (map (juxt :transform_rid :status :rows_out) jobs)]))
            render
            (str/join "\n"
                      (for [{:keys [run-id started phase cancel? jobs]} snaps]
                        (let [secs  (when started
                                      (quot (- (System/currentTimeMillis) started) 1000))
                              freq  (frequencies (map (comp str :status) jobs))
                              total (count jobs)
                              done  (+ (get freq "SUCCEEDED" 0) (get freq "COMPLETED" 0)
                                       (get freq "DID_NOT_RUN" 0) (get freq "SKIPPED" 0))
                              hdr   (mark (if cancel? :yellow :green) "▸"
                                          (str (short-id run-id)
                                               "  " (paint :bold (name (or phase :?)))
                                               "  " (if (pos? total)
                                                      (paint :cyan (str done "/" total " jobs"))
                                                      (paint :dim "resolving jobs…"))
                                               (when secs (paint :dim (str "  " secs "s")))
                                               (when cancel? (paint :yellow "  (cancelling)"))))
                              rows  (->> jobs
                                         (sort-by (fn [j] [(get job-state-order (str (:status j)) 9)
                                                           (or (get tname (:transform_rid j))
                                                               (str (:transform_rid j)))]))
                                         (map #(watch-job-line tname %)))]
                          (str/join "\n" (cons hdr rows)))))]
        (when (not= sig prev) (say! ctx render))
        sig))))

(defn- live-elapsed
  [ms]
  (.format (java.time.format.DateTimeFormatter/ofPattern "HH:mm:ss")
           (java.time.LocalDateTime/ofInstant (java.time.Instant/ofEpochMilli (long ms))
                                              (java.time.ZoneId/systemDefault))))

(defn- live-caller
  [tname caller]
  (let [c (str caller)]
    (or (get tname c)
        (when (seq c) (subs c (max 0 (- (count c) 8))))
        "—")))

(def ^:private live-level-color
  {:error :red :warn :yellow :info :blue :debug :dim})

(defn- live-level
  [ev]
  (let [l (keyword (str/replace (str (:level ev :info)) #"^:" ""))]
    (if (contains? live-level-color l) l :info)))

(defn- scan-line
  [at {:keys [source table folders files mb denied current final?]} txt]
  (str "  " at " " (paint :dim (if final? "■ " "⋯ ")) (paint :bold (str source)) " "
       (i18n/t :cli/scan-progress {:folders folders :files files
                                   :gb (format "%.1f" (/ (double (or mb 0)) 1024))})
       (when (seq denied)
         (paint :yellow (str " · " (i18n/t :cli/scan-denied {:n (count denied)})
                             (when final? (str ": " (str/join ", " denied))))))
       (when-not final? (paint :dim (str " · " (or (txt current) table))))))

(defn- at-mark
  [{:keys [at c]} glyph msg]
  (str "  " at " " (mark c glyph msg)))

(def ^:private live-renderers
  {:build/start
   (fn [{:keys [ev] :as e}]
     (at-mark e "▸" (str "build " (short-id (:run-id ev))
                         (when (:total ev)
                           (paint :dim (str "  " (i18n/t :cli/n-jobs {:n (:total ev)})))))))

   :build/end
   (fn [{:keys [ev] :as e}]
     (at-mark e "■" (i18n/t :cli/build-done {:id (short-id (:run-id ev))})))

   :build/queued
   (fn [{:keys [ev] :as e}]
     (at-mark e "…" (i18n/t :cli/build-queued
                            {:id       (short-id (:run-id ev))
                             :blockers (str/join ", " (map short-id (:blocked-by ev)))})))

   :build/dequeued
   (fn [{:keys [ev] :as e}]
     (at-mark e "×" (i18n/t :cli/build-dequeued {:id (short-id (:run-id ev))})))

   :job/log
   (fn [{:keys [ev at c txt tname]}]
     (str "  " at " " (paint c "│ ")
          (paint :bold (rpad (live-caller tname (:transform ev)) 24)) " "
          (paint c (or (txt (:line ev)) ""))))

   :build/scan
   (fn [{:keys [ev at txt]}] (scan-line at ev txt))

   :build/phase
   (fn [{:keys [ev at]}]
     (str "  " at " " (paint :dim (str "· " (i18n/t :cli/phase) " "))
          (paint :bold (str (:phase ev)))))

   :build/cancel-requested
   (fn [{:keys [ev] :as e}]
     (at-mark e "⏸" (str (i18n/t :cli/cancel-requested {:id (short-id (:run-id ev))})
                         (paint :dim (str "  " (i18n/t :cli/n-jobs-abort-pending
                                                       {:n (:jobs ev)}))))))

   :build/transform-start
   (fn [{:keys [ev at c tname]}]
     (str "  " at " " (paint c "▶ ") (paint :bold (live-caller tname (:transform ev)))))

   :build/transform-done
   (fn [{:keys [ev at c tname]}]
     (str "  " at " " (paint c "✔ ") (paint :bold (rpad (live-caller tname (:transform ev)) 26))
          " " (paint :dim (str (:rows ev) " rows  " (:ms ev) "ms"))))

   :eval/start
   (fn [{:keys [ev] :as e}]
     (at-mark e "▸" (str "eval " (:suite ev)
                         (paint :dim (str "  " (:cases ev) " cases × " (:iterations ev)
                                          (when (:model ev) (str "  " (:model ev)))
                                          (when (:params ev) (str "  " (:params ev))))))))

   :eval/case
   (fn [{:keys [ev at c txt]}]
     (str "  " at " " (paint c "· ")
          (paint :bold (rpad (str (:suite ev) " " (:case ev)) 34)) " "
          (paint :dim (rpad (str "#" (:iteration ev) "  " (:ms ev) "ms"
                                 (when (:total ev) (str "  " (:done ev) "/" (:total ev))))
                            26))
          (when (:spans ev) (paint :dim (str " [" (:spans ev) "]")))
          (when (:error ev) (str " " (paint c (txt (:error ev)))))))

   :eval/phase
   (fn [{:keys [ev at]}]
     (str "  " at " " (paint :dim (str "· eval " (:suite ev) " ")) (paint :bold (:phase ev))))

   :eval/end
   (fn [{:keys [ev] :as e}]
     (at-mark e "■" (str "eval " (:suite ev) "  " (:passed ev) " passed · " (:failed ev) " failed"
                         (paint :dim (str "  answers " (:answer-ms ev) "ms (mean "
                                          (:mean-ms ev) " · max " (:max-ms ev)
                                          ")  scoring " (:scoring-ms ev) "ms")))))

   :eval/failed
   (fn [{:keys [ev txt] :as e}]
     (at-mark e "✗" (str "eval " (:suite ev) " " (short-id (:job ev)) "  " (txt (:error ev)))))

   :llm/request
   (fn [{:keys [ev at c txt who]}]
     (str "  " at " " (paint c "→ ") (who) " "
          (paint :dim (rpad (str (:model ev)) 28)) " "
          (paint :dim (or (txt (:prompt ev)) ""))))

   :llm/response
   (fn [{:keys [ev at c txt who]}]
     (str "  " at " " (paint c "← ") (who) " "
          (paint :dim (rpad (str (when (:ms ev) (str (:ms ev) "ms"))
                                 (when (:prompt-tokens ev)
                                   (str "  " (:prompt-tokens ev) "→"
                                        (or (:completion-tokens ev) 0) " tok")))
                            28)) " "
          (if (:error ev)
            (paint c (or (txt (:error ev)) "error"))
            (paint :dim (or (txt (:answer ev)) "")))))})

(defn- shown?
  [tname {:keys [llm? build? eval? errors? job]} k lvl ev]
  (and (or (not (or llm? build? eval?))
           (and llm? (str/starts-with? k "llm/"))
           (and build? (or (str/starts-with? k "build/") (str/starts-with? k "job/")))
           (and eval? (str/starts-with? k "eval/")))
       (or (not errors?) (#{:error :warn} lvl))
       (or (nil? job)
           (= (str job) (live-caller tname (or (:transform ev) (:caller ev)))))))

(defn- live-line
  [tname {:keys [full?] :as opts} ev]
  (let [k   (str/replace (str (:kind ev)) #"^:" "")
        lvl (live-level ev)
        f   (live-renderers (keyword k))]
    (when (and f (shown? tname opts k lvl ev))
      (f {:ev    ev
          :tname tname
          :c     (live-level-color lvl)
          :at    (paint :dim (live-elapsed (:at ev)))
          :txt   (fn [s] (when (seq (str s)) (if full? (str s) (clip (str s) 88))))
          :who   #(paint :bold (rpad (live-caller tname (:caller ev)) 24))}))))

(defmethod step! :watch [ctx [_ {:keys [once? jobs? since] :as opts}]]
  (let [tname (transform-name-index)]
    (loop [after since, i 0, sig nil]
      (let [{:keys [events dropped]
             cursor :seq} (req! "watch" (api/get! :live (when after {:since after})))]
        (when (pos? (long (or dropped 0)))
          (warn! (paint :yellow (str "  … " (i18n/t :cli/events-dropped {:n dropped})))))
        (let [lines (keep #(live-line tname opts %) events)]
          (doseq [ln lines] (say! ctx ln))
          (when (and (zero? i) (empty? lines))
            (say! ctx (paint :dim
                             (cond
                               (seq events)
                               (str "  "
                                    (i18n/t :cli/n-events-none-matching {:n (count events)}))
                               once?        (str "  " (i18n/t :cli/nothing-in-window))
                               :else
                               (str "  " (i18n/t :cli/nothing-in-flight-streaming)))))))
        (when once?
          (say! ctx (paint :dim
                           (str "  ── cursor " cursor "  ·  "
                                (i18n/t :cli/resume-with {:cursor cursor})))))
        (let [sig' (if jobs? (watch-roster! ctx tname sig) sig)]
          (when-not once?
            (Thread/sleep 1000)
            (recur cursor (inc i) sig')))))))
