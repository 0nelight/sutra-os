(ns sutra.cli.verbs.imports
  (:require [sutra.types.upload :as upload]
            [sutra.cli.dispatch :as dispatch :refer [started! step!]]
            [sutra.cli.errors :as errors :refer [req!]]
            [sutra.cli.out :refer [emit! mark paint say! show-lines warn!]]
            [sutra.i18n.core :as i18n]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- report-import-run!
  [ctx tok]
  (let [r    (binding [tx/*auth-token* tok] (first (:runs (api/list-runs "import"))))
        full (when r (binding [tx/*auth-token* tok] (:body (api/get! :run {:id (str (:id r))}))))
        status   (str (:status full))
        pf   (seq (get-in full [:summary :publish-failures]))]
    (if-not r
      (say! ctx (paint :yellow (str "  " (i18n/t :cli/build-no-run-yet))))
      (emit! ctx (cond-> {:run (str (:id r)) :status status
                          :objects (:objects-count full)
                          :links (or (get-in full [:summary :links-standing]) (:links-count full))}
                   pf (assoc :publish-failures (vec pf))
                   (= status
                      "failed") (assoc :error (or (:error full) (some-> full :summary :error))))))))

(defn- settle-import!
  [ctx {:keys [resp err]} tok only]
  (let [status (:status resp)]
    (cond
      (and resp (not err) status (<= 200 status 299))
      (emit! ctx (:body resp))

      (and resp status)
      (do (some->> (get-in resp [:body :active-runs]) seq show-lines (run! warn!))
          (req! (str "run-import " (pr-str only)) resp))

      :else (report-import-run! ctx tok))))

(defmethod step! :run-import [ctx [_ opts]]
  (let [only     (:only opts)
        trace    (:trace opts)
        strategy (:strategy opts)
        from     (:from opts)
        abort-on-failure (:abort-on-failure opts)
        body  (cond-> {}
                only     (assoc :only only)
                strategy (assoc :strategy strategy)
                (seq from) (assoc :from from)
                trace    (assoc :trace true)
                (:profile opts) (assoc :profile true)
                abort-on-failure (assoc :abort-on-failure true)
                (some? (:retry-count opts)) (assoc :retry-count (:retry-count opts))
                (:retry-backoff opts) (assoc :retry-backoff (:retry-backoff opts))
                (:full opts) (assoc :full true)
                (:force opts) (assoc :force true)
                (not (:wait opts)) (assoc :detach true))
        tok  tx/*auth-token*
        done (atom nil)]
    (when-not only
      (throw (ex-info (str "run-import requires --only <type,…>. A bare run-import would "
                           "re-pull ALL sources (mailbox re-ingest) and re-run ALL "
                           "transforms (full LLM compute) — that path is disabled. "
                           "Scope it, e.g. `run-import --only konto,monatswert`.")
                      {:refused :run-import-unscoped})))
    (dispatch/guard-restart! :run-import (dissoc body :detach) (:again opts))
    (if-not (:wait opts)
      (let [resp (req! (str "run-import " (pr-str only)) (api/post! :import-run body))]
        (dispatch/remember-start! :run-import (dissoc body :detach) (:run-id resp))
        (started! ctx resp)
        (say! ctx (paint :dim (str "  " (i18n/t :cli/build-follow)))))
      (do
        (dispatch/remember-start! :run-import (dissoc body :detach) nil)
        (future
          (binding [tx/*auth-token* tok]
            (reset! done
                    (try {:resp (if (seq body) (api/post! :import-run body)
                                    (api/post! :import-run))}
                         (catch Throwable e {:err e})))))
        (when only
          (say! ctx (paint :dim (str "  " (i18n/t :cli/scoped-build) " → " (pr-str only)
                                     (when strategy
                                       (str "  " (i18n/t :cli/strategy) " "
                                            (clojure.core/name strategy)))
                                     (when (seq from)
                                       (str "  " (i18n/t :cli/from) " " (pr-str from)))))))
        (say! ctx (paint :dim (str "  " (i18n/t :cli/build-submitted) ":")))
        (loop [i 0, seen? false, prev nil]
          (let [builds  (binding [tx/*auth-token* tok]
                          (try (:builds (api/get! :builds-active))
                               (catch Exception e (errors/rethrow-fatal! e) nil)))
                active? (boolean (seq builds))
                status  (when active?
                          (str/join "\n"
                                    (for [{:keys [run-id phase current done idle-ms stuck?]} builds]
                                      (str "  "
                                           (paint (if stuck? :yellow :green) (if stuck? "⏸" "▸"))
                                           " " (subs (str run-id) 0 (min 8 (count (str run-id))))
                                           "  "
                                           (paint :bold (name (or phase :?)))
                                           (when current (str " · " (paint :cyan current)))
                                           "  " (count done) " xf"
                                           (when stuck?
                                             (paint :yellow
                                                    (str "  — " (quot (or idle-ms 0) 1000)
                                                         "s still; `build stop "
                                                         (subs (str run-id) 0
                                                               (min 8 (count (str run-id))))
                                                         " --force`")))))))]
            (when (and status (not= status prev)) (say! ctx status))
            (cond
              (some? @done)
              (settle-import! ctx @done tok only)

              (and seen? (not active?))
              (do (Thread/sleep 2500)
                  (settle-import! ctx @done tok only))

              (>= i 2000)
              (say! ctx (paint :yellow (str "  " (i18n/t :cli/monitor-timed-out))))

              :else
              (do (Thread/sleep 2000) (recur (inc i) (or seen? active?) status)))))))
    nil))

(defmethod step! :dataset-write [ctx [_ {:keys [name rows mode message] pk :key}]]
  (let [dsname (clojure.core/name name)
        b (req! "dataset-write"
                (api/post! :dataset-write {:name dsname} (cond-> {:rows rows}
                                                           mode    (assoc :mode mode)
                                                           pk      (assoc :key pk)
                                                           message (assoc :message message))))]
    (emit! ctx (assoc b :dataset dsname))))

(defn- sha256-hex
  [^bytes bs]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md bs)))))

(defn landed-build-line
  [dataset triggered-by]
  (if (seq triggered-by)
    (mark :dim "»" (i18n/t :cli/landed-scoped-build-follows
                           {:dataset   (paint :bold dataset)
                            :zeitplaene (str/join ", " (map #(paint :bold (str %)) triggered-by))}))
    (mark :yellow "⚠" (i18n/t :cli/landed-nothing-watches {:dataset (paint :bold dataset)}))))

(defn- upload-file!
  ([file dataset media-column fields-fn build-types]
   (upload-file! file dataset media-column fields-fn build-types nil nil nil))
  ([file dataset media-column fields-fn build-types ctx on-upload action-args]
   (let [f (io/file file)]
     (when-not (.exists f)
       (throw (ex-info (str "upload: keine Datei " file) {:file file})))
     (let [bytes  (java.nio.file.Files/readAllBytes (.toPath f))
           nm     (.getName f)
           local  (sha256-hex bytes)
           landed (req! "media-land"
                        (api/post! :media-land
                                   {:dataset      (clojure.core/name dataset)
                                    :media-column (clojure.core/name media-column)
                                    :bytes-b64
                                    (.encodeToString (java.util.Base64/getEncoder) bytes)
                                    :filename     nm
                                    :values       (or (fields-fn local) {})
                                    :message      (str "upload " nm)}))
           hash   (:hash landed)
           size   (:size landed)]
       (when (not= hash local)
         (throw (ex-info (str "upload: der Server meldet die Content-Adresse " hash
                              ", lokal berechnet wurde " local
                              " — die Feldwerte passen nicht zur gelandeten Zeile")
                         {:local local :server hash :file file})))
       (say! ctx (mark :green "✓"
                       (i18n/t :cli/uploaded {:datei (paint :bold nm) :bytes size
                                              :blob (paint :dim (str (subs hash 0 12) "…"))
                                              :dataset (paint :bold dataset)})))
       (when on-upload
         (let [args (upload/resolve-args (or action-args {})
                                         {:hash hash :filename nm :dataset dataset})]
           (say! ctx (mark :dim "»"
                           (i18n/t :cli/on-upload-action
                                   {:action (paint :bold (clojure.core/name on-upload))})))
           (try
             (step! ctx [:action {:name on-upload :args args}])
             (catch Exception e
               (warn! (mark :yellow "⚠"
                            (i18n/t :cli/upload-landed-action-failed
                                    {:blob (subs hash 0 12) :dataset dataset
                                     :action (clojure.core/name on-upload)
                                     :grund (.getMessage e)})))))))
       (if (seq build-types)
         (step! nil [:run-import {:only (vec build-types)}])
         (say! ctx (landed-build-line dataset (:triggered-by landed))))
       hash))))

(defmethod step! :upload [ctx [_ {:keys [file dataset media-column build on-upload args] :as a}]]
  (upload-file! file dataset (or media-column "media_ref")
                (constantly (or (:set a) {})) build
                ctx on-upload args))

(defmethod step! :bank-import [_ [_ {:keys [file jahr art dataset]}]]
  (let [nm    (.getName (io/file file))
        jahr* (or jahr (some-> (re-find #"(?:19|20)\d{2}" nm) Integer/parseInt))
        art*  (or art "erfolgskonten")]
    (upload-file! file (or dataset "konten_uploads") "media_ref"
                  (fn [hash] {:import_id      (str art* "-" (or jahr* "0000") "-" (subs hash 0 8))
                              :dateiname      nm
                              :jahr           (or jahr* 0)
                              :art            art*
                              :hochgeladen_am (str (java.time.Instant/now))})
                  nil)))
