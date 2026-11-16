(ns sutra.cli.errors
  (:require [clojure.string :as str]
            [sutra.cli.out :as out :refer [paint]]
            [sutra.i18n.core :as i18n]
            [sutra.parser.ent-errors :as ent-errors]))

(defn ok? [{:keys [status]}] (and (integer? status) (<= 200 status 299)))

(defn req!
  [desc {:keys [status body] :as r}]
  (if (ok? r)
    body
    (throw (ex-info (str desc " → HTTP " status)
                    {:desc desc :status status :body body}))))

(def ^:private category-hints
  {:unavailable :cli/hint-unavailable})

(defn- answer-hint
  [status body]
  (let [body (when (map? body) body)]
    (cond
      (= "restarting" (some-> (:status body) name))   :cli/hint-restarting
      (and (= 503 status) (:retry (:data body)))      :cli/hint-pool-exhausted)))

(defn- status-hint
  [status body]
  (or (category-hints (some-> (when (map? body) (:category body)) keyword))
      (when (= 503 status) :cli/hint-unavailable)
      (when (#{408 504} status) :cli/hint-timeout)))

(def ^:private server-texts
  [[#"(?i)the server is restarting|\"status\":\"restarting\"" :cli/hint-restarting]
   [#"(?i)connection is not available|connection pool|temporarily unavailable"
    :cli/hint-pool-exhausted]
   [#"(?i)at capacity" :cli/hint-at-capacity]])

(def ^:private upstream-texts
  [[#"(?i)no such file|can't find source path" :cli/hint-source-file-missing]
   [#"(?i)link refused" :cli/hint-connection-refused]
   [#"(?i)access denied|authentication|permission denied" :cli/hint-credential-rejected]
   [#"(?i)unknown column|no such table|syntax error" :cli/hint-query-schema-mismatch]
   [#"(?i)unallowed structure or keys|disallowed key" :cli/hint-unknown-key]])

(defn- text-hint [rules text] (some (fn [[re k]] (when (re-find re text) k)) rules))

(defn error-hint
  ([status text] (error-hint status nil text))
  ([status body text]
   (let [t (str text)]
     (some-> (or (answer-hint status body)
                 (text-hint server-texts t)
                 (status-hint status body)
                 (text-hint upstream-texts t))
             i18n/t))))

(def ^:private http-phrase
  {400 :cli/http-400 401 :cli/http-401 403 :cli/http-403
   404 :cli/http-404 409 :cli/http-409 422 :cli/http-422
   429 :cli/http-429 500 :cli/http-500 501 :cli/http-501
   502 :cli/http-502 503 :cli/http-503 504 :cli/http-504})

(defn- timed-out?
  [e]
  (some #(or (str/includes? (.getName (class %)) "Timeout")
             (re-find #"(?i)timed out" (str (ex-message %))))
        (take-while some? (iterate ex-cause e))))

(defn- humanized [errors] (ent-errors/humanize->lines errors))

(defn- detail-lines
  [label xs]
  (when (seq xs)
    (cons (str "    " (paint :yellow (str "· " label ":"))) (map #(str "        " %) xs))))

(defn explain-error
  [e fallback]
  (let [{:keys [desc status body]} (ex-data e)]
    (if-not body
      (cond-> (str (paint :red "✗ ") fallback)
        (or (timed-out? e)
            (#{408 504} status)) (str "\n    " (paint :cyan "→ ") (i18n/t :cli/hint-timeout)))
      (let [{:keys [error data]} (when (map? body) body)
            rid    (:transform data)
            stderr (some-> (:stderr data) str/trim not-empty)
            hint   (error-hint status body (str stderr " " error))
            what   (or desc (i18n/t :cli/err-request))]
        (str/join
         "\n"
         (concat
          [(if error
             (str (paint :red (str "✗ " what ": ")) (paint :bold error)
                  " " (paint :dim (str "(HTTP " status ")")))
             (str (paint :red (str "✗ " what " — "
                                   (i18n/t (http-phrase status :cli/err-request-failed))))
                  " " (paint :dim (str "(HTTP " status ")"))))]
          (when (and rid (not (str/includes? (str error) (str rid))))
            [(str "    "
                  (paint :dim
                         (str "· " (i18n/t :cli/err-transform
                                           {:name (or (:transform-name data) rid)
                                            :rid  rid}))))])
          (when (contains? data :exit)
            [(str "    " (paint :dim (str "· " (i18n/t :cli/err-exit) " ")) (:exit data))])
          (when-let [at (and (:phase data)
                             (seq (keep (fn [k] (some-> (get data k) name))
                                        [:phase :kind :entity])))]
            [(str "    " (paint :yellow (str "· " (i18n/t :cli/err-failed-at) " "))
                  (paint :bold (str/join " " at))
                  (when-let [c (:class data)] (str " " (paint :dim (str "(" c ")")))))])
          (when stderr (detail-lines (i18n/t :cli/err-stderr) (str/split-lines stderr)))
          (detail-lines (i18n/t :cli/err-problems)
                        (for [{:keys [rule entity message]} (:problems data)]
                          (str (paint :bold (str (some-> rule name)
                                                 (when entity (str " " (name entity)))))
                               " — " message)))
          (when (some #(= :migration-required (some-> (:rule %) keyword)) (:problems data))
            [(str "    " (paint :dim (str "· " (i18n/t :cli/hint-stale-checkout))))])
          (detail-lines (i18n/t :cli/err-still-referenced)
                        (for [{:keys [in entity]} (:referrers body)]
                          (str (paint :bold (str (some-> entity name)))
                               (when in (str " " (paint :dim (str "(" (some-> in name) ")")))))))
          (detail-lines (i18n/t :cli/err-in-detail)
                        (if (map? (:errors data))
                          (humanized (:errors data))
                          (map str (:errors data))))
          (when-let [r (:reasons data)]
            [(str "    " (paint :yellow (str "· " (i18n/t :cli/err-reasons) " ")) (pr-str r))])
          (when hint [(str "    " (paint :cyan "→ ") hint)])))))))

(defn fatal?
  [e]
  (boolean (:sutra.wire.transport/protocol-mismatch (ex-data e))))

(defn rethrow-fatal!
  [e]
  (when (fatal? e) (throw e)))

(defn optional
  [what f]
  (try (f)
       (catch Exception e
         (rethrow-fatal! e)
         (out/warn! (paint :dim (str "  · " (i18n/t :cli/read-skipped
                                                    {:was    what
                                                     :grund  (or (not-empty (ex-message e))
                                                                 (.getName (class e)))}))))
         nil)))

(defn print-error!
  [e fallback]
  (rethrow-fatal! e)
  (out/warn! (explain-error e (or (not-empty (ex-message e)) fallback))))
