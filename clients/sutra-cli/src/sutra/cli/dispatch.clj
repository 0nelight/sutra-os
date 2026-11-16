(ns sutra.cli.dispatch
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.cli.auth :as auth]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.out :as out :refer [emit! kname mark paint say!]]
            [sutra.cli.table :as table]
            [sutra.i18n.core :as i18n]
            [sutra.types.endpoint-verbs :as ev]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn new-ctx
  [{:keys [creds-path yes? session json? fields secrets]}]
  {:token       (atom nil)
   :refs        (atom {})
   :creds-path  (or creds-path auth/default-creds-path)
   :yes?        (boolean yes?)
   :json?       (boolean json?)
   :fields      fields
   :secrets     (or secrets {})
   :session     session})

(defn ref!
  [ctx k]
  (or (get @(:refs ctx) k)
      (let [u (str (java.util.UUID/randomUUID))]
        (swap! (:refs ctx) assoc k u)
        u)))

(defn deref1
  [ctx x]
  (get @(:refs ctx) x x))

(defn resolve-refs
  [ctx form]
  (walk/postwalk #(deref1 ctx %) form))

(defmulti step!
  (fn [_ctx [verb _]] verb))

(def ^:private homes
  (into {} (for [{:keys [verb home]} table/verbs :when home]
             [verb (symbol (str "sutra.cli.verbs." (name home)))])))

(defn load-verbs!
  []
  (run! require (distinct (vals homes))))

(def ^:private contract-verbs (set (keys ev/cli-verbs)))

(defn- looked-up
  [{:keys [param from rows by value]}]
  (let [found (some #(when (= (str value) (str (get % by))) %)
                    (get (req! (str "list " (kname from)) (api/get! from)) rows))]
    (or (get found param)
        (throw (ex-info (str "no " (kname rows) " with " (kname by) " " value)
                        {:from from by value})))))

(defn- confirmed?
  [ctx verb args]
  (if-let [q (:confirm (ev/cli-verbs verb))]
    (out/prompt-yes? ctx (str/replace q #"\{([a-z-]+)\}"
                                      #(paint :bold (str (get args (keyword (second %)))))))
    true))

(defn- contract-step!
  [ctx verb args]
  (let [{:keys [method id params body lookups]} (ev/cli-call verb args)
        params (into params (map (juxt :param looked-up)) lookups)
        label  (str/join " " (concat (:path (ev/cli-verbs verb))
                                     (for [[k {:keys [pos]}] (sort-by (comp :pos second)
                                                                      (drop 2 (ev/cli-args verb)))
                                           :when pos
                                           :let [v (get args k)] :when (some? v)]
                                       (kname v))))]
    (if (confirmed? ctx verb args)
      (emit! ctx (req! label (case method
                               :get    (api/get! id params)
                               :delete (api/delete! id params)
                               :post   (api/post! id params body)
                               :put    (api/put! id params body))))
      (say! ctx (mark :dim "»" (i18n/t :cli/left-alone))))
    nil))

(defmethod step! :default [ctx [verb args :as s]]
  (if (contains? contract-verbs verb)
    (contract-step! ctx verb args)
    (let [n (or (homes verb)
                (throw (ex-info (i18n/t :cli/unknown-step-verb {:verb verb}) {:verb verb})))]
      (require n)
      (if (contains? (methods step!) verb)
        (step! ctx s)
        (throw (ex-info (str "the table names " n " as the home of " verb
                             ", and it defines no step! method for it")
                        {:verb verb :home n}))))))

(defn await-line
  [handle]
  (str "bin/sutra await " handle))

(defn started!
  [ctx {:keys [run-id] :as body}]
  (when-not (out/json? ctx)
    (say! ctx (paint :dim (str "  " (i18n/t :cli/run-started {:id run-id})))))
  (emit! ctx (cond-> body run-id (assoc :await (await-line run-id)))))

(def ^:private recent-starts-path ".run/recent-starts.edn")
(def ^:private restart-window-ms (* 10 60 1000))

(defn- start-key
  [verb args]
  (pr-str [(str (:base-url (tx/ambient))) (or tx/*branch* "main") verb
           (into (sorted-map) (remove (comp nil? val)) args)]))

(defn- recent-starts
  []
  (let [now (System/currentTimeMillis)
        m   (try (edn/read-string (slurp recent-starts-path))
                 (catch java.io.IOException _ {})
                 (catch RuntimeException _ {}))]
    (into {} (filter #(< (- now (:at (val %) 0)) restart-window-ms)) (if (map? m) m {}))))

(defn guard-restart!
  [verb args again?]
  (when-not (or again? (out/tty?))
    (when-let [{:keys [at handle]} (get (recent-starts) (start-key verb args))]
      (throw (ex-info (str (name verb) " " (pr-str (into {} (remove (comp nil? val)) args))
                           " was started "
                           (quot (- (System/currentTimeMillis) at) 1000) " s ago"
                           (when handle (str " as " handle))
                           " — this verb STARTS a run, it does not read one. "
                           (if handle (str "Wait: `" (await-line handle) "`. ") "")
                           "Start another on purpose: add --again.")
                      {:refused :repeated-start :handle handle})))))

(defn remember-start!
  [verb args handle]
  (try
    (io/make-parents recent-starts-path)
    (spit recent-starts-path
          (pr-str (assoc (recent-starts) (start-key verb args)
                         {:at (System/currentTimeMillis) :handle (some-> handle str)})))
    (catch java.io.IOException _ nil)))

(defn run-runbook!
  [runbook {:keys [base-url] :as opts}]
  (when base-url (tx/set-base! base-url))
  (let [ctx   (new-ctx opts)
        steps (:steps runbook)]
    (println (paint :bold
                    (str "▶ " (:title runbook) "  (" (i18n/t :cli/n-steps {:n (count steps)}) ")")))
    (when base-url (println (paint :dim (str "  ⇢ " (i18n/t :cli/backend) " " base-url))))
    (auth/ensure-prod-login! ctx)
    (loop [i 0, [step & more] steps]
      (if (nil? step)
        (do (println (paint :green "✓ done")) {:ran i :aborted? false})
        (let [[verb] step
              outcome (binding [tx/*auth-token* @(:token ctx)]
                        (println (str (paint :dim (str "[" (inc i) "/" (count steps) "] "))
                                      (paint [:bold :cyan] (str verb))))
                        (step! ctx step))]
          (if (= :abort outcome)
            (do (println (paint :red (str "■ " (i18n/t :cli/aborted))))
                {:ran (inc i) :aborted? true})
            (recur (inc i) more)))))))
