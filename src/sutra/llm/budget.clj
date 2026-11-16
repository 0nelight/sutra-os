(ns sutra.llm.budget
  (:require [sutra.kit.noted :as noted]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.kit.coerce :refer [->num]]
            [sutra.store.control :as ctl]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.rid :as rid]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.store.storage.instance-config :as ic]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(def budget-key "llm_daily_budget_eur")
(def default-budget-eur 1.0)

(defn budget-eur
  [store]
  (or (->num (ic/get-cfg store budget-key))
      (->num (not-empty (or (env/getenv "SUTRA_LLM_DAILY_BUDGET_EUR") "")))
      default-budget-eur))

(defn set-budget!
  [store eur]
  (let [v (double (or (->num eur) default-budget-eur))]
    (ic/set-cfg! store budget-key (str v))
    v))

(def reserve-key "llm_interactive_reserve_pct")
(def default-reserve-pct 20.0)
(def quotas-key "llm_project_quotas_eur")

(defn- stored-map
  [store k]
  (let [s (ic/get-cfg store k)
        m (try (edn/read-string (or s "{}"))
               (catch RuntimeException e
                 (noted/note (str "config " k " does not read, taken as {}") e)
                 {}))]
    (if (map? m) m {})))

(defn reserve-pct
  [store]
  (let [v (or (->num (ic/get-cfg store reserve-key)) default-reserve-pct)]
    (double (max 0.0 (min 100.0 v)))))

(defn set-reserve-pct!
  [store pct]
  (let [v (double (max 0.0 (min 100.0 (or (->num pct) default-reserve-pct))))]
    (ic/set-cfg! store reserve-key (str v))
    v))

(defn project-quotas
  [store]
  (let [m (stored-map store quotas-key)]
    (into {} (keep (fn [[k v]] (when-let [n (->num v)] [(str k) (double n)]))) (when (map? m) m))))

(defn set-project-quota!
  [store p eur]
  (let [n (->num eur)
        m (cond-> (dissoc (project-quotas store) (str p))
            (some? n) (assoc (str p) (double (max 0.0 n))))]
    (ic/set-cfg! store quotas-key (pr-str (into (sorted-map) m)))
    m))

(def source-budgets-key "llm_source_budgets_eur")

(defn source-budgets
  [store]
  (let [m (stored-map store source-budgets-key)]
    (into {} (keep (fn [[k v]] (when-let [n (->num v)] [(name k) (double n)]))) (when (map? m) m))))

(defn set-source-budget!
  [store s eur]
  (let [n (->num eur)
        m (cond-> (dissoc (source-budgets store) (name s))
            (some? n) (assoc (name s) (double (max 0.0 n))))]
    (ic/set-cfg! store source-budgets-key (pr-str (into (sorted-map) m)))
    m))

(def exhausted-key "llm_exhausted_sources")

(def exhausted-pause-ms
  (* 60 60 1000))

(def ^:private exhaustion-words
  (re-pattern (str "(?i)credits|spending limit|insufficient[_ ](balance|funds|quota)"
                   "|exceeded your current quota|billing")))

(defn account-exhausted?
  [error-text]
  (let [t (str error-text)]
    (boolean (or (re-find #"HTTP 402\b" t)
                 (and (re-find #"HTTP (403|429)\b" t) (re-find exhaustion-words t))))))

(defn exhausted-sources
  [store]
  (stored-map store exhausted-key))

(defn exhausted!
  [store s detail]
  (let [now (ctl/millis store)
        m   (exhausted-sources store)
        e   {:since  (get-in m [(str s) :since] now)
             :until  (+ now exhausted-pause-ms)
             :detail (let [d (str detail)] (subs d 0 (min 300 (count d))))}]
    (ic/set-cfg! store exhausted-key (pr-str (assoc m (str s) e)))
    e))

(defn recovered!
  [store s]
  (let [m (exhausted-sources store)]
    (when (contains? m (str s))
      (ic/set-cfg! store exhausted-key (pr-str (dissoc m (str s))))
      true)))

(defn limits
  [store]
  {:budget         (budget-eur store)
   :reserve-pct    (reserve-pct store)
   :quotas         (project-quotas store)
   :source-budgets (source-budgets store)
   :exhausted      (exhausted-sources store)})

(def interactive "interactive")

(defn pool-of
  [schema caller]
  (let [c (some-> caller str str/trim not-empty)
        t (when c (or (get-in schema [:transforms c])
                      (get-in schema [:transforms (lookup/rid-of schema :transforms (keyword c))])
                      (get-in schema [:transforms (lookup/rid-of schema :transforms c)])))]
    (if t
      (str "batch:"
           (or (some-> (:project t) (#(or (get-in schema [:projects (str %) :project-name]) %))
                       rid/->name) ""))
      interactive)))

(defn batch? [pool] (str/starts-with? (str pool) "batch:"))

(defn project-of-pool [pool] (when (batch? pool) (not-empty (subs pool 6))))

(def ^:const day-zone
  "UTC")

(defn day-of
  [^java.util.Date d]
  (str (.toLocalDate (.atZone (.toInstant d) (java.time.ZoneId/of day-zone)))))

(defn today
  [store]
  (day-of (ctl/clock store)))

(defn spent-today
  [store]
  (or (:spent (ctl/get store :llm-budget {:day (today store)})) 0.0))

(def ^:private default-call-ms 120000)
(def ^:private default-max-tokens 4096)

(defn max-cost
  [schema model request]
  (llm-models/cost schema model
                   (quot (count (pr-str (:messages request request))) 4)
                   (or (some-> (or (:max_tokens request) (:max-tokens request)) ->num long)
                       default-max-tokens)))

(defn- live [reserved now]
  (into {} (filter (fn [[_ {:keys [until]}]] (> (long until) now))) reserved))

(defn- used
  [d open pred]
  (+ (reduce + 0.0 (for [[p e] (:spent-by d) :when (pred p)] e))
     (reduce + 0.0 (for [[_ {:keys [eur pool]}] open :when (pred (or pool interactive))] eur))))

(declare pool-refusal)

(defn- used-by-source
  [d open source]
  (+ (get-in d [:spent-by-source source] 0.0)
     (reduce + 0.0 (for [[_ r] open :when (= source (:source r))] (:eur r)))))

(defn refusal
  ([d limits pool eur now] (refusal d limits pool nil eur now))
  ([d {:keys [source-budgets exhausted] :as lim} pool source eur now]
   (or (when-let [{:keys [until detail]} (and source (get exhausted source))]
         (when (< now (long until))
           {:limit :account :source source :until until :detail detail :waitable? false}))
       (when-let [cap (and source (get source-budgets source))]
         (let [open (live (:reserved d) now)
               u    (used-by-source d open source)]
           (when (> (+ u eur) cap)
             {:limit :source :source source :cap cap :used u
              :waitable? (<= (+ (used-by-source d {} source) eur) cap)})))
       (pool-refusal d lim pool eur now))))

(defn- pool-refusal
  [d {:keys [budget reserve-pct quotas]} pool eur now]
  (let [open  (live (:reserved d) now)
        _all   (constantly true)
        proj  (project-of-pool pool)
        total (+ (or (:spent d) 0.0) (reduce + 0.0 (map (comp :eur val) open)))
        check (fn [limit cap used-open used-settled]
                (when (and cap (> (+ used-open eur) cap))
                  (cond-> {:limit limit :cap cap :used used-open
                           :waitable? (<= (+ used-settled eur) cap)}
                    proj (assoc :project proj))))]
    (or (when (pos? budget)
          (check :instance budget total (or (:spent d) 0.0)))
        (when (and (pos? budget) (batch? pool))
          (check :batch (* budget (- 1.0 (/ (or reserve-pct 0.0) 100.0)))
                 (used d open batch?) (used d {} batch?)))
        (when-let [q (and proj (get quotas proj))]
          (check :project q (used d open #{pool}) (used d {} #{pool}))))))

(defn reserving
  ([d budget reqid eur now until]
   (reserving d {:budget budget} interactive reqid eur now until))
  ([d limits pool reqid eur now until]
   (reserving d limits pool nil reqid eur now until))
  ([d limits pool source reqid eur now until]
   (when-not (refusal d limits pool source eur now)
     (assoc d :reserved (assoc (live (:reserved d) now) (str reqid)
                               (cond-> {:eur eur :until until :pool pool}
                                 source (assoc :source source)))))))

(defn releasing
  [d reqid]
  (when (contains? (:reserved d) (str reqid))
    (update d :reserved dissoc (str reqid))))

(defn settling
  [d reqid cost]
  (let [{:keys [pool source]} (get-in d [:reserved (str reqid)])]
    (cond-> (-> d
                (update :spent (fnil + 0.0) (or cost 0.0))
                (update-in [:spent-by (or pool interactive)] (fnil + 0.0) (or cost 0.0))
                (update :reserved dissoc (str reqid)))
      source (update-in [:spent-by-source source] (fnil + 0.0) (or cost 0.0)))))

(defonce ^:private day-lock (Object.))

(defn- swap-day! [store k f]
  (locking day-lock (ctl/swap! store :llm-budget k f)))

(defn admit!
  ([store reqid eur timeout-ms pool] (admit! store reqid eur timeout-ms pool nil))
  ([store reqid eur timeout-ms pool source]
   (when (some? eur)
     (let [lim  (limits store)
           now  (ctl/millis store)
           k    {:day (today store)}
           hit  (volatile! nil)]
       (swap-day! store k
                  (fn [d]
                    (if-let [r (refusal d lim pool source (double eur) now)]
                      (do (vreset! hit r) nil)
                      (do (vreset! hit nil)
                          (reserving d lim pool source reqid (double eur) now
                                     (+ now (long (or timeout-ms default-call-ms))))))))
       @hit))))

(defn reserve!
  [store reqid eur timeout-ms]
  (nil? (admit! store reqid eur timeout-ms interactive)))

(defn sources-today
  [store]
  (let [d    (or (ctl/get store :llm-budget {:day (today store)}) {})
        now  (ctl/millis store)
        open (live (:reserved d) now)
        b    (source-budgets store)]
    (vec (for [s (into (sorted-set)
                       (concat (keys (:spent-by-source d)) (keep (comp :source val) open)
                               (keys b)))]
           (cond-> {:source s
                    :spent (get-in d [:spent-by-source s] 0.0)
                    :reserved (reduce + 0.0 (for [[_ r] open :when (= s (:source r))] (:eur r)))}
             (get b s) (assoc :cap (get b s)))))))

(defn pools-today
  [store]
  (let [d     (or (ctl/get store :llm-budget {:day (today store)}) {})
        now   (ctl/millis store)
        open  (live (:reserved d) now)
        q     (project-quotas store)
        pools (into (sorted-set)
                    (concat (keys (:spent-by d))
                            (keep (comp :pool val) open)
                            (map #(str "batch:" %) (keys q))))]
    (vec (for [p pools]
           (let [proj (project-of-pool p)]
             (cond-> {:pool p
                      :kind (if (batch? p) "batch" "interactive")
                      :spent (get-in d [:spent-by p] 0.0)
                      :reserved (reduce + 0.0
                                        (for [[_ {:keys [eur pool]}] open :when (= p pool)] eur))}
               proj (assoc :project proj)
               (and proj (get q proj)) (assoc :cap (get q proj))))))))

(defn release!
  [store reqid]
  (swap-day! store {:day (today store)} #(releasing % reqid)))

(defn settle!
  ([store schema model prompt-tokens completion-tokens reqid]
   (settle! store schema model prompt-tokens completion-tokens reqid nil))
  ([store schema model prompt-tokens completion-tokens reqid cache]
   (let [cost (llm-models/cost schema model prompt-tokens completion-tokens cache)]
     (swap-day! store {:day (today store)} #(settling % reqid cost))
     nil)))
