(ns sutra.kit.system
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]))

(set! *warn-on-reflection* true)

(def registry
  (edn/read-string (slurp (io/resource "sutra/kit/plugin.malli.edn"))))

(def ^:private opts {:registry (merge (m/default-schemas) (:shapes registry))})

(def ^:private schema (memoize (fn [s] (m/schema s opts))))

(defn- problem [s v] (some-> (m/explain (schema s) v) me/humanize))

(defn- refuse! [msg data] (anomaly/raise! :fault msg data))

(defn entry-problem
  [t k v]
  (let [{ks :key es :entry} (get-in registry [:tables t])]
    (or (some->> (problem ks k) (hash-map :key))
        (some->> (problem es v) (hash-map :entry)))))

(defn check-module
  [mod]
  (when-let [p (problem :plugin/module mod)]
    (refuse! (str "module " (:name mod) " is malformed: " (pr-str p))
             {:module (:name mod) :problem p}))
  (let [{:keys [name provides opens contributes requires wants]} mod
        caps (:capabilities registry)]
    (doseq [k (concat (keys provides) (keys opens) requires wants)
            :when (not (contains? caps k))]
      (refuse! (str "module " name " names " k ", which is not a registered capability")
               {:module name :capability k}))
    (doseq [[k v] provides
            :let [p (problem (get-in caps [k :schema]) v)]
            :when p]
      (refuse! (str "module " name " provides " k " in the wrong shape: " (pr-str p))
               {:module name :capability k :problem p}))
    (doseq [[t entries] contributes
            :let [spec (get-in registry [:tables t])]]
      (when-not spec
        (refuse! (str "module " name " contributes to " t ", which is not a registered table")
                 {:module name :table t}))
      (doseq [[k v] entries
              :let [p (entry-problem t k v)]
              :when p]
        (refuse! (str "module " name " contributes " t " " (pr-str k) " in the wrong shape: "
                      (pr-str p))
                 {:module name :table t :entry k :problem p})))
    mod))

(defn- instrumented
  [ps]
  (into {} (for [[k v] ps
                 :let [sch (schema (get-in registry [:capabilities k :schema]))]]
             [k (if (#{:=> :function} (m/type sch))
                  (m/-instrument {:schema sch} v)
                  v)])))

(defn instrument
  [s]
  (-> s (update :provides instrumented) (assoc :instrumented? true)))

(def ^:private empty-system
  {:provides {} :opens {} :tables {} :owner {} :requires [] :wants [] :jobs []})

(defonce ^:private installed (atom empty-system))

(defn process
  []
  @installed)

(defn of
  [store]
  (or (::system store) (process)))

(defn provided
  [s k]
  (get-in s [:provides k]))

(defn required
  [s k]
  (or (provided s k)
      (anomaly/raise! :fault (str "no module provides " k) {:capability k})))

(defn table
  [s t]
  (get-in s [:tables t] {}))

(defn- claim
  [system path module]
  (if-let [prior (get-in system (into [:owner] path))]
    (anomaly/raise! :fault (str (pr-str path) " is contributed by both " prior " and " module)
                    {:path path :modules [prior module]})
    (assoc-in system (into [:owner] path) module)))

(defn- add [system path module v]
  (-> system (claim path module) (assoc-in path v)))

(defn- add-leaves
  [system path module v]
  (if (map? v)
    (reduce-kv (fn [s k v] (add-leaves s (conj path k) module v)) system v)
    (add system path module v)))

(defn- fold [system {:keys [name provides opens contributes requires wants jobs]}]
  (as-> system s
    (reduce-kv (fn [s k v] (add s [:provides k] name v)) s provides)
    (reduce-kv (fn [s k f]
                 (-> (claim s [:provides k] name)
                     (assoc-in [:opens k] {:open f :module name :requires (set requires)})))
               s opens)
    (reduce-kv (fn [s t entries] (add-leaves s [:tables t] name entries)) s contributes)
    (update s :requires into (map (fn [k] [k name])) requires)
    (update s :wants into (map (fn [k] [k name])) wants)
    (update s :requires into (when (some :tick jobs) [[:jobs/tick name]]))
    (update s :jobs into (map #(assoc % :module name)) jobs)))

(defn- absent [system needs]
  (reduce (fn [acc [k m]]
            (cond-> acc
              (not (or (contains? (:provides system) k) (contains? (:opens system) k)))
              (update k (fnil conj (sorted-set)) m)))
          (sorted-map)
          needs))

(defn assemble
  [modules]
  (run! check-module modules)
  (let [system  (reduce fold empty-system modules)
        missing (absent system (:requires system))]
    (when (seq missing)
      (anomaly/raise! :fault (str "capabilities required but provided by no module: "
                                  (str/join ", "
                                            (map (fn [[k ms]] (str k " (" (str/join ", " ms) ")"))
                                                 missing)))
                      {:missing missing}))
    (assoc system :unwired (absent system (:wants system)))))

(defn contributions
  [system t]
  (get-in system [:tables t] {}))

(defn role-problem
  [{:keys [provides opens requires wants jobs contributes]}]
  (or (when (seq provides) (str "provides " (str/join ", " (sort (keys provides)))))
      (when (seq opens) (str "opens " (str/join ", " (sort (keys opens)))))
      (when (seq requires) (str "requires " (str/join ", " (sort requires))))
      (when (seq wants) (str "wants " (str/join ", " (sort wants))))
      (when (seq jobs) "runs jobs")
      (some (fn [t] (when-not (get-in registry [:tables t :role])
                      (str "contributes to " t ", which is no role")))
            (sort (keys contributes)))))

(defn discover
  ([] (discover nil))
  ([only]
   (->> (enumeration-seq (.getResources (.getContextClassLoader (Thread/currentThread))
                                        "sutra/plugin.edn"))
        (mapcat (fn [url]
                  (let [p (edn/read-string (slurp url))]
                    (when-let [why (problem :plugin/edn p)]
                      (refuse! (str url " is malformed: " (pr-str why))
                               {:plugin (str url) :problem why}))
                    (when (or (nil? only) (and (= :connectors only) (:connectors p)))
                      (for [sym (:modules p)] [sym (:roles-only p) url])))))
        distinct
        (sort-by first)
        (mapv (fn [[sym roles-only? url]]
                (let [mod (deref (requiring-resolve sym))]
                  (when-let [why (and roles-only? (role-problem mod))]
                    (refuse! (str url ": module " (:name mod) " " why
                                  " — a {:roles-only true} plugin"
                                  " contributes only to role tables")
                             {:plugin (str url) :module (:name mod) :problem why}))
                  mod))))))

(defn config
  [s]
  (:config s))

(defn- open-one
  [config opened [k {:keys [open module]}]]
  (let [v (open config opened)]
    (when-let [p (problem (get-in registry [:capabilities k :schema]) v)]
      (refuse! (str "module " module " opens " k " in the wrong shape: " (pr-str p))
               {:module module :capability k :problem p}))
    (assoc opened k v)))

(defn- open-all
  [opens config]
  (loop [todo (into (sorted-map) opens) opened {}]
    (if (empty? todo)
      opened
      (let [ready? (fn [[_ {:keys [requires]}]]
                     (every? #(or (contains? opened %) (not (contains? opens %))) requires))
            ready  (filter ready? todo)]
        (when (empty? ready)
          (refuse! (str "capabilities opened in a cycle: " (str/join ", " (keys todo)))
                   {:opens (vec (keys todo))}))
        (recur (apply dissoc todo (map key ready))
               (reduce (partial open-one config) opened ready))))))

(defn configure
  [s config]
  (let [opened (open-all (:opens s) config)]
    (-> s
        (assoc :config config :opened opened)
        (update :provides merge (cond-> opened (:instrumented? s) instrumented)))))

(defn close!
  [s]
  (doseq [v (vals (:opened s)) :when (instance? java.lang.AutoCloseable v)]
    (.close ^java.lang.AutoCloseable v)))

(defn install!
  [system]
  (when (and (seq (:opens system)) (not (contains? system :opened)))
    (refuse! (str "the system opens " (str/join ", " (sort (keys (:opens system))))
                  " — configure it before it is installed")
             {:opens (vec (keys (:opens system)))}))
  (doseq [[k ms] (:unwired system)]
    (println (str "[system] " k " is not wired — wanted by " (str/join ", " (sort ms)))))
  (reset! installed (assoc system :armed (atom {}))))

(defn- interval-ms
  [lookup {:keys [every default]}]
  (let [v  (lookup every)
        ms (if (number? v) (long v) (some-> v str str/trim not-empty parse-long))]
    (some-> (if (some? ms) ms default) long (#(when (pos? %) %)))))

(defn- in-start-order
  [jobs]
  (sort-by #(cond (:tick %) 2 (:every %) 1 :else 0) jobs))

(defn- job-key [job] [(:module job) (:name job)])

(defn- arm!
  [s ticker {:keys [start tick] :as job} ms]
  (when ms (if tick ((:start ticker) job ms) (start ms)))
  (swap! (:armed s) assoc (job-key job) ms))

(defn start-jobs!
  [s lookup]
  (let [ticker (provided s :jobs/tick)]
    (doseq [{:keys [every start tick] :as job} (in-start-order (:jobs s))
            :when (or start tick)]
      (if (or tick every)
        (arm! s ticker job (interval-ms lookup job))
        (start)))))

(defn rearm-jobs!
  [s lookup]
  (let [ticker (provided s :jobs/tick)]
    (doseq [{:keys [stop tick] :as job} (in-start-order (:jobs s))
            :let [k (job-key job)]
            :when (contains? @(:armed s) k)
            :let [was (get @(:armed s) k)
                  now (interval-ms lookup job)]
            :when (not= was now)]
      (when was (if tick ((:stop ticker) job) (when stop (stop))))
      (println (str "[system] " (name (:name job)) " re-armed: "
                    (if now (str "every " now "ms") "stopped")))
      (arm! s ticker job now))))

(defn stop!
  [s]
  (let [ticker (provided s :jobs/tick)]
    (doseq [{:keys [stop tick] :as job} (reverse (in-start-order (:jobs s)))]
      (cond
        tick (when ticker ((:stop ticker) job))
        stop (stop))))
  (compare-and-set! installed s empty-system))
