(ns sutra.cli.hints
  (:require [sutra.i18n.core :as i18n]
            [sutra.parser.step-verbs :as sv]
            [sutra.types.endpoint-verbs :as ev]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def AntiPattern
  [:map {:closed true}
   [:rule :string]
   [:why {:optional true} :string]
   [:doc {:optional true} :string]])

(def Hint
  [:map {:closed true}
   [:theme   {:optional true} :keyword]
   [:purpose :string]
   [:when    {:optional true} :string]
   [:patterns      {:optional true} [:vector :string]]
   [:anti-patterns {:optional true} [:vector AntiPattern]]
   [:examples      {:optional true} [:vector :string]]
   [:see-also      {:optional true} [:vector :keyword]]])

(def Registry
  [:map {:closed true}
   [:verbs    [:map-of :keyword Hint]]
   [:concepts {:optional true} [:map-of :keyword Hint]]])

(def ^:private hints-path "cli/hints.edn")

(defn- read-source
  []
  (some-> (io/resource hints-path) slurp))

(defn load-registry
  []
  (if-let [src (read-source)]
    (edn/read-string src)
    {:verbs {} :concepts {}}))

(defn- overlay
  [locale]
  (some-> (io/resource (str "cli/hints." (name locale) ".edn")) slurp edn/read-string))

(defn- fold-overlay
  [base ov]
  (reduce (fn [m sek] (update m sek #(merge-with merge % (get ov sek))))
          base
          [:verbs :concepts]))

(def ^:private per-locale (atom {}))

(defn- locale-registry
  [l]
  (let [base (load-registry)]
    (if-let [ov (overlay l)] (fold-overlay base ov) base)))

(defn problems
  [l]
  (let [explain  (requiring-resolve 'malli.core/explain)
        humanize (requiring-resolve 'malli.error/humanize)]
    (some-> (explain Registry (locale-registry l)) humanize)))

(def ^:private declared-hints
  (into {} (map (fn [[v {:keys [sum]}]] [v {:purpose sum}])) ev/cli-verbs))

(defn registry
  []
  (let [l (i18n/locale)]
    (or (get @per-locale l)
        (let [data (update (locale-registry l) :verbs #(merge-with merge declared-hints %))]
          (swap! per-locale assoc l data)
          data))))

(defn verb-hint    [v] (get-in (registry) [:verbs (keyword v)]))
(defn concept-hint [c] (get-in (registry) [:concepts (keyword c)]))

(defn topic-hint
  [topic]
  (let [k (keyword topic)]
    (cond
      (verb-hint k)    [:verb (verb-hint k)]
      (concept-hint k) [:concept (concept-hint k)]
      :else            nil)))

(defn verbs-by-theme
  []
  (->> (:verbs (registry))
       (group-by (fn [[_ h]] (or (:theme h) :other)))
       (into (sorted-map))
       (reduce-kv (fn [m g pairs] (assoc m g (sort-by key pairs))) {})))

(defn concepts
  []
  (sort-by key (:concepts (registry))))

(defn coverage-gap
  []
  (->> sv/verbs
       (remove #(some-> (verb-hint %) :purpose))
       set))
