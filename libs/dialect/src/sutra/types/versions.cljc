(ns sutra.types.versions
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]))

(def history-keys
  [:version :releases :published :release-notes :revision])

(def ^:private not-content
  (into #{:name :rid :auto-publish} cat
        [(vals rid/name-field-of) ents/placement-fields history-keys]))

(defn of [section] (get-in ents/by-section [section :versions]))

(defn mode [section] (:mode (of section)))

(defn publishes? [section] (= :publish (mode section)))

(def publish-sections (into #{} (filter publishes?) ents/sections))

(defn snapshot
  [spec]
  (apply dissoc spec not-content))

(defn- serial [v] (parse-long (str v)))

(defn versions
  [spec]
  (vec (sort-by (fn [v] [(or (serial v) ##Inf) (str v)]) (keys (:releases spec)))))

(defn latest [spec] (peek (versions spec)))

(defn next-version [spec]
  (str (inc (reduce max 0 (keep serial (versions spec))))))

(defn publish
  [spec {:keys [version note by at]}]
  (cond
    version
    (if (contains? (:releases spec) version)
      (assoc spec :published version)
      (anomaly/coded! :refusal.dialect/no-version-to-publish :incorrect
        "no version {version} to publish — versions: {versions}"
        {:version version :versions (versions spec)}))

    (and (latest spec) (= (snapshot spec) (get-in spec [:releases (latest spec)])))
    (assoc spec :published (latest spec))

    :else
    (let [v (next-version spec)]
      (-> spec
          (assoc-in [:releases v] (snapshot spec))
          (assoc-in [:release-notes v]
                    (cond-> {} note (assoc :note note) by (assoc :by by) at (assoc :at at)))
          (assoc :published v)))))

(defn published?
  [spec]
  (contains? (:releases spec) (some-> (:published spec) str)))

(defn publish-head
  [want live moved?]
  (if (or (nil? live)
          (and (:auto-publish want) (or moved? (not (published? want)))))
    (publish want {})
    want))

(defn at-version
  [spec v]
  (if-let [s (get-in spec [:releases v])]
    (merge (apply dissoc spec (keys (snapshot spec))) s)
    (anomaly/coded! :refusal.dialect/no-version :incorrect "no version {version}"
      {:version v :versions (versions spec)})))

(defn effective
  [spec]
  (if (and (:published spec) (get-in spec [:releases (:published spec)]))
    (at-version spec (:published spec))
    spec))

(defn as-version
  [spec v]
  (cond (nil? v)       (effective spec)
        (= "draft" v)  spec
        :else          (at-version spec (str v))))

(defn published-view
  [schema]
  (reduce (fn [s section] (cond-> s (map? (get s section)) (update section update-vals effective)))
          schema publish-sections))

(defn- section-view
  [specs trunk-specs]
  (reduce-kv (fn [m rid spec]
               (assoc m rid (if (= (snapshot spec) (some-> (get trunk-specs rid) snapshot))
                              (effective spec)
                              spec)))
             {} specs))

(defn branch-view
  [schema trunk]
  (reduce (fn [s section]
            (cond-> s
              (map? (get s section)) (update section section-view (get trunk section))))
          schema publish-sections))

(defn carry
  [want live]
  (let [merged (fn [k] (not-empty (merge (get live k) (get want k))))]
    (cond-> want
      (merged :releases)      (assoc :releases (merged :releases))
      (merged :release-notes) (assoc :release-notes (merged :release-notes))
      (and (nil? (:published want)) (:published live)) (assoc :published (:published live)))))

(defn authored
  [section spec]
  (cond->> (dissoc spec :revision) (publishes? section) (#(apply dissoc % history-keys))))

(defn listing
  [spec]
  (let [pub (:published spec)]
    {:published pub
     :draft?    (not= (snapshot spec) (get-in spec [:releases pub]))
     :versions  (vec (for [v (rseq (versions spec))]
                       (merge {:version v :published? (= v pub)}
                              (get-in spec [:release-notes v]))))}))
