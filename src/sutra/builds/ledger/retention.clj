(ns sutra.builds.ledger.retention
  (:require [sutra.kit.refusal :as refusal]
            [malli.core :as m]
            [clojure.string :as str]
            [sutra.store.control :as ctl]
            [sutra.store.access.datasets :as dsets]
            [sutra.types.retention :as ret]))

(set! *warn-on-reflection* true)

(def ^:private day-ms (* 24 60 60 1000))

(defn- row->policy
  [{:keys [name datasets selectors allow-latest-view updated-at updated-by]}]
  (cond-> {:name name :datasets datasets :selectors (vec selectors)
           :allow-latest-view? (boolean allow-latest-view)}
    updated-at (assoc :updated-at updated-at)
    updated-by (assoc :updated-by updated-by)))

(defn policies
  [store]
  (into ret/recommended (map row->policy) (ctl/list store :retention-policies {})))

(defn set-policy!
  [store actor p]
  (let [p (-> p
              (update :datasets #(if (or (nil? %) (= "all" (str %)) (= :all %)) :all (vec %)))
              (update :selectors #(mapv (fn [[k n]] [(keyword k) (long n)]) %)))]
    (when-not (m/validate ret/policy-schema p)
      (refusal/refuse! :refusal.retention/policy-incomplete
                       {:policy p :selectors-known (sort ret/selector-kinds)}))
    (when (str/starts-with? (:name p) "recommended/")
      (refusal/refuse! :refusal.retention/recommended-unchangeable {:name (:name p)}))
    (when (empty? (:selectors p))
      (refusal/refuse! :refusal.retention/no-selectors {:name (:name p)}))
    (ctl/upsert! store :retention-policies
                 {:name (:name p) :datasets (:datasets p) :selectors (:selectors p)
                  :allow-latest-view (boolean (:allow-latest-view? p)) :updated-by actor})
    p))

(defn delete-policy!
  [store policy-name]
  (when (str/starts-with? (str policy-name) "recommended/")
    (refusal/refuse! :refusal.retention/recommended-unremovable {:name policy-name}))
  (ctl/delete! store :retention-policies {:name policy-name})
  {:deleted policy-name})

(defn- datasets-of
  [schema only]
  (for [[rid d] (sort-by (comp str :dataset-name val) (:datasets schema))
        :let [nm (some-> (:dataset-name d) name)]
        :when (or (nil? only) (= only nm) (= only (str rid)))]
    {:rid rid :name nm}))

(defn plan
  [store schema {:keys [dataset now-ms]}]
  (let [ps  (policies store)
        now (or now-ms (ctl/millis store))]
    (into []
          (keep (fn [{:keys [rid name] :as ds}]
                  (let [txns (dsets/transactions store rid)]
                    (when (seq txns)
                      (let [{:keys [horizon deleted by expire-through]}
                            (ret/plan ps ds txns now)
                            current (dsets/horizon store rid)]
                        {:dataset name :rid (str rid)
                         :transactions (count txns)
                         :horizon current
                         :proposed (when (and horizon (> (long horizon) (long current)))
                                     horizon)
                         :deletes deleted
                         :expire-through expire-through
                         :policies (vec (sort by))})))))
          (datasets-of schema dataset))))

(defn retain!
  [store schema {:keys [apply? grace-days] :as opts}]
  (let [rows  (plan store schema opts)
        grace (* (long (or grace-days ret/grace-days)) day-ms)]
    (if-not apply?
      {:dry-run true :datasets (filterv (some-fn :proposed :expire-through) rows)}
      (let [marked (into [] (keep (fn [{:keys [rid proposed] :as r}]
                                    (when proposed
                                      (assoc r :horizon
                                             (:horizon (dsets/retire-below! store rid
                                                                            proposed))))))
                         rows)
            swept  (into [] (keep (fn [{:keys [rid dataset]}]
                                    (try
                                      (let [{:keys [swept rows]} (dsets/sweep-past! store rid
                                                                                    grace)]
                                        (when (pos? (long swept))
                                          {:dataset dataset :swept swept :rows rows}))
                                      (catch InterruptedException e (throw e))
                                      (catch Exception e
                                        {:dataset dataset :error (.getMessage e)}))))
                         rows)
            main?   (= "main" (str (or (:branch store) "main")))
            expired (into [] (keep (fn [{:keys [rid dataset expire-through]}]
                                     (when (and main? expire-through)
                                       (let [n (:rows (dsets/expire! store rid expire-through))]
                                         (when (pos? (long n))
                                           {:dataset dataset :rows n})))))
                          rows)]
        {:marked (mapv #(select-keys % [:dataset :horizon :deletes :policies]) marked)
         :swept swept
         :expired expired}))))
