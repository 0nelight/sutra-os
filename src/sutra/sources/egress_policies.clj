(ns sutra.sources.egress-policies
  (:require [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.kit.lifecycle :as lc]
            [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.store.control.lifecycle :as lcs]))

(set! *warn-on-reflection* true)

(defn- where
  ([] {:branch ctl/trunk})
  ([id] {:branch ctl/trunk :id id}))

(defn policy-id [address port] (str address ":" port))

(defn all [store] (vec (ctl/list store :egress-policies (where))))

(defn- norm [entry] (some-> entry str str/trim not-empty))

(defn state-of
  [{:keys [state paused]}]
  (let [s (keyword state)]
    (if (and paused (= :active s)) :paused s)))

(defn entry-state
  [policies entry]
  (when (seq policies)
    (or (some #(when (= (norm entry) (:id %)) (state-of %)) policies) :none)))

(defn permitted?
  [policies entry]
  (contains? #{nil :active :paused} (entry-state policies entry)))

(defn effective
  [policies entries]
  (filterv #(permitted? policies %) entries))

(defn assert-permitted!
  [store {:keys [egress source-name]}]
  (let [policies (all store)]
    (doseq [e egress
            :let [st (entry-state policies e)]
            :when (contains? #{:none :pending :revoked :paused} st)]
      (refusal/refuse! :refusal.foreign/egress-policy-not-active
                       {:source source-name :address (norm e) :state st}
                       {:address (norm e) :state (name st)}))))

(defn proxy-agents
  [policies entry]
  (some #(when (and (= (norm entry) (:id %)) (= :active (state-of %))) (seq (:agents %)))
        policies))

(defn- planned!
  [store plans]
  (ctl/commit! store (mapv (fn [p] [:plan :egress-policies p]) plans)))

(defn propose!
  [store {:keys [address port justification agents]}]
  (let [id (policy-id address port)]
    (or (ctl/get store :egress-policies (where id))
        (do (planned! store
                      [(fn [s _]
                         {:records
                          [(ctl/entry s :egress-policies
                                      (assoc (where id)
                                             :address (str address) :port (str port)
                                             :state :pending
                                             :agents (some->> agents seq (mapv str))
                                             :justification justification
                                             :proposed-by
                                             (grants/acting-username (grants/actor store))))]})])
            (ctl/get store :egress-policies (where id))))))

(defn- parts [entry]
  (let [e (norm entry) i (str/last-index-of e ":")]
    {:address (subs e 0 i) :port (subs e (inc i))}))

(defn propose-entry! [store entry justification]
  (propose! store (assoc (parts entry) :justification justification)))

(def ^:private lifecycle
  (let [any #{:pending :active :revoked}]
    (lc/table {:name   :egress-policy
               :states any
               :final  #{}
               :edges  {:approve {:from any :to :active}
                        :revoke  {:from any :to :revoked}}})))

(defn decide!
  [store id decision]
  (let [event (keyword decision)]
    (when-not (contains? (:edges lifecycle) event)
      (refusal/refuse! :refusal.foreign/egress-policy-decision {:decision decision}
                       {:decision (str decision)}))
    (when-not (ctl/get store :egress-policies (where id))
      (refusal/refuse! :refusal.foreign/egress-policy-unknown {:id id} {:id (str id)}))
    (ctl/apply! store
                (lcs/transition-plan
                 store :egress-policies (where id) lifecycle event
                 {:id       id
                  :state-of #(some-> (:state %) keyword)
                  :sets     (fn [_ to] {:state      to
                                        :decided-by (grants/acting-username
                                                     (grants/actor store))
                                        :decided-at (ctl/clock store)})}))
    (ctl/get store :egress-policies (where id))))

(defn adopt!
  [store declared except]
  (let [skip (set (map norm except))]
    (vec (for [e (distinct (keep norm declared))]
           (let [p (propose-entry! store e "uebernommen: die Quelle deklarierte das Ziel schon")]
             (if (and (= :pending (keyword (:state p))) (not (skip e)))
               (decide! store e :approve)
               p))))))

(defn report-dropped!
  [store dropped]
  (let [gone (set (map norm dropped))]
    (planned! store
              (for [{:keys [id paused]} (all store)
                    :let [now (contains? gone id)]
                    :when (not= (boolean paused) now)]
                (fn [s at] (ctl/updating s :egress-policies (where id) {:paused now} at))))
    {:dropped (count gone)}))
