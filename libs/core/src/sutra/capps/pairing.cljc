(ns sutra.capps.pairing
  (:require [sutra.kit.coerce :refer [->kw]]))

(defn initial [id display-name] {:id id :name display-name :peers {} :paired nil :paused? false})

(defn hello [{:keys [id name]}] {:type :hello :from id :name name})

(defn bye [{:keys [id]}] {:type :bye :from id})

(defn- for-me? [{:keys [id]} {:keys [to]}] (or (nil? to) (= to id)))

(defn receive
  [{:keys [id paired paused?] :as st} {:keys [type from name vars] :as msg}]
  (cond
    (= from id)            {:state st}
    (not (for-me? st msg)) {:state st}
    :else
    (case (->kw type)
      :hello  {:state (assoc-in st [:peers from] name)
               :send  [{:type :here :from id :name (:name st) :to from}]}
      :here   {:state (assoc-in st [:peers from] name)}
      :bye    {:state    (cond-> (update st :peers dissoc from) (= paired from) (assoc :paired nil))
               :changed? (= paired from)}
      :pair   {:state    (-> st (assoc-in [:peers from] name) (assoc :paired from))
               :changed? (not= paired from)
               :send     [{:type :paired :from id :name (:name st) :to from}]
               :publish? true
               :toast    name}
      :paired {:state    (-> st (assoc-in [:peers from] name) (assoc :paired from))
               :changed? (not= paired from)
               :publish? true
               :toast    name}
      :unpair {:state    (cond-> st (= paired from) (assoc :paired nil))
               :changed? (= paired from)}
      :state  (if (and (= paired from) (not paused?))
                {:state st :apply vars}
                {:state st})
      {:state st})))

(defn pair-with
  [{:keys [id] :as st} peer]
  {:state    (assoc st :paired peer)
   :changed? true
   :send     [{:type :pair :from id :name (:name st) :to peer}]})

(defn unpair
  [{:keys [id paired] :as st}]
  {:state    (assoc st :paired nil)
   :changed? (some? paired)
   :send     (when paired [{:type :unpair :from id :to paired}])})

(defn pause
  [st paused?]
  {:state (assoc st :paused? paused?) :publish? (not paused?)})

(defn outgoing
  [{:keys [id paired paused?]} shared values]
  (when (and paired (not paused?))
    (let [vars (into {}
                     (keep (fn [{:keys [key state]}]
                             (when (not= :open-data (->kw state)) [key (get values key)])))
                     shared)]
      (when (seq vars) {:type :state :from id :to paired :vars vars}))))

(defn opens-menu?
  [mode moment]
  (case (->kw mode)
    :enabled        true
    :only-initially (= :load moment)
    :only-on-change (= :change moment)
    false))

(defn wire-row
  [x enrich?]
  (cond
    (and (map? x) enrich?) (select-keys x [:id :title :values :object-type-ref :object-type])
    (map? x)               {:id (:id x)}
    :else                  x))

(defn wire-value
  [v enrich?]
  (if (or (sequential? v) (set? v))
    (mapv #(wire-row % enrich?) v)
    (wire-row v enrich?)))

(defn scope-channel
  [scope]
  (str "sutra-app-pairing:" (or (not-empty (str scope)) "default")))
