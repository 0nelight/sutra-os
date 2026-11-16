(ns sutra.catalog.world-derived
  (:require [sutra.capps.refs :as app-refs]
            [sutra.capps.variables :as vars]
            [sutra.world.db :as kdb]
            [sutra.world.delta :as d]
            [sutra.world.index :as ix]
            [sutra.world.view :as view]
            [sutra.ref.install :as ri]
            [sutra.types.refs :as refs]
            [sutra.types.world-rules :as rules]))

(set! *warn-on-reflection* true)

(defn- named-refs
  [sec e]
  (filterv (fn [[_ x]] (and (some? x) (not (ri/rid? x))))
           (refs/ref-values (get ri/refs-of sec) e)))

(defn- outputs [t] (concat (some-> (:output t) vector) (:outputs t)))

(defn- named-datasets
  [t]
  (for [o (outputs t)
        :let [{:keys [kind dataset]} (when (map? o) o)]
        :when (and (= :dataset kind) dataset (not (ri/rid? dataset)))]
    dataset))

(def ^:private section-rules
  {:object-types {:unbacked? (fn [e] (nil? (:dataset e)))}
   :transforms   {:unbacked? (fn [e]
                               (cond (:output-columns e) (nil? (:dataset e))
                                     (:output e)         (nil? (get-in e [:output :dataset]))
                                     (seq (:outputs e))  (not-every? :dataset (:outputs e))
                                     :else               false))
                  :datasets  named-datasets}})

(defn- rule [sec k] (get-in section-rules [sec k] (constantly nil)))

(defn- unbacked?
  [sec k e]
  (boolean (and (ri/rid? k) ((rule sec :unbacked?) e))))

(defn- name-key [x] (if (string? x) (keyword x) x))

(defn- facts
  [sec e]
  (concat (for [[ts x] (named-refs sec e) t ts] [:by-name [t (name-key x)]])
          (for [n ((rule sec :datasets) e)] [:by-name-str [:datasets (ix/nm n)]])))

(defn- underived?
  [db [sec k] e]
  (or (and (contains? ri/rid-kinds sec)
           (not (and (ri/well-formed-for? sec k) (= k (:id e)))))
      (unbacked? sec k e)
      (some #(empty? (kdb/nodes db :by-name-str [:datasets (ix/nm %)])) ((rule sec :datasets) e))
      (some (fn [[ts x]] (some #(seq (kdb/nodes db :by-name [% (name-key x)])) ts))
            (named-refs sec e))))

(defn- judged
  [st db [sec :as node]]
  (let [e (kdb/entry db node)]
    (-> st
        (ix/refile-record [:of] {[:deps] identity} node
                          (when (map? e) (not-empty (set (facts sec e)))))
        (update :unfixed (if (and (map? e) (underived? db node e)) conj disj) node))))

(def ^:private empty-fixed {:unfixed #{}})

(def fixed
  {:id    ::fixed
   :reads #{}
   :init  (fn [db]
            (reduce (fn [st node] (judged st db node))
                    empty-fixed
                    (for [[sec es] (kdb/world db) :when (map? es) k (keys es)] [sec k])))
   :step  (fn [old changes db0 db1]
            (let [ns    (d/nodes changes)
                  moved (ix/moved (kdb/world db0) (kdb/world db1) ns)]
              (reduce (fn [st node] (judged st db1 node))
                      old
                      (into (set ns) (mapcat #(get-in old [:deps %])) moved))))})

(defn unfixed
  [d]
  (sort-by str (:unfixed (kdb/view d ::fixed))))

(defn- type-of
  [e]
  (when (and (:form-factor e) (map? (:layout e))) (:object-type e)))

(defn- lifted-app
  [db e]
  (app-refs/lift-application (vars/lift-url-flags e)
                             #(some-> (kdb/entry db [:object-types %]) :object-type-name)))

(defn- lifted-at [st db k]
  (let [e (kdb/entry db [:applications k])
        a (when (some? e) (lifted-app db e))]
    (-> st
        (ix/refile-record [:type-of] {[:by-type] #(some-> % vector)} k (some-> e type-of))
        (update :lifted #(if (not= a e) (assoc % k a) (dissoc % k))))))

(def lifted
  {:id    ::lifted
   :reads #{:applications :object-types}
   :init  (fn [db]
            (reduce (fn [st k] (lifted-at st db k))
                    {:lifted {}}
                    (keys (get (kdb/world db) :applications))))
   :step  (fn [old changes _ db1]
            (reduce (fn [st k] (lifted-at st db1 k))
                    old
                    (into (view/touched changes :applications)
                          (mapcat #(get-in old [:by-type %]))
                          (view/touched changes :object-types))))})

(defn lifted-apps
  [d]
  (:lifted (kdb/view d ::lifted)))

(def signature
  {:id    ::sig
   :reads #{}
   :init  (fn [db] (rules/sig-init (kdb/world db)))
   :step  (fn [old changes db0 db1]
            (rules/sig-step old (kdb/world db0) (kdb/world db1) (d/nodes changes)))})

(defn sig-state
  [d]
  (kdb/view d ::sig))

(def rows [fixed lifted])
