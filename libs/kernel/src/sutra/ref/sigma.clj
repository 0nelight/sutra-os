(ns sutra.ref.sigma
  (:require [clojure.string :as str]
            [sutra.ref.err :refer [fail!]]
            [sutra.ref.params :as p]))

(set! *warn-on-reflection* true)

(defn- name-field? [k] (and (keyword? k) (str/ends-with? (name k) "-name")))

(defn- rid-of [W section x]
  (let [m (get W section)]
    (cond
      (nil? x) nil
      (contains? m x) x
      :else (some (fn [[k e]]
                    (when (and (map? e) (some (fn [[fk fv]] (and (name-field? fk) (= fv x))) e)) k))
                  m))))

(defn- attr-list [W T]
  (get-in W [:object-types (rid-of W :object-types T) :attribute-list]))

(defn- validate-row [W T values complete?]
  (let [rid (rid-of W :object-types T)]
    (when-not rid (fail! :unknown-type (str "Unknown type: " T) {:type T}))
    (let [attrs (attr-list W rid)
          known (set (map :attribute-name attrs))
          bad   (remove known (keys values))]
      (when (seq bad) (fail! :bad-attributes (str "Bad attributes on " T) {:attributes (vec bad)}))
      (when-let [vs (seq (p/violations attrs values complete?))]
        (fail! :constraint-violation "constraint violation" {:violations (vec vs)})))))

(def table
  {
   :object        {:doc "(fn [T id]) → handle | nil — its presence = \"can read\""
                   :optional true}
   :lookup        {:doc "(fn [W T q]) → [row …], q {:key [k v]} | {:id raw}, at most 2 rows"}
   :link-ends     {:doc "(fn [W L id]) → {:from-id :to-id} | nil"}
   :traverse      {:doc "(fn [self over]) → [handle …] (§5.4)" :optional true}
   :query         {:doc "(fn [W os args installed]) → result (K-query); without it
                         sutra.ref.objectset evaluates over :extension and :edges"
                   :optional true}
   :extension     {:doc "(fn [W T]) → [object]"}
   :edges         {:doc "(fn [W L]) → [[from to] …]"}
   :under         {:doc "(fn [R]) → the reads above as they see R, the run's ops so far"
                   :optional true}
   :install       {:doc "(fn [W wire-step cx]) → {:world W' :events [..]} (K-install, §8);
                         without it sutra.ref.install runs §8 itself"
                   :optional true}
   :foreign       {:doc "(fn [op step run-state cx]) → run-state' (K-foreign, §13.4)"}
   :machine-op?   {:doc "(fn [op]) → bool: an op lower does not know that runs through
                         :foreign"
                   :optional true}
   :expand        {:doc "(fn [via step run-state cx]) → {:steps :bindings :world :events :ops :out}
                         (§12)"}
   :fresh-uuid    {:doc "(fn []) → uuid"}
   :now           {:doc "(fn []) → instant"}
   :zone          {:doc "(fn []) → zone id"}
   :authorize            {:doc "(fn [op target]) → nil | ⊥" :default (fn [_op _target] nil)}
   :rid-of               {:doc "(fn [W section x]) → rid" :default rid-of}
   :object-key           {:doc "(fn [W rid])"
                          :default (fn [W rid] (get-in W [:object-types rid :object-key]))}
   :validate-row         {:doc "(fn [W T values complete?]) → nil | ⊥" :default validate-row}
   :interface?           {:doc "(fn [W i]) → bool"
                          :default (fn [W i] (contains? (:interfaces W) i))}
   :implementers         {:doc "(fn [W i]) → [ot]"
                          :default (fn [W i]
                                     (keep (fn [[k o]] (when (some #{i} (:implements o)) k))
                                           (:object-types W)))}
   :interface-properties {:doc "(fn [W i]) → [attribute]"
                          :default (fn [W i]
                                     (map :attribute-name
                                          (get-in W [:interfaces i :property-list])))}
   :pk-attribute         {:doc "(fn [W ot])"
                          :default (fn [W ot] (get-in W [:object-types ot :primary-key]))}
   :action               {:doc "(fn [W a]) — a is the action's RID or its name"
                          :default (fn [W a]
                                     (or (get-in W [:actions a])
                                         (some (fn [[_ e]] (when (= a (:action-name e)) e))
                                               (:actions W))))}
   :param-object-type    {:doc "(fn [W t]) → ot rid | nil"
                          :default (fn [W t]
                                     (cond (contains? (:object-types W) t) t
                                           (keyword? t)
                                           (or (rid-of W :object-types t)
                                               (rid-of W :object-types (keyword (name t))))))}
   :attr-types           {:doc "(fn [W ot]) → {attribute type}"
                          :default (fn [W ot]
                                     (into {} (map (juxt :attribute-name :attribute-type))
                                           (attr-list W ot)))}
   :edited-arrays        {:doc "(fn [W log-type]) → {…}" :default (fn [_W _log-type] {})}
   :log-stamps           {:doc "(fn [W log-type]) → #{attr}" :default (fn [_W _log-type] #{})}
   :user                 {:doc "(fn []) → user | nil" :default (fn [] nil)}
   :username             {:doc "(fn []) → name | nil" :default (fn [] nil)}
   :checkpoint           {:doc "(fn [run-state])" :default (fn [_st] nil)}
   :allow-notify?        {:doc "(fn []) → bool" :default (fn [] true)}
   :introspect         {:doc "(fn [body]) → body: transform code introspection (8.1.3)"
                        :default identity}
   :transform-contract {:doc "(fn [value]) → ⊥ on a contract violation (8.1.4)"
                        :default (fn [_] nil)}
   :stream             {:doc "(fn [stream]) → stream | nil: normalisation (8.1.4)"
                        :default (fn [_] nil)}
   :fk-link?           {:doc "(fn [W lt]) → bool: link type with FK backing (8.4)"
                        :default (fn [_W _lt] false)}
   :unpack             {:doc "(fn [W]) → W: legacy :objects/:links (8.5.1.1)"}
   :migrate-external   {:doc "(fn [W]) → W: external sources → transforms (8.5.1.2)"}
   :system-datasets    {:doc "(fn []) → {rid ds} (8.5.1.5)" :default (fn [] {})}
   :structure          {:doc "(fn [W]) → [finding]: structure findings (8.5.3)"
                        :default (fn [_] [])}
   :structure-ok?      {:doc "(fn [W]) → bool; without it: no :structure findings"
                        :optional true}
   :gate-problems      {:doc "{gate-id (fn [W]) → [problem]}: gates not stated in §8.5"
                        :value true :default {}}
   :lint               {:doc "(fn [W]) → [{:rule :entity}]: lint gate findings (gate 12)"
                        :default (fn [_] [])}
   :index              {:doc "(fn [W]) → W: the world the gates of one judgement share, e.g.
                               carrying indexes they would each rebuild; equal to W"
                        :default identity}
   :place              {:doc "(fn [W W1]) → W1': a step's world before it is judged, with
                               what the context places"
                        :default (fn [_W W1] W1)}
   :persisted          {:doc "(fn []) → W: the installed schema at run start (8.5.2);
                               without it there is no grandfathering"
                        :default (fn [] nil)}
   :sql-gate           {:doc "(fn [W-pre W' name-key]) → [finding] (8.5.7)"
                        :default (fn [_ _ _] nil)}})

(defn- spec-of [k]
  (or (table k) (fail! :sigma-unknown (str k " is no key of Σ") {:sigma k})))

(defn check
  [sigma]
  (let [bad (for [[k v] sigma
                  :let [spec (table k)]
                  :when (or (nil? spec) (and (not (:value spec)) (not (ifn? v))))]
              k)]
    (when (seq bad)
      (fail! :sigma-unknown (str "Σ carries what it does not declare: " (vec (sort bad)))
             {:sigma (vec (sort bad))}))
    sigma))

(defn provides?
  [cx k]
  (spec-of k)
  (contains? (:sigma cx) k))

(defn value
  [cx k]
  (let [spec (spec-of k)]
    (get (:sigma cx) k (:default spec))))

(defn call [cx k & args]
  (let [f (or (get-in cx [:sigma k])
              (:default (spec-of k))
              (fail! :sigma-missing (str "Σ provides no " k) {:sigma k}))]
    (apply f args)))

(defn under
  [cx run-state]
  (if-let [u (get-in cx [:sigma :under])]
    (update cx :sigma merge (u (:ops run-state)))
    cx))

(defn can-read? [cx]
  (provides? cx :object))
