(ns ^:unit platform.retirement-local-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [platform.push-index-test]
            [sutra.authoring.install.findings.retire :as retire]
            [sutra.authoring.install.live :as live]
            [sutra.catalog.world-install :as world-install]
            [sutra.catalog.retirement :as retirement]
            [sutra.world.db :as kdb]
            [sutra.ref.install :as ri]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [support.every-index :as every]))

(def ^:private g-backed
  (gen/let [i gen/nat j gen/nat own? gen/boolean]
    (fn [W]
      (let [ots (sort-by str (keys (:object-types W)))
            dss (sort-by str (keys (:datasets W)))]
        (if (and (seq ots) (seq dss))
          (let [ot (nth ots (mod i (count ots))) ds (nth dss (mod j (count dss)))]
            (cond-> (assoc-in W [:object-types ot :backing] {:dataset ds})
              own? (-> (assoc-in [:object-types ot :dataset] ds)
                       (assoc-in [:datasets ds :kind] :object-backing))))
          W)))))

(def ^:private world
  (gen/let [W @#'platform.push-index-test/world ts (gen/vector g-backed 0 2)]
    (reduce #(%2 %1) W ts)))

(def ^:private secs [:object-types :link-types :datasets :transforms :interfaces :actions
                     :shared-properties :value-types :projects :applications])

(defn- outcome [f]
  (try {:ok (f)}
       (catch Exception e {:thrown [(class e) (ex-message e)]})))

(defn- index [W] (live/index-of (kdb/db W world-install/views)))

(defn- pick
  [W [sec n]]
  (let [ks (sort-by str (keys (get W sec)))]
    (when (seq ks) [sec (nth ks (mod n (count ks)))])))

(defn- ident-of [W [sec k]]
  (or (get-in W [sec k (rid/name-field-of sec)]) k))

(def ^:private g-at (gen/tuple (gen/elements secs) gen/nat))

(defn- each-delete
  [W [sec k :as at] cascade? ix]
  (let [id (ident-of W at)]
    {:blockers (outcome #(retirement/deletion-blockers W sec id cascade? ix))
     :warnings (outcome #(retirement/deletion-warnings W sec id ix))
     :links    (outcome #(retirement/associated-link-types W sec id ix))
     :backed   (outcome #(retirement/backing-referrers W sec id ix))
     :cascade  (outcome #(retirement/cascade-associated-links W sec id ix))
     :status   (outcome #(retirement/cascade-status-block W sec id ix))
     :owned    (outcome #(retirement/remove-owned-link-producers W sec id cascade? ix))
     :backing  (outcome #(retirement/remove-own-object-backing W sec id ix))
     :key      k}))

(defspec a-delete-reads-the-referrers-as-the-world-whole 1500
  (prop/for-all [W world at g-at cascade? gen/boolean]
    (let [node (pick W at)]
      (or (nil? node)
          (= (each-delete W node cascade? (every/index W))
             (each-delete W node cascade? (index W)))))))

(def ^:private dangling (ri/stated-gates :no-dangling-refs?))

(defn- gate-blockers
  [W going]
  (let [after  (reduce-kv (fn [s kk ids] (update s kk #(apply dissoc % ids))) W going)
        before (set (dangling W))]
    (into [] (remove #(or (contains? before %) (ents/in-role? :dangles-freely (:in %))))
          (@#'retirement/ordered (dangling after)))))

(defn- going-of [W nodes]
  (for [[sec k] (distinct nodes)
        :let [kind (if (= :actions sec) :action (get rid/kinds-key->tag sec))]
        :when kind]
    {:kind kind :entity (ident-of W [sec k]) :kinds-key sec :id k :status :experimental}))

(defspec a-retirement-reads-the-referrers-as-the-world-whole 1500
  (prop/for-all [W world ats (gen/vector g-at 1 4)]
    (let [nodes (keep #(pick W %) ats)
          going (reduce (fn [m [sec k]] (update m sec (fnil conj #{}) k)) {} nodes)
          ix    (index W)
          gs    (going-of W nodes)]
      (= [(outcome #(gate-blockers W going))
          (outcome #(retire/orphaning W gs (every/index W)))]
         [(outcome #(retirement/retirement-blockers W going ix))
          (outcome #(retire/orphaning W gs ix))]))))
