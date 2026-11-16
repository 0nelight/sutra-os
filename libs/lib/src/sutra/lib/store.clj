(ns sutra.lib.store
  (:refer-clojure :exclude [read])
  (:require [sutra.lib :as lib]
            [sutra.ref.clock :as clock]
            [sutra.store.api :as api]
            [sutra.store.machine :as machine]))

(set! *warn-on-reflection* true)

(def system {:system? true})

(def ^:private defaults {:branch "main" :authority system :actor {:principal "sutra.lib"}})

(defn- ask-at [s authority] (fn [q] (api/read s authority q)))

(defn- answer
  [db opts f]
  (let [{:keys [branch authority]} (merge defaults opts)]
    (with-open [s (api/snapshot db {:branch branch})]
      (f (ask-at s authority)))))

(defn installed
  ([db] (installed db {}))
  ([db opts]
   (answer db (assoc opts :authority system) #(% [:schema/world {:derived? true}]))))

(defn install!
  ([db schema] (install! db schema {}))
  ([db schema opts]
   (let [{:keys [clock actor branch]} (merge defaults {:clock clock/system} opts)
         W (lib/world schema clock (not-empty (installed db opts)))]
     (machine/command! db {:branch branch}
                       (fn [_] {:tx {:actor actor :schema {:authored W :migration []}}}))
     (installed db opts))))

(defn put!
  ([db T objects] (put! db T objects {}))
  ([db T objects opts]
   (let [{:keys [actor branch]} (merge defaults opts)]
     (machine/command! db {:branch branch}
                       (fn [_] {:tx {:actor actor
                                     :ops (for [{:keys [id values]} objects]
                                            {:op :create-object :type T
                                             :payload {:id (str id) :values values}})}})))))

(defn read
  ([db W T ids] (read db W T ids {}))
  ([db W T ids opts]
   (answer db opts (fn [ask] (lib/read-objects W ask T ids)))))

(defn select
  ([db W ir] (select db W ir {}))
  ([db W ir opts] (answer db opts #(lib/select W % ir))))

(defn query
  ([db W n args] (query db W n args {}))
  ([db W n args opts] (answer db opts #(lib/query W % n args))))

(defn- recording
  [ask seen]
  (fn [q]
    (let [r (ask q)]
      (when (#{:objects :select} (first q))
        (vswap! seen into (keep (fn [o] (when (:object-type-ref o)
                                          [:object (:object-type-ref o) (str (:id o))
                                           (api/object-version o)])))
                r))
      r)))

(defn fences
  [seen ops]
  (let [written (into #{} (map (juxt :type (comp str :id :payload))) ops)]
    (filterv (fn [[_ T id]] (contains? written [T id])) seen)))

(defn decide
  [s W an args opts]
  (let [{:keys [clock actor authority]} (merge defaults {:clock clock/system} opts)
        seen (volatile! #{})
        r    (lib/run-action W (lib/sigma W (recording (ask-at s authority) seen) clock) an args)]
    (cond-> {:result r}
      (and (not (:error r)) (seq (:ops r)))
      (assoc :tx {:actor actor :ops (:ops r) :fences (fences @seen (:ops r))}))))

(defn submit!
  ([db W an args] (submit! db W an args {}))
  ([db W an args opts]
   (let [d (machine/command! db {:branch (:branch (merge defaults opts))}
                             #(decide % W an args opts))]
     (cond-> (:result d) (:committed d) (assoc :committed (:committed d))))))
