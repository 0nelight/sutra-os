(ns support.store
  (:require [sutra.store.access.run-log :as run-log]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.signature :as signature]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.tx-vector :as tv]
            [sutra.store.world :as world]))

(def ^:private actor {:principal "test"})

(defn fork!
  ([b] (fork! "main" b))
  ([parent b] (world/fork! (ctx/store) parent b)))

(defn retire! [b] (world/retire! (ctx/store) b))

(defn fresh-branch!
  [prefix]
  (let [b (str prefix "-" (subs (str (random-uuid)) 0 8))]
    (fork! b)
    b))

(defmacro with-fresh-branch
  [[sym prefix] & body]
  `(let [~sym (fresh-branch! ~prefix)]
     (try ~@body
          (finally (try (retire! ~sym) (catch Exception _# nil))))))

(defn install!
  [b W]
  (world/install! (ctx/store b) (ri/derived {:sigma clock/system} W)
                  {:dataset-decls signature/datasets-of :actor actor}))

(defn commit-ops!
  [b ops]
  (world/commit! (ctx/store b) {:ops (vec ops) :actor actor :at-head? true}))

(defn submit-tx-vector!
  [store txv]
  (let [writes (filterv tv/store-write? txv)
        log    (run-log/effect-entries (remove tv/store-op? txv)
                                       {:actor (grants/acting-for store)})]
    (when (or (seq writes) (seq log))
      (world/commit! store {:ops writes :log log :actor (grants/principal store)
                            :at-head? true}))
    txv))

(defn create-object
  [T {:keys [id values markings]}]
  {:op :create-object :type T
   :payload (cond-> {:id (str id) :values (or values {})}
              (seq markings) (assoc :markings (vec markings)))})

(defn update-object [T id values]
  {:op :update-object :type T :payload {:id (str id) :values values}})

(defn delete-object [T id]
  {:op :delete-object :type T :payload {:id (str id)}})

(defn create-link
  [L {:keys [id from-id to-id markings]}]
  {:op :create-link :type L
   :payload (cond-> {:id (str (or id (str from-id "->" to-id))) :from-id (str from-id)
                     :to-id (str to-id)}
              (seq markings) (assoc :markings (vec markings)))})

(defn delete-link [L id]
  {:op :delete-link :type L :payload {:id (str id)}})

(defn insert-object! [b T row] (commit-ops! b [(create-object T row)]))
(defn insert-link! [b L link] (commit-ops! b [(create-link L link)]))
(defn update-object! [b T id values] (commit-ops! b [(update-object T id values)]))
(defn delete-object! [b T id] (commit-ops! b [(delete-object T id)]))

(defn objects
  [b T]
  (sub/read-type (ctx/as-system (ctx/store b)) T nil))

(defn object [b T id]
  (first (sub/read-objects (ctx/as-system (ctx/store b)) T [(str id)])))

(defn links [b L]
  (sub/read-links (ctx/as-system (ctx/store b)) L nil))
