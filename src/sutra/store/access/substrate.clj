(ns sutra.store.access.substrate
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.counts :as counting]
            [sutra.catalog.grants :as grants]
            [sutra.store.api :as api]
            [sutra.store.served :as served]))

(set! *warn-on-reflection* true)

(defn query-request
  ([branch ir]        (query-request branch nil ir))
  ([branch as-of ir]  {:branch branch :as-of as-of :ir ir}))

(defn require-read-authority
  [authority store]
  (or authority
      (grants/read-rights store)
      (anomaly/raise!
       :fault "read reached the substrate seam with no authority (fail-closed)"
       {:seam :substrate-read
        :remedy
          "bind a principal authority, or grants/system-authority for a service read"})))

(defn- rev-of
  [as-of]
  (cond (nil? as-of) nil
        (int? as-of) as-of
        (re-matches #"\d+" (str as-of)) (parse-long (str as-of))
        :else (refusal/refuse! :refusal.store/not-a-revision {:as-of as-of})))

(defn- ask
  [store branch as-of authority query]
  (let [b (str (or branch (:branch store) "main"))
        a (require-read-authority authority store)]
    (with-open [snap ^java.io.Closeable
                     (api/snapshot (:db store)
                                   (cond-> {:branch b} (rev-of as-of) (assoc :rev (rev-of as-of))))]
      (api/read snap a query))))

(defn with-read
  [store f]
  (if (served/reads-held? (:db store))
    (f store)
    (with-open [snap ^java.io.Closeable
                     (api/snapshot (:db store) {:branch (str (or (:branch store) "main"))})]
      (f (update store :db served/holding snap)))))

(defn query
  [store {:keys [branch as-of ir authority]}]
  (ask store branch as-of authority [:select ir]))

(defn explain
  [store {:keys [branch as-of ir authority]}]
  (ask store branch as-of authority [:select/explain ir]))

(defn read-type
  ([store type-ref]       (read-type store type-ref nil))
  ([store type-ref as-of] (ask store nil as-of nil [:select {:root type-ref}])))

(defn count-type
  [store type-ref]
  (or (ask store nil nil nil [:objects/count type-ref]) 0))

(defn type-clock
  [store type-ref]
  (or (ask store nil nil grants/system-authority [:objects/clock type-ref]) 0))

(defn changed-since?
  [store type-ref rev]
  (> (type-clock store type-ref) (or rev 0)))

(defn stored-count
  [store type-ref]
  (or (ask store nil nil grants/system-authority [:objects/count type-ref]) 0))

(defn count-links
  [store L]
  (long (ask store nil nil nil [:links L {:count? true}])))

(def ^:private count-of
  {:object-type count-type
   :link-type   count-links})

(defn layout
  [store]
  (ask store nil nil grants/system-authority [:store/layout]))

(defn counts
  [store plan]
  (with-read store
    (fn [store] (mapv (fn [{:keys [kind ref]}] ((count-of kind) store ref)) (:ask plan)))))

(defn counts-by-type
  [store schema]
  (with-read store
    (fn [store]
      (let [p (counting/plan schema (:missing (layout store)) #{:object-type})]
        (:by-type (counting/tally p (counts store p)))))))

(defn reader
  [store]
  (fn [query] (ask store nil nil nil query)))

(defn select
  [reads ir]
  (reads [:select ir]))

(defn links
  [reads L where]
  (reads [:links L (or where {})]))

(defn objects
  [reads type-ref ids]
  (if (empty? ids)
    []
    (select reads {:root type-ref :where [:id-in (mapv str ids)]})))

(defn read-links
  [store L where]
  (links (reader store) L where))

(defn read-objects
  [store type-ref ids]
  (objects (reader store) type-ref ids))
