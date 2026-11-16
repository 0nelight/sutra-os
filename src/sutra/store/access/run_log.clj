(ns sutra.store.access.run-log
  (:require [clojure.walk :as walk]
            [sutra.catalog.grants :as grants]
            [sutra.store.control.collections :as collections]
            [sutra.store.api :as api]
            [sutra.store.machine :as machine]))

(set! *warn-on-reflection* true)

(def ^:private cursor-coll (:coll collections/outbox-cursors))

(defn edn-safe
  [x]
  (walk/postwalk
   (fn [v]
     (cond
       (instance? java.util.Date v) (java.util.Date. (.getTime ^java.util.Date v))
       (or (nil? v) (string? v) (number? v) (keyword? v) (symbol? v) (boolean? v) (char? v)
           (uuid? v) (coll? v) (map-entry? v)) v
       :else (str v)))
   x))

(defn- seq-of [{:keys [rev index]}] (+ (* rev 100000) index))

(defn action-entry
  ([action-name payload objects
    provenance] (action-entry action-name payload objects provenance nil))
  ([action-name payload objects provenance action-id]
   {:kind    :run/action
    :payload (edn-safe (cond-> {:action-name (some-> action-name name)
                                :payload     payload
                                :applied-at  (str (java.time.Instant/now))}
                         action-id        (assoc :action-id (str action-id))
                         (seq objects)    (assoc :objects objects)
                         (seq provenance) (assoc :provenance provenance)))}))

(defn action-failed-entry
  [action-name action-id category error]
  {:kind    :run/action-failed
   :payload (edn-safe {:action-id   (str action-id)
                       :action-name (some-> action-name name)
                       :category    category
                       :error       error
                       :at          (str (java.time.Instant/now))})})

(defn outbox-entry
  [request authority]
  {:kind    :outbox/effect
   :outbox? true
   :payload (edn-safe {:id (str (java.util.UUID/randomUUID)) :request request
                       :authority authority})})

(defn side-effect-failed-entry
  [{:keys [id request]} ^Throwable e]
  {:kind    :run/side-effect-failed
   :payload (edn-safe {:id id :kind (:kind request) :error (ex-message e)
                       :at (str (java.time.Instant/now))})})

(defn audit-entry
  [{:keys [actor op target-type target-id detail]}]
  {:kind    :run/audit
   :payload (edn-safe {:actor       (some-> actor str)
                       :op          (name op)
                       :target-type (some-> target-type str)
                       :target-id   (some-> target-id str)
                       :detail      detail
                       :created-at  (str (java.time.Instant/now))})})

(defn effect-entries
  [effects {:keys [actor authority]}]
  (vec
   (for [{:keys [op target after outbox]} effects
         :let [e (cond
                   outbox (outbox-entry outbox authority)
                   (#{:apply-marking :remove-marking} op)
                   (audit-entry {:actor actor :op op :target-type (:type target)
                                 :target-id (:id target) :detail after}))]
         :when e]
     e)))

(defn- branch [store] (str (or (:branch store) "main")))

(defn- reading [store f]
  (with-open [s ^java.io.Closeable (api/snapshot (:db store) {:branch (branch store)})]
    (f s)))

(defn- own
  [snap kind]
  (api/read snap nil [:log/range {:kinds #{kind} :own? true}]))

(defn append!
  [store entries]
  (when (seq entries)
    (machine/command! (:db store) {:branch (branch store)}
                      (fn [_]
                        {:tx {:actor (grants/principal store) :log (vec entries)}})))
  nil)

(defn- action-row [e] (assoc (:payload e) :seq (seq-of e)))

(defn action-log
  [store]
  (reading store #(mapv action-row (own % :run/action))))

(defn action-log-tail
  [store limit]
  (reading store
           #(into []
                  (map (fn [e]
                         (select-keys (action-row e) [:seq :action-name :applied-at :objects])))
                  (api/read % nil [:log/range {:kinds #{:run/action} :own? true
                                               :newest? true :limit limit}]))))

(defn action-failures
  [store]
  (reading store
           #(into [] (map (fn [e] (assoc (:payload e) :seq (seq-of e))))
                  (rseq (own % :run/action-failed)))))

(defn copy-action-log!
  [store from to]
  (let [db      (:db store)
        entries (with-open [s ^java.io.Closeable (api/snapshot db {:branch (str (or from "main"))})]
                  (mapv #(select-keys % [:kind :payload]) (own s :run/action)))]
    (when (seq entries)
      (machine/command! db {:branch (str to)}
                        (fn [_] {:tx {:actor (grants/principal store) :log entries}})))
    nil))

(defn- cursor-key [b kind] [b (subs (str kind) 1)])

(defn pending
  [store kind]
  (reading store
           (fn [s]
             (let [b   (:branch (api/basis s))
                   cur (api/read s nil [:records/get cursor-coll (cursor-key b kind)])]
               (into []
                     (comp (filter #(or (nil? cur) (pos? (compare [(:rev %) (:index %)] cur))))
                           (map #(assoc (:payload %) ::at [(:rev %) (:index %)])))
                     (api/read s nil [:log/range (cond-> {:kinds #{kind} :own? true}
                                                   cur (assoc :after (dec (first cur))))]))))))

(defn advance!
  [store kind ids]
  (let [ids (set (map str ids))
        at  (->> (pending store kind) (filter #(ids (:id %))) (map ::at) (sort compare) last)]
    (when at
      (machine/command! (:db store) {:branch (branch store)}
                        (fn [_] {:tx {:actor (grants/principal store)
                                      :records
                                        [{:coll cursor-coll :key (cursor-key (branch store) kind)
                                          :value at}]}}))))
  nil)

(defn audit
  [store limit]
  (reading store #(into [] (comp (map (fn [e] (assoc (:payload e) :seq (seq-of e)))) (take limit))
                     (rseq (own % :run/audit)))))
