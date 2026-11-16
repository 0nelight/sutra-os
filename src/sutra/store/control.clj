(ns sutra.store.control
  (:refer-clojure :exclude [get list count swap!])
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.store.api :as api]
            [sutra.store.control.tables :as cc]
            [sutra.catalog.grants :as grants]
            [sutra.store.records :as r]
            [sutra.store.served :as served]
            [sutra.types.plan :as plan]))

(set! *warn-on-reflection* true)

(def trunk r/trunk)
(def now r/now)
(def coalesce r/coalesce)

(defn decl [t]
  (or (cc/by-coll (keyword "ctl" (name t)))
      (anomaly/raise! :fault (str "no control collection " t) {:table t})))

(defn served
  [store t]
  (if-let [l (:lane (decl t))] (update store :db served/on-lane l) store))

(defn- project-of
  [store d row]
  (let [{:keys [project own-if parent]} (cc/owners (:coll d))
        [kc kv] own-if]
    (when (map? row)
      (if (or (nil? own-if) (= kv (some-> (clojure.core/get row kc) keyword)))
        (clojure.core/get row project)
        (let [[pc key-col] parent]
          (when-let [p (clojure.core/get row pc)]
            (clojure.core/get (r/get store d {key-col p :branch (:branch row)}) project)))))))

(defn judge!
  [store auth records]
  (doseq [{:keys [coll expect value]} records
          :let  [d (cc/by-coll coll)]
          :when (cc/owners coll)
          p     (distinct (keep #(project-of store d %) [(when-not (= api/absent expect) expect)
                                                         value]))]
    (when (nil? auth)
      (anomaly/raise! :fault
                      (str "a write of " (name coll) " names its authority (sutra.store.control"
                           " upsert-as!, update-as!, delete-as!, plan-as!)")
                      {:coll coll :project p}))
    (when-not (grants/allows? auth p false :update)
      (refusal/refuse! :refusal.store/write-denied
                       {:sutra/acl-denied true :coll coll :project p
                        :operation :update}
                       {:coll-name (name coll)}))))

(defn- guarded
  [auth p]
  (fn [store t]
    (let [res (p store t)]
      (judge! store auth (:records res))
      res)))

(defn- owned? [t] (contains? cc/owners (:coll (decl t))))

(defn- judged-rows!
  [store auth t rows]
  (when (owned? t)
    (let [d (decl t)]
      (judge! store auth (for [row rows
                               :let [old (r/get store d (select-keys row (or (:conflict d)
                                                                             (:key-cols d))))]]
                           {:coll (:coll d) :expect old :value (merge old row)})))))

(defn list ([store t where] (r/list (served store t) (decl t) where))
  ([store t where opts] (r/list (served store t) (decl t) where opts)))
(defn get [store t where] (r/get (served store t) (decl t) where))
(defn count [store t where] (r/count (served store t) (decl t) where))
(defn counts [store t where attr] (r/counts (served store t) (decl t) where attr))
(defn written [store t] (r/written (served store t) (decl t)))
(defn clock
  ^java.util.Date [store]
  (if-let [c (:clock store)] (c) (java.util.Date.)))
(defn instant ^java.time.Instant [store] (.toInstant (clock store)))
(defn millis ^long [store] (.getTime (clock store)))
(defn stamp ^String [store] (str (instant store)))
(defn entry [store t row] (r/entry (decl t) row (clock store)))
(defn expect [store t where] (r/expect store (decl t) where))
(defn expecting [t row] (r/expecting (decl t) row))
(defn swapping [store t where f at] (r/swapping store (decl t) where f at))
(defn updating
  ([store t where sets at] (r/updating store (decl t) where sets nil at)))
(defn deleting [store t where] (r/deleting store (decl t) where))
(defn rewriting [t rows f] (r/rewriting (decl t) rows f))
(defn upserting [store t rows at] (r/upserting store (decl t) rows at))
(defn inserting
  [store t rows]
  {:records (mapv #(entry store t %) rows) :result (clojure.core/count rows)})

(defn plan-as!
  [store auth plans]
  (r/plan! store (mapv #(guarded auth %) plans)))

(defn plan! [store plans] (plan-as! store nil plans))

(defn- op-table [op] (when (vector? op) (second op)))

(defn ops-lane
  [ops]
  (let [ts    (vec (distinct (keep op-table ops)))
        lanes (into #{} (map #(:lane (decl %))) ts)]
    (if (< 1 (clojure.core/count lanes))
      (refusal/refuse! :refusal.store/mixed-lanes {:tables ts :lanes (vec (sort-by str lanes))})
      (first lanes))))

(defn- planner
  [op]
  (if (fn? op)
    op
    (let [[verb t a b] op]
      (case verb
        :upsert (fn [s at] (upserting s t a at))
        :insert (fn [s _] (inserting s t a))
        :delete (fn [s _] (deleting s t a))
        :update (fn [s at] (updating s t a b at))
        :swap   (fn [s at] (swapping s t a b at))
        :touch  (fn [s at] (updating s t a {} at))
        :plan   a
        (anomaly/raise! :fault (str "no commit op " verb) {:op op})))))

(defn commit!
  ([store ops] (commit! store nil ops))
  ([store auth ops]
   (let [l (ops-lane ops)]
     (plan-as! (cond-> store l (update :db served/on-lane l)) auth (mapv planner ops)))))

(defn apply!
  ([store p] (apply! store nil p nil))
  ([store auth p opts]
   (plan/apply! p #(commit! store auth %) opts)))

(defn- planned! [store auth t p] (first (r/plan! (served store t) [(guarded auth p)])))

(defn swap! [store t where f]
  (if (owned? t)
    (planned! store nil t #(r/swapping %1 (decl t) where f %2))
    (r/swap! (served store t) (decl t) where f)))
(defn insert! [store t row]
  (judged-rows! store nil t [row])
  (r/insert! (served store t) (decl t) row))
(defn upsert-as!
  ([store auth t row] (upsert-as! store auth t row nil))
  ([store auth t row opts]
   (judged-rows! store auth t [row])
   (r/upsert! (served store t) (decl t) row opts)))
(defn upsert! ([store t row] (upsert-as! store nil t row nil))
  ([store t row opts] (upsert-as! store nil t row opts)))
(defn insert-many! ([store t rows] (insert-many! store t rows nil))
  ([store t rows opts]
   (judged-rows! store nil t rows)
   (r/insert-many! (served store t) (decl t) rows opts)))
(defn update-as!
  ([store auth t where sets] (update-as! store auth t where sets nil))
  ([store auth t where sets opts]
   (if (owned? t)
     (planned! store auth t #(r/updating %1 (decl t) where sets opts %2))
     (r/update! (served store t) (decl t) where sets opts))))
(defn update! ([store t where sets] (update-as! store nil t where sets nil))
  ([store t where sets opts] (update-as! store nil t where sets opts)))
(defn delete-as!
  [store auth t where]
  (if (owned? t)
    (planned! store auth t (fn [store _] (r/deleting store (decl t) where)))
    (r/delete! (served store t) (decl t) where)))
(defn delete! [store t where] (delete-as! store nil t where))

(def ^:private delete-batch 500)

(defn delete-all!
  [store t where]
  (let [d    (decl t)
        step (fn [st _] (r/rewriting d (r/list st d where {:limit delete-batch})
                                     (constantly nil)))
        once #(if (owned? t)
                (planned! store nil t step)
                (first (r/plan! (served store t) [step])))]
    (loop [total 0]
      (let [n (once)]
        (if (pos? n) (recur (+ total (long n))) total)))))
