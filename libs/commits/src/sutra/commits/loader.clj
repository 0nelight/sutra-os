(ns sutra.commits.loader
  (:require [sutra.world.db :as kdb]
            [sutra.world.delta :as d]
            [sutra.commits.log :as log]
            [sutra.store.core.kv :refer [authored-k kv-get kv-pages kv-scan rev-key]]
            [sutra.store.core.reads :as reads]
            [sutra.store.core.resources :as res]
            [sutra.store.memo :as memo]))

(set! *warn-on-reflection* true)

(def caches
  {::db {:cap 32}})

(defn- began-anew? [[_ e]] (or (:fork e) (:converted e)))

(defn- upgrade-of [store] (or (:upgrade store) identity))

(defn- last-entry [store h b]
  (first (kv-scan store h [:wlog b] {:upto (rev-key Long/MAX_VALUE) :reverse? true :limit 1})))

(def ^:private builds
  (atom {}))

(defn- counted! [why b]
  (let [n (get (swap! builds update why (fnil inc 0)) why)]
    (when (or (= 1 n) (zero? (mod n 100)))
      (binding [*out* *err*]
        (println "sutra.impl built" (pr-str {:reason why :branch b :n n}))))))

(defn- built
  [store h b rows why]
  (counted! why b)
  (let [[k e]   (last-entry store h b)
        upgrade (upgrade-of store)
        all     (into {} (map (fn [[[id _] v]] [id v]))
                      (reads/fetch store h (vec (reads/head-revs store h b))))
        vs      (dissoc all res/frame-id)
        twice   (keep (fn [[n ids]] (when (next ids) n))
                      (group-by #(first (log/upgraded upgrade % {})) (keys vs)))
        _       (when (seq twice)
                  (throw (ex-info "a node kept under its name and its rid"
                                  {:precondition :rekeyed :branch b :nodes (vec (take 5 twice))})))
        e0      (kdb/db {})
        cs      (log/changes e0 upgrade vs)
        W       (d/apply-to {} (d/around (log/frame-changes e0 (get all res/frame-id {})) cs))]
    {:at    (some-> k res/read-rev)
     :entry e
     :db    (kdb/db W rows)}))

(defn- versions
  [store h es]
  (let [tos (reduce (fn [m [_ e]]
                      (reduce-kv (fn [m id [from to]] (if (or from to) (assoc m id to) m))
                                 m (:changes e)))
                    {} es)
        got (reads/fetch store h (into [] (keep (fn [[id r]] (when r [id r]))) tos))]
    (into {} (map (fn [[id r]] [id (when r (get got [id r]))])) tos)))

(defn- stepped
  [store db vs]
  (let [db    (assoc db :delta [])
        frame (when (contains? vs res/frame-id) (or (get vs res/frame-id) {}))
        cs    (log/changes db (upgrade-of store) (dissoc vs res/frame-id))]
    (kdb/db-with db (d/around (when frame (log/frame-changes db frame)) cs))))

(defn- carried
  ([store h b kept] (carried store h b kept nil))
  ([store h b {:keys [at entry db] :as kept} upto]
   (if-not (and at (or (nil? upto) (<= at upto)) (= entry (kv-get store h (res/log-k b at))))
     :not-past
     (let [es (vec (kv-pages store h [:wlog b]
                             (cond-> {:after (rev-key at)} upto (assoc :upto (rev-key upto)))))]
       (cond
         (some began-anew? es) :began-anew
         (empty? es)           kept
         :else                 (let [[k e] (peek es)]
                                 {:at    (res/read-rev k)
                                  :entry e
                                  :db    (stepped store db (versions store h es))}))))))

(defn- reached
  [c]
  (when (map? c) c))

(defn- forked
  [{:keys [memo] :as store} h b rows]
  (let [[k e] (first (kv-scan store h [:wlog b] {:limit 1}))]
    (when-let [{f :from r :rev} (:fork e)]
      (when-let [[parent] (memo/hit memo ::db [f rows])]
        (when-let [at-r (reached (carried store h f parent r))]
          (reached (carried store h b {:at (res/read-rev k) :entry e
                                       :db (assoc (:db at-r) :delta [])})))))))

(defn- kept-db [{:keys [memo] :as store} h b rows]
  (let [k      [b rows]
        [old]  (memo/hit memo ::db k)
        c      (if old (carried store h b old) :cold)
        kept   (or (reached c)
                   (forked store h b rows)
                   (built store h b rows c))]
    (when-not (identical? kept old) (memo/put! memo caches ::db [[k kept]]))
    (:db kept)))

(defn head-db
  [store h b rows]
  (if-not (reads/kept-since store h b)
    (if (kv-get store h (authored-k b))
      (throw (ex-info "branch kept as one text" {:precondition :one-text :branch b}))
      (kdb/db {} rows))
    (kept-db store h b rows)))
