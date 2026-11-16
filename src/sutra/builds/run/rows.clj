(ns sutra.builds.run.rows
  (:require [sutra.foundation.settings :as settings]
            [sutra.store.control :as ctl]
            [sutra.store.records :as r]
            [sutra.kit.lifecycle :as lc]))

(set! *warn-on-reflection* true)

(defn- ago ^java.util.Date [store secs]
  (java.util.Date. (- (ctl/millis store) (* 1000 (long secs)))))
(defn- in-ms ^java.util.Date [store ms]
  (java.util.Date. (+ (ctl/millis store) (long ms))))
(defn- before? [a b]
  (and a b
       (let [^java.util.Date d (if (instance? java.util.Date a) a (java.util.Date. (long a)))]
         (neg? (compare (.getTime d) (.getTime ^java.util.Date b))))))

(defn latest-succeeded-run-id [store branch]
  (:id (first (ctl/list store :runs {:branch branch :status "succeeded"} {:limit 1}))))

(defn lease-expiry [store lease-ms] (in-ms store lease-ms))

(defn- running-row
  [store branch
   {:keys [id kind actor inputs-hash inputs parent-run-id working-branch owner lease-ms] :as run}]
  (merge {:id id :branch branch :kind kind :actor actor :inputs-hash (or inputs-hash "")
          :inputs inputs :status "running" :parent-run-id parent-run-id
          :working-branch working-branch :started-at ctl/now}
         (when owner {:owner owner :lease-until (lease-expiry store lease-ms)})
         (select-keys run [:retry-count :retry-backoff-ms :abort-on-failure :targets])))

(defn start-run! [store branch run]
  (ctl/insert! store :runs (running-row store branch run))
  nil)

(defn renew-run-leases! [store owner run-ids lease-ms]
  (if (seq run-ids)
    (r/update! store (ctl/decl :runs) {:owner owner :status "running" :id (set (map str run-ids))}
               {:lease-until (lease-expiry store lease-ms)} {:all-branches? true})
    0))

(defn request-cancel!
  [store run-id reason]
  (or (r/update! store (ctl/decl :runs) {:status "running" :id (str run-id)}
                 {:cancel-requested reason} {:all-branches? true})
      0))

(defn cancel-requests
  [store owner run-ids]
  (if (seq run-ids)
    (into {}
          (keep (fn [{:keys [id cancel-requested]}]
                  (when cancel-requested [(str id) cancel-requested])))
          (r/list store (ctl/decl :runs)
                  {:owner owner :status "running" :id (set (map str run-ids))}
                  {:all-branches? true}))
    {}))

(defn note-progress!
  [store owner run-id progress]
  (r/update! store (ctl/decl :runs) {:owner owner :status "running" :id (str run-id)}
             {:progress progress} {:all-branches? true}))

(def run-table
  (lc/table {:name   :run
             :states #{:running :succeeded :failed :canceled}
             :final  #{:succeeded :failed :canceled}
             :edges  {:succeed {:from #{:running} :to :succeeded
                                :guard (fn [{:keys [lease-until]} {:keys [at]}]
                                         (when (and lease-until (before? lease-until at))
                                           "its lease lapsed — the sweep owns the run"))}
                      :fail    {:from #{:running} :to :failed}}}))

(defn- succeeded [{:keys [objects-count links-count summary commit-hash]}]
  {:status "succeeded" :finished-at ctl/now :objects-count objects-count
   :links-count links-count :summary summary :snapshot-id commit-hash})

(defn finish-run! [store branch run-id facts]
  (r/plan! store [(fn [store at]
                    (ctl/swapping store :runs {:branch branch :id run-id}
                                  (fn [old]
                                    (lc/decide run-table run-id :succeed
                                               (some-> (:status old) keyword) old {:at at})
                                    (merge old (succeeded facts)))
                                  at))])
  nil)

(defn finished-run-entry
  [store branch run facts]
  (let [row (running-row store branch run)]
    (lc/decide run-table (:id run) :succeed :running row {:at (ctl/clock store)})
    (ctl/entry store :runs (merge row (succeeded facts)))))

(defn failing-run
  [store branch run-id {:keys [error-message working-branch summary]} at]
  (ctl/updating store :runs {:branch branch :id run-id}
                {:status "failed" :finished-at ctl/now :error-message error-message
                 :working-branch (ctl/coalesce working-branch) :summary (ctl/coalesce summary)}
                at))

(defn failed-before-start!
  [store branch {:keys [id error-message] :as run}]
  (let [row (assoc (running-row store branch run)
                   :status "failed" :finished-at ctl/now :error-message error-message)]
    (r/plan! store [(fn [store at]
                      (ctl/swapping store :runs {:branch branch :id id}
                                    (fn [old] (when-not old row))
                                    at))])
    nil))

(defn fail-run! [store branch run-id facts]
  (r/plan! store [#(failing-run %1 branch run-id facts %2)])
  nil)

(def ^:private prune-page 200)

(defn prune-run-payloads!
  ([store] (prune-run-payloads! store {}))
  ([store {:keys [days apply?]}]
   (let [d   (long (or days (settings/value :run-payload-days)))
         cut (ago store (* d 86400))]
     (loop [lo nil seen #{} n 0 bytes 0]
       (let [page  (r/list store (ctl/decl :runs) {}
                           {:all-branches? true :range [:started-at lo cut]
                            :order [[:started-at :asc]] :limit prune-page})
             fresh (remove #(seen [(:branch %) (:id %)]) page)
             old   (filter #(and (or (:summary %) (:inputs %))
                                 (before? (or (:finished-at %) (:started-at %)) cut)) fresh)
             n     (+ n (clojure.core/count old))
             bytes (+ bytes (reduce + 0 (map #(+ (clojure.core/count (pr-str (or (:summary %) "")))
                                                 (clojure.core/count (pr-str (or (:inputs %) ""))))
                                             old)))]
         (when apply?
           (doseq [o old]
             (ctl/update! store :runs {:branch (:branch o) :id (:id o)}
                          {:summary nil :inputs nil})))
         (if (or (< (clojure.core/count page) prune-page) (empty? fresh))
           {:days d :applied? (boolean apply?) :runs n :bytes bytes}
           (let [hi (:started-at (last page))]
             (recur hi
                    (into #{} (comp (filter #(= hi (:started-at %))) (map (juxt :branch :id))) page)
                    n (long bytes)))))))))

(def ^:private list-attrs
  (delay (into #{} (comp (map #(if (vector? %) (first %) %)) (remove #{:inputs}))
               (:index (ctl/decl :runs)))))

(def ^:private list-cap 2000)

(defn recent-runs
  ([store branch limit kind] (recent-runs store branch limit kind nil))
  ([store branch limit kind within-seconds]
   (ctl/list store :runs (cond-> {:branch branch} kind (assoc :kind kind))
             (cond-> {:limit (min list-cap (long (or limit list-cap))) :project @list-attrs}
               within-seconds (assoc :range [:started-at (ago store within-seconds) nil])))))

(defn fetch-run [store branch run-id] (ctl/get store :runs {:branch branch :id run-id}))

(defn fetch-run-anywhere [store run-id]
  (first (r/list store (ctl/decl :runs) {:id (str run-id)}
                 {:order [[:started-at :desc]] :limit 1 :all-branches? true})))

(defn stale-running-runs [store min-age-seconds uptime-seconds]
  (let [t (ctl/clock store)]
    (filterv (fn [{:keys [lease-until started-at]}]
               (if lease-until
                 (before? lease-until t)
                 (or (before? started-at (ago store min-age-seconds))
                     (before? started-at (ago store uptime-seconds)))))
             (r/list store (ctl/decl :runs) {:status "running"} {:all-branches? true}))))

(defn running-runs-elsewhere [store owner]
  (let [t (ctl/clock store)]
    (filterv #(and (:owner %) (not= owner (:owner %)) (:lease-until %)
                   (not (before? (:lease-until %) t)))
             (r/list store (ctl/decl :runs) {:status "running"} {:all-branches? true}))))

(defn reap-run! [store branch run-id reason]
  (or (ctl/update! store :runs {:branch branch :id run-id :status "running"}
                   {:status "failed" :finished-at ctl/now :error-message reason})
      0))

(defn reap-expired-run! [store branch run-id reason]
  (let [run (ctl/get store :runs {:branch branch :id run-id})]
    (if (and run (= "running" (:status run))
             (or (nil? (:lease-until run)) (before? (:lease-until run) (ctl/clock store))))
      (reap-run! store branch run-id reason)
      0)))
