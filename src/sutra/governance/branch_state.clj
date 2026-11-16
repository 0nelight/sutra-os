(ns sutra.governance.branch-state
  (:require [sutra.foundation.ops :as op]
            [sutra.store.control :as ctl]
            [sutra.store.control.lifecycle :as lifecycle]
            [sutra.store.world :as world]
            [sutra.kit.lifecycle :as lc]
            [clojure.string :as str]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.catalog.branch-policy :refer
             [actor-name branch-lifecycle branch-state default-branch-name effective ended-in
              ended? owner-role retention-action status-of]]
            [sutra.governance.proposals :as proposals]
            [sutra.governance.protection :as protection]
            [sutra.governance.roles :as roles]))

(set! *warn-on-reflection* true)

(defn- age-days
  [store v]
  (some-> (proposals/->instant v)
          (as-> i (.toDays (java.time.Duration/between i (proposals/now store))))))

(defn- system-actor?
  [principal]
  (let [p (str principal)]
    (or (str/blank? p) (str/starts-with? p "sutra."))))

(defonce ^:private people-tips (atom {}))

(def ^:private commits-looked-at 50)

(defn- by-people
  [store b rev]
  (let [k [(:db store) b rev]]
    (if-let [hit (find @people-tips k)]
      (val hit)
      (let [c (->> (world/schema-commits (world/at store b)
                                         {:newest? true :limit commits-looked-at})
                   (remove #(system-actor? (get-in % [:actor :principal])))
                   first)]
        (swap! people-tips #(assoc (if (> (count %) 4096) {} %) k c))
        c))))

(defn- world-tips
  [store]
  (let [bs   (mapv :name (world/branches store))
        tips (world/tips store bs)]
    (for [b bs :let [{:keys [last fork]} (get tips b)
                     own (if (system-actor? (get-in last [:actor :principal]))
                           (when last (by-people store b (:rev last)))
                           last)]]
      {:branch b :last-at (:at own) :last-by (some-> own :actor :principal)
       :forked-at (:at fork)})))

(defn contributors
  [store branch]
  (->> (world/schema-commits (world/at store branch) {:newest? true :limit commits-looked-at})
       (keep #(get-in % [:actor :principal]))
       (remove system-actor?)
       (map str)
       distinct
       vec))

(defn- later
  [a b]
  (let [ia (proposals/->instant a) ib (proposals/->instant b)]
    (cond (nil? ia) b
          (nil? ib) a
          (.isAfter ^java.time.Instant ia ib) a
          :else b)))

(defn- branch-rows
  [store]
  (let [by   (protection/governance store)
        live (mapv (fn [r] (cond-> {:name              (str (:branch r))
                                    :latest-commit-date (or (:last-at r) (:forked-at r))
                                    :latest-committer   (:last-by r)}
                             (:error r) (assoc :error (:error r))))
                   (world-tips store))
        gone (for [[nm r] by
                   :when (and (not= nm "*")
                              (or (:closed-at r) (:merged-at r))
                              (not-any? #(= nm (:name %)) live))]
               {:name nm})]
    (mapv (fn [{:keys [name latest-commit-date latest-committer error]}]
            (let [g  (get by name)
                  ended (or (:merged-at g) (:closed-at g))]
              (cond-> (merge (effective by name)
                             {:name           name
                              :parent         nil
                              :status         (status-of g)
                              :last-commit-at (some-> latest-commit-date str)
                              :last-committer (some-> latest-committer str)
                              :age-days       (age-days store (later latest-commit-date
                                                                     (:reactivated-at g)))
                              :machine        (branch-kinds/machine-branch? name)
                              :space          (some-> (:space g) str)
                              :inactive-at    (some-> (:inactive-at g) str)
                              :closed-at      (some-> (:closed-at g) str)
                              :merged-at      (some-> (:merged-at g) str)
                              :purged-at      (some-> (:purged-at g) str)
                              :ended-days     (age-days store ended)})
                error (assoc :error error))))
          (concat live gone))))

(defn list-branches
  ([store] (list-branches store {}))
  ([store {:keys [closed? machine?]}]
   (cond->> (branch-rows store)
     (not closed?)  (filterv (complement ended?))
     (not machine?) (filterv (complement :machine)))))

(defn ended-branches
  [store]
  (ended-in (protection/governance store)))

(defn closed?
  [store branch]
  (ended? (ctl/get store :branch-governance {:branch branch})))

(defn close!
  [store branch]
  (cond
    (= branch default-branch-name)
    {:error (str "refusing to close the trunk branch " default-branch-name)}

    (closed? store branch)
    {:branch branch :closed true :already true}

    :else
    (do (ctl/commit! store (cons (proposals/branch-transitioning store branch :close :closed-at)
                                 (for [{:keys [id]}
                                       (ctl/list store :proposals
                                                 {:source-branch branch :status "open"}
                                                 {:cols [:id]})]
                                   (proposals/proposal-transitioning store id :close))))
        {:branch branch :closed true})))

(defn forked!
  [store branch]
  (lc/decide branch-lifecycle branch :fork nil nil nil)
  (ctl/commit! store [(fn [store _] (ctl/deleting store :branch-governance {:branch branch}))
                      (fn [store _] (ctl/deleting store :branch-roles {:branch branch}))
                      (fn [store _]
                        {:records [(ctl/entry store :branch-roles
                                              {:branch branch :principal (actor-name store)
                                               :role owner-role
                                               :granted-by (actor-name store)
                                               :granted-at ctl/now})]})])
  {:branch branch :owner (actor-name store)})

(defn set-inactive!
  [store branch on?]
  (cond
    (= branch default-branch-name)
    {:error (str "refusing to label the trunk branch " default-branch-name " inactive")}

    (closed? store branch)
    {:error (str branch " is closed — it cannot become active again")}

    :else
    (let [[event to] (if on? [:idle :inactive] [:reactivate :active])]
      (when-not (= to (branch-state (ctl/get store :branch-governance {:branch branch})))
        (ctl/commit! store
                     [(lifecycle/transitioning
                       :branch-governance {:branch branch} branch-lifecycle event
                       {:id branch :state-of branch-state
                        :sets (fn [_ _]
                                (cond-> {:branch branch :inactive-at (when on? ctl/now)
                                         :updated-by (actor-name store)}
                                  (not on?) (assoc :reactivated-at ctl/now)))})]))
      {:branch branch :inactive (boolean on?)})))

(defn- open-proposal-branches
  [store]
  (into #{}
        (comp cat (keep #(some-> (or (:source-branch %) (:source %)) str)))
        [(ctl/list store :proposals {:status "open"} {:cols [:source-branch]})
         (ctl/list store :git-pending {})]))

(defn retention-plan
  [store opts]
  (let [opts (assoc opts :open-proposals (open-proposal-branches store))]
    (->> (branch-rows store)
         (group-by #(retention-action % opts))
         (into {} (keep (fn [[k bs]] (when k [k (mapv :name bs)])))))))

(defn orphaned-proposals
  [store]
  (into []
        (comp (remove #(world/exists? store (str (:source-branch %))))
              (map #(select-keys % [:id :title :source-branch :target-branch])))
        (ctl/list store :proposals {:status "open"}
                  {:cols [:id :title :source-branch :target-branch]})))

(defn- purge-branch!
  [store branch drop-branch-fn]
  (try
    (when drop-branch-fn (drop-branch-fn branch))
    (ctl/commit! store [(proposals/branch-transitioning store branch :purge :purged-at)])
    {:branch branch :purged true}
    (catch InterruptedException e (throw e))
    (catch Exception e
      {:branch branch :purged false :error (.getMessage e)})))

(defn retention-sweep!
  [store {:keys [days dry-run? drop-branch-fn notify-fn] :as opts}]
  (let [{close         :close
         mark-inactive :mark-inactive
         reactivate    :reactivate
         purge         :purge} (retention-plan store opts)
        orphans (orphaned-proposals store)]
    (if dry-run?
      {:days days :dry-run true
       :would-close         (vec close)
       :would-mark-inactive (vec mark-inactive)
       :would-reactivate    (vec reactivate)
       :would-purge         (vec purge)
       :would-close-proposals (vec orphans)}
      (let [purged (mapv #(purge-branch! store % drop-branch-fn) purge)
            marked (mapv (fn [b] (set-inactive! store b true) b) mark-inactive)
            tell!  (fn [kind bs]
                     (when notify-fn
                       (doseq [b bs]
                         (try (notify-fn {:kind kind :branch b :owners (roles/owners store b)})
                              (catch InterruptedException e (throw e))
                              (catch Exception e
                                (println "[retention] owners of" b "not told:"
                                         (ex-message e)))))))
            closed (mapv (fn [b] (close! store b) b) close)]
        (tell! :branch-inactive marked)
        (tell! :branch-closed closed)
        {:days days
         :closed          closed
         :marked-inactive marked
         :reactivated     (mapv (fn [b] (set-inactive! store b false) b) reactivate)
         :purged          (mapv :branch (filterv :purged purged))
         :purge-failed    (filterv :error purged)
         :closed-proposals (mapv (fn [{:keys [id] :as o}]
                                   (cond-> o
                                     (:error (proposals/close-proposal! store id))
                                     (assoc :error true)))
                                 orphans)}))))

(defn delete-branch!
  [store branch drop-branch-fn]
  (let [c (close! store branch)]
    (if (:error c)
      c
      (purge-branch! store branch drop-branch-fn))))

(op/defop :branch/ended [{:keys [store]} _]
  (ended-branches store))
