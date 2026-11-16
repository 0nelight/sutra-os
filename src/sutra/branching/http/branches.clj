(ns sutra.branching.http.branches
  (:require [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [sutra.authoring.api :as authoring]
            [sutra.governance.api :as governance-api]
            [sutra.store.access.scope :as ctx]
            [sutra.branching.proposals :as proposals]
            [sutra.branching.retention :as retention]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.grants :as grants]
            [sutra.store.kept :as kept]
            [sutra.store.world :as world]
            [sutra.types.project-graph :as pgraph]
            [sutra.branching.rebase :as rebase]
            [sutra.store.edge :as edge]
            [sutra.actions.api :as actions]
            [sutra.foundation.ops :as op]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok too-many]]
            [sutra.kit.rate-limit :as rate]))

(set! *warn-on-reflection* true)

(defn- require-owner!
  [{:keys [store req]} nm act]
  (when-let [{:keys [error] :as block}
             (governance-api/owner-block store nm act
                                         (and (= (ep/signed-in-user req)
                                                 grants/admin-username)
                                              (not (:sutra/agent? req))))]
    (refusal/refuse! :refusal.branching/passed-error block {:message error})))

(def list-branches
  (ep/handler :branches
              {:call (fn [{:keys [store branch closed machine]}]
                       {:branches (governance-api/branch-states store {:closed? closed
                                                                       :machine? machine})
                        :current  branch
                        :head     (governance-api/current-commit (ctx/at store branch))})}))

(def creation-rate
  {:capacity 60 :per-ms (* 60 60 1000)})

(defonce ^:private creations (rate/limiter creation-rate #(System/currentTimeMillis)))

(defn- fork!
  [store schema {:keys [name parent space] :as body}]
  (let [nm (some-> name clojure.core/name)
        pa (some-> parent clojure.core/name)
        sp (some-> space clojure.core/name)]
    (when-not (and nm pa)
      (refusal/refuse! :refusal.branch/name-or-parent-missing {:body body}))
    (when (and sp (not (contains? (set (map str (keys (:spaces schema)))) sp)))
      (refusal/refuse! :refusal.branch/unknown-space {:space sp}))
    (governance-api/create-branch! store (cond-> {:parent pa :name nm} sp (assoc :space sp)))))

(def create-branch
  (ep/handler :branches
              {:respond (fn [{:keys [store schema body req]}]
                          (let [who  (ep/signed-in-user req)
                                wait (when-not (or (= who grants/admin-username)
                                                   (grants/auth-off? store))
                                       (rate/spend! creations (str who)))]
                            (if wait
                              (too-many "too many branches forked in the last hour" wait)
                              (ok (fork! store schema body)))))}))

(def drop-branch
  (ep/handler :branch
              {:call (fn [{:keys [store name] :as a}]
                       (require-owner! a name "close it")
                       (let [r (governance-api/delete-branch!
                                store name #(retention/purge-branch-data! store %))]
                         (if (:error r)
                           (refusal/refuse! :refusal.branching/passed-error-r {} {:message (:error r)})
                           {:dropped name :status "closed" :purged (:purged r)})))}))

(defn- merge-target [store name]
  (or (some #(when (= name (:name %)) (:parent %)) (governance-api/branch-parents store))
      policy/default-branch-name))

(defn- task-row [t]
  (select-keys t [:task :section :name :status :rid :protected :requires :approvals]))

(defn- branch-world [store b]
  (or (world/authored (world/at store b)) {}))

(defn- without-resources
  [mine theirs tasks]
  (reduce (fn [w task]
            (let [[sec k] (str/split (str task) #"/" 2)
                  section (keyword sec)
                  id      (some #(when (= k (str %)) %)
                                (concat (keys (get mine section)) (keys (get theirs section))))]
              (cond
                (nil? id)                      w
                (contains? (get theirs section) id) (assoc-in w [section id]
                                                              (get-in theirs [section id]))
                :else                          (update w section dissoc id))))
          mine tasks))

(defn- without-orphans
  [mine theirs w]
  (let [used    #(into #{} (map (comp str :to)) (pgraph/references %))
        now     (used w)
        before  (used mine)
        orphan? (fn [[r spec]]
                  (and (= :object-backing (some-> (:kind spec) keyword))
                       (not (contains? (:datasets theirs) r))
                       (contains? before (str r))
                       (not (contains? now (str r)))))]
    (update w :datasets #(into {} (remove orphan?) %))))

(def branch-resources
  (ep/handler :branch-resources
              {:call
               (fn [{:keys [store name body] :as a}]
                 (require-owner! a name "remove its changes")
                 (let [tasks  (map str (:remove body))
                       target (merge-target store name)
                       known  (set (map :task (governance-api/proposal-tasks store name target)))
                       stray  (vec (remove known tasks))]
                   (cond
                     (= name policy/default-branch-name)
                     (refusal/refuse! :refusal.branch/main-unbranched {})
                     (empty? tasks)
                     (refusal/refuse! :refusal.branch/remove-unnamed {})
                     (seq stray)
                     (refusal/refuse! :refusal.branch/not-changed {:not-changed stray}
                                      {:branch name :not-changed-list (str/join ", " stray)}))
                   (governance-api/data-guard (ctx/at store name))
                   (let [theirs (branch-world store target)
                         mine   (branch-world store name)
                         w      (without-orphans mine theirs
                                                 (without-resources mine theirs tasks))
                         {:keys [error]} (governance-api/judged
                                          (kept/onto (kept/db store target) w))]
                     (when error
                       (refusal/refuse! :refusal.branch/removal-needed {:reasons error}
                                        {:reasons-text (str/join "; " (flatten [error]))}))
                     (authoring/install-world! (ctx/at store name) name w
                                               {:message (str "remove from branch: "
                                                              (str/join ", " tasks))})
                     {:branch name :removed (vec tasks)})))}))

(def branch-changes
  (ep/handler :branch-changes
              {:call (fn [{:keys [store schema name]}]
                       (when-not (world/exists? store name)
                         (refusal/refuse! :refusal.branch/unknown-branch {} {:branch name}))
                       (let [target (merge-target store name)
                             open
                             (first
                              (governance-api/list-proposals store {:status "open" :source name}))
                             p      (some->> (:id open) (proposals/proposal store schema))]
                         {:branch   name
                          :target   (or (:target-branch p) target)
                          :tasks    (mapv task-row
                                          (if p
                                            (:tasks p)
                                            (governance-api/proposal-tasks store name target)))
                          :proposal (some-> p
                                            (select-keys
                                             [:id :title :status :stage :open-tasks :preview
                                              :judgement-error])
                                            (assoc :checks (get-in p [:checks :status])))}))}))

(op/defop :branching/tasks [{:keys [store]} {:keys [branch]}]
  (when (world/instance-home (world/at store branch))
    (governance-api/proposal-tasks store branch (merge-target store branch))))

(def rebase-branch
  (ep/handler :branch-rebase
              {:call (fn [{:keys [store name body] :as a}]
                       (require-owner! a name "rebase it")
                       (let [decided (into {}
                                           (for [{:keys [table side]} (:resolutions body)
                                                 :when (#{"mine"
                                                          "theirs"}
                                                        (some-> side clojure.core/name))]
                                             [(str table) (clojure.core/name side)]))
                             r
                             (rebase/rebase-branch!
                              store (actions/staging-view (ctx/at store name))
                              name (merge-target store name) decided)]
                         (if (:error r)
                           (refusal/refuse! :refusal.branching/passed-error-r (dissoc r :error) {:message (:error r)})
                           r)))}))

(def rebase-staged
  (ep/handler :staging-rebase
              {:call #(let [r (rebase/rebase-staged! (ctx/at (:store %) (:branch %))
                                                     (edge/staging-view (:edge %))
                                                     (or (:resolutions (:body %)) {}))]
                        (if (:error r)
                          (refusal/refuse! :refusal.branching/passed-error-r (dissoc r :error) {:message (:error r)})
                          r))}))

(def module
  {:name        :branches
   :contributes {:http/routes
                 {:branch   {:delete drop-branch}
                  :branch-resources {:post branch-resources}
                  :branch-changes {:get branch-changes}
                  :branch-rebase  {:post rebase-branch}
                  :staging-rebase {:post rebase-staged}
                  :branches {:get list-branches :post create-branch}}}})
