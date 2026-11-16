(ns sutra.branching.http.proposals
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.edge :as edge]
            [clojure.string :as str]
            [sutra.builds.api :as builds]
            [sutra.catalog.grants :as grants]
            [sutra.governance.api :as governance]
            [sutra.catalog.branch-policy :as policy]
            [clojure.edn :as edn]
            [sutra.branching.access :as bac]
            [sutra.branching.proposals :as proposals]
            [sutra.branching.retention :as retention]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]))

(set! *warn-on-reflection* true)

(defn- ->bool [v] (if (string? v) (contains? #{"true" "1" "yes"} v) (boolean v)))

(defn- answered
  [r ctx]
  (if (:error r) (refusal/refused :refusal.branching/passed-error-r ctx {:message (:error r)}) (ok r)))

(defn- with-proposal
  [store id f]
  (if-let [p (governance/fetch store id)]
    (f p)
    (refusal/refused :refusal.branching/no-such-proposal {} {:id id})))

(def get-policy
  (ep/handler :branch-policy {:call #(governance/policy (:store %) (:name %))}))

(defn- ->rules [v] (if (string? v) (edn/read-string v) v))

(def ^:private policy-fields
  {:protected      ->bool
   :review         ->rules
   :retention-days identity
   :inactive-days  identity
   :auto-land      ->bool
   :land-pushes    ->bool
   :require-checks ->bool})

(def set-policy
  (ep/handler :branch-policy
              {:respond (fn [{:keys [store name body]}]
                          (let [opts (into {} (keep (fn [[k coerce]]
                                                      (when (contains? body k)
                                                        [k (coerce (get body k))])))
                                           policy-fields)]
                            (if (empty? opts)
                              (refusal/refused :refusal.branching/nothing-to-set-pass-one body {:value (vec (sort (keys policy-fields)))})
                              (answered (governance/set-policy! store name opts)
                                        {:branch name}))))}))

(declare admin?)

(def set-status
  (ep/handler :branch-status
              {:respond
               (fn [{:keys [store name body req]}]
                 (cond
                   (contains? body :inactive)
                   (let [on? (->bool (get body :inactive))]
                     (if-let [{:keys [error] :as block}
                              (when-not on?
                                (governance/owner-block store name "clear its retention label"
                                                        (admin? req)))]
                       (refusal/refuse! :refusal.branching/passed-error block {:message error})
                       (answered (governance/set-inactive! store name on?) {:branch name})))

                   (->bool (get body :closed))
                   (if-let [{:keys [error] :as block}
                            (governance/owner-block store name "close it" (admin? req))]
                     (refusal/refuse! :refusal.branching/passed-error block {:message error})
                     (answered (governance/close! store name) {:branch name}))

                   :else
                   (refusal/refused :refusal.branching/pass-or-a-closed-branch body)))}))

(def sweep
  (ep/handler :branch-sweep
              {:call #(governance/retention-sweep!
                       (:store %)
                       (merge (retention/sweep-deps (:store %))
                              {:days (:days (:body %))
                               :dry-run? (->bool (:dry-run? (:body %)))}))}))

(def retention-plan
  (ep/handler :dataset-retention
              {:call (fn [{:keys [store schema dataset]}]
                       {:policies (builds/policies store)
                        :datasets (builds/plan store schema {:dataset dataset})})}))

(def retention-run
  (ep/handler :dataset-retention
              {:call (fn [{:keys [store schema body]}]
                       (builds/retain! store schema
                                       (cond-> {:apply? (->bool (:apply? body))}
                                         (:dataset body) (assoc :dataset (str (:dataset body)))
                                         (some? (:grace-days body))
                                         (assoc :grace-days (long (:grace-days body))))))}))

(def retention-policies
  (ep/handler :retention-policies
              {:call (fn [{:keys [store body user]}]
                       (if (seq body)
                         (builds/set-policy! store user body)
                         (builds/policies store)))}))

(def retention-policy
  (ep/handler :retention-policy
              {:call (fn [{:keys [store name]}] (builds/delete-policy! store name))}))

(defn- admin?
  [req]
  (and (= (ep/signed-in-user req) grants/admin-username)
       (not (:sutra/agent? req))))

(def get-roles
  (ep/handler :branch-roles
              {:call (fn [{:keys [store name]}]
                       (let [roles (governance/branch-roles store name)]
                         {:branch name :roles roles :governed (boolean (seq roles))}))}))

(def set-role
  (ep/handler :branch-roles
              {:respond
               (fn [{:keys [store name body req]}]
                 (let [principal (some-> (:principal body) clojure.core/name)
                       role      (or (some-> (:role body) clojure.core/name) policy/owner-role)]
                   (if-not principal
                     (refusal/refused :refusal.branching/roles-need-a-principal body)
                     (if-let [{:keys [error]
                               :as block}
                              (governance/owner-block store name "change its roles" (admin? req))]
                       (refusal/refuse! :refusal.branching/passed-error block {:message error})
                       (answered (if (->bool (:revoke? body))
                                   (governance/revoke-role! store name principal)
                                   (governance/assign-role! store name principal role))
                                 {:branch name})))))}))

(def list-proposals
  (ep/handler :proposals
              {:call (fn [{:keys [store schema status target source brief]}]
                       (let [fs {:status status :target target :source source}]
                         {:proposals (if (= "true" brief)
                                       (vec (governance/list-proposals store fs))
                                       (proposals/listed store schema fs))}))}))

(defn create-proposal
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch :as req}]
  (ok (proposals/propose! store schema (update body-params :source #(or % branch)) (admin? req))))

(defn get-proposal
  [{{:keys [id]} :path-params store :sutra/store schema :sutra/schema}]
  (if-let [p (proposals/proposal store schema id)]
    (ok p)
    (refusal/refused :refusal.branching/no-such-proposal {} {:id id})))

(defn- require-source-owner!
  [store id act req]
  (when-let [source (:source-branch (governance/fetch store id))]
    (when-let [{:keys [error] :as block} (governance/owner-block store source act (admin? req))]
      (refusal/refuse! :refusal.branching/passed-error block {:message error}))))

(defn ask-review
  [{{:keys [id]} :path-params :keys [body-params] store :sutra/store schema :sutra/schema
    :as req}]
  (require-source-owner! store id "ask for its review" req)
  (let [r (governance/ask-review! store id
                                  (map #(some-> % clojure.core/name) (:reviewers body-params))
                                  (:justification body-params))
        _ (when-not (:error r)
            (try (proposals/notify-review! store schema r)
                 (catch InterruptedException e (throw e))
                 (catch Exception e
                   (println "[governance] reviewers of" id "not told:" (ex-message e)))))]
    (if (:error r)
      (refusal/refused :refusal.branching/passed-error-r {:proposal id} {:message (:error r)})
      (ok (proposals/with-stage (proposals/target-reads store) r)))))

(defn list-tasks
  [{{:keys [id]} :path-params store :sutra/store}]
  (with-proposal store id
    (fn [p] (ok {:proposal id
                 :tasks (vec (proposals/with-reviews
                               p (proposals/tasks-of (proposals/target-reads store) p)
                               (governance/comments-of store id)))}))))

(defn list-comments
  [{{:keys [id]} :path-params store :sutra/store}]
  (with-proposal store id
    (fn [_] (ok {:proposal id :comments (vec (governance/comments-of store id))}))))

(defn add-comment
  [{{:keys [id]} :path-params :keys [body-params] store :sutra/store}]
  (let [task (some-> (:task body-params) clojure.core/name not-empty)
        ids  (into #{} (map #(str (:task %)))
                   (proposals/tasks-of (proposals/target-reads store) (governance/fetch store id)))]
    (if (and task (seq ids) (not (contains? ids task)))
      (refusal/refused :refusal.branching/no-such-task-on-this {:tasks (vec (sort ids))} {:task task})
      (answered (governance/add-comment! store id {:task task :body (:body body-params)})
                {:proposal id}))))

(defn delete-comment
  [{{:keys [id comment]} :path-params store :sutra/store :as req}]
  (answered (governance/delete-comment! store id comment (admin? req)) {:proposal id}))

(defn approve-proposal
  [{{:keys [id]} :path-params :keys [body-params] store :sutra/store auth :sutra/auth}]
  (let [tasks (proposals/tasks-of (proposals/target-reads store) (governance/fetch store id))]
    (answered (governance/approve! store id (:note body-params)
                                   {:task (:task body-params) :tasks tasks
                                    :owns? (grants/owner-of auth)
                                    :approver (proposals/owner?
                                               store (:target-branch (governance/fetch store id)))})
              {:proposal id})))

(defn reject-proposal
  [{{:keys [id]} :path-params :keys [body-params] store :sutra/store auth :sutra/auth}]
  (let [tasks (proposals/tasks-of (proposals/target-reads store) (governance/fetch store id))]
    (answered (governance/reject! store id (:note body-params)
                                  {:task (:task body-params) :tasks tasks
                                   :owns? (grants/owner-of auth)
                                   :approver (proposals/owner?
                                              store (:target-branch (governance/fetch store id)))})
              {:proposal id})))

(defn merge-proposal
  [{{:keys [id]} :path-params store :sutra/store schema :sutra/schema :as req}]
  (let [r (proposals/land! store schema id (admin? req))]
    (answered r (dissoc r :error))))

(defn close-proposal
  [{{:keys [id]} :path-params store :sutra/store :as req}]
  (require-source-owner! store id "close its proposal" req)
  (answered (governance/close-proposal! store id) {:proposal id}))

(defn protections
  [{:keys [request-method body-params] store :sutra/store auth :sutra/auth schema :sutra/schema}]
  (ok (cond
        (not= :post request-method) (bac/protections store schema)
        (:all body-params)          (bac/protect-everything! store auth schema)
        :else (do (bac/protect! store auth schema (:resources body-params)
                                (not (false? (:on body-params))))
                  (bac/protections store schema)))))

(defn project-policy
  [{:keys [request-method body-params path-params] store :sutra/store auth :sutra/auth
    schema :sutra/schema}]
  (let [nm (str/replace (str (:name path-params)) #"^:" "")]
    (ok (if (= :post request-method)
          (bac/set-project-policy! store auth schema nm body-params)
          (bac/project-policy store auth schema nm)))))

(defn default-project
  [{:keys [request-method body-params] store :sutra/store auth :sutra/auth schema :sutra/schema
    project :sutra/project}]
  (when (= :post request-method)
    (bac/set-default-project! store auth schema (:project body-params)))
  (ok (bac/default-project store (force project))))

(defn access-requests
  [{:keys [request-method body-params] :as req store :sutra/store auth :sutra/auth
    schema :sutra/schema}]
  (if (= :post request-method)
    (ok (bac/file-request! store schema body-params))
    (let [q #(not-empty (str (get-in req [:query-params %])))]
      (ok {:requests (bac/requests store auth (q "status") (= "true" (q "inbox")))}))))

(defn access-request
  [{:keys [request-method path-params] :as req store :sutra/store auth :sutra/auth}]
  (ok (if (= :delete request-method)
        (bac/close-request! store (:id path-params) (admin? req))
        (bac/request store auth (:id path-params)))))

(defn- decide-request
  [verdict
   {:keys [path-params body-params] store :sutra/store auth :sutra/auth schema :sutra/schema
    e :sutra/edge}]
  (ok (bac/decide-request! store auth schema (edge/guarding e) verdict (:id path-params)
                           (:note body-params))))

(defn approve-access-request [req] (decide-request policy/approved-verdict req))

(defn reject-access-request [req] (decide-request policy/rejected-verdict req))

(def module
  {:name        :governance
   :jobs        [{:name :retention :every :retention-ms :tick retention/retention-tick!}
                 {:name :git-pending :every "SUTRA_GIT_PENDING_MS" :tick proposals/git-pending-tick!
                  :default 60000}]
   :contributes {:http/routes
                 {:branch-policy       {:get get-policy :post set-policy}
                  :branch-roles        {:get get-roles :post set-role}
                  :branch-status       {:post set-status}
                  :branch-sweep        {:post sweep}
                  :dataset-retention   {:get retention-plan :post retention-run}
                  :retention-policies  {:get retention-policies :post retention-policies}
                  :retention-policy    {:delete retention-policy}
                  :proposal            {:get get-proposal :delete close-proposal}
                  :proposal-approve    {:post approve-proposal}
                  :proposal-ask-review {:post ask-review}
                  :proposal-comment    {:delete delete-comment}
                  :proposal-comments   {:get list-comments :post add-comment}
                  :proposal-merge      {:post merge-proposal}
                  :proposal-reject     {:post reject-proposal}
                  :proposal-tasks      {:get list-tasks}
                  :proposals           {:get list-proposals :post create-proposal}
                  :protections         {:get protections :post protections}
                  :project-policy      {:get project-policy :post project-policy}
                  :default-project     {:get default-project :post default-project}
                  :access-requests     {:get access-requests :post access-requests}
                  :access-request      {:get access-request :delete access-request}
                  :access-request-approve {:post approve-access-request}
                  :access-request-reject  {:post reject-access-request}}}})
