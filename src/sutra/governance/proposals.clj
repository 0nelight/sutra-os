(ns sutra.governance.proposals
  (:require [sutra.store.control :as ctl]
            [sutra.store.control.lifecycle :as lifecycle]
            [sutra.store.world :as world]
            [sutra.catalog.grants :as grants]
            [clojure.string :as str]
            [sutra.types.review :as review]
            [sutra.kit.lifecycle :as lc]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.branch-policy :as policy :refer
             [actor-name approved-verdict branch-lifecycle branch-state git-half-outcome
              git-half-reason git-half-backoff-ms hydrated proposal-lifecycle proposal-state
              rejected-verdict reviews verdict-lifecycle verdict-state]])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defn ->instant
  [v]
  (condp instance? v
    java.sql.Timestamp     (.toInstant ^java.sql.Timestamp v)
    java.time.Instant      v
    java.time.LocalDateTime (-> ^java.time.LocalDateTime v
                                (.atZone (java.time.ZoneId/systemDefault))
                                .toInstant)
    java.util.Date         (.toInstant ^java.util.Date v)
    nil))

(defn now
  [store]
  (ctl/instant store))

(defn branch-transitioning
  [store branch event column]
  (lifecycle/transitioning :branch-governance {:branch branch} branch-lifecycle event
                           {:id branch :state-of branch-state
                            :sets (fn [_ _] {:branch branch column ctl/now
                                             :updated-by (actor-name store)})}))

(defn proposal-transitioning
  [store id event]
  (lifecycle/transitioning :proposals {:id id} proposal-lifecycle event
                           {:id id :state-of proposal-state
                            :sets (fn [_ to] {:status (name to) :resolved-at ctl/now
                                              :resolved-by (actor-name store)})}))

(defn- merged-stamp
  [store branch]
  (branch-transitioning store branch :merge :merged-at))

(defn reviews-of
  [store id]
  (reviews (ctl/list store :proposal-verdicts {:proposal-id id} {:order [[:seq :asc]]})))

(defn- hydrate [store r]
  (when r (hydrated (reviews-of store (:id r)) r)))

(defn git-half
  [store id]
  (when-let [r (ctl/get store :git-pending {:proposal-id (str id)})]
    {:state (keyword (or (not-empty (str (:state r))) "retry"))
     :attempts (long (or (:attempts r) 0))
     :error (:error r)
     :next-at (some-> (:next-at r) str)}))

(defn fetch
  [store id]
  (when-let [p (hydrate store (ctl/get store :proposals {:id id}))]
    (let [g (git-half store id)]
      (cond-> p g (assoc :git-half g)))))

(defn- on-open-proposal
  [store id event f]
  (let [p (fetch store id)]
    (cond
      (nil? p) {:error (str "no such proposal: " id)}
      (not (lc/allowed? proposal-lifecycle event (proposal-state p)))
      {:error (str "proposal is " (:status p) ", not open")}
      :else    (f p))))

(defn list-proposals
  [store {:keys [status target source]}]
  (let [status (or status "open")
        ps     (ctl/list store :proposals
                         (cond-> {}
                           (not= "all" status) (assoc :status status)
                           target              (assoc :target-branch target)
                           source              (assoc :source-branch source))
                         {:order [[:created-at :desc]]})
        vs     (when (seq ps)
                 (group-by #(str (:proposal-id %))
                           (ctl/list store :proposal-verdicts
                                     {:proposal-id (set (map :id ps))}
                                     {:order [[:seq :asc]]})))]
    (mapv #(hydrated (reviews (get vs (str (:id %)) [])) %) ps)))

(defn open-proposal-of
  [store source target]
  (ctl/get store :proposals {:status "open" :source-branch source :target-branch target}))

(defn refused
  ([code data] (refused code data nil))
  ([code data params]
   (cond-> (assoc data :error (refusal/message code (merge data params)) :refusal/code code)
     (seq params) (assoc :refusal/params params))))

(defn raise-refused!
  [r ctx fallback]
  (refusal/refuse! (or (:refusal/code r) fallback)
                   (merge ctx (dissoc r :error :refusal/code :refusal/params))
                   (or (:refusal/params r) {:why (:error r)})))

(defn proposal-refusal
  [{:keys [source target missing open]}]
  (cond
    (str/blank? (str source)) (refused :refusal.governance/proposal-no-source {})
    (= source target)         (refused :refusal.governance/proposal-same-branch {}
                                       {:source source :target target})
    (seq missing)             (refused :refusal.governance/proposal-missing-branch {}
                                       {:missing (str/join ", " missing)})
    open                      (refused :refusal.governance/proposal-already-open
                                       {:proposal-id (:id open)}
                                       {:source source :target target})))

(defn open-proposal!
  [store {:keys [source target title description justification merge-mode]}]
  (let [id (str (UUID/randomUUID))]
    (lc/decide proposal-lifecycle id :open nil nil nil)
    (ctl/commit! store [[:insert :proposals
                         [{:id id :title (or title (str "Merge " source " into " target))
                           :description description
                           :justification (some-> justification str not-empty)
                           :source-branch source :target-branch target
                           :author (actor-name store) :merge-mode (some-> merge-mode name)}]]])
    (fetch store id)))

(defn verdicts-decided!
  [p me verdict tasks eligible? needs]
  (let [mine  (some #(when (= (str me) (str (:approver %))) %) (:reviews p))
        event (if (= rejected-verdict verdict) :reject :approve)]
    (doseq [t tasks]
      (lc/decide verdict-lifecycle (str (:id p) "/" (:task t)) event
                 (verdict-state t (get-in mine [:task-verdicts (str (:task t))]))
                 nil {:eligible? (boolean (eligible? t)) :needs (needs t)}))))

(defn- review!
  [store id verdict note {:keys [task tasks owns? approver]}]
  (on-open-proposal
   store id :review
   (fn [p]
     (let [me     (actor-name store)
           ctx    {:author (:author p)}
           ap     (fn [t]
                    (if approver
                      (assoc (policy/approver-facts approver me (:project t)) :who me)
                      {:who me
                       :owner? (boolean (or (nil? owns?) (nil? (:project t))
                                            (owns? (:project t))))}))
           by-id  (into {} (for [t tasks] [(str (:task t)) t]))
           darf   (into #{} (comp (filter #(review/eligible? % (ap %) ctx)) (map (comp str :task)))
                        tasks)
           betrifft (if task [(str task)] (filter darf (keys by-id)))
           why    (fn [t] (str/join " or " (map review/describe (:requires t))))]
       (cond
         (and task (seq tasks) (not (contains? by-id (str task))))
         {:error (str "no such task on this proposal: " task)
          :tasks (vec (keys by-id))}

         (and task (seq tasks) (not (darf (str task))))
         {:error (str "not eligible to review task: " task " — it needs "
                      (why (by-id (str task))))
          :eligible-tasks (vec (sort darf))}

         (and (nil? task) (seq by-id) (empty? betrifft))
         {:error (str "not eligible to review any task on this proposal — "
                      (str/join "; " (for [t tasks] (str (:task t) " needs " (why t)))))
          :tasks (vec (sort (keys by-id)))}

         :else
         (let [rev  (:rev (world/head (world/at store (:source-branch p))))
               fact (fn [t]
                      {:proposal-id id :approver me :task (some-> t :task str) :verdict verdict
                       :fingerprint (some-> t :fingerprint str) :source-rev rev :note note})]
           (verdicts-decided! p me verdict (keep by-id betrifft) #(contains? darf (str (:task %)))
                              why)
           (ctl/commit! store [[:insert :proposal-verdicts
                                (cond-> (mapv (comp fact by-id) betrifft)
                                  (nil? task) (conj (fact nil)))]])
           (fetch store id)))))))

(defn approve!
  ([store id] (approve! store id nil nil))
  ([store id note] (approve! store id note nil))
  ([store id note opts] (review! store id approved-verdict note opts)))

(defn reject!
  ([store id] (reject! store id nil nil))
  ([store id note] (reject! store id note nil))
  ([store id note opts] (review! store id rejected-verdict note opts)))

(defn ask-review!
  ([store id] (ask-review! store id nil))
  ([store id reviewers] (ask-review! store id reviewers nil))
  ([store id reviewers justification]
   (on-open-proposal
    store id :review
    (fn [_p]
      (ctl/commit! store [[:update :proposals {:id id}
                           (fn [{:keys [review-requested-at review-requested-by]}]
                             (cond-> {:review-requested-at (or review-requested-at ctl/now)
                                      :review-requested-by (or review-requested-by
                                                               (actor-name store))
                                      :reviewers (pr-str (vec reviewers))}
                               (some-> justification str not-empty)
                               (assoc :justification (str justification))))]])
      (fetch store id)))))

(defn close-proposal!
  [store id]
  (let [p (fetch store id)]
    (cond
      (nil? p) {:error (str "no such proposal: " id)}
      (not (lc/allowed? proposal-lifecycle :close (proposal-state p)))
      {:error (str "proposal is already " (:status p))}
      :else
      (do (ctl/commit! store [(proposal-transitioning store id :close)])
          (assoc (fetch store id) :source-merged false)))))

(defn comments-of
  [store id]
  (mapv #(update % :created-at str)
        (ctl/list store :proposal-comments {:proposal-id id}
                  {:order [[:created-at :asc] [:id :asc]]})))

(defn add-comment!
  [store id {:keys [task body]}]
  (on-open-proposal
   store id :review
   (fn [_p]
     (if (str/blank? (str body))
       {:error "a comment needs a :body"}
       (let [cid (str (UUID/randomUUID))]
         (ctl/commit! store [[:insert :proposal-comments
                              [{:id cid :proposal-id id :task (some-> task not-empty str)
                                :author (actor-name store) :body (str body)}]]])
         (first (filter #(= cid (str (:id %))) (comments-of store id))))))))

(defn delete-comment!
  [store id comment-id admin?]
  (let [c (ctl/get store :proposal-comments {:id comment-id :proposal-id id})]
    (cond
      (nil? c) {:error (str "no such comment: " comment-id)}
      (not (or admin? (= (actor-name store) (str (:author c)))))
      {:error "only the comment's author (or the admin) may delete it"
       :author (:author c)}
      :else
      (do (ctl/commit! store [[:delete :proposal-comments {:id comment-id}]])
          {:deleted comment-id}))))

(defn due-git-halves
  [store]
  (filter (fn [{:keys [state next-at]}]
            (and (not= "unresolved" (str state))
                 (or (nil? next-at)
                     (<= (.getTime ^java.util.Date next-at) (ctl/millis store)))))
          (ctl/list store :git-pending {})))

(defn settle-git-half!
  [store id fp]
  (let [k    {:proposal-id (str id)}
        halt (fn [state next-at]
               (ctl/commit! store [[:update :git-pending k
                                    (fn [old]
                                      (let [n (inc (long (or (:attempts old) 0)))]
                                        {:state (name state) :attempts n
                                         :error (git-half-reason fp) :next-at (next-at n)}))]]))]
    (case (git-half-outcome fp)
      :done       (ctl/commit! store [[:delete :git-pending k]])
      :retry      (halt :retry (fn [n]
                                 (let [w (git-half-backoff-ms n)]
                                   (when (pos? w)
                                     (java.util.Date. (long (+ (ctl/millis store) w)))))))
      :unresolved (halt :unresolved (constantly nil)))))

(defn landing-records
  [store id p merged-source?]
  (let [pending (fn [store _]
                  {:records [(ctl/entry store :git-pending
                                        {:proposal-id (str id) :source (:source-branch p)
                                         :target (:target-branch p) :actor (actor-name store)
                                         :authority (grants/request-authority store)
                                         :state "pending"
                                         :merge-mode (some-> (:merge-mode p) name)})]})
        plans   (cond-> [(proposal-transitioning store id :land)]
                  merged-source? (conj (merged-stamp store (:source-branch p)))
                  true           (conj pending))
        now     (ctl/clock store)]
    (into [] (mapcat #(:records (% store now))) plans)))

(defn merge-attempted!
  [store id error]
  (ctl/commit! store [[:insert :proposal-merge-attempts
                       [{:proposal-id (str id) :actor (actor-name store)
                         :outcome (if error "refused" "merged") :error (some-> error str)}]]]))

(defn merge-attempts
  [store id]
  (mapv #(update % :at str)
        (ctl/list store :proposal-merge-attempts {:proposal-id (str id)} {:order [[:seq :asc]]})))

(defn merge-committed!
  [store id rev]
  (ctl/commit! store [[:update :proposals {:id id} {:merge-commit (str rev)}]]))
