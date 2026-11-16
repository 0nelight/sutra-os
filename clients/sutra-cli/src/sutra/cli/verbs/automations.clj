(ns sutra.cli.verbs.automations
  (:require [sutra.cli.dispatch :refer [started! step!]]
            [sutra.cli.errors :refer [ok? req!]]
            [sutra.cli.live :refer [parse-param-flags schema-sections]]
            [sutra.cli.local :refer [vom-aufrufer]]
            [sutra.cli.out :refer [emit! mark paint render say! show-lines warn!]]
            [clojure.edn :as edn]
            [sutra.i18n.core :as i18n]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sutra.types.semver :as semver]
            [sutra.types.opid :as opid]
            [sutra.wire.api :as api]))

(defn- find-by-name
  [coll nm]
  (some #(when (= (:name %) nm) %) coll))

(def ^:private op-records
  {:automation {:noun    "automation"
                :list    #(:automations (req! "list automations" (api/get! :automations)))
                :upsert  api/upsert-automation}
   :schedule   {:noun    "schedule"
                :list    #(:schedules (req! "list schedules" (api/get! :schedules)))
                :upsert  api/upsert-schedule}})

(defn- record-op!
  [ctx kind nm op]
  (let [{:keys [noun list upsert]} (op-records kind)
        r (find-by-name (list) nm)]
    (cond
      (nil? r) (say! ctx (mark :yellow "⏸"
                               (i18n/t :cli/no-such-named {:art noun :name (paint :bold nm)})))
      (#{:pause :unpause} op)
      (do (req! (str op " " noun " " nm) (upsert (assoc r :paused (= op :pause))))
          (emit! ctx {:name nm :paused (= op :pause)}))
      :else (emit! ctx r))))

(defn- need-proposal! [op nm proposal]
  (or proposal
      (throw (ex-info (str "automation " (name op) " " nm
                           " <proposal-id> — list them with `automation proposals " nm "`")
                      {:automation nm}))))

(defn- execution
  [ids where batch-size parallelism]
  (let [ids (if (string? ids)
              (into [] (comp (map str/trim) (remove str/blank?)) (str/split ids #","))
              (vec ids))]
    (when (or (seq ids) where)
      (cond-> {}
        (seq ids)   (assoc :ids ids)
        where       (assoc :where where)
        batch-size  (assoc :batch-size batch-size)
        parallelism (assoc :parallelism parallelism)))))

(defmethod step! :automation
  [ctx [_ {nm :name do* :do proposal :proposal object :object with-model :with-model
           :keys [status limit offset args ids where batch-size parallelism]}]]
  (let [id (opid/name-id :automation nm)
        op (or do* :status)]
    (case op
      :proposals (let [{:keys [proposals counts]}
                       (req! (str "automation proposals " nm)
                             (api/automation-proposals id status {:limit limit :offset offset}))]
                   (emit! ctx {:proposals proposals :counts counts}))
      :accept    (emit! ctx (req! (str "accept proposal " proposal)
                                  (api/accept-proposal id (need-proposal! op nm proposal)
                                                       (not-empty args))))
      :reject    (do (req! (str "reject proposal " proposal)
                           (api/post! :automation-proposal-reject
                                      {:id id :proposal (need-proposal! op nm proposal)} nil))
                     (emit! ctx {:rejected proposal}))
      :run     (emit! ctx (req! (str "run automation " nm)
                                (api/post! :automation-run {:id id}
                                           (execution ids where batch-size parallelism))))
      :preview (emit! ctx (req! (str "preview automation " nm)
                                (api/preview-automation
                                 id {:object-id object :with-model? with-model})))
      :events  (let [{:keys [events counts]} (req! (str "automation events " nm)
                                                   (api/automation-events
                                                    id status {:limit limit :offset offset}))]
                 (emit! ctx {:events events :counts counts}))
      :delete  (do (req! (str "delete automation " nm) (api/delete! :automation-of {:id id}))
                   (emit! ctx {:deleted nm}))
      (:mute :unmute)
      (emit! ctx (select-keys (req! (str (name op) " automation " nm)
                                    (api/post! :automation-mute {:id id} {:muted (= :mute op)}))
                              [:name :muted :muted-by :muted-at]))
      :subscribe   (emit! ctx (req! (str "subscribe automation " nm)
                                    (api/post! :automation-subscribe {:id id} nil)))
      :unsubscribe (emit! ctx (req! (str "unsubscribe automation " nm)
                                    (api/delete! :automation-subscribe {:id id})))
      :runs    (emit! ctx (:runs (req! (str "automation runs " nm)
                                       (api/get! :automation-runs {:id id}))))
      :executions (emit! ctx (:executions (req! (str "automation executions " nm)
                                                (api/get! :automation-executions {:id id}))))
      :cancel  (let [execution (or proposal
                                   (throw (ex-info (str "automation " nm
                                                        " <execution-id> --do cancel"
                                                        " — list them with `automation "
                                                        nm " --do executions`")
                                                   {:automation nm})))]
                 (emit! ctx (req! (str "cancel execution " proposal)
                                  (api/post! :automation-execution-cancel
                                             {:id id :execution execution}
                                             nil))))
      (record-op! ctx :automation nm op))))

(defmethod step! :schedule [ctx [_ {nm :name do* :do args :args}]]
  (let [id (opid/name-id :schedule nm)
        op (or do* :status)]
    (case op
      :run    (started! ctx (req! (str "run schedule " nm)
                                  (api/post! :schedule-run {:id id}
                                             (when (seq args) {:parameters args}))))
      :delete (do (req! (str "delete schedule " nm) (api/delete! :schedule-of {:id id}))
                  (emit! ctx {:deleted nm}))
      (record-op! ctx :schedule nm op))))

(defmethod step! :schedule-list [ctx [_ _]]
  (let [{:keys [schedules]} (req! "list schedules" (api/get! :schedules))]
    (emit! ctx {:schedules (vec (sort-by :name schedules))}))
  nil)

(defmethod step! :automation-list [ctx [_ _]]
  (let [{:keys [automations]} (req! "list automations" (api/get! :automations))]
    (emit! ctx {:automations (vec (sort-by :name automations))}))
  nil)

(defmethod step! :linter-snooze [ctx [_ {:keys [rule resource days reason off]}]]
  (let [one {:rule rule :resource resource}]
    (cond
      off   (emit! ctx (req! "linter unsnooze"
                             (api/post! :linter-snooze nil (assoc one :off true))))
      (and days (seq reason))
      (emit! ctx (req! "linter snooze"
                       (api/post! :linter-snooze nil (assoc one :days days :reason reason))))
      :else (throw (ex-info "linter snooze needs --days N and --reason R, or --off" one))))
  nil)

(defmethod step! :health-check-list [ctx [_ _]]
  (let [{:keys [checks]} (req! "list health-checks" (api/get! :health-checks))]
    (emit! ctx {:checks (vec (sort-by :name checks))}))
  nil)

(defn- run-one-check!
  [chk]
  (assoc (req! (str "run health-check " (:name chk))
               (api/post! :health-check-run {:id (:id chk)} nil))
         :check (:name chk)))

(defmethod step! :health-check-run [ctx [_ {nm :name}]]
  (let [{:keys [checks]} (req! "list health-checks" (api/get! :health-checks))
        chk (find-by-name checks nm)]
    (if-not chk
      (say! ctx (mark :yellow "⏸" (i18n/t :cli/no-health-check-named {:name nm})))
      (emit! ctx (run-one-check! chk))))
  nil)

(defn- num-arg [v] (if (string? v) (or (parse-long v) (parse-double v) v) v))

(defn- flag-rider
  [{:keys [name on target kind severity column within-hours min max comparison query measure
           tolerance paused?]}]
  (cond-> {:name name :kind kind}
    on                 (assoc :on on)
    target             (assoc :target target)
    severity           (assoc :severity severity)
    column             (assoc :column column)
    (some? within-hours) (assoc :within-hours (num-arg within-hours))
    (some? min)        (assoc :min (num-arg min))
    (some? max)        (assoc :max (num-arg max))
    comparison         (assoc :comparison comparison)
    query              (assoc :query query)
    measure            (assoc :measure measure)
    (some? tolerance)  (assoc :tolerance (num-arg tolerance))
    paused?            (assoc :paused true)))

(defn- file-riders
  [from]
  (let [v (edn/read-string (slurp (vom-aufrufer from)))]
    (if (map? v) [v] (vec v))))

(defn- rider-problems
  [r]
  (let [schema @(requiring-resolve 'sutra.parser.operational/EntHealthCheck)
        explain @(requiring-resolve 'malli.core/explain)
        humanize @(requiring-resolve 'malli.error/humanize)]
    (some-> (explain schema r) humanize)))

(defmethod step! :health-check-set [ctx [_ {:keys [from] :as a}]]
  (let [riders (if from (file-riders from) [(flag-rider a)])]
    (doseq [r riders]
      (when-let [p (rider-problems r)]
        (throw (ex-info (str "health-check " (pr-str (:name r)) ": " (pr-str p))
                        {:rider r :problems p}))))
    (emit! ctx {:checks (mapv (fn [r] (select-keys (req! (str "health-check " (:name r))
                                                         (api/upsert-health-check r))
                                                   [:id :name :node-kind :node-key :kind
                                                    :severity :enabled]))
                              riders)}))
  nil)

(defmethod step! :health-check-delete [ctx [_ {nm :name}]]
  (let [{:keys [checks]} (req! "list health-checks" (api/get! :health-checks))
        chk (find-by-name checks nm)]
    (if-not chk
      (say! ctx (mark :yellow "⏸" (i18n/t :cli/no-health-check-named {:name nm})))
      (emit! ctx (req! (str "delete health-check " nm)
                       (api/delete! :health-check-of {:id (:id chk)})))))
  nil)

(defn- check-named!
  [nm]
  (let [{:keys [checks]} (req! "list health-checks" (api/get! :health-checks))]
    (or (find-by-name checks nm)
        (throw (ex-info (i18n/t :cli/no-health-check-named {:name nm}) {:name nm})))))

(defmethod step! :health-check-watch [ctx [_ {nm :name level :level}]]
  (let [chk (check-named! nm)]
    (emit! ctx (assoc (req! (str "watch health-check " nm)
                            (api/post! :health-check-watch {:id (:id chk)}
                                       {:level (name (or level :all))}))
                      :check nm)))
  nil)

(defmethod step! :health-check-snooze [ctx [_ {nm :name :keys [hours until reason]}]]
  (when-not (or hours until)
    (throw (ex-info "health-check snooze needs --hours N or --until <iso>" {:name nm})))
  (let [chk (check-named! nm)]
    (emit! ctx (assoc (req! (str "snooze health-check " nm)
                            (api/post! :health-check-snooze {:id (:id chk)}
                                       (cond-> {:reason reason}
                                         hours (assoc :hours (num-arg hours))
                                         until (assoc :until until))))
                      :check nm)))
  nil)

(defn- check-groups []
  (:groups (req! "list check groups" (api/get! :health-check-groups))))

(defn- group-named! [nm]
  (or (find-by-name (check-groups) nm)
      (throw (ex-info (str "no check group named " nm) {:name nm}))))

(defmethod step! :health-check-group-list [ctx [_ _]]
  (emit! ctx {:groups (vec (sort-by :name (check-groups)))})
  nil)

(defmethod step! :health-check-group-set [ctx [_ {nm :name :keys [checks description]}]]
  (let [{all :checks} (req! "list health-checks" (api/get! :health-checks))
        names (into [] (comp (map str/trim) (remove str/blank?)) (str/split checks #","))
        ids   (mapv #(or (:id (find-by-name all %))
                         (throw (ex-info (i18n/t :cli/no-health-check-named {:name %})
                                         {:name %})))
                    names)]
    (emit! ctx (req! (str "check group " nm)
                     (api/post! :health-check-groups
                                {:name nm :description description :check-ids ids}))))
  nil)

(defmethod step! :health-check-group-subscribe [ctx [_ {nm :name every :every all? :all?}]]
  (emit! ctx (req! (str "subscribe check group " nm)
                   (api/post! :health-check-group-subscribe {:id (:id (group-named! nm))}
                              {:every (or every "daily") :only-failing (not all?)})))
  nil)

(defn- delivery-pairs
  [s]
  (into {} (for [p (str/split (str s) #",")
                 :let [[k v] (map str/trim (str/split p #"=" 2))]
                 :when (and (seq k) (seq v))]
             [(keyword k) v])))

(defmethod step! :notification-settings [ctx [_ {pairs :set :keys [mute unmute]}]]
  (let [now (:settings (req! "notification settings"
                             (api/get! :notification-preferences)))]
    (if (or pairs mute unmute)
      (emit! ctx (req! "notification settings"
                       (api/put! :notification-preferences
                                 (cond-> now
                                   pairs  (update :delivery merge (delivery-pairs pairs))
                                   mute   (update :muted (fnil conj []) mute)
                                   unmute (update :muted #(vec (remove #{unmute} %)))))))
      (emit! ctx (req! "notification settings" (api/get! :notification-preferences)))))
  nil)

(defmethod step! :media-set-items [ctx [_ {nm :name}]]
  (let [{:keys [items]} (req! (str "list items of media set " nm)
                              (api/get! :doc-intel-items {:kind "media-set" :name nm}))]
    (emit! ctx {:media-set nm :count (count items) :items (vec (take 200 items))}))
  nil)

(defmethod step! :transform-check [ctx [_ {t :name}]]
  (let [{:keys [checks]} (req! "list health-checks" (api/get! :health-checks))
        mine (filterv #(and (= "transform" (:node-kind %)) (= t (:node-key %))) checks)]
    (if (empty? mine)
      (say! ctx (mark :yellow "⏸" (str (i18n/t :cli/no-checks-on-transform {:name t})
                                       "  — " (i18n/t :cli/author-health-check))))
      (emit! ctx {:checks (mapv run-one-check! (sort-by :name mine))})))
  nil)

(defn- schema-functions
  [body]
  (let [nm* (fn [v] (cond (keyword? v) (clojure.core/name v) (some? v) (str v) :else nil))
        ver (fn [s] {:version (:version s) :releases (count (:releases s))})
        edit  (for [[_ s] (:functions body)]
                (merge {:name (:function-name s) :kind "edit"  :language (:language s)
                        :sig  (nm* (:target-type s))}
                       (ver s)))
        query (for [[_ s] (:transforms body) :when (:function-name s)]
                (merge {:name (:function-name s) :kind "query" :language (:language s)
                        :sig  (let [o (:output s)] (nm* (or (:object-type o) (:dataset o))))}
                       (ver s)))]
    (sort-by (juxt :kind (comp str :name)) (concat edit query))))

(defmethod step! :function-list [ctx [_ _]]
  (emit! ctx {:functions (vec (schema-functions (schema-sections :functions :transforms)))})
  nil)

(defmethod step! :function-show [ctx [_ {nm :name}]]
  (let [body  (schema-sections :functions :transforms)
        find* (fn [section] (some (fn [[_ s]] (when (= nm (:function-name s)) s))
                                  (get body section)))
        spec  (or (find* :functions) (find* :transforms))]
    (if (nil? spec)
      (say! ctx (mark :yellow "⏸" (i18n/t :cli/no-such-function {:name (clojure.core/name nm)})))
      (emit! ctx spec ::function)))
  nil)

(defmethod render ::function [_ spec]
  (str/split-lines
   (with-out-str
     (pprint/pprint (update spec :releases #(some->> % keys (sort semver/compare-versions)))))))

(defmethod step! :run-function
  [ctx [_ {:keys [name inputs]}]]
  (let [wf (keyword name)
        b  (req! (str "run-function " (clojure.core/name wf))
                 (api/post! :function-run {:function-name wf :inputs (or inputs {})}))]
    (if (= "query" (some-> (:kind b) clojure.core/name))
      (emit! ctx (select-keys b [:function :kind :rows]) ::query-rows)
      (emit! ctx b))
    nil))

(defmethod render ::query-rows [_ {:keys [rows]}]
  (show-lines (mapv #(merge {:id (:id %)} (:values %)) rows)))

(defmethod step! :test-transform [ctx [_ {:keys [name]}]]
  (let [r (api/test-transform name)
        b (:body r)]
    (if-not (ok? r)
      (warn! (mark :red "✗" (str "transform test: " (or (:error b) (:error r)
                                                        (i18n/t :cli/failed)))))
      (emit! ctx (select-keys b [:transform :passed :failed :tests]) ::transform-test))))

(defmethod render ::transform-test [_ {:keys [tests transform] :as b}]
  [(for [{:keys [name passed? detail]} tests]
     (if passed?
       (mark :green "✓" (str name " — " detail))
       (mark :red "✗" (str name " — " detail))))
   (when (empty? tests) (mark :yellow "⚠" (str transform " declares no :tests")))
   (show-lines b)])

(defmethod step! :preview-transform [ctx [_ {:keys [name limit sample params output]}]]
  (let [r (api/preview-transform name limit sample (parse-param-flags params) output)
        b (:body r)]
    (if-not (ok? r)
      (warn! (mark :red "✗"
                   (str "preview-transform: " (or (:error b) (:error r) (i18n/t :cli/failed)))))
      (emit! ctx (select-keys b [:transform :language :rows :count :error :input-sample
                                 :input-caps])
             ::preview))
    nil))

(defmethod render ::preview [_ b]
  [(for [{:keys [input kept total]} (:input-caps b)]
     (mark :yellow "⚠"
           (str "input " (clojure.core/name (keyword input)) " sampled to " kept
                " of " total " rows — read it whole with --sample " total)))
   (show-lines b)])
