(ns sutra.cli.verbs.branches
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.out :refer [emit! mark paint prompt-yes? render say! show-lines]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.cli.instance :as inst]
            [sutra.cli.view :refer [kw-name]]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defmethod step! :branches [ctx [_ {:keys [closed? machine?]}]]
  (let [r  (req! "branches" (api/list-branches {:closed? closed? :machine? machine?}))
        bs (:branches r)]
    (emit! ctx {:branches (vec bs) :current (:current r)})
    nil))

(defn- policy-settings
  [settings]
  (let [fields (when (seq settings)
                 (into {} (map (juxt (comp keyword :key) identity)) (:policy-fields (inst/facts))))]
    (into {}
          (for [[k v] settings
                :let [{:keys [kind]} (or (fields (keyword k))
                                         (throw
                                          (ex-info
                                           (str "no branch-policy field " (name k)
                                                " — the server knows "
                                                (str/join ", " (sort (map name (keys fields)))))
                                           {:field k})))
                      v (str v)]]
            [(keyword k) (case (keyword kind)
                           :bool         (contains? #{"true" "on" "yes" "1"} (str/lower-case v))
                           :long         (parse-long v)
                           :rules        (edn/read-string v)
                           :edn-keywords (mapv keyword (str/split v #","))
                           v)]))))

(defmethod step! :protect [ctx [_ {:keys [name on? off? review default-review? retention-days
                                          inactive-days require-checks? no-require-checks?
                                          auto-land? no-auto-land? settings]}]]
  (let [body (cond-> (policy-settings settings)
               on?            (assoc :protected true)
               off?           (assoc :protected false)
               review         (assoc :review review)
               default-review? (assoc :review nil)
               retention-days (assoc :retention-days retention-days)
               inactive-days  (assoc :inactive-days inactive-days)
               require-checks?    (assoc :require-checks true)
               no-require-checks? (assoc :require-checks false)
               auto-land?         (assoc :auto-land true)
               no-auto-land?      (assoc :auto-land false))
        lesen? (empty? body)
        p    (if lesen?
               (req! (str "policy " name) (api/get! :branch-policy {:name name}))
               (req! (str "protect " name) (api/post! :branch-policy {:name name} body)))]
    (emit! ctx p)
    nil))

(defmethod step! :branch-roles [ctx [_ {:keys [name own revoke]}]]
  (when own
    (req! (str "own " name) (api/post! :branch-roles {:name name} {:principal own})))
  (when revoke
    (req! (str "revoke " name)
          (api/post! :branch-roles {:name name} {:principal revoke :revoke? true})))
  (emit! ctx (req! (str "roles " name) (api/get! :branch-roles {:name name})))
  nil)

(defmethod step! :sweep [ctx [_ {:keys [days apply?]}]]
  (emit! ctx (req! "branch-sweep" (api/post! :branch-sweep (cond-> {:dry-run? (not apply?)}
                                                             days (assoc :days days)))))
  nil)

(defmethod step! :propose [ctx [_ {:keys [title target source desc justification merge-mode]}]]
  (let [p (req! "propose" (api/post! :proposals (cond-> {}
                                                  title  (assoc :title title)
                                                  target (assoc :target target)
                                                  source (assoc :source source)
                                                  desc   (assoc :description desc)
                                                  justification (assoc :justification justification)
                                                  merge-mode (assoc :merge-mode merge-mode))))]
    (emit! ctx p)
    nil))

(defmethod step! :proposals [ctx [_ {:keys [status target source]}]]
  (let [ps (:proposals (req! "proposals"
                             (api/list-proposals {:status status :target target :source source})))]
    (emit! ctx {:proposals (vec ps)})
    nil))

(defmethod step! :proposal [ctx [_ {:keys [id]}]]
  (let [p  (req! (str "proposal " id) (api/get! :proposal {:id id}))
        ts (seq (:tasks (req! (str "tasks " id) (api/get! :proposal-tasks {:id id}))))]
    (emit! ctx (cond-> p ts (assoc :tasks (vec ts))))
    nil))

(defmethod step! :ask-review [ctx [_ {:keys [id reviewer justification]}]]
  (let [rs (when reviewer (remove str/blank? (str/split (str reviewer) #",")))]
    (emit! ctx (req! (str "ask-review " id)
                     (api/post! :proposal-ask-review {:id id}
                                (cond-> {}
                                  (seq rs)      (assoc :reviewers (vec rs))
                                  justification (assoc :justification justification)))))
    nil))

(defmethod step! :protection [ctx [_ {:keys [resource off? all?]}]]
  (emit! ctx (req! "protection" (cond
                                  all?     (api/post! :protections {:all true})
                                  resource (api/post! :protections {:resources [resource]
                                                                    :on (not off?)})
                                  :else    (api/get! :protections))))
  nil)

(defmethod step! :remove-resources [ctx [_ {:keys [name resources]}]]
  (emit! ctx (req! (str "remove from " name)
                   (api/post! :branch-resources {:name name} {:remove (mapv str resources)})))
  nil)

(defmethod step! :project-use [ctx [_ {:keys [project default?]}]]
  (cond
    (and project default?)
    (do (emit! ctx (req! "project use --default" (api/post! :default-project {:project project})))
        {:kind :project :project project})

    project
    (do (emit! ctx {:project project :scope "session"})
        {:kind :project :project project})

    :else
    (do (emit! ctx (merge (req! "project use" (api/get! :default-project)) {:session tx/*project*}))
        nil)))

(defmethod step! :project-policy
  [ctx [_ {:keys [project review default-review? protect-all? no-protect-all?]}]]
  (let [body (cond-> {}
               review          (assoc :review review)
               default-review? (assoc :review nil)
               protect-all?    (assoc :protect-all true)
               no-protect-all? (assoc :protect-all false))]
    (emit! ctx (req! (str "project policy " project)
                     (if (empty? body)
                       (api/get! :project-policy {:name project})
                       (api/post! :project-policy {:name project} body))))
    nil))

(defn- build-after-merge!
  [{:keys [build-scope]} build]
  (when-let [names (seq (get build-scope (case build "all" :affected "modified" :modified nil)))]
    (req! "build" (api/post! :import-run {:only (mapv keyword names) :strategy :selected
                                          :detach true}))))

(defmethod step! :merge-proposal [ctx [_ {:keys [id build]}]]
  (let [p (req! (str "merge " id) (api/post! :proposal-merge {:id id} nil))
        b (when (and build (:merge-commit p)) (build-after-merge! p build))]
    (emit! ctx (cond-> (assoc p :id id) b (assoc :build (select-keys b [:run-id]))) ::merged))
  nil)

(defmethod render ::merged [_ {:keys [id] :as p}]
  [(show-lines (dissoc p :id))
   (case (some-> (:file-plane-status p) clojure.core/name)
     "error"     (mark :red "✗" (i18n/t :cli/file-plane-did-not-merge
                                        {:grund (:error (:file-plane p))}))
     "conflicts" (mark :yellow "⚠"
                       (i18n/t :cli/file-plane-conflicts
                               {:repos (str/join ", " (for [[repo r] (:file-plane p)
                                                            :when (seq (:conflicts r))]
                                                        (clojure.core/name repo)))}))
     "pending"   (mark :dim "·" (i18n/t :cli/file-plane-pending {:id id}))
     nil)])

(defmethod step! :preview-staged [ctx [_ _]]
  (let [r (req! "changeset preview" (api/get! :staging-preview))]
    (emit! ctx r ::staged-preview)
    nil))

(defmethod render ::staged-preview [_ r]
  [(show-lines r)
   (when (= "conflicts" (kw-name (:status r)))
     (mark :dim "»" (i18n/t :cli/settle-on-branch)))])

(defn- resolution-key
  [s]
  (let [[cls k] (str/split (str s) #"/" 2)]
    (when (str/blank? (str k))
      (throw (ex-info (str "bad entity " (pr-str s)
                           " — expected <class>/<key>, e.g. object-types/widget"
                           " (exactly as `changeset preview` prints it)")
                      {:entity s})))
    [(keyword cls) (keyword k)]))

(defmethod step! :rebase-staged [ctx [_ {:keys [mine theirs]}]]
  (let [pick (fn [s v] (into {} (map (fn [e] [(resolution-key e) v]))
                             (remove str/blank? (str/split (str s) #","))))
        res  (merge (pick theirs :theirs) (pick mine :mine))]
    (emit! ctx (req! "changeset rebase" (api/post! :staging-rebase {:resolutions res})))
    nil))

(defmethod step! :discard-one [ctx [_ {:keys [seq]}]]
  (let [r (req! (str "discard-staged " seq) (api/delete! :staged-by-seq {:seq seq}))]
    (if (pos? (or (:deleted r) 0))
      (emit! ctx r)
      (say! ctx (mark :yellow "⚠" (i18n/t :cli/no-staged-change {:nr seq})))))
  nil)

(defmethod step! :rollback [ctx [_ {:keys [name to kind]}]]
  (let [k (or kind :dataset)]
    (if-not (prompt-yes? ctx (i18n/t :cli/rollback-confirm
                                     {:art (clojure.core/name k)
                                      :name (paint :bold (clojure.core/name name))
                                      :ziel (paint :bold (str to))}))
      (do (say! ctx (mark :dim "»" (i18n/t :cli/left-alone))) nil)
      (do (emit! ctx (req! (str "rollback " (clojure.core/name name))
                           (api/post! :lineage-rollback {:node-kind k :node-key name :commit to})))
          nil))))

(defmethod step! :version-show [ctx [_ {:keys [kind name as-of]}]]
  (let [nm (clojure.core/name name)
        r  (case kind
             :object-type (req! (str "object-type " nm " @ " as-of)
                                (api/get! :at-object-type {:commit as-of :type nm}))
             :transform   (req! (str "transform "   nm " @ " as-of)
                                (api/get! :transform-spec {:name nm :as-of as-of}))
             :source      (req! (str "source "      nm " @ " as-of)
                                (api/get! :datasource-spec {:name nm :as-of as-of}))
             (throw
              (ex-info (str "version show: --kind must be object-type, transform or source, got "
                            (clojure.core/name kind))
                       {:kind kind})))]
    (emit! ctx r)
    nil))

(defmethod step! :version-rows [ctx [_ {:keys [type as-of limit]}]]
  (let [t    (clojure.core/name type)
        r    (req! (str "objects " t " @ " as-of) (api/get! :at-objects {:commit as-of :type t}))
        objs (vec (or (:objects r) (:rows r) (when (sequential? r) r)))
        shown (if limit (vec (take limit objs)) objs)]
    (emit! ctx {:type t :as-of as-of :count (count objs) :rows shown})
    nil))
