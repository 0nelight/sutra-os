(ns sutra.access.http.identity
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [sutra.access.ports :as ports]
            [sutra.http.responses :refer [ok]]
            [sutra.access.authority :as authority]
            [sutra.store.api :as api]
            [sutra.catalog.shape :as shape]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.storage.instance-config :as ic]
            [sutra.access.principals :as pr]
            [sutra.store.world :as world]
            [sutra.types.directory :as directory]
            [sutra.types.rid :as rid]
            [sutra.types.settings :as settings]
            [sutra.store.run :as run]))

(set! *warn-on-reflection* true)

(defn providers
  [store]
  (ports/identity-providers store))

(defn- provider [store id] (get (providers store) (keyword id)))

(defn config
  [store {fields :settings}]
  (settings/decode fields (into {} (for [[_ {:keys [key]}] fields] [key (ic/get-cfg store key)]))))

(defn ready
  [store capability]
  (for [[id p] (sort-by key (providers store))
        :when (get p capability)
        :let [cfg (config store p)]
        :when ((:ready? p) cfg)]
    [id p cfg]))

(defn status
  [store p]
  (let [cfg (config store p)]
    (settings/view (:settings p) cfg ((:ready? p) cfg))))

(defn set-config!
  [store p opts]
  (if-let [problem (settings/problem (:settings p) opts)]
    (refusal/refuse! :refusal.access/passed-error-problem problem {:message (:error problem)})
    (do (ic/set-cfgs! store (settings/rows (:settings p) opts))
        (status store p))))

(defn- provider-subject
  [id p cfg username password]
  (try ((:password p) cfg username password)
       (catch Exception e
         (attempt/rethrow-fatal! e)
         (binding [*out* *err*]
           (println (str "[auth] identity provider " (name id) " failed a password check: "
                         (.getName (class e)) ": " (ex-message e))))
         nil)))

(defn password-assertion
  [store username password]
  (when-not (or (str/blank? (str username)) (str/blank? (str password)))
    (some (fn [[id p cfg]]
            (when-let [subject (provider-subject id p cfg (str username) (str password))]
              {:provider id :source (:source p) :subject subject}))
          (ready store :password))))

(defn negotiation
  [store token]
  (when-let [[id p cfg] (first (ready store :negotiate))]
    (let [r ((:negotiate p) cfg token)]
      (merge {:provider id :source (:source p) :complete? false :failed? (nil? r)} r))))

(defn- provisioned
  [store source]
  (into {} (map (fn [{:keys [external-subject user-object-id]}]
                  [(str/lower-case external-subject) user-object-id]))
        (pr/federated-subjects store source)))

(defn preview
  [store p]
  (let [cfg (config store p)]
    (when-not ((:ready? p) cfg)
      (refusal/refuse! :refusal.access/is-not-configured-set-its {} {:label (:label p)}))
    (let [snapshot ((:directory p) cfg)]
      (if-let [pv (:preview p)]
        (assoc (pv cfg snapshot (provisioned store (:source p))) :directory snapshot)
        {:directory snapshot}))))

(defn- declares? [schema t attr]
  (some #(= attr (keyword (name (:attribute-name %)))) (lookup/attrs schema t)))

(defn- apply-steps!
  [store schema source label steps]
  (let [main   (world/at store "main")
        user-t (lookup/rid-of schema :object-types :user)
        by     (group-by :step steps)
        _      (when (and (nil? user-t) (seq (concat (:create-user by) (:disable by))))
                 (refusal/refuse! :refusal.access/this-instance-has-no-user-2 {}))
        ids    (into {} (for [{:keys [subject]} (:create-user by)] [subject (str (random-uuid))]))
        keyed? (and user-t (declares? schema user-t :user-id))
        off?   (and user-t (declares? schema user-t :disabled))
        ops    (vec (concat
                     (for [{:keys [subject display]} (:create-user by)]
                       {:op :create-object :type user-t
                        :payload {:id     (ids subject)
                                  :values (cond-> {:name display :rolle "consumer"}
                                            keyed? (assoc :user-id (ids subject)))}})
                     (when off?
                       (for [{:keys [user]} (:disable by)]
                         {:op :update-object :type user-t
                          :payload {:id user :values {:disabled true}}}))))
        _      (when (seq ops)
                 (run/run! main
                           {:fences [[:type user-t]]
                            :decide (fn [_]
                                      {:tx    {:ops ops :message (str "directory sync " label)}
                                       :after (for [{:keys [subject username]} (:create-user by)]
                                                {:kind      :register-principal
                                                 :principal {:username username :branch "main"
                                                             :user-id (ids subject)
                                                             :external-subject subject
                                                             :auth-source source}})})}))
        user   (fn [r] (if (vector? r) (ids (second r)) r))
        w0     (world/authored main)
        rids   (into {} (for [{:keys [project]} (:create-project by)]
                          [project (rid/mint-for :projects (str (random-uuid)))]))
        rid-of (fn [p] (or (rids p)
                           (some (fn [[r s]] (when (= (name p) (some-> (:project-name s) name)) r))
                                 (:projects w0))))
        w      (as-> w0 w
                 (reduce (fn [w [p r]]
                           (assoc-in w [:projects r]
                                     {:project-name p
                                      :description (str "Portal-Projekt " (name p))}))
                         w rids)
                 (reduce (fn [w {:keys [project role] :as g}]
                           (let [grant {:principal (:user g) :role (keyword (name role))}]
                             (update-in w [:projects (rid-of project) :grants]
                                        (fn [gs]
                                          (if (some #(= grant (select-keys % [:principal :role]))
                                                    gs) gs (conj (vec gs) grant))))))
                         w (map #(assoc % :user (user (:user %))) (:grant by))))
        _      (when (not= w w0)
                 (ports/install-world! main "main" w
                                          {:message (str "directory sync " label)}))]
    (vec (for [s steps]
           (case (:step s)
             :create-user    (assoc s :user-id (ids (:subject s)))
             :disable        (if off? (assoc s :done true)
                                 (assoc s
                                        :skipped "the :user type declares no `disabled` attribute"))
             (assoc s :done true))))))

(defn- membership-types
  [schema]
  (let [g  (lookup/rid-of schema :object-types :group)
        mo (lookup/rid-of schema :link-types :member-of)]
    (when (and g mo)
      {:group g :member-of mo :subgroup-of (lookup/rid-of schema :link-types :subgroup-of)})))

(defn- membership-read
  [s {:keys [group member-of subgroup-of]} user-ids]
  (let [read   #(api/read s grants/system-authority %)
            gs     (read [:objects group])
            by-id  (into {}
                         (for [g gs :let [n (get-in g [:values :name])] :when n]
                           [(str (:id g)) (str n)]))
        links  (fn [lt from] (if (and lt (seq from)) (read [:links lt {:from (vec from)}]) []))]
    {:groups  (into {} (map (fn [[id n]] [n id])) by-id)
     :members (set (keep (fn [l]
                           (when-let [g (by-id (str (:to-object-id l)))]
                             [(str (:from-object-id l)) g]))
                         (links member-of user-ids)))
     :nested  (set (keep (fn [l]
                           (let [a (by-id (str (:from-object-id l)))
                                 b (by-id (str (:to-object-id l)))]
                             (when (and a b) [a b])))
                         (links subgroup-of (keys by-id))))}))

(defn- membership-state
  [store ts user-ids]
  (world/with-snapshot (world/at store "main") #(membership-read % ts user-ids)))

(defn- membership-ops
  [schema {:keys [group member-of subgroup-of]} groups user steps]
  (let [new-g (into {}
                    (for [{:keys [step] g :group} steps :when (= :create-group step)]
                      [g (str (random-uuid))]))
        gid   (merge groups new-g)
        lid   (fn [lt a b] ((shape/link-id-fn schema lt) a b))
        link  (fn [op lt a b] (if (= op :create-link)
                                {:op :create-link :type lt
                                 :payload {:id (lid lt a b) :from-id a :to-id b}}
                                {:op :delete-link :type lt :payload {:id (lid lt a b)}}))]
    (vec (concat
          (for [[g id] new-g]
            {:op :create-object :type group :payload {:id id :values {:name g}}})
          (for [{:keys [step] u :user g :group p :in} steps
                :let [lt (if (#{:nest :unnest} step) subgroup-of member-of)]
                :when (and lt (not= :create-group step))]
            (case step
              :join   (link :create-link lt (user u) (gid g))
              :leave  (link :delete-link lt (user u) (gid g))
              :nest   (link :create-link lt (gid g) (gid p))
              :unnest (link :delete-link lt (gid g) (gid p))))))))

(defn- step-results [{:keys [subgroup-of]} steps]
  (mapv (fn [{:keys [step] :as s}]
          (if (and (#{:nest :unnest} step) (nil? subgroup-of))
            (assoc s :skipped "no :subgroup-of link type")
            (assoc s :done true)))
        steps))

(defn- apply-memberships!
  [store schema {:keys [group member-of subgroup-of] :as ts} directory refs user label]
  (let [user-ids (distinct (filter string? (map user (vals refs))))]
    (run/run! (world/at store "main")
              {:fences (into [[:type group]]
                             (map (fn [lt] [:link-type lt]))
                             (remove nil? [member-of subgroup-of]))
               :decide (fn [s]
                         (let [{:keys [groups members nested]} (membership-read s ts user-ids)
                               steps (directory/membership-steps directory groups refs
                                                                 members nested)
                               ops   (membership-ops schema ts groups user steps)]
                           (cond-> {:result {:steps steps :results (step-results ts steps)}}
                             (seq ops) (assoc :tx {:ops     ops
                                                   :message (str "directory sync " label)}))))})))

(defn sync!
  [store p {:keys [dry-run? disable-gone?]}]
  (let [{:keys [users directory] :as pv} (preview store p)
        schema  (authority/usable-schema store "main")
        have    (provisioned store (:source p))
        gone    (into (sorted-map) (remove (fn [[s _]] (contains? (:users directory) s))) have)
        steps   (directory/sync-steps users (keep :project-name (vals (:projects schema)))
                                      (if disable-gone? gone {}))
        ts      (membership-types schema)
        refs    (merge (select-keys have (keys (:users directory)))
                       (into {}
                             (for [{:keys [step subject]} steps :when (= :create-user step)]
                               [subject [:subject subject]])))
        state   (when ts (membership-state store ts (distinct (filter string? (vals refs)))))
        msteps  (when ts
                  (directory/membership-steps directory (:groups state) refs (:members state)
                                              (:nested state)))
        report  (cond-> {:steps steps :memberships (or msteps [])
                         :gone (vec (keys gone)) :skipped (:skipped pv) :dry-run (boolean dry-run?)}
                  (nil? ts) (assoc :memberships-skipped
                                   "the instance has no :group type with a :member-of link"))]
    (if dry-run?
      report
      (let [results (apply-steps! store schema (:source p) (:label p) steps)
            ids     (into {}
                          (keep (fn [{:keys [step subject user-id]}]
                                  (when (= :create-user step) [subject user-id]))) results)
            user    (fn [r] (if (vector? r) (ids (second r)) r))
            applied (when (seq msteps)
                      (apply-memberships! store (authority/usable-schema store "main") ts
                                          directory refs user (:label p)))]
        (assoc report
               :results results
               :memberships (or (:steps applied) msteps [])
               :membership-results (not-empty (:results applied)))))))

(defn- with-provider
  [f]
  (fn [{store :sutra/store {id :provider} :path-params :as req}]
    (if-let [p (provider store id)]
      (try (ok (f store p req))
           (catch clojure.lang.ExceptionInfo e
             (if-let [r (anomaly/of e)]
               {:status (anomaly/status r)
                :body   (merge {:error (anomaly/message r)}
                               (anomaly/data r))}
               {:status 502 :body {:error (str (name id) ": " (ex-message e))}}))
           (catch Exception e
             (attempt/rethrow-fatal! e)
             {:status 502 :body {:error (str (name id) ": " (ex-message e))}}))
      (refusal/refused :refusal.access/no-identity-provider {:providers (vec (sort (map name (keys (providers store)))))} {:id id}))))

(def module
  {:name        :identity
   :contributes {:http/routes
                 {:identity-status  {:get (with-provider (fn [store p _] (status store p)))}
                  :identity-config  {:get  (with-provider (fn [store p _] (status store p)))
                                     :post
                                     (with-provider
                                       (fn [store p req]
                                         (set-config! store p (:body-params req))))}
                  :identity-sync    {:post (with-provider
                                             (fn [store p req]
                                               (when-not (:directory p)
                                                 (refusal/refuse! :refusal.access/reads-no-directory {} {:label (:label p)}))
                                               (sync! store p (:body-params req))))}
                  :identity-preview {:get (with-provider
                                            (fn [store p _]
                                              (if (:directory p)
                                                (preview store p)
                                                (refusal/refuse! :refusal.access/reads-no-directory {} {:label (:label p)}))))}}}})
