(ns sutra.branching.access
  (:require [clojure.edn :as edn]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.access.api :as access]
            [sutra.authoring.api :as authoring]
            [sutra.branching.proposals :as proposals]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.shape :as shape]
            [sutra.governance.api :as governance]
            [sutra.store.api :as api]
            [sutra.store.protection :as protection]
            [sutra.store.run :as run]
            [sutra.store.world :as world]
            [sutra.types.entities :as ents]
            [sutra.types.review :as review]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn- unprefixed [x] (some-> x str (str/replace #"^:" "")))

(defn- ->bool [v] (if (string? v) (contains? #{"true" "1" "yes"} v) (boolean v)))

(defn- refused-unless-ok
  [r ctx]
  (if (:error r) (governance/raise-refused! r ctx :refusal.branching/refused) r))

(defn- resource-rid
  [schema ref]
  (let [s (str ref)
        [sec nm] (str/split s #"/" 2)]
    (if (and nm (contains? schema (keyword sec)))
      (let [section (keyword sec) nf (rid/name-field-of section)]
        (some (fn [[r spec]] (when (= nm (some-> (get spec nf) name)) (str r)))
              (get schema section)))
      (some (fn [[_ es]] (when (and (map? es) (contains? es s)) s)) schema))))

(defn- project-of-rid [schema r]
  (some (fn [[_ es]] (when (map? es) (some-> (get-in es [r :project]) str))) schema))

(defn- owns?
  [auth p]
  (or (nil? p) ((grants/owner-of auth) p)))

(defn- named-protection [schema {:keys [resources projects]}]
  (let [label (fn [r] (or (some (fn [[section es]]
                                  (when-let [spec (and (map? es) (get es r))]
                                    (str (name section) "/"
                                         (some-> (get spec (rid/name-field-of section)) name))))
                                schema)
                          r))]
    {:resources (vec (sort (map label resources)))
     :projects  (vec (sort (keep #(some-> (get-in schema [:projects % :project-name]) name)
                                 projects)))}))

(defn- reviewed-resources
  ([schema] (reviewed-resources schema nil))
  ([schema project]
   (for [section ents/sections
         :when (ents/reviewed-section? section)
         [r spec] (get schema section)
         :when (or (nil? project) (= (str project) (some-> (:project spec) str)))]
     [section (str r) spec])))

(defn protections
  [store schema]
  (let [prot  (protection/protection store)
        label (fn [[section _ spec]]
                (str (name section) "/" (some-> (get spec (rid/name-field-of section)) name)))]
    (assoc (named-protection schema prot)
           :effective (vec (sort (for [[_ r spec :as row] (reviewed-resources schema)
                                       :when (policy/protected-row?
                                              prot {:rid r :project (some-> (:project spec) str)})]
                                   (label row))))
           :sections (mapv name (filter ents/reviewed-section? ents/sections))
           :trunk-protected (governance/protected? store policy/default-branch-name))))

(defn protect-everything!
  [store auth schema]
  (when-not (grants/all-projects? auth)
    (refusal/refuse! :refusal.access/protect-all-admin-only {}))
  (protection/protect! store (map second (reviewed-resources schema)) true)
  (protections store schema))

(defn protect!
  [store auth schema refs on?]
  (let [resolved (for [ref refs] [ref (resource-rid schema ref)])
        unknown  (vec (for [[ref r] resolved :when (nil? r)] ref))
        denied   (vec (for [[ref r] resolved
                            :when (and r (not (owns? auth (project-of-rid schema r))))] ref))]
    (cond
      (empty? refs)
      (refusal/refuse! :refusal.access/resources-unnamed {})
      (seq unknown)
      (refusal/refuse! :refusal.access/unknown-resource {:unknown unknown}
                       {:unknown-list (str/join ", " unknown)})
      (seq denied)
      (refusal/refuse! :refusal.access/protection-owner-only {:denied denied}
                       {:denied-list (str/join ", " denied)})
      :else
      (named-protection schema (protection/protect! store (map second resolved) on?)))))

(defn- project-named [schema nm]
  (or (some (fn [[r spec]] (when (= nm (some-> (:project-name spec) name)) (str r)))
            (:projects schema))
      (refusal/refuse! :refusal.access/unknown-project {:project nm})))

(defn project-policy
  [store auth schema nm]
  (let [rid (project-named schema nm)
        p   (protection/container-policy store rid)]
    (assoc p :project nm :custom (review/custom-of (:review p))
           :may-manage (boolean (owns? auth rid)))))

(defn set-project-policy!
  [store auth schema nm params]
  (let [rid (project-named schema nm)
        rev (fn [v] (if (string? v) (edn/read-string v) v))
        on? (and (contains? params :protect-all) (->bool (:protect-all params)))]
    (when-not (owns? auth rid)
      (refusal/refuse! :refusal.access/policy-owner-only {:project nm}))
    (when (and on? (not (:protect-all (protection/container-policy store rid))))
      (protection/release-unmarked! store (map second (reviewed-resources schema rid))))
    (refused-unless-ok
     (protection/set-container-policy!
      store rid (cond-> {}
                  (contains? params :protect-all)
                  (assoc :protect-all (->bool (:protect-all params)))
                  (contains? params :review) (assoc :review (rev (:review params)))
                  (contains? params :custom)
                  (assoc :review (some-> (:custom params) rev review/custom-rule vector))))
     {:project nm})))

(defn default-project
  [store context]
  {:project (:default-project (protection/container-policy store protection/space))
   :context context})

(defn set-default-project!
  [store auth schema p]
  (when-not (grants/all-projects? auth)
    (refusal/refuse! :refusal.access/default-project-admin-only {}))
  (let [p (not-empty (unprefixed p))]
    (when (and p (not-any? #(= p (some-> (:project-name (val %)) name)) (:projects schema)))
      (refusal/refuse! :refusal.access/unknown-project {:project p}))
    (protection/set-container-policy! store protection/space {:default-project p})))

(defn- project-rid [schema p]
  (let [p (unprefixed p)]
    (some (fn [[r spec]] (when (or (= p (str r)) (= p (some-> (:project-name spec) name))) (str r)))
          (:projects schema))))

(defn- group-of
  [store schema g]
  (when-let [t (lookup/rid-of schema :object-types :group)]
    (let [g  (str g)
          os (world/with-snapshot (world/at store "main")
               #(api/read % grants/system-authority [:objects t]))
          o  (some #(when (or (= g (str (:id %))) (= g (str (get-in % [:values :name])))) %) os)]
      (when o [(str (:id o)) (some-> (get-in schema [:object-types t :project]) str)]))))

(defn file-request!
  [store schema {:keys [kind for role project group justification]}]
  (let [kind   (some-> kind unprefixed keyword)
        for    (some-> for str not-empty)
        who    (if for (some-> (access/fetch store for) :user-object-id str)
                   (policy/actor-name store))
        fields (case kind
                 :project-role     {:kind kind :principal who :role (unprefixed role)
                                    :project-rid (project-rid schema project)
                                    :project (some-> project str)}
                 :group-membership (let [[gid gp] (group-of store schema group)]
                                     {:kind kind :principal who :group-id gid
                                      :group-project gp :group (some-> group str)})
                 {:kind kind})]
    (when (and for (nil? who))
      (refusal/refuse! :refusal.access/unknown-principal {:for for}))
    (refused-unless-ok (governance/create-request! store fields justification) {:kind kind})))

(defn- may-decide?
  [store auth r]
  (let [own (grants/owner-of auth)
        me  (policy/actor-name store)]
    (some #(review/eligible? %
                             {:who me
                              :owner?
                              (boolean (or (grants/all-projects? auth)
                                           (and (:project %) (own (:project %)))))}
                             {:author (:author r)})
          (:tasks r))))

(defn- may-see?
  [store auth r]
  (let [me (str (policy/actor-name store))]
    (boolean (or (grants/auth-off? store)
                 (grants/all-projects? auth)
                 (contains? #{(str (:author r)) (str (:principal r))} me)
                 (may-decide? store auth r)))))

(defn requests
  [store auth status inbox?]
  (filterv (if inbox? #(may-decide? store auth %) #(may-see? store auth %))
           (governance/list-requests store {:status status})))

(defn request
  ([store id]
   (or (governance/fetch-request store id)
       (refusal/refuse! :refusal.access/unknown-request {:id id})))
  ([store auth id]
   (let [r (request store id)]
     (if (may-see? store auth r)
       r
       (refusal/refuse! :refusal.access/unknown-request {:id id})))))

(defn close-request!
  [store id admin?]
  (let [r (request store id)]
    (when-not (or admin? (= (str (:author r)) (str (policy/actor-name store))))
      (refusal/refuse! :refusal.access/close-request-author-only {:id id}))
    (governance/resolve-request! store (:id r) :closed nil)))

(defn- apply-request!
  [store seen schema r]
  (let [main (world/at store "main")]
    (case (:kind r)
      :project-role
      (let [grant {:principal (:principal r) :role (keyword (:role r))}
            w     (update-in (world/authored main) [:projects (:project-rid r) :grants]
                             (fn [gs]
                               (if (some #(= grant (select-keys % [:principal :role])) gs) gs
                                   (conj (vec gs) grant))))]
        (authoring/install-world! main "main" w {:message (str "access request " (:id r))})
        {:grant grant :project (:project-rid r)})
      :group-membership
      (let [lt (lookup/rid-of schema :link-types :member-of)
            id ((shape/link-id-fn schema lt) (:principal r) (:group-id r))]
        (run/run! main
                  {:fences [[:link-type lt]]
                   :decide (fn [_]
                             {:tx {:ops     [{:op :create-link :type lt
                                              :payload {:id id :from-id (:principal r)
                                                        :to-id (:group-id r)}}]
                                   :message (str "access request " (:id r))}})})
        {:link (str id) :group (:group-id r)})
      :instance-change
      (authoring/apply-instance-change! store seen r))))

(defn decide-request!
  [store auth schema seen verdict id note]
  (let [r (refused-unless-ok (governance/judge-request! store id verdict note
                                                        (grants/owner-of auth))
                             {:request id})
        block (policy/tasks-block {:author (:author r) :reviews (:reviews r)
                                   :justification (:justification r)}
                                  (:tasks r) (proposals/owner? store "main"))]
    (cond
      (= verdict policy/rejected-verdict) (governance/resolve-request! store (:id r) :rejected nil)
      (nil? block) (governance/resolve-request! store (:id r) :completed
                                                (apply-request! store seen schema r))
      :else (assoc r :open (:error block)))))
