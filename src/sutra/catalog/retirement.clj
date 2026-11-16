(ns sutra.catalog.retirement
  (:require [sutra.types.transform-io :as tio]
            [sutra.types.kinds :as kinds]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.entities :as ents]
            [sutra.ref.install :as ri]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.outputs :as outputs]))

(set! *warn-on-reflection* true)

(defn orphaned-producers
  [schema]
  (let [live? (fn [kk r] (or (not (rid/rid? r)) (contains? (get schema kk) r)))]
    (into {}
          (for [[t-rid t] (:transforms schema)
                :let [gone (into #{}
                                 (keep (fn [o]
                                         (let [[sec r] (kinds/output-target o)]
                                           (when (and (kinds/is? :typed-output? (:kind o))
                                                      (not (live? sec r)))
                                             r))))
                                 (outputs/outputs-of t))]
                :when (seq gone)]
            [t-rid gone]))))

(def soft-referrer-kinds
  (ents/sections-in :soft-referrer))

(defn- ordered
  [hits]
  (vec (sort-by (fn [{:keys [in entity]}] [(str in) (str entity)]) hits)))

(defn- live-hit? [W {:keys [targets ref]}] (some #(contains? (get W %) ref) targets))

(defn- new-danglers
  [pre after gone referring]
  (ordered (for [[sec k] (sort-by str (into #{} (mapcat referring) gone))
                 :let [e (get-in after [sec k])]
                 :when (some? e)
                 h (ri/ref-hits {sec {k e}})
                 :when (and (contains? gone (:ref h)) (live-hit? pre h)
                            (not (live-hit? after h)))]
             h)))

(defn- deletion-ref-delta
  ([schema kinds-key ident ix] (deletion-ref-delta schema schema kinds-key ident ix))
  ([pre base kinds-key ident ix]
   (if-not (contains? rid/rid-kinds kinds-key)
     []
     (let [rid (rid/resolve-name->rid (sch/name-index pre) #{kinds-key} ident)]
       (if-not (and rid (contains? (get base kinds-key) rid))
         []
         (let [after     (update base kinds-key dissoc rid)
               referring (:referring ix)]
           (new-danglers pre after
                         (into #{rid}
                               (for [[sec k] (referring rid)
                                     :when (and (= :link-types sec)
                                                (contains? (:link-types pre) k)
                                                (not (contains? (:link-types base) k)))]
                                 k))
                         referring)))))))

(declare cascade-associated-links protected-statuses)

(defn deletion-blockers
  [schema kinds-key ident cascade? ix]
  (into [] (remove #(contains? soft-referrer-kinds (:in %)))
        (if cascade?
          (deletion-ref-delta schema (cascade-associated-links schema kinds-key ident ix)
                              kinds-key ident ix)
          (deletion-ref-delta schema kinds-key ident ix))))

(defn retirement-blockers
  [schema going ix]
  (let [after (reduce-kv (fn [s kk ids] (update s kk #(apply dissoc % ids))) schema going)]
    (into [] (remove #(ents/in-role? :dangles-freely (:in %)))
          (new-danglers schema after (into #{} cat (vals going)) (:referring ix)))))

(defn deletion-warnings
  [schema kinds-key ident ix]
  (into [] (filter #(contains? soft-referrer-kinds (:in %)))
        (deletion-ref-delta schema kinds-key ident ix)))

(defn associated-link-types
  [schema kinds-key ident ix]
  (into [] (filter #(= :link-types (:in %))) (deletion-ref-delta schema kinds-key ident ix)))

(defn backing-referrers
  [schema kinds-key ident ix]
  (if-let [rid (when (= :datasets kinds-key)
                 (rid/resolve-name->rid (sch/name-index schema) #{kinds-key} ident))]
    (into #{} (filter #(= rid (backing/authored-backing-dataset schema %)))
          ((:backed-by ix) rid))
    #{}))

(defn cascade-associated-links
  [schema kinds-key ident ix]
  (let [link-rids (into #{} (map :entity) (associated-link-types schema kinds-key ident ix))
        backed    (backing-referrers schema kinds-key ident ix)]
    (cond-> schema
      (seq link-rids) (update :link-types #(apply dissoc % link-rids))
      (seq backed)    (update :object-types
                              #(reduce (fn [ots ot] (update ots ot dissoc :backing))
                                       % backed)))))

(defn- owned-link-producers
  [schema link-rid ix]
  (when-let [nm (some-> (get-in schema [:link-types link-rid :link-type-name]) name)]
    (into #{} (for [[trid t] (for [n [(str "link-" nm) (str "resolve-" nm)]
                                   k ((:names ix) :transforms n)
                                   :let [t (get-in schema [:transforms k])]
                                   :when t]
                               [k t])
                    :let [tn (some-> (:transform-name t) name)
                          outs (keep :link-type (tio/outputs-of t))]
                    :when (or (and (= tn (str "link-" nm)) (some #{link-rid} outs))
                              (= tn (str "resolve-" nm)))]
                trid))))

(def ^:private link-types-taken
  {:link-types   (fn [schema _ ident _ _]
                   (some-> (rid/resolve-name->rid (sch/name-index schema) #{:link-types} ident)
                           vector))
   :object-types (fn [schema kinds-key ident cascade? ix]
                   (when cascade?
                     (map :entity (associated-link-types schema kinds-key ident ix))))})

(defn remove-owned-link-producers
  [schema kinds-key ident cascade? ix]
  (let [links (when-let [f (link-types-taken kinds-key)] (f schema kinds-key ident cascade? ix))
        gone  (into #{} (mapcat #(owned-link-producers schema % ix)) links)
        outs  (into #{} (comp (mapcat #(let [t (get-in schema [:transforms %])]
                                         (cons (:dataset t)
                                               (keep :dataset (tio/outputs-of t)))))
                              (filter #(= :transform-output (get-in schema [:datasets % :kind]))))
                    gone)]
    (cond-> schema
      (seq gone) (update :transforms #(apply dissoc % gone))
      (seq outs) (update :datasets #(apply dissoc % outs)))))

(defn remove-own-object-backing
  [schema kinds-key ident ix]
  (if-not (= :object-types kinds-key)
    schema
    (let [rid (rid/resolve-name->rid (sch/name-index schema) #{kinds-key} ident)
          ds  (get-in schema [:object-types rid :dataset])]
      (if-not (and ds (= :object-backing (get-in schema [:datasets ds :kind]))
                   (not (contains? protected-statuses
                                   (get-in schema [:datasets ds :status]))))
        schema
        (let [without-type (update schema :object-types dissoc rid)]
          (cond-> schema
            (empty? (deletion-ref-delta without-type :datasets ds ix))
            (update :datasets dissoc ds)))))))

(defn rename-own-object-backing
  [schema kinds-key rid old new]
  (let [ds (get-in schema [:object-types rid :dataset])
        nf (rid/name-field-of :datasets)]
    (if (and (= :object-types kinds-key)
             ds
             (= :object-backing (get-in schema [:datasets ds :kind]))
             (= (name old) (some-> (get-in schema [:datasets ds nf]) name))
             (not (lookup/rid-of schema :datasets new)))
      (assoc-in schema [:datasets ds nf] (keyword (name new)))
      schema)))

(defn status-of
  [schema kinds-key ident]
  (when (contains? rid/rid-kinds kinds-key)
    (let [rid (rid/resolve-name->rid (sch/name-index schema) #{kinds-key} ident)]
      (some-> (get-in schema [kinds-key rid]) :status))))

(defn set-status
  [schema kinds-key ident status deprecation]
  (let [rid (rid/resolve-name->rid (sch/name-index schema) #{kinds-key} ident)]
    (cond-> schema
      rid (update-in [kinds-key rid]
                     (fn [spec]
                       (cond-> (assoc spec :status (keyword status))
                         (some? deprecation) (assoc :deprecation deprecation)
                         (nil? deprecation)  (dissoc :deprecation)))))))

(def object-type-only-statuses
  #{:endorsed})

(def protected-statuses
  #{:active :endorsed})

(defn- status-block
  [schema kinds-key ident verb tail]
  (let [status (or (status-of schema kinds-key ident) :experimental)]
    (when (contains? protected-statuses status)
      {:status status
       :message (str "cannot " verb " " (name (rid/kinds-key->tag kinds-key)) " " (name ident)
                     ": it is " status tail)})))

(defn deletion-status-block
  [schema kinds-key ident]
  (status-block schema kinds-key ident "delete"
                " — set its status to :experimental or :deprecated first"))

(defn cascade-status-block
  [schema kinds-key ident ix]
  (some (fn [{link :entity}]
          (let [status (or (get-in schema [:link-types link :status]) :experimental)]
            (when (contains? protected-statuses status)
              {:status status
               :message (str "cannot delete " (name (rid/kinds-key->tag kinds-key)) " "
                             (name ident) " with --cascade: its link type "
                             (name (or (lookup/name-of schema :link-types link) link))
                             " is " status " and an " status " link type cannot be deleted —"
                             " set its status to :experimental or :deprecated first")})))
        (associated-link-types schema kinds-key ident ix)))

(defn deletion-owner-block
  [schema kinds-key ident]
  (when (= :repo (:tree (ents/by-section kinds-key)))
    (let [rid   (lookup/rid-of schema kinds-key ident)
          owner (some-> (get-in schema [kinds-key rid]) :repository)
          repo  (get-in schema [:repositories (lookup/rid-of schema :repositories owner)])]
      (when repo
        (let [what (str (name (rid/kinds-key->tag kinds-key)) " " (name ident))
              rn   (rid/->name (:repository-name repo))]
          {:repository rn
           :entity     (name ident)
           :message    (str "cannot delete " what ": repository " rn " declares it, and"
                            " its source is the author's. Leave it out of that repository's"
                            " tree (its `.ent` entry and its code file) and push — the push"
                            " retires it.")})))))

(defn deletion-referrers
  [schema]
  (reduce (fn [m {:keys [in entity ref]}]
            (cond-> m
              (not (contains? soft-referrer-kinds in))
              (update ref (fnil conj []) {:in in :entity entity})))
          {} (ri/ref-hits schema)))

(defn deletion-reasons
  ([schema kinds-key ident] (deletion-reasons schema kinds-key ident (deletion-referrers schema)))
  ([schema kinds-key ident referrers]
   (let [rid    (lookup/rid-of schema kinds-key ident)
         names  (fn [sec n]
                  (for [[k e] (get schema sec)
                        :when (= n (some-> (get e (rid/name-field-of sec)) name))]
                    k))
         after  (remove-owned-link-producers schema kinds-key ident false {:names names})
         refs   (vec (distinct (for [r (get referrers rid)
                                     :when (and (not= rid (:entity r))
                                                (contains? (get after (:in r)) (:entity r)))]
                                 r)))
         status (deletion-status-block schema kinds-key ident)
         owned  (deletion-owner-block schema kinds-key ident)]
     (cond-> []
       status     (conj {:reason :status :status (:status status) :message (:message status)})
       owned      (conj {:reason :declared-in-repository :repository (:repository owned)
                         :message (:message owned)})
       (seq refs) (conj {:reason :referenced :referrers refs})))))

(defn rename-status-block
  [schema kinds-key ident]
  (status-block schema kinds-key ident "rename"
                (str " and its name is the API contract every consumer holds"
                     " — set its status to :experimental to rename it, or keep the name,"
                     " deprecate it with a :deprecation {:replacement <new-name>} and stand"
                     " the new one up beside it")))

(defn unbind-soft-referrers
  [schema kinds-key ident]
  (let [rid   (rid/resolve-name->rid (sch/name-index schema) #{kinds-key} ident)
        drop* (fn [coll] (vec (remove #{rid} coll)))
        each  (fn [W sec f] (cond-> W (map? (get W sec)) (update sec update-vals f)))]
    (if-not rid
      schema
      (-> schema
          (each :applications
                (fn [app] (cond-> app (= rid (:object-type app)) (dissoc :object-type))))
          (each :custom-applications
                (fn [app]
                  (cond-> app
                    (some #{rid} (get-in app [:scope :object-types]))
                    (update-in [:scope :object-types] drop*)
                    (some #{rid} (get-in app [:scope :actions]))
                    (update-in [:scope :actions] drop*))))
          (each :object-type-groups
                (fn [g] (cond-> g (some #{rid} (:members g)) (update :members drop*))))
          (each :pipelines
                (fn [p]
                  (cond-> p
                    (some #{rid} (:imports p)) (update :imports drop*)
                    (some #{rid} (:outputs p)) (update :outputs drop*))))))))
