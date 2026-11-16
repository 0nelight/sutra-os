(ns sutra.projects.kompass
  (:require [sutra.store.control :as ctl]
            [sutra.store.control.import-keys :as import-keys]
            [sutra.kit.uuid :as uuid]))

(set! *warn-on-reflection* true)

(defn- b [store] (:branch store))

(def ^:private kompass-cols [:rid :node-kind :parent-rid :name :ref :position :project])

(defn read-kompass-nodes [store]
  (ctl/list store :atlas-nodes {:branch (b store)} {:cols (conj kompass-cols :updated-at)}))

(defn write-kompass-node! [store auth node]
  (ctl/commit! store auth [[:upsert :atlas-nodes
                            [(-> (select-keys node kompass-cols) (update :position #(or % 0))
                                 (assoc :branch (b store)))]]])
  nil)

(defn rename-kompass-node! [store auth rid new-name]
  (ctl/commit! store auth [[:update :atlas-nodes {:branch (b store) :rid rid}
                            {:name new-name}]])
  nil)

(defn move-kompass-node! [store auth rid parent-rid]
  (ctl/commit! store auth [[:update :atlas-nodes {:branch (b store) :rid rid}
                            {:parent-rid parent-rid}]])
  nil)

(defn set-kompass-project! [store auth rids project]
  (when (seq rids)
    (ctl/commit! store auth [[:update :atlas-nodes {:branch (b store) :rid (set rids)}
                              {:project project}]]))
  nil)

(defn delete-kompass-node! [store auth rid]
  (ctl/delete-as! store auth :atlas-nodes {:branch (b store) :rid rid})
  nil)

(defn kompass-node [store rid]
  (some-> (ctl/get store :atlas-nodes {:branch (b store) :rid rid}) (select-keys kompass-cols)))

(defn- annotation-row->map [r]
  (-> (select-keys r [:ref :tags :trashed :curated :doc :pinned :promoted])
      (update :tags #(or % #{}))))

(defn read-kompass-annotations [store]
  (into {}
        (map (juxt :ref annotation-row->map))
        (ctl/list store :atlas-annotations {:branch (b store)})))

(defn write-kompass-annotation! [store {:keys [ref tags trashed curated doc pinned promoted]}]
  (ctl/commit! store
               [[:upsert :atlas-annotations
                 [{:ref ref :branch (b store) :tags (when (seq tags) (set tags)) :trashed trashed
                   :curated curated :doc doc :pinned (boolean pinned)
                   :promoted (boolean promoted)}]]])
  nil)

(defn delete-kompass-annotation! [store ref]
  (ctl/commit! store [[:delete :atlas-annotations {:branch (b store) :ref ref}]])
  nil)

(defn portfolios [store]
  (ctl/list store :portfolios {} {:cols [:rid :name :description :projects :updated-at]}))

(defn- take-projects-from-others!
  [store rid projects]
  (doseq [{other :rid held :projects} (portfolios store)
          :when (and (not= other rid) (some (set projects) held))]
    (ctl/commit! store [[:update :portfolios
                         {:rid other}
                         {:projects (vec (remove (set projects) held))}]])))

(defn create-portfolio! [store {:keys [rid name description projects created-by]}]
  (take-projects-from-others! store rid projects)
  (ctl/commit! store [[:insert :portfolios
                       [{:rid rid :name name :description description
                         :projects (vec projects) :created-by created-by}]]])
  nil)

(defn update-portfolio! [store rid changes]
  (when-let [projects (:projects changes)] (take-projects-from-others! store rid projects))
  (ctl/commit! store [[:update :portfolios
                       {:rid rid}
                       (cond-> (select-keys changes [:name :description])
                         (:projects changes) (assoc :projects (vec (:projects changes))))]])
  nil)

(defn delete-portfolio! [store rid]
  (ctl/commit! store [[:delete :portfolios {:rid rid}]])
  nil)

(defn collections [store]
  (ctl/list store :collections {} {:cols [:rid :name :description :refs :updated-at]}))

(defn create-collection! [store {:keys [rid name description refs created-by]}]
  (ctl/commit! store [[:insert :collections
                       [{:rid rid :name name :description description
                         :refs (vec refs) :created-by created-by}]]])
  nil)

(defn update-collection! [store rid changes]
  (ctl/commit! store [[:update :collections
                       {:rid rid}
                       (cond-> (select-keys changes [:name :description])
                         (:refs changes) (assoc :refs (vec (:refs changes))))]])
  nil)

(defn delete-collection! [store rid]
  (ctl/commit! store [[:delete :collections {:rid rid}]])
  nil)

(def ^:private file-cols [:id :name :project :size :mime :author :created-at :updated-at])

(defn files [store]
  (mapv #(select-keys % file-cols) (ctl/list store :files {})))

(defn file [store id] (ctl/get store :files {:id (str id)}))

(defn create-file! [store row]
  (ctl/commit! store [[:insert :files [row]]])
  (select-keys row file-cols))

(defn move-file! [store id project]
  (ctl/commit! store [[:update :files {:id (str id)} {:project project}]]))

(defn delete-file! [store id]
  (ctl/commit! store [[:delete :files {:id (str id)}]]))

(defn file-addresses
  [store]
  (into #{} (map :address) (ctl/list store :files {})))

(defn unplace!
  [store auth ref]
  (ctl/commit! store auth [[:delete :atlas-nodes {:branch (b store) :ref ref}]])
  nil)

(defn favorites [store principal]
  (mapv :ref (ctl/list store :favorites {:principal principal})))

(defn set-favorite! [store principal ref on?]
  (if on?
    (ctl/commit! store [[:upsert :favorites [{:principal principal :ref ref}]]])
    (ctl/commit! store [[:delete :favorites {:principal principal :ref ref}]]))
  nil)

(defn write-import-plan! [store plan]
  (ctl/commit! store [[:upsert :import-plans [{:branch (b store) :plan plan}]]])
  nil)

(defn read-import-plan [store]
  (:plan (ctl/get store :import-plans {:branch (b store)})))

(def ^:const pk-value-max-len 512)

(defn- encode-pk-value [pk-val]
  (let [s (pr-str pk-val)]
    (if (<= (count s) pk-value-max-len)
      s
      (pr-str [::pk-digest (str (uuid/v5 uuid/dns-namespace s))]))))

(defn write-import-keys! [store pk-index]
  (import-keys/write-import-keys! store (b store)
                          (for [[obj-type pk->uuid] pk-index
                                [pk-val uuid]       pk->uuid]
                            [(name obj-type) (encode-pk-value pk-val) uuid])))

(defn import-key-counts [store]
  (import-keys/import-key-counts store (b store)))

(defn placement-deletions
  [store branch ref]
  (:records (ctl/deleting store :atlas-nodes {:branch branch :ref ref})))

(defn write-index-times! [store object-types run-id]
  (when (seq object-types)
    (import-keys/write-index-times! store (b store) (map name object-types) run-id)))

(defn index-times [store]
  (import-keys/index-times store (b store)))

(defn- crosswalk-key [store object-type id-kind id-value]
  {:branch (b store) :object-type (name object-type) :id-kind (name id-kind)
   :id-value (str id-value)})

(defn crosswalk-lookup [store object-type id-kind id-value]
  (:entity-id (ctl/get store :entity-crosswalk (crosswalk-key store object-type id-kind id-value))))

(defn crosswalk-put! [store object-type id-kind id-value entity-id]
  (ctl/commit! store [[:upsert :entity-crosswalk
                       [(assoc (crosswalk-key store object-type id-kind id-value)
                               :entity-id entity-id)]]])
  nil)

(defn crosswalk-redirect! [store object-type loser-ids survivor-id]
  (let [losers (set (remove #(= % survivor-id) loser-ids))]
    (when (seq losers)
      (ctl/commit! store [[:update :entity-provenance
                           {:branch (b store) :entity-id losers}
                           {:entity-id survivor-id}]])
      (ctl/commit! store [[:update :entity-crosswalk
                           {:branch (b store) :object-type (name object-type) :entity-id losers}
                           {:entity-id survivor-id}]]))))

(defn write-provenance! [store rows]
  (ctl/commit! store
               [[:insert :entity-provenance
                 (map (fn [{:keys [entity-id property value source run-id]}]
                        {:branch (b store) :entity-id entity-id :property property :value value
                         :source source :run-id (some-> run-id str)})
                      rows)]]))

(def provenance-retraction "ENTITY-PROVENANCE-RETRACTED")

(defn latest-provenance-by-source [store entity-id]
  (into []
        (keep (fn [{:keys [property source value]}]
                (when (and (some? value) (not= value provenance-retraction))
                  {:source source :property property :value value})))
        (->> (ctl/list store :entity-provenance {:branch (b store) :entity-id entity-id})
             (group-by (juxt :property :source))
             vals
             (map #(apply max-key :seq %)))))

(defn retract-provenance! [store entity-id source run-id]
  (let [src   (some-> source keyword)
        props (->> (latest-provenance-by-source store entity-id)
                   (filter #(= src (:source %)))
                   (map :property))]
    (write-provenance! store
     (for [p props] {:entity-id entity-id :property p
                        :value provenance-retraction :source source :run-id run-id}))
    (count props)))

