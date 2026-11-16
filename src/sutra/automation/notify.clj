(ns sutra.automation.notify
  (:require [sutra.actions.api :as actions]
            [sutra.access.api :as access]
            [sutra.automation.runs :as autos]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.notifications.api :as notifications]
            [sutra.store.api :as api]
            [sutra.types.run-limits :as limits]))

(set! *warn-on-reflection* true)

(defn groups
  [grouping objs]
  (cond
    (= :all grouping) (if (seq objs) [(vec objs)] [])
    (map? grouping)   (let [k #(select-keys (:values %) (:by grouping))]
                        (mapv (group-by k objs) (distinct (map k objs))))
    :else             (mapv vector objs)))

(defn group-values
  [group]
  (assoc (:values (first group)) :count (count group) :objects (mapv :values group)))

(defn- template [{:keys [heading message email-subject email-html]} values]
  (into {} (keep (fn [[k t]] (when t [k (notifications/render t values)])))
        {:heading heading :message message :email-subject email-subject :email-html email-html}))

(defn- link [spec values]
  (when spec
    (let [url (notifications/link-url
               (cond-> (select-keys spec [:app :url])
                 (:object-type spec) (assoc :object [(:object-type spec)
                                                     (notifications/render (:id spec) values)]))
               values)]
      (when url {:link-url url
                 :link-label (some-> (:label spec) (notifications/render values))}))))

(defn- function-content [row]
  (when (map? row)
    {:heading       (or (:heading row) (:subject row) (:title row))
     :message       (or (:message row) (:body row) (:content row))
     :link-url      (or (:link-url row) (:url row))
     :link-label    (or (:link-label row) (:link-text row))
     :email-subject (:email-subject row)
     :email-html    (or (:email-html row) (:email-body row))}))

(defn- viewer?
  [store branch schema rid]
  (let [project (get-in schema [:object-types rid :project])]
    (fn [u]
      (or (grants/auth-off? store)
          (boolean (some-> (access/of-principal store branch u)
                           (grants/read-allowed? project false)))))))

(defn- contents
  [store schema notify values users]
  (if-let [f (get-in notify [:content :function])]
    (into {} (keep (fn [u]
                     (when-let [c (function-content
                                   (first (actions/run-query-function
                                           store schema (keyword f)
                                           (assoc values :recipient u)
                                           {:call-site :automation-notify})))]
                       [u c])))
          users)
    (let [c (template notify values)] (zipmap users (repeat c)))))

(defn- tell-group!
  [store branch schema {:keys [id notify]} rid group]
  (let [values  (group-values group)
        names   (mapcat #(notifications/recipients notify (:values %)) group)
        n       {:objects (mapv #(vector rid (str (:id %))) group)
                 :allowed? (viewer? store branch schema rid)}
        {:keys [allowed denied]} (notifications/admitted store branch names n)
        cap     (get notifications/max-recipients
                     (if (get-in notify [:content :function]) :function :template))
        total   (+ (count allowed) (count denied))]
    (if (> total cap)
      {:error (str total " recipients, more than the " cap " one notification may have — "
                   "none was sent about " (mapv :id group))}
      (let [base   (cond-> {:source :automation :automation-id id :branch branch
                            :object-type rid}
                     (= 1 (count group)) (assoc :object-id (str (:id (first group))))
                     (:link notify)      (merge (link (:link notify) values)))
            ds     (for [[u c] (contents store schema notify values allowed)
                         :when (seq (:heading c))]
                     (assoc (into {} (remove (comp nil? val)) c) :recipient u))
            rows   (notifications/deliver!
                    store (notifications/plan-each base ds
                                                   (notifications/settings-of store allowed))
                    nil)]
        {:sent (if (seq rows) (mapv (comp str :id) group) [])}))))

(defn previews
  [{:keys [notify]} matched]
  (vec (for [g (groups (:grouping notify) matched)]
         {:object  (str (:id (first g)))
          :objects (count g)
          :heading (if-let [f (get-in notify [:content :function])]
                     (str "(Inhalt aus Funktion " (name f) ", je Empfänger)")
                     (notifications/render (:heading notify) (group-values g)))})))

(defn notify!
  [store branch {:keys [id last-fired-at] :as auto} {:keys [notify]} schema on matched run-id]
  (let [rid   (lookup/rid-of schema :object-types (keyword (name on)))
        seen  (into #{} (map first) (autos/applied-versions store branch id))
        fresh (if (= :changed (:objects notify))
                matched
                (filterv #(not (seen (str (:id %)))) matched))
        cap   (:scheduled-per-object-max limits/automate)
        mark! #(autos/mark-versions! store branch id run-id
                                     (into {} (map (fn [o] [(str (:id o)) (api/object-version o)]))
                                           matched))]
    (cond
      (nil? last-fired-at)
      (do (mark!) {:affected [] :errors [] :proposed 0 :skipped {:baseline (count matched)}})

      (> (count fresh) cap)
      (do (mark!)
          {:affected [] :proposed 0
           :errors [(str (count fresh) " new objects, more than the " cap
                         " one run notifies about — none was sent")]})

      :else
      (let [told (mapv #(tell-group! store branch schema (assoc auto :notify notify) rid %)
                       (groups (:grouping notify) fresh))]
        (mark!)
        {:affected (into [] (mapcat :sent) told)
         :errors   (into [] (keep :error) told)
         :proposed 0}))))
