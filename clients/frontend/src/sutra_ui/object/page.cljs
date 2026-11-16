(ns sutra-ui.object.page
  (:require [clojure.string :as str]
            [sutra.capps.catalog :as cat]
            [sutra.capps.intent :as ci]
            [sutra.types.submission :as csub]
            [sutra-ui.api :as api]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.object-view :as object-view]))

(defn- action-entries
  [view facts]
  (let [own    (or (seq (:actions view))
                   (map (fn [a] {:action a}) (cat/used-actions view)))
        seen   (into #{} (map #(some-> (:action %) name keyword)) own)
        nested (->> (vals (:object-views facts))
                    (mapcat vals)
                    (mapcat cat/used-actions)
                    (remove seen)
                    distinct
                    (map (fn [a] {:action a})))]
    (concat own nested)))

(defn- action-descriptors
  [schema entries]
  (reduce
   (fn [acc entry]
     (let [nm     (some-> (:action entry) name keyword)
           act    (csub/with-section-overrides (get-in schema [:actions nm]))
           ps     (:params act)
           chosen (some->> (:parameters entry) (mapv #(some-> % name keyword)))
           params (when (seq ps) (csub/action-param-descriptors ps chosen))
           intent (some-> (get-in schema [:actions nm]) ci/action-intent)]
       (cond-> acc
         params (assoc-in [:action-params nm] (csub/with-param-formats params schema act))
         params (assoc-in [:action-forms nm] (csub/form-sections (:form act) params))
         intent (assoc-in [:action-intents nm] intent))))
   {:action-params {} :action-forms {} :action-intents {}}
   entries))

(defn evaluate-action-params!
  ([action-name args] (evaluate-action-params! action-name args nil))
  ([action-name args search]
   (api/post! :action-params nil (cond-> {:action (keyword action-name) :args args}
                                   (seq search) (assoc :search search)))))

(defn- freshness-read! [items] (api/data-freshness items))

(defn- history-page [res]
  (-> (select-keys res [:total :next-page-token :timeline])
      (assoc :history (mapv (fn [e] {:pk             (:pk e)
                                     :updated_at     (:edited-at e)
                                     :edit_kind      (:edit-kind e)
                                     :edited_by      (:edited-by e)
                                     :edited_by_name (:edited-by-name e)
                                     :action         (:action e)
                                     :values         (:edited-values e)
                                     :previous       (:previous-values e)})
                            (:history res)))))

(defn- history-read!
  [_descriptor {:keys [type props kinds editors] :as spec} & _]
  (.then (api/edit-history type (cond-> (select-keys spec [:limit :pk :token :sort :timeline])
                                  (seq props)   (assoc :props (str/join "," props))
                                  (seq kinds)   (assoc :kinds (str/join "," kinds))
                                  (seq editors) (assoc :editors (str/join "," editors))))
         history-page))

(defn around-read!
  [_descriptor {:keys [type id standard vars]} & _]
  (.then (api/post! :object-view-standard (cond-> {:type type :id id}
                                            standard   (assoc :standard true)
                                            (seq vars) (assoc :vars vars)))
         #(update % :objects instances/objects-by-name)))

(defn- evaluate!
  [app]
  (fn [act values search]
    (if-let [rid (:id app)]
      (client/evaluate-params! {:id rid} act values {:search search})
      (evaluate-action-params! act values search))))

(defn- refresh-read!
  [object-type id]
  (fn [_descriptor {:keys [vars]}]
    (object-view/ask-object-view! object-type id vars)))

(defn- reload!
  [object-type id]
  (fn []
    (.then (object-view/refresh-object-view-data! object-type id)
           (fn [_] (get-in @db/db [:object-view :object-view-data [object-type (str id)]
                                    :data])))))

(defn page-descriptor
  [schema object-type id entry app view]
  (-> (merge (:facts entry) (dissoc app :layout))
      (dissoc :standard-view :object-page :inputs)
      (merge (action-descriptors schema (action-entries view (:facts entry))))
      (assoc :layout          view
             :data-inputs     (:inputs (:facts entry))
             :refresh-fn      (refresh-read! object-type id)
             :object          (:instance entry)
             :edit-history-fn history-read!
             :data-freshness-fn freshness-read!
             :around-fn       around-read!
             :evaluate-fn     (evaluate! app)
             :reload          (reload! object-type id))))
