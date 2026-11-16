(ns sutra-ui.builder.bottom-panel.events
  (:require [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.lineage-status :as lstatus]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.sync :as sync]))

(def renameable-kinds #{:object-type :dataset :transform})

(machine/reg-sub :shared.panel/at (fn [s [_ path]] (get-in s path)))

(defn at
  [& path]
  @(machine/subscribe [:shared.panel/at (into [:shared.panel] path)] {:frame db/sutra}))

(defn- request [c ok prefix] {:ask c :then [ok] :report prefix})

(def ^:private resnap [:invoke [sync/refresh-snapshot!]])

(defn- health-path [kind key] [:shared.panel :health [kind key]])

(defn- of-node [kind key]
  (fn [{:keys [checks]}]
    (filterv #(and (= (name kind) (:node-kind %)) (= (name key) (:node-key %))) checks)))

(db/reg :shared.panel/health
        (fn [s [_ kind key fresh?]]
          {:db (cond-> s fresh? (assoc-in (health-path kind key) rd/loading))
           :fx [(db/load (health-path kind key) [api/get! :health-checks]
                         {:pick (of-node kind key) :report "health-checks"})]}))

(db/reg :shared.panel/add-check
        (fn [s [_ kind key check]]
          (rq/request s (request [api/post! :health-checks
                                  (assoc check :node-kind kind :node-key key :kind "row-count")]
                                 [:shared.panel/check-added kind key] "health-check"))))

(db/reg :shared.panel/check-added
        (fn [_ [_ kind key c]] {:fx [[:dispatch [:shared.panel/run-check kind key (:id c)]]]}))

(db/reg :shared.panel/run-check
        (fn [s [_ kind key id]]
          (rq/request s (request [api/post! :health-check-run {:id id} nil]
                                 [:shared.panel/checks-changed kind key 150] "health-check"))))

(db/reg :shared.panel/checks-changed
        (fn [_ [_ kind key ms]]
          {:fx [[:after {:key [:shared.panel/health kind key] :ms ms
                         :event [:shared.panel/health kind key]}]
                [:invoke [lstatus/refresh-lineage-status!]]]}))

(db/reg :shared.panel/delete-check
        (fn [_ [_ kind key {:keys [id name]}]]
          {:fx [[:confirm {:ask {:body    (i18n/t :ui/health-check-delete-confirm {:name name})
                                 :danger? true}
                           :ok  [:rf/request
                                 (request [api/delete! :health-check-of {:id id}]
                                          [:shared.panel/checks-changed kind key 0]
                                          "health-check")]}]]}))

(db/reg :shared.panel/edits
        (fn [s [_ type-kw]]
          {:db (assoc-in s [:shared.panel :edits type-kw] rd/loading)
           :fx [(db/load [:shared.panel :edits type-kw] [api/edit-history (name type-kw)]
                         {:pick #(or % {})})]}))

(db/reg :shared.panel/rename
        (fn [s [_ kind kw v]]
          (rq/request s {:ask    [api/rename kind (name kw) v]
                         :fx     [[:invoke [canvas/carry-node-position! kind kw v]]
                                  resnap
                                  [:invoke [panel/open-bottom-panel! kind (keyword v)]]]
                         :report #(i18n/t :ui/rename-error {:error %})})))

(def ^:private wb [:shared.panel :writeback])

(db/reg :shared.panel/writeback
        (fn [s [_ object-type object-id]]
          (rq/request (assoc-in s wb {:for object-id :preview rd/loading})
                      {:ask     [api/post! :writeback-preview
                                 {:object-type object-type :object-id object-id}]
                       :path    (conj wb :preview) :rd? true :pick #(or % {})
                       :current [(conj wb :for) object-id]})))

(db/reg :shared.panel/writeback-failed
        (fn [s [_ sentence]] {:db (assoc-in s (conj wb :preview) (rd/failed sentence))}))

(db/reg :shared.panel/writeback-force
        (fn [s [_ v]] {:db (assoc-in s (conj wb :force?) v)}))

(db/reg :shared.panel/writeback-confirm
        (fn [s _]
          (let [{p :preview force? :force? id :for} (get-in s wb)]
            (rq/request s {:ask     [api/post! :writeback-confirm
                                     {:object-type (:object-type p) :object-id (:object-id p)
                                      :force? (boolean force?) :seen (:seen p)}]
                           :busy    (conj wb :sending?)
                           :path    (conj wb :done) :pick #(or % {:pushed true})
                           :current [(conj wb :for) id]
                           :fx      [resnap]
                           :else    [[:shared.panel/writeback-failed]]}))))

(db/reg :shared.panel/writeback-left (fn [s _] {:db (update s :shared.panel dissoc :writeback)}))

(db/reg :shared.panel/rollback
        (fn [_ [_ {:keys [kind node-kw commit label]}]]
          {:fx [[:confirm {:ask {:body          (i18n/t :ui/rollback-confirm
                                                        {:node   (name node-kw)
                                                         :commit (subs (str commit) 0 7)})
                                 :danger?       true
                                 :confirm-label (or label (i18n/t :ui/rollback-to-this))}
                           :ok  [:rf/request
                                 (request [api/post! :lineage-rollback
                                           {:node-kind kind :node-key node-kw :commit commit}]
                                          [:shared.panel/changed nil] "rollback")]}]]}))

(db/reg :shared.panel/force-snapshot
        (fn [_ [_ d-kw]]
          {:fx [[:confirm {:ask {:body          (i18n/t :ui/force-snapshot-confirm
                                                        {:name (name d-kw)})
                                 :danger?       true
                                 :confirm-label (i18n/t :ui/force-snapshot)}
                           :ok  [:rf/request
                                 (request [api/post! :dataset-write {:name (name d-kw)}
                                           {:rows [] :mode :snapshot
                                            :message (str "force-snapshot " (name d-kw))}]
                                          [:shared.panel/changed d-kw] "force-snapshot")]}]]}))

(db/reg :shared.panel/changed
        (fn [_ [_ d-kw]]
          {:fx [(when (keyword? d-kw) [:invoke [panel/load-panel! :transactions d-kw]])
                resnap]}))
