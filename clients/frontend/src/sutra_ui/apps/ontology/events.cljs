(ns sutra-ui.apps.ontology.events
  (:require [cljs.reader]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra.types.languages :as langs]
            [sutra.types.scaffold :as scaffold]
            [sutra-ui.api :as api]
            [sutra-ui.forms.base :as forms]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]
            [sutra-ui.state.sync :as sync]))

(defn- mutate [action args] [:invoke [ontology/mutate! action args]])

(defn function-spec [{:keys [name target-type language source param-types]}]
  (assoc (scaffold/edit-function
          {:target-type target-type :language language :source source
           :param-types (when (seq param-types)
                          (into {} (for [{:keys [role type]} param-types
                                         :when (and (seq role) (seq type))]
                                     [(keyword role) (keyword type)])))})
         :name (keyword name)))

(defn- transform-spec
  [{:keys [name language code inputs output-type primary-key column-map]}]
  (let [lang (or language "python")]
    (cond-> {:name (keyword name) :language (keyword lang)}
      true              (assoc (langs/body-field (keyword lang)) code)
      (seq inputs)      (assoc :inputs inputs)
      (seq output-type) (assoc :output {:kind :object-type
                                        :object-type (keyword output-type)})
      (seq primary-key) (assoc :primary-key (keyword primary-key))
      (seq column-map)  (assoc :column-map column-map))))

(defn- shared-property-spec [form]
  (cond-> {:name (keyword (str/trim (str (:name form))))
           :type (keyword (or (not-empty (:type form)) "string"))}
    (seq (:value-type form)) (assoc :value-type (keyword (:value-type form)))))

(defn- run-function! [function-name inputs]
  (shell/with-loading
    (api/post! :function-run {:function-name (name function-name) :inputs inputs})))

(defn- install! [id preview?]
  (-> (api/install-package id {:preview? preview?})
      (.catch (fn [e]
                (throw (str (.-message e) " " (some-> (ex-data e) :body :data pr-str)))))))

(defn- stage-form!
  [form-atom old-kw]
  (or (object-forms/save-object-form! form-atom old-kw) (js/Promise.resolve ::invalid)))

(defn- stage-edits! [k dirty?]
  (if dirty? (stage-form! forms/edit-object-form k) (js/Promise.resolve nil)))

(defn- clear-staged! []
  (when (seq (staging/staged-list)) (sync/clear-staged!)))

(defn- preview-backing! [obj-kw]
  (when-let [id (some (fn [[k v]]
                        (when (and (= :object-backing (:kind v)) (= obj-kw (:dataset-name v)))
                          k))
                      (rd/value-or @data/datasets {}))]
    (data/load-dataset-preview! id)))

(defn- run-input [s]
  (let [s (str/trim (str s))]
    (cond
      (re-matches #"(?i)[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" s) (uuid s)
      (str/starts-with? s ":") (keyword (subs s 1))
      :else s)))

(defn run-inputs
  [{:keys [values inputs]}]
  (if (str/blank? inputs)
    (into {} (for [[role v] values :when (seq (str/trim (str v)))]
               [(keyword role) (run-input v)]))
    (cljs.reader/read-string inputs)))

(machine/reg-fx :ontology/load-object-form
                (fn [{f :frame} [k known?]]
                  (object-forms/load-object-form! k)
                  (machine/dispatch [:ontology.otv/loaded k @forms/edit-object-form known?]
                                    {:frame f})))

(machine/reg-event :ontology/fx (fn [_ [_ fx]] {:fx fx}))

(machine/reg-event :ontology/edit
                   (fn [{:keys [db]} [_ path f & args]]
                     {:db (if (seq path) (apply update-in db path f args) (apply f db args))}))

(machine/reg-event :ontology/confirm
                   (fn [_ [_ ask fx]] {:fx [[:confirm {:ask ask :ok [:ontology/fx fx]}]]}))

(machine/reg-event :ontology/open-application
                   (fn [_ [_ app-kw]]
                     {:fx [[:invoke [catalog/hydrate-body! :applications app-kw]]
                           [:invoke [shell/enter! :applications]]]}))

(machine/reg-event :ontology/mutate (fn [_ [_ action args]] {:fx [(mutate action args)]}))

(machine/reg-event :ontology.function/create
                   (fn [_ [_ form]] {:fx [(mutate :create-function (function-spec form))]}))

(machine/reg-event :ontology.function/update
                   (fn [{:keys [db]} [_ old-name form then]]
                     (rq/request db {:ask [ontology/rename-or-update! (keyword old-name)
                                           (function-spec form) :update-function
                                           (partial api/rename :function)]
                                     :fx  [[:invoke [then]]]})))

(machine/reg-event :ontology.function/delete
                   (fn [{:keys [db]} [_ k then]]
                     (rq/request db {:ask [ontology/mutate! :delete-function {:name k}]
                                     :fx  [[:invoke [then]]]})))

(machine/reg-event :ontology.interface/update
                   (fn [_ [_ old-kw form]]
                     {:fx [[:invoke [ontology/rename-or-update! old-kw
                                     (ontology/->interface-spec form) :update-interface
                                     (partial api/rename :interface)]]]}))

(machine/reg-event :ontology.group/update
                   (fn [_ [_ g-kw form]]
                     {:fx [[:invoke [ontology/update-object-type-group! g-kw form]]]}))

(machine/reg-event :ontology.link/delete
                   (fn [_ [_ conn-id]] {:fx [[:invoke [ontology/delete-link-by-id! conn-id]]]}))

(machine/reg-event :ontology.transform/create
                   (fn [_ [_ m]] {:fx [(mutate :create-transform (transform-spec m))]}))

(machine/reg-event :ontology.shared-property/create
                   (fn [_ [_ form]]
                     {:fx [(mutate :create-shared-property (shared-property-spec form))]}))

(machine/reg-event :ontology.run/value
                   (fn [{:keys [db]} [_ role type-name x]]
                     {:db (assoc-in db [:fn-run :values role] x)
                      :fx [(if (>= (count (str/trim x)) 2)
                             [:after {:key [:run-hits role] :ms 250
                                      :event [:ontology.run/search role type-name
                                              (str/trim x)]}]
                             [:cancel [:run-hits role]])]}))

(machine/reg-event :ontology.run/search
                   (fn [{:keys [db]} [_ role type-name q]]
                     (rq/request db {:ask  [api/post! :ontology-query
                                            {:find type-name :keywords q :limit 20}]
                                     :path [:fn-run :hits role] :pick #(vec (:rows %))
                                     :else [[:rf/set [:fn-run :hits role] []]]})))

(machine/reg-event :ontology.run/inputs
                   (fn [{:keys [db]} [_ s]] {:db (assoc-in db [:fn-run :inputs] s)}))

(machine/reg-event :ontology.run/start
                   (fn [{:keys [db]} [_ fn-name]]
                     (let [db (assoc-in db [:fn-run :error] nil)]
                       (try
                         (rq/request db {:ask   [run-function! fn-name (run-inputs (:fn-run db))]
                                         :path  [:fn-run :result] :error [:fn-run :error]
                                         :fx    [[:invoke [sync/refresh-snapshot!]]]})
                         (catch :default e
                           {:db (assoc-in db [:fn-run :error]
                                          (i18n/t :ui/parse-error {:error e}))})))))

(machine/reg-event :ontology.packages/enter
                   (fn [{:keys [db]} _]
                     {:db (assoc db :package {}) :fx [[:dispatch [:ontology.packages/load]]]}))

(machine/reg-event :ontology.packages/load
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask  [api/get! :packages] :path [:packages]
                                     :pick #(vec (:packages %)) :report "packages"})))

(defn- install [db id preview?]
  (rq/request (assoc-in db [:package :error] nil)
              (cond-> {:ask   [install! id preview?]
                       :path  [:package (if preview? :preview :staged) id]
                       :busy  [[:package :busy] id] :error [:package :error]}
                (not preview?) (assoc :fx   [[:invoke [sync/refresh-after-staging-change!]]]
                                      :then [[:ontology.packages/load]]))))

(machine/reg-event :ontology.package/preview
                   (fn [{:keys [db]} [_ id]] (install db id true)))

(machine/reg-event :ontology.package/install
                   (fn [{:keys [db]} [_ id]] (install db id false)))

(machine/reg-event :ontology.package/commit
                   (fn [{:keys [db]} [_ id]]
                     (rq/request db {:ask   [sync/commit-staged!] :error [:package :error]
                                     :then  [[:ontology.package/committed id]]})))

(machine/reg-event :ontology.package/committed
                   (fn [{:keys [db]} [_ id]]
                     {:db (update-in db [:package :staged] dissoc id)
                      :fx [[:dispatch [:ontology.packages/load]]]}))

(machine/reg-event :ontology.usage/load
                   (fn [{:keys [db]} [_ k]]
                     (rq/request (assoc-in db [:usage k] nil)
                                 {:ask  [api/get! :object-type-usage {:type (name k)}]
                                  :path [:usage k] :else [[:rf/set [:usage k] :failed]]})))

(machine/reg-event :ontology.dataset/open-backing
                   (fn [{:keys [db]} [_ k]]
                     (rq/request (assoc db :section :datasets :detail nil)
                                 {:ask [data/refresh-datasets!]
                                  :fx  [[:invoke [preview-backing! k]]]})))

(machine/reg-event :ontology.otv/load
                   (fn [_ [_ k known?]] {:fx [[:ontology/load-object-form [k known?]]]}))

(machine/reg-event :ontology.otv/loaded
                   (fn [{:keys [db]} [_ k form known?]]
                     {:db (-> db
                              (assoc :otv-baseline form)
                              (update :otv assoc :type k :sel #{} :checked #{} :review? false
                                      :menu? false)
                              (cond-> (some? known?) (assoc-in [:otv :known?] known?)))}))

(machine/reg-event :ontology.otv/review
                   (fn [{:keys [db]} _]
                     {:db (update db :otv assoc :review? true :review-tab :all :confirm-text ""
                                  :save-error nil)
                      :fx [[:invoke [staging/refresh-staged!]]]}))

(defn- save-step [ask then]
  {:ask ask :error [:otv :save-error] :then then})

(machine/reg-event :ontology.otv/save
                   (fn [{:keys [db]} [_ k dirty?]]
                     (rq/request db (save-step [stage-edits! k dirty?]
                                               [[:ontology.otv/staged k]]))))

(machine/reg-event :ontology.otv/staged
                   (fn [{:keys [db]} [_ k r]]
                     (if (= ::invalid r)
                       {:db (assoc-in db [:otv :review-tab] :errors)}
                       (rq/request db (save-step [sync/commit-staged!]
                                                 [[:rf/set [:otv :review?] false]
                                                  [:rf/set [:otv :save-error] nil]
                                                  (fn [_] [:ontology.otv/load k])])))))

(machine/reg-event :ontology.otv/discard
                   (fn [_ [_ k ask]]
                     {:fx [[:confirm {:ask ask :ok [:ontology.otv/discarded k]}]]}))

(machine/reg-event :ontology.otv/discarded
                   (fn [_ [_ k]]
                     {:fx [[:dispatch [:ontology.otv/load k]] [:invoke [clear-staged!]]]}))

(machine/reg-event :ontology.otv/delete
                   (fn [_ [_ k ask on-close]]
                     {:fx [[:confirm {:ask ask :ok [:ontology.otv/deleted k on-close]}]]}))

(machine/reg-event :ontology.otv/deleted
                   (fn [{:keys [db]} [_ k on-close]]
                     {:db (dissoc db :otv-baseline)
                      :fx [[:invoke [ontology/delete-object-type! k]] [:invoke [on-close]]]}))

(machine/reg-event :ontology.staged/discard
                   (fn [_ [_ n]] {:fx [[:invoke [sync/discard-staged! n]]]}))

(machine/reg-event :ontology.otv/create
                   (fn [{:keys [db]} [_ m actions]]
                     (if (str/blank? (str (:title m)))
                       {:db (assoc-in db [:otv-wizard :error] (i18n/t :ui/otv-err-title))}
                       (rq/request db {:ask   [stage-form! forms/new-object-form nil]
                                       :error [:otv-wizard :error]
                                       :then  [[:ontology.otv/created
                                                (keyword (str/trim (:name m))) actions]]}))))

(machine/reg-event :ontology.otv/created
                   (fn [{:keys [db]} [_ k actions r]]
                     (when-not (= ::invalid r)
                       (let [groups (get-in db [:otv-wizard :groups])]
                         {:db (-> db (dissoc :otv-wizard) (assoc-in [:otv :page] :overview))
                          :fx (-> (if (seq groups)
                                    [[:invoke [ontology/set-object-type-groups! k groups]]]
                                    [])
                                  (into (map #(mutate :create-action %)) actions)
                                  (conj [:invoke [object-forms/open-object-detail! k]]))}))))
