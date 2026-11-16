(ns sutra-ui.forms.edit-modals
  (:require [sutra-ui.state.forms :as fs]
            [sutra-ui.ui.overlay :as overlay]
            [sutra.types.languages :as langs]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra.types.rid :as rid]
            [sutra-ui.editor.code :as ce]
            [sutra-ui.forms.events]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.core :as ui]))

(defn- close! []
  (shell/close-edit-node-modal!))

(defn- text-field [{:keys [label value on-change] :as props}]
  [ui/form-group {:label label}
   [ui/input-group
    (merge {:fill true :value (or value "") :on-change #(on-change (.. % -target -value))}
           (dissoc props :label :value :on-change))]])

(defn- object-type-handover
  [name-kw]
  (js/setTimeout #(do (close!) (object-forms/open-edit-object! name-kw)) 0)
  (fn [_] nil))

(defn- datasource-body [name-kw form-atom frame]
  (let [form @form-atom
        set-f (fn [k v] (fs/edit! form-atom assoc k v))
        type-str (or (:type form) "sqlite")
        csv? (= "csv" type-str)
        ready? (and (seq (some-> (:type form) str/trim))
                    (if csv?
                      (seq (some-> (:path form) str/trim))
                      (seq (some-> (:jdbc-url form) str/trim))))
        submit
        (fn []
          (when ready?
            (let [base (cond-> {:name name-kw
                                :type (keyword type-str)}
                         csv?       (assoc :path (:path form))
                         (not csv?) (assoc :jdbc-url (:jdbc-url form)))
                  cred (when-not (or csv? (= "sqlite" type-str))
                         (into {} (for [k [:user :password] :when (seq (get form k))]
                                    [k (get form k)])))]
              (db/send! [:shared.forms/save-datasource base cred
                         (clojure.core/name name-kw) close!]))))]
    [overlay/modal (assoc frame :ok-label (i18n/t :ui/save) :on-ok submit
                          :ok-disabled? (not ready?))
      [ui/form-group {:label (i18n/t :ui/type)}
       [ui/html-select {:fill      true
                        :value     type-str
                        :on-change #(set-f :type (.. % -target -value))
                        :options   ["sqlite" "postgres" "mysql" "mssql" "csv"]}]]
      (if csv?
        [text-field {:label (i18n/t :ui/ge-path) :value (:path form)
                     :placeholder "/absolute/path/to/file.csv"
                     :on-change #(set-f :path %)}]
        [:<>
         [text-field {:label "JDBC URL" :value (:jdbc-url form) :placeholder "jdbc:…"
                      :class "sutra-mono" :on-change #(set-f :jdbc-url %)}]
         [text-field {:label (i18n/t :ui/ge-user-optional) :value (:user form)
                      :on-change #(set-f :user %)}]
         [text-field {:label (i18n/t :ui/ge-password-optional) :value (:password form)
                      :type "password" :on-change #(set-f :password %)}]])]))

(defn- transform-body [name-kw form-atom frame]
  (let [spec      (get-in @catalog/canonical-schema [:transforms name-kw])
        language  (some-> (:language spec) clojure.core/name)
        form      @form-atom
        set-f     (fn [k v] (fs/edit! form-atom assoc k v))
        sql?      (langs/query-language? (keyword language))
        code?     (langs/code-language? (keyword language))
        compute?  (or sql? code?)
        field-k   (if sql? :query :code)
        body-text (or (get form field-k) "")
        ready?    (and compute? (seq (str/trim (or body-text ""))))
        submit
        (fn []
          (when ready?
            (ontology/mutate!
             :update-transform
             (merge
              {:name name-kw
               :language (keyword language)
               :inputs (:inputs spec)
               :primary-key (:primary-key spec)
               :column-map (:column-map spec)
               :output (:output spec)}
              (when (:outputs spec) {:outputs (:outputs spec)})
              {field-k body-text}))
            (close!)))]
    (if-not compute?
      [overlay/modal frame
       [:div {:class "form-readonly-notice"} (i18n/t :ui/ge-transform-not-compute)]]
      (let [inputs (str/join ", " (map clojure.core/name (keys (:inputs spec))))]
        [overlay/modal (assoc frame :ok-label (i18n/t :ui/save) :on-ok submit
                              :ok-disabled? (not ready?))
          [ui/form-group
           {:label (if sql?
                     (i18n/t :ui/sql-query)
                     (str (clojure.core/name name-kw)
                          (if (= "clojure" language) ".clj" ".py")))
            :helper-text (if sql?
                           (i18n/t :ui/ge-inputs-as-views {:inputs inputs})
                           (i18n/t :ui/ge-inputs-as-keys {:inputs inputs}))}
           ^{:key (str "ce-" language)}
           [ce/code-editor {:value      body-text
                            :on-change  #(set-f field-k %)
                            :language   language
                            :min-height "260px"}]]]))))

(defn- title [kind name-kw]
  (let [n (clojure.core/name name-kw)]
    (case kind
      :datasource (i18n/t :ui/ge-change-datasource {:name n})
      :transform
      (let [language (some-> (get-in @catalog/canonical-schema [:transforms name-kw :language])
                             clojure.core/name)]
        (i18n/t :ui/ge-change-transform
                {:language (cond (langs/query-language? (keyword language)) "SQL"
                                 (langs/code-language? (keyword language))
                                 (str/capitalize language)
                                 :else "")
                 :name n}))
      n)))

(defn- form-defaults-for [kind name-kw]
  (case kind
    :datasource
    (let [spec (get (data/datasource-map) name-kw)]
      {:type     (some-> (:type spec) clojure.core/name)
       :jdbc-url (:jdbc-url spec)
       :path     (:path spec)
       :user     (:user spec)})

    :transform
    (let [spec (get-in @catalog/canonical-schema [:transforms name-kw])]
      {:query (:query spec)
       :code  (:code spec)})
    nil))

(defn edit-node-modal
  []
  (let [form-atom (r/atom nil)
        shown     (atom nil)]
    (fn []
      (when-let [{:keys [kind name] :as m} (:edit-node-modal @shell/ui-state)]
        (reset! shown m)
        (let [section (rid/tag->kinds-key kind)]
          (catalog/hydrate-body! section name)
          (let [tag [kind name (catalog/bodies-here? section name)]]
            (when (not= tag (:tag @form-atom))
              (fs/put! form-atom
                       (assoc (form-defaults-for kind name) :tag tag))))))
      (let [open?               (some? (:edit-node-modal @shell/ui-state))
            {:keys [kind name]} @shown]
        (if (= :object-type kind)
          (when open? ^{:key (str name)} [object-type-handover name])
          (when (and open? kind)
            (let [frame {:on-close close! :title (title kind name)
                         :size     (when (= :transform kind) 720)}]
              (case kind
                :datasource [datasource-body name form-atom frame]
                :transform  [transform-body name form-atom frame]
                [overlay/modal frame
                 (i18n/t :ui/ge-unknown-kind {:kind (pr-str kind)})]))))))))
