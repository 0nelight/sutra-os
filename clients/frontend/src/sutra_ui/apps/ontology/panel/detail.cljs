(ns sutra-ui.apps.ontology.panel.detail
  (:require [sutra-ui.ui.core :as ui :refer [card] :rename {card bp-card}]
            [sutra-ui.ui.page :as page]
            [clojure.string :as str]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.forms.base :as forms]
            [sutra-ui.forms.external :as ext-forms]
            [sutra-ui.forms.group :as group-forms]
            [sutra-ui.forms.interface :as iface-forms]
            [sutra-ui.forms.link :as link-forms]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.forms.link-type :as ltf]
            [sutra-ui.object.card :as oc]
            [sutra-ui.health.badge :as hb]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.apps.ontology.ot-view :as ot-view]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.ontology.panel.actions :refer [action-wizard empty-wizard]]
            [sutra-ui.apps.ontology.panel.common :refer [confirm-delete!]]))

(def ^:private detail-kind->kinds-key
  {:object-type :object-types :interface :interfaces
   :object-type-group :object-type-groups
   :link-type :link-types :function :functions
   :transform :transforms})

(defn- rid-line
  [rid]
  (when rid
    [:div {:class "ontology-detail__rid-line"}
     [:code {:class "ontology-detail__rid-code"}
      rid]
     [ui/tooltip {:content (i18n/t :ui/copy-rid)}
      [ui/button
       {:minimal true :small true :icon :duplicate
        :on-click #(some-> js/navigator .-clipboard (.writeText (str rid)))} (i18n/t :ui/copy)]]]))

(defn- detail-header
  [{:keys [kind on-back kind-label entity-label api-name rid badge
           on-save on-delete dirty? save-disabled?]}]
  (let [{:keys [icon section-label]} (kind forms/entity-views)
        {:keys [glyph bg fg]} icon]
    [page/header
     {:icon     [:span {:class "ontology-detail__icon" :style {:background bg :color fg}} glyph]
      :title    [:<> entity-label
                 (when (and (seq api-name) (not= api-name entity-label))
                   [ui/tooltip {:content (i18n/t :ui/api-name)}
                    [:span {:class "u-mono-12 u-ml-6 u-muted"} api-name]])]
      :subtitle [:<> kind-label (rid-line rid) badge]}
     (when on-back
       [ui/tooltip
        {:content (i18n/t :ui/back-to {:section (some-> section-label clojure.core/name)})}
        [ui/button {:minimal true :icon :arrow-left :on-click on-back} (i18n/t :ui/back)]])
     (when on-delete
       [ui/button {:intent :danger :outlined true :on-click on-delete} (i18n/t :ui/delete)])
     (when on-save
       [ui/button {:intent :primary :disabled save-disabled? :on-click on-save}
        (i18n/t (if dirty? :ui/save-changes :ui/save))])]))

(defn- key-value-row [k v]
  [:div {:class "ontology-detail__kv-row"}
   [:div {:class "ontology-detail__kv-key"}
    k]
   [:div {:class "ontology-detail__kv-value"}
    v]])

(defn card
  [body]
  [bp-card {:class "ontology-detail__card"} body])

(defn card-eyebrow [label]
  [:div {:class "ontology-detail__card-eyebrow"}
   label])

(defn entity-detail-shell
  [{:keys [kind entity-label api-name form-component on-save on-delete on-close]}
   & extra-cards]
  (let [api-name   (or api-name (str entity-label))
        kind-label (i18n/t (-> kind forms/entity-views :label))
        rid        (catalog/rid-of (detail-kind->kinds-key kind) (keyword api-name))]
    [:div {:class "ontology-detail"}
     [detail-header {:kind kind :on-back on-close
                     :kind-label kind-label :entity-label entity-label :api-name api-name :rid rid
                     :on-save on-save :on-delete on-delete
                     :badge (when (= :object-type kind)
                              (let [k (keyword api-name)]
                                [hb/health-badge :object-type k
                                 #(do (feature/command! :open-in-lineage [:object-type k])
                                      (panel/open-bottom-panel! :object-type k)
                                      (panel/set-panel-tab! :health))]))}]
     (into [:div {:class "ontology-detail__body"}
            [card [:div [card-eyebrow (i18n/t :ui/definition)] form-component]]]
           (filter some? extra-cards))]))

(defn- object-functions
  [obj-kw functions]
  (sort-by key
           (filter (fn [[_ spec]]
                     (= (some-> (:target-type spec) keyword) obj-kw))
                   functions)))

(defn- linked-row
  [{:keys [on-click primary secondary]}]
  [:div {:on-click on-click
         :class "ontology-detail__linked-row"}
   [:span {:class "u-bold"} primary]
   [:span {:class "ontology-detail__linked-sub"} secondary]])

(defn- actions-on-type
  [obj-kw]
  (let [rid   (catalog/rid-of :object-types obj-kw)
        hits? (fn [spec]
                (some #(or (and rid (= rid %)) (= obj-kw %)
                           (and (string? %) (= obj-kw (keyword %))))
                      (concat (tree-seq coll? seq (select-keys spec [:rules :edits]))
                              (map :type (vals (:params spec))))))]
    (sort-by (fn [[k spec]] (str/lower-case (or (:display-name spec) (name k))))
             (filter (comp hits? val) (catalog/declared-actions)))))

(defn function-label [k spec]
  (or (not-empty (:display-name spec))
      (some-> (:function-name spec) clojure.core/name)
      (some-> (catalog/name-of k) clojure.core/name)
      (clojure.core/name k)))

(defn- function-row-data
  [wf-name spec]
  {:name        (name wf-name)
   :target-type (some-> (:target-type spec) clojure.core/name)
   :language    (some-> (:language spec) clojure.core/name)
   :source      (:source spec)
   :param-types (vec (for [[r t] (:param-types spec)]
                       {:role (clojure.core/name r)
                        :type (clojure.core/name t)}))})

(defn open-function-detail! [wf-name spec]
  (fs/set-form! :edit-function (function-row-data wf-name spec))
  (kompass/note-recent! {:kind :function :id wf-name :label (function-label wf-name spec)})
  (ont-model/edit! [] assoc
         :section :functions
         :detail {:kind :function :id wf-name}))

(defn open-transform!
  [k]
  (feature/command! :open-in-lineage [:transform k])
  (panel/open-bottom-panel! :transform k))

(defn- object-dependents-card
  [obj-kw]
  (let [impls  (get-in @catalog/schema [:object-types obj-kw :implements])
        xforms (->> (:transforms @catalog/canonical-schema)
                    (filter (fn [[_ spec]]
                              (= obj-kw (some-> (get-in spec [:output :object-type])
                                                catalog/name-of))))
                    (sort-by key))
        wfs    (object-functions obj-kw (:functions @catalog/canonical-schema))]
    (when (or (seq impls) (seq xforms) (seq wfs))
      [card
       [:div
        [card-eyebrow (i18n/t :ui/dependents)]
        (when (seq impls)
          [:div {:class "u-mb-8"}
           [:div {:class "ontology-detail__group-label"}
            (i18n/t :ui/implements-interfaces)]
           (for [k impls]
             ^{:key k}
             [linked-row
              {:on-click  #(iface-forms/open-interface-detail! k)
               :primary   (or (not-empty (get-in @catalog/schema [:interfaces k :display-name]))
                              (clojure.core/name k))
               :secondary (i18n/t :ui/kind-interface)}])])
        (when (seq xforms)
          [:div {:class "u-mb-8"}
           [:div {:class "ontology-detail__group-label"}
            (i18n/t :ui/produced-by-transforms)]
           (for [[k spec] xforms]
             ^{:key k}
             [linked-row {:on-click  #(open-transform! k)
                          :primary   (clojure.core/name k)
                          :secondary (or (some-> (:language spec) clojure.core/name)
                                         (i18n/t :ui/kind-transform))}])])
        (when (seq wfs)
          [:div
           [:div {:class "ontology-detail__group-label"}
            (i18n/t :ui/functions-targeting {:name (oc/type-label obj-kw)})]
           (for [[wf-name spec] wfs]
             ^{:key wf-name}
             [linked-row
              {:on-click  #(open-function-detail! wf-name spec)
               :primary   (function-label wf-name spec)
               :secondary (str (some-> (:language spec) clojure.core/name)
                               (when (seq (:param-types spec))
                                 (i18n/t :ui/params-suffix
                                         {:n (count (:param-types spec))})))}])])]])))

(defn- usage-view
  [obj-kw]
  (let [u (get-in @ont-model/ui [:usage obj-kw])]
    [card
     [:div
      [card-eyebrow (i18n/t :ui/om-usage)]
      (cond
        (nil? u)       [busy/block {:class "ontology-entityrow__sub"} (i18n/t :ui/loading)]
        (= :failed u)  [:div {:class "ontology-entityrow__sub"} (i18n/t :ui/om-usage-failed)]
        :else
        [:div
         [key-value-row (i18n/t :ui/om-usage-writes {:days (:window-days u)}) (:writes u)]
         (for [{:keys [edited-by writes last-at]} (:editors u)]
           ^{:key (str edited-by)}
           [linked-row
            {:primary   (principals/principal-label edited-by)
             :secondary (i18n/t :ui/om-usage-editor
                                {:n writes
                                 :last
                                 (subs (str last-at) 0 (min 16 (count (str last-at))))})}])
         [:div {:class "ontology-entityrow__sub u-mb-8"}
          (i18n/t :ui/om-usage-reads-not-counted)]
         (when (seq (:applications u))
           [:div {:class "u-mb-8"}
            [:div {:class "ontology-detail__group-label"} (i18n/t :ui/om-usage-apps)]
            (for [{:keys [id name display-name status]} (:applications u)]
              ^{:key (str id)}
              [linked-row {:on-click  #(ont-model/open-application! (keyword name))
                           :primary   (or (not-empty display-name) name)
                           :secondary status}])])
         (when (seq (:object-views u))
           [:div {:class "u-mb-8"}
            [:div {:class "ontology-detail__group-label"} (i18n/t :ui/om-usage-views)]
            (for [{:keys [id name form-factor]} (:object-views u)]
              ^{:key (str id)}
              [linked-row {:primary name :secondary form-factor}])])
         (when (seq (:workspaces u))
           [:div
            [:div {:class "ontology-detail__group-label"} (i18n/t :ui/om-usage-workspaces)]
            (for [{:keys [id name]} (:workspaces u)]
              ^{:key (str id)}
              [linked-row {:primary name}])])])]]))

(def ^:private usage-card
  (machine/entering ont-model/ontology (fn [k] [:ontology.usage/load k]) usage-view))

(defn- open-backing-dataset! [obj-kw]
  (ont-model/send! [:ontology.dataset/open-backing obj-kw]))

(defn object-type-view [obj-kw on-close]
  [ot-view/object-type-view obj-kw
   {:on-close on-close
    :actions actions-on-type
    :usage (fn [k] ^{:key k} [usage-card k])
    :dependents object-dependents-card
    :view-dataset! open-backing-dataset!
    :new-action! (fn [k]
                   (fs/put! action-wizard (assoc empty-wizard :object-type (name k)))
                   (ont-model/edit! [] assoc
                          :section :action-types
                          :detail nil
                          :editor-open? :wizard))
    :new-link! (fn [k]
                 (fs/reset-form! :new-link)
                 (fs/edit! forms/new-link-form assoc :from (name k))
                 (shell/show-panel! :add-link))}])

(defn- link-by-id [conn-id]
  (some #(when (= (catalog/link-id %) conn-id) %)
        (:links @catalog/schema)))

(defn- link-datasources-card
  [conn]
  (let [canon @catalog/canonical-schema
        b     (ltf/backing-of canon (:name conn))
        es    (:external-source conn)
        [fk-t pk-t] (case (:fk-side b)
                      :from [(:from conn) (:to conn)]
                      :to   [(:to conn) (:from conn)]
                      nil)]
    [card
     [:div
      [card-eyebrow (i18n/t :ui/datasources)]
      (cond
        (= :fk (:kind b))
        [:div
         [key-value-row (i18n/t :ui/foreign-key)
          (str (clojure.core/name fk-t) "." (clojure.core/name (:fk-attribute b)))]
         [key-value-row (i18n/t :ui/primary-key)
          (if-let [pk (ltf/primary-key-of canon pk-t)]
            (str (clojure.core/name pk-t) "." (clojure.core/name pk))
            (str (clojure.core/name pk-t) " · " (i18n/t :ui/link-pk-undeclared)))]
         [key-value-row (i18n/t :ui/link-producer) (clojure.core/name (:producer b))]]

        (and b (nil? es))
        [key-value-row (i18n/t :ui/link-producer) (clojure.core/name (:producer b))]

        (nil? es)
        [:div {:class "ontology-detail__note"}
         (i18n/t :ui/link-in-system)]

        :else
        [:div
         [key-value-row (i18n/t :ui/datasource) (some-> (:datasource es) clojure.core/name)]
         [key-value-row (i18n/t :ui/strategy) (or (some-> (:strategy es) clojure.core/name) "—")]
         (when (:table es) [key-value-row (i18n/t :ui/table) (:table es)])
         (when (:query es)
           [key-value-row (i18n/t :ui/query)
            [:code {:class "u-mono-12"} (:query es)]])
         (when (:from-key-column es)
           [key-value-row (i18n/t :ui/from-key-column) (:from-key-column es)])
         (when (:to-key-column es)
           [key-value-row (i18n/t :ui/to-key-column) (:to-key-column es)])])]]))

(defn- endpoint-type-card
  [type-kw side-label]
  (let [m (oc/type-meta type-kw)]
    [ui/tooltip {:content (clojure.core/name type-kw)}
     [:div {:class "sutra-objcard sutra-objcard--compact u-flex-1 u-pointer"
            :on-click #(object-forms/open-object-detail! type-kw)}
      [:div {:class "sutra-objcard__head"}
       [:span {:class "sutra-objcard__type"} (i18n/t :ui/object-type)]
       [:span {:class "sutra-objlink sutra-objlink--open"}
        (when-let [s (:symbol m)] [:span {:class "sutra-objlink__icon"} s])
        [:span {:class "sutra-objlink__title"} (oc/type-label m)]]]
      (when (seq side-label)
        [:div {:class "ontology-entityrow__sub"} side-label])]]))

(defn- link-endpoints-card
  [{:keys [from to cardinality] :as conn}]
  [card
   [:div
    [card-eyebrow (i18n/t :ui/om-link-endpoints)]
    [:div {:class "u-flex-gap-8"}
     [endpoint-type-card from (:reverse-name conn)]
     [:span {:class "u-mt-12"} "⇄"]
     [endpoint-type-card to (:forward-name conn)]]
    [:div {:class "ontology-entityrow__sub u-mt-8"}
     (link-forms/link-sentence from to cardinality)]]])

(defn link-detail-page [conn-id on-close]
  (if-let [conn (link-by-id conn-id)]
    (let [{:keys [name]} conn
          label (or (not-empty (:forward-name conn)) (clojure.core/name name))
          save! #(link-forms/submit-link! forms/edit-link-form false on-close)]
      [entity-detail-shell
       {:kind :link-type
        :entity-label label
        :api-name (clojure.core/name name)
        :on-save save! :on-close on-close
        :on-delete (fn [_]
                     (confirm-delete! (i18n/t :ui/link-type-delete-confirm {:name label})
                                      [[:dispatch [:ontology.link/delete conn-id]]
                                       [:invoke [on-close]]]))
        :form-component [link-forms/link-form
                         {:form-atom forms/edit-link-form
                          :save-label (i18n/t :ui/save)
                          :on-create save! :on-cancel on-close}]}
       [link-endpoints-card conn]
       [link-datasources-card conn]])
    (do (on-close) [:div])))

(defn- implementers-card [impls]
  (when (seq impls)
    [card
     [:div
      [card-eyebrow (i18n/t :ui/implementers)]
      (for [k impls]
        ^{:key k}
        [:div {:on-click #(object-forms/open-object-detail! k)
               :title (clojure.core/name k)
               :class "ontology-detail__implementer-row"}
         (oc/type-label k)])]]))

(defn interface-detail-page [iface-kw on-close]
  (let [impls (->> (:object-types @catalog/schema)
                   (filter (fn [[_ ot]]
                             (some #(= % iface-kw) (or (:implements ot) []))))
                   (mapv first))
        save! (fn []
                (let [{:keys [name properties] :as form} @forms/edit-interface-form]
                  (when (seq name)
                    (ont-model/update-interface!
                     iface-kw
                     (assoc form :properties (ext-forms/form-fields->schema-fields properties)))
                    (on-close))))]
    [entity-detail-shell
     {:kind :interface
      :entity-label (or (not-empty (get-in @catalog/schema [:interfaces iface-kw :display-name]))
                        (clojure.core/name iface-kw))
      :api-name (clojure.core/name iface-kw)
      :on-save save! :on-close on-close
      :on-delete (fn [_]
                   (confirm-delete! (i18n/t :ui/interface-delete-confirm
                                            {:name (clojure.core/name iface-kw)})
                                    [[:dispatch [:ontology/mutate :delete-interface
                                                 {:name iface-kw}]]
                                     [:invoke [on-close]]]))
      :form-component [iface-forms/interface-form
                       {:form-atom forms/edit-interface-form
                        :save-label (i18n/t :ui/save)
                        :on-save save! :on-cancel on-close}]}
     [implementers-card impls]]))

(defn object-type-group-detail-page [g-kw on-close]
  (let [save! (fn []
                (let [{:keys [name] :as form} @forms/edit-object-type-group-form]
                  (when (seq name)
                    (ont-model/update-object-type-group! g-kw form)
                    (on-close))))]
    [entity-detail-shell
     {:kind :object-type-group
      :entity-label (clojure.core/name g-kw)
      :on-save save! :on-close on-close
      :on-delete (fn [_]
                   (confirm-delete! (i18n/t :ui/object-type-group-delete-confirm
                                            {:name (clojure.core/name g-kw)})
                                    [[:dispatch [:ontology/mutate :delete-object-type-group
                                                 {:name g-kw}]]
                                     [:invoke [on-close]]]))
      :form-component [group-forms/object-type-group-form
                       {:form-atom forms/edit-object-type-group-form
                        :save-label (i18n/t :ui/save)
                        :on-save save! :on-cancel on-close}]}]))
