(ns sutra-ui.forms.lists
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.inputs :as inputs]
            [clojure.string :as str]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.shell :as shell]
            [sutra.kit.remote :as rd]
            [sutra.i18n.core :as i18n]
            [sutra-ui.forms.external :refer [row-check]]
            [sutra-ui.forms.interface :refer [open-interface-detail!]]
            [sutra-ui.forms.link :refer [link-sentence open-link-detail!]]
            [sutra-ui.forms.object :refer [open-object-detail!]]))

(defn- impl-count
  [iface-kw]
  (count (for [[_ ot] (:object-types @catalog/schema)
               :when (some #(= % iface-kw) (or (:implements ot) []))]
           ot)))

(defn list-section-header [label n]
  [:div {:class "form-list__header"}
   (str label " (" n ")")])

(defn- plan-entry-for
  [name-kw plan-section]
  (some (fn [[k v]] (when (= name-kw (catalog/name-of k)) v)) plan-section))

(defn entity-health
  [name-kw plan-section]
  (let [entry (plan-entry-for name-kw plan-section)
        ds    (when entry
                (or (get-in entry [:from :datasource])
                    (:datasource entry)
                    (get-in entry [:join-table :datasource])))]
    (cond
      (nil? ds) {:level :ok}
      (and (data/datasources-here?) (not (contains? (data/datasource-map) ds)))
      {:level :error
       :msg  (i18n/t :ui/source-not-registered {:name (clojure.core/name ds)})}
      (data/source-unhealthy? ds)
      {:level :error
       :msg  (i18n/t :ui/source-connection-problem
                     {:name (clojure.core/name ds)
                      :message (:message (data/source-health ds))})}
      :else {:level :ok})))

(defn- health-dot
  [{:keys [level msg]}]
  (when (= level :error)
    [ui/tooltip {:content msg} [:span {:class "form-health-dot"}]]))

(defn favorite-star
  [ref]
  (let [fav? (kompass/favorite? ref)]
    [ui/tooltip
     {:content (i18n/t (if fav? :ui/favorite-remove :ui/favorite-add))}
     [ui/button {:small    true
                 :minimal  true
                 :icon     (if fav? :star :star-empty)
                 :intent   (when fav? :warning)
                 :on-click (fn [e] (.stopPropagation e) (kompass/toggle-favorite! ref))
                 :aria-label (i18n/t (if fav? :ui/favorite-remove :ui/favorite-add))}]]))

(defn type-row
  [{:keys [icon primary api-name secondary aside on-click health fav-ref]}]
  [:div {:on-click on-click
         :class "form-list__row"
         :on-mouse-enter (fn [e]
                           (set! (.. e -currentTarget -style -backgroundColor) "var(--c-hover)"))
         :on-mouse-leave (fn [e]
                           (set! (.. e -currentTarget -style -backgroundColor) "transparent"))}
   (when health [health-dot health])
   (when (seq icon) [:span {:class "sutra-objcard__icon"} icon])
   [:div {:class "u-flex-1-min-0"}
    [:div {:class "form-list__primary"}
     primary
     (when (and (seq api-name) (not= api-name primary))
       [ui/tooltip {:content (i18n/t :ui/api-name)}
        [:span {:class "u-mono-12 u-ml-6" :style {:opacity 0.55 :font-weight 400}}
         api-name]])]
    (when secondary
      [:div {:class "form-list__secondary"}
       secondary])]
   (when (some? aside) [:span {:class "form-list__secondary"} aside])
   (when fav-ref [favorite-star fav-ref])])

(defn property-label
  [type-kw prop]
  (when prop
    (let [f (some #(when (= (keyword prop) (keyword (:name %))) %)
                  (get-in @catalog/schema [:object-types (catalog/name-of type-kw) :fields]))]
      (or (not-empty (get-in f [:opts :name]))
          (oc/type-label {:api-name (clojure.core/name prop)})))))

(defn- object-type-row [k v]
  (let [m     (oc/type-meta k)
        label (oc/type-label m)
        n     (get (rd/value-or @data/object-counts {}) k)]
    [type-row
     {:icon (:symbol m)
      :primary label
      :api-name (clojure.core/name k)
      :secondary (str/join " · "
                           (remove nil?
                                   [(when-let [p (:plural m)]
                                      (i18n/t :ui/om-plural-line {:plural p}))
                                    (when-let [tk (:title-key m)]
                                      (i18n/t :ui/om-title-line {:prop (property-label k tk)}))
                                    (i18n/t :ui/om-properties-count {:n (count (:fields v))})]))
      :aside (when (some? n) (i18n/t :ui/om-objects-count {:n n}))
      :health (entity-health k (:objects @data/import-plan))
      :fav-ref {:kind :object-type :id k :label label}
      :on-click #(open-object-detail! k)}]))

(defn- object-types-controls
  [groups]
  (let [{:keys [object-types-filter-group object-types-group-by?]} @shell/ui-state]
    [:div {:class "form-toolbar u-mb-8"}
     [inputs/select-input nil
      {:value       object-types-filter-group
       :placeholder (i18n/t :ui/all-groups)
       :strict?     true
       :items       (for [g (sort (keys groups))] {:value g :label (name g)})
       :on-change   (fn [v _] (shell/set-object-types-filter-group! v))}]
     (when (seq groups)
       [row-check {:checked (boolean object-types-group-by?)
                   :on-change #(shell/toggle-object-types-group-by!)
                   :label (i18n/t :ui/group-by-group)}])]))

(defn object-types-list
  []
  (let [ots     (sort-by key (:object-types @catalog/schema))
        groups  (catalog/object-type-groups)
        {:keys [object-types-filter-group object-types-group-by?]} @shell/ui-state
        member-set (when object-types-filter-group
                     (set (:members (get groups object-types-filter-group))))
        shown   (cond->> ots
                  member-set (filter (fn [[k _]] (contains? member-set k))))]
    [:div {:class "u-mt-10"}
     [list-section-header (i18n/t :ui/object-types) (count shown)]
     [object-types-controls groups]
     (cond
       (empty? shown)
       [:div {:class "form-list__empty"} (i18n/t :ui/object-types-none)]

       object-types-group-by?
       (let [shown-set (set (map first shown))
             obj->groups (fn [k] (catalog/groups-of k))
             ungrouped (filter #(empty? (obj->groups (first %))) shown)]
         (doall
          (concat
           (for [[g gspec] (sort-by key groups)
                 :let [members (filter shown-set (:members gspec))]
                 :when (seq members)]
             ^{:key g}
             [:div
              [list-section-header (clojure.core/name g) (count members)]
              (doall (for [k (sort members)]
                       ^{:key k} (object-type-row k (get (:object-types @catalog/schema) k))))])
           (when (seq ungrouped)
             [^{:key :--other}
              [:div
               [list-section-header (i18n/t :ui/other) (count ungrouped)]
               (doall (for [[k v] ungrouped]
                        ^{:key k} (object-type-row k v)))]]))))

       :else
       (doall (for [[k v] shown]
                ^{:key k} (object-type-row k v))))]))

(defn link-types-list
  []
  (let [cts (sort-by (comp name :name) (:links @catalog/schema))]
    [:div {:class "u-mt-10"}
     [list-section-header (i18n/t :ui/link-types) (count cts)]
     (if (empty? cts)
       [:div {:class "form-list__empty"}
        (i18n/t :ui/link-types-none)]
       (doall
        (for [{nm :name :keys [from to cardinality] :as conn} cts]
          ^{:key (catalog/link-id conn)}
          [type-row
           {:primary (or (not-empty (:forward-name conn)) (clojure.core/name nm))
            :api-name (clojure.core/name nm)
            :secondary (link-sentence from to cardinality)
            :health (entity-health nm (:links @data/import-plan))
            :fav-ref {:kind :link-type :id (catalog/link-id conn)
                      :label (clojure.core/name nm)}
            :on-click #(open-link-detail! (catalog/link-id conn))}])))]))

(defn interfaces-list
  []
  (let [ifaces (sort-by key (:interfaces @catalog/schema))]
    [:div {:class "u-mt-10"}
     [list-section-header (i18n/t :ui/interfaces) (count ifaces)]
     (if (empty? ifaces)
       [:div {:class "form-list__empty"}
        (i18n/t :ui/interfaces-none)]
       (doall
        (for [[k iface] ifaces]
          ^{:key k}
          [type-row
           {:primary (or (not-empty (:display-name iface)) (clojure.core/name k))
            :api-name (clojure.core/name k)
            :secondary (let [n (impl-count k)]
                         (i18n/t (if (= 1 n) :ui/implementers-one :ui/implementers-many) {:n n}))
            :fav-ref {:kind :interface :id k :label (clojure.core/name k)}
            :on-click #(open-interface-detail! k)}])))]))
