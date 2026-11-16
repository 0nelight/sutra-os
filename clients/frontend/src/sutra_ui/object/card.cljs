(ns sutra-ui.object.card
  (:require [sutra-ui.ui.core :as ui]
            [sutra.kit.coerce :refer [->kw]]
            [sutra-ui.render-ctx :as rctx]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.standard-view :as sv]
            [sutra.capps.table :as tbl]
            [sutra.capps.variables :as vars]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.object-view :as object-view]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]))

(defn- fields->display [fields]
  (into {}
        (map-indexed (fn [i {nm :name t :type o :opts}]
                       [nm (cond-> {:prop nm :type t :order i}
                             (seq (:name o))   (assoc :name (:name o))
                             (seq (:format o)) (assoc :format (:format o))
                             (:visibility o)   (assoc :visibility (->kw (:visibility o)))
                             (seq (:render-hints o))
                             (assoc :render-hints (mapv ->kw (:render-hints o))))]))
        fields))

(defn type-meta [t]
  (let [k  (catalog/name-of t)
        ot (get-in @catalog/schema [:object-types k])]
    {:type         k
     :api-name     (some-> k name)
     :display-name (not-empty (:display-name ot))
     :plural       (not-empty (:plural ot))
     :symbol       (:symbol ot)
     :title-key    (:title-key ot)
     :display      (fields->display (:fields ot))}))

(defn descriptor-meta [descriptor t]
  (let [sv-info (lp/type-entry descriptor :standard-views t)]
    {:type         t
     :api-name     (or (some-> (lp/type-entry descriptor :object-names t) name)
                       (some-> (->kw t) name))
     :display-name (not-empty (:display-name sv-info))
     :plural       (not-empty (:plural sv-info))
     :symbol       (:symbol sv-info)
     :title-key    (->kw (lp/type-entry descriptor :object-titles t))
     :display      (lp/type-entry descriptor :property-display t)}))

(defn type-label
  ([x] (type-label x false))
  ([x plural?]
   (let [m (if (map? x) x (type-meta x))]
     (or (when plural? (:plural m))
         (:display-name m)
         (tbl/humanize (:api-name m))))))

(defn link-type-label
  [{cn :name :keys [from to forward-name]}]
  (str (or (not-empty forward-name) (tbl/humanize (name cn)))
       "  (" (type-label from) " → " (type-label to) ")"))

(defn spec-label
  [spec k]
  (or (not-empty (str (:display-name spec))) (name k)))

(defn title [meta row id]
  (let [row (or row {:id id})]
    (or (some-> (:title-key meta) (as-> tk (vars/row-prop row tk)) str not-empty)
        (catalog/instance-display-name row (:title-key meta)))))

(defn- loaded-row
  [k id]
  (let [s (str id)]
    (or (some #(when (= (str (:id %)) s) %) (panel/rows-of k))
        (instances/object-by-id id))))

(defn- resolve-opts [t id {:keys [meta row] :as opts}]
  (let [m (or meta (type-meta t))]
    (assoc opts :meta m :row (or row (when-not meta (loaded-row (:type m) id))))))

(defn- opener [{:keys [on-open open?]}]
  (when-not (false? open?)
    (or on-open (when (object-view/object-view-surface?) shell/open-object!))))

(defn object-link
  ([t id] (object-link t id nil))
  ([t id opts]
   (let [{:keys [meta row class icon?] :or {icon? true} :as o} (resolve-opts t id opts)
         id   (or id (:id row))
         open (opener o)
         sym  (when icon? (:symbol meta))
         txt  (title meta row id)
         body [:<>
               (when sym [:span {:class "sutra-objlink__icon"} sym])
               [:span {:class "sutra-objlink__title"} txt]]
         attrs {:class (cond-> "sutra-objlink" open (str " sutra-objlink--open")
                               class (str " " class))
                :title (str txt "\n" (type-label meta) " · " id)}]
     (if open
       [:a (assoc attrs
                  :href "#"
                  :aria-label (i18n/t :ui/oc-open-object {:name txt})
                  :on-click (fn [e]
                              (.preventDefault e)
                              (.stopPropagation e)
                              (open (:type meta) id)))
        body]
       [:span attrs body]))))

(defn- card-entries [{:keys [display title-key]} row props]
  (if (seq props)
    (sv/property-entries row display {:props props} (rctx/current))
    (let [all  (into [] (remove #(= (:prop %) (->kw title-key)))
                     (sv/property-entries row display {} (rctx/current)))
          prom (filterv :prominent? all)]
      (if (seq prom) prom (subvec all 0 (min 2 (count all)))))))

(defn object-card
  ([t id] (object-card t id nil))
  ([t id opts]
   (let [{:keys [meta row props compact? class] :as o} (resolve-opts t id opts)
         entries (when row (card-entries meta row props))]
     [(if compact? :div ui/card) {:class (cond-> "sutra-objcard"
                                           compact? (str " sutra-objcard--compact")
                                           class    (str " " class))}
      [:div {:class "sutra-objcard__head"}
       [:span {:class "sutra-objcard__type"}
        (when-let [s (:symbol meta)] [:span {:class "sutra-objcard__icon"} s])
        (type-label meta)]
       [object-link t id (-> o (dissoc :class) (assoc :icon? false))]]
      (when (seq entries)
        (into [:dl {:class "sutra-objcard__props"}]
              (for [{:keys [prop label text]} entries]
                ^{:key (str prop)}
                [:div {:class "sutra-objcard__prop"}
                 [:dt label]
                 [:dd (if (seq text) text "—")]])))])))

(defn object-header
  ([meta row] (object-header meta row nil))
  ([meta row {:keys [subtitle]}]
   (let [sym (:symbol meta)]
     [ui/entity-title
      {:class    "sutra-objhead"
       :icon     (when sym [:span {:class "sutra-objhead__icon"} sym])
       :title    [:span {:class "sutra-objhead__title" :elementtiming "object-head"}
                  (title meta row nil)]
       :subtitle [:<>
                  (when (seq subtitle) [:span {:class "sutra-objhead__subtitle"} subtitle])
                  [:span {:class "sutra-objhead__type"} (type-label meta)]]}])))
