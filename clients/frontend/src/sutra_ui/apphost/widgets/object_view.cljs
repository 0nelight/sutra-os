(ns sutra-ui.apphost.widgets.object-view
  (:require [sutra-ui.ui.page :as page]
            [sutra-ui.ui.display :as display]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.core :as ui :refer [empty-state]]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.bind :as bind]
            [sutra.capps.layout :as lay]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.standard-view :as sv]
            [sutra.capps.variables :as vars :refer [deref*]]
            [sutra.capps.warn :as warn]
            [sutra.i18n.core :as i18n]
            [sutra-ui.render-ctx :as rctx]
            [sutra-ui.object.card :as oc]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.state :refer [vars]]))

(defn objekt-attrs
  [descriptor row]
  (let [t  (or (:object-type-ref row) (:object-type row))
        id (some-> (:id row) str str/trim not-empty)]
    (when (and t id (not (coll? (:id row))))
      (let [meta (oc/descriptor-meta descriptor t)]
        {:data-objekt       (str (:api-name meta) "/" id)
         :data-objekt-titel (oc/title meta row id)}))))

(defn- objectview-frame
  [row descriptor & children]
  (into [:div (assoc (objekt-attrs descriptor row) :class "capp-objectview")]
        children))

(defn- embedded-object-view
  [view row0 cfg {ctx0 :app}]
  (let [{:keys [bind problems mirror]} (vars/interface-bindings (:vars view) (:interface cfg))
        mirrors (update-vals (or mirror {}) r/atom)
        initial-tab (->kw (:initial-tab cfg))
        initial-tab-on-switch? (:initial-tab-on-switch? cfg)
        tab-var (when (contains? #{:tabs :pages} (->kw (:kind view)))
                  (bind/name-in :var (:select view)))
        own-tab (when (and initial-tab-on-switch? tab-var)
                  (vars {tab-var (-> (get-in view [:vars tab-var])
                                     (select-keys [:as])
                                     (assoc :default initial-tab))}
                        false))
        bind    (merge bind mirrors own-tab)
        own-row (fn [descriptor row]
                  (let [t (or (:object-type-ref row) (:object-type row))
                        k (->kw (or (get (:object-names descriptor) t) t))]
                    (if (and k row) {k [row] vars/view-object [row]} {})))
        data*   (r/atom (own-row (:descriptor ctx0) row0))
        waits?  (vars/folds-payload-at-build? view)
        ready?  (r/atom (not waits?))
        error   (r/atom nil)
        for*    (atom nil)
        seen-generation (atom nil)
        reload  (atom nil)
        built   (atom nil)
        inputs  (atom #{})
        held    (atom {})
        timer   (atom nil)
        current (fn [] (into {} (keep (fn [nm] (when-let [h (get @held nm)] [nm (deref* h)])))
                             @inputs))
        reask!  (fn []
                  (some-> @timer js/clearTimeout)
                  (reset! timer (js/setTimeout #(when-let [f @reload] (f)) 150)))
        on-scope (fn [handles]
                   (swap! held merge handles)
                   (doseq [[nm h] handles
                           :when (satisfies? IWatchable h)]
                     (add-watch h [::view-input nm]
                                (fn [_ _ o n]
                                  (when (and (not= o n) (contains? @inputs nm)) (reask!))))))]
    (doseq [p problems] (warn/warn! p))
    (when (and (or initial-tab initial-tab-on-switch? (:hide-tabs? cfg)) (nil? tab-var))
      (warn/warn! (str "app/object-view: a tab option was set, but this view's root is a "
                       (pr-str (->kw (:kind view))) " with no `:select {:var …}` — there is no"
                       " strip to open, hide or go back to")))
    (when (:scenario view)
      (warn/warn! (str "app/object-view: this view's root declares :scenario — an embedded"
                       " view reads the fork its host subtree is on, so the rider is ignored")))
    (fn [view row {:keys [hide-header?] :as cfg} {ctx :app :keys [scope-view]}]
      (doseq [[nm v] (:mirror (vars/interface-bindings (:vars view) (:interface cfg)))
              :let [h (get mirrors nm)]
              :when (and h (not= v @h))]
        (reset! h v))
      (let [descriptor (:descriptor ctx)
            id   (some-> (:id row) str str/trim not-empty)
            t    (or (:object-type-ref row) (:object-type row))
            key* [id t]
            g    (client/page-generation descriptor)
            load! (fn []
                    (frame/request!
                     (:frame ctx)
                     (assoc (frame/kept
                             (fn [res]
                               (doseq [p (vars/object-view-problems res)] (warn/warn! p))
                               (reset! inputs (into #{} (map ->kw) (:inputs res)))
                               (reset! data* (vars/object-view-payload res row))
                               (reset! ready? true))
                             (fn [msg] (reset! error msg) (reset! ready? true)))
                            :ask [(or (:around-fn descriptor) client/around!)
                                  descriptor
                                  (cond-> {:type (if (keyword? t) (name t) (str t)) :id id}
                                    (::standard? cfg)   (assoc :standard true)
                                    (seq (current))     (assoc :vars (current)))])))]
        (reset! reload load!)
        (if (and id t)
          (when (not= key* @for*)
            (when-let [h (and @for* (get own-tab tab-var))] (reset! h initial-tab))
            (when @for*
              (reset! data* (own-row descriptor row))
              (when waits? (reset! ready? false)))
            (reset! for* key*)
            (reset! error nil)
            (reset! seen-generation g)
            (load!))
          (reset! ready? true))
        (when (and id t (not= g @seen-generation))
          (reset! seen-generation g)
          (load!))
        [objectview-frame row descriptor
         (when-not hide-header?
           (let [meta (oc/descriptor-meta descriptor t)
                 info (lp/type-entry descriptor :standard-views t)
                 ganz (some-> (get @data* (->kw (or (get (:object-names descriptor) t) t)))
                              first)
                 sub  (sv/subtitle-text (or ganz row)
                                        (lp/type-entry descriptor :property-display t)
                                        (:subtitle info)
                                        (rctx/current))]
             [oc/object-header meta (or ganz row) {:subtitle sub}]))
         (cond
           @error        [ui/problem (i18n/t :ui/capp-object-failed)
                          (str "app/object-view: " @error)]
           (not @ready?) [busy/block {:class "capp-objectview__loading"}
                          (i18n/t :ui/capp-object-loading)]
           :else
           (let [fresh (let [k (->kw (or (get (:object-names descriptor) t) t))]
                         (or (first (get @data* k)) row))
                 node (lay/object-view-node view fresh
                                            {:bound       (keys bind)
                                             :tab-var     tab-var
                                             :initial-tab initial-tab
                                             :hide-tabs?  (:hide-tabs? cfg)})]
             ^{:key (when waits? id)}
             [scope-view node
              (or @built
                  (reset! built {:descriptor   descriptor
                                 :handles      bind
                                 :derived      {}
                                 :data*        data*
                                 :routing?     false
                                 :scenario-var nil
                                 :on-scope     on-scope}))
              (assoc ctx :handles bind
                     :descriptor (assoc descriptor :object row :object-of-type t)
                     :refresh (fn [d]
                                (when-let [f @reload] (f))
                                (when-let [hf (:refresh ctx)] (hf d))))]))]))))

(defn- property-list-view
  [row display {:keys [columns label-position] :as cfg}]
  (let [entries (sv/property-entries row display cfg (rctx/current))]
    (cond
      (nil? row)       [:div.capp-objectview__empty (i18n/t :ui/property-list-no-object)]
      (empty? entries) [:div.capp-objectview__empty (i18n/t :ui/property-list-empty)]
      :else [display/property-list entries {:columns columns
                                        :label-position (->kw label-position)}])))

(defn- around-payload!
  [desc spec row]
  (.then (client/around! desc spec)
         (fn [res]
           (doseq [p (vars/object-view-problems res)] (warn/warn! p))
           (vars/object-view-payload res row))))

(defn- relations-view
  [env]
  (r/with-let [f    (frame/frame-for env)
               held (atom nil)]
    (let [{ctx :app :keys [scope render-node]} env
          desc  (:descriptor ctx)
          row   (:object desc)
          t     (or (:object-type-ref row) (:object-type row))
          links (:links (lp/type-entry desc :standard-views t))
          data  (or (some-> (:data* scope) deref) {})
          held? (some #(contains? data (vars/link-key (:api %))) links)
          id    (some-> (:id row) str)
          slot  [::relations id t]
          {own :data err :error}
          (when (and id (seq links) (not held?))
            (when (not= slot @held)
              (some->> @held (vector :capp/drop-read) (frame/send! f))
              (reset! held slot))
            (frame/read! env slot (client/page-generation desc)
                         [around-payload! desc {:type (if (keyword? t) (name t) (str t)) :id id
                                                :standard true}
                          row]))]
      (cond
        (nil? row)                   [empty-state
                                      {:icon :link :title (i18n/t :ui/property-list-no-object)}]
        (empty? links)               [empty-state {:icon :link :title (i18n/t :ui/capp-no-links)}]
        err                          [ui/problem (i18n/t :ui/capp-links-failed) err]
        (and (not held?) (nil? own)) [busy/block {:class "capp-objectview__loading"}
                                      (i18n/t :ui/capp-loading)]
        :else
        (render-node (vars/resolve-scoped {:kind :rows :items (sv/link-sections links)}
                                          (:handles scope) (:derived scope)
                                          (if held? data (merge data own)))
                     env)))
    (finally (some->> @held (vector :capp/drop-read) (frame/send! f)))))

(defn- host-widget
  [k]
  (fn [cfg env]
    (let [desc (:descriptor (:app env))
          row  (:object desc)
          f    (page/view-fn k)]
      (cond
        (and (= :relations k)
             (or (nil? f)
                 (seq (:links (lp/type-entry desc :standard-views
                                             (or (:object-type-ref row) (:object-type row)))))))
        [relations-view env]
        (nil? row) [empty-state {:icon :widget :title (i18n/t :ui/property-list-no-object)}]
        (nil? f)   [empty-state {:icon :widget :title (i18n/t :ui/capp-widget-unavailable)}]
        :else      [f (assoc cfg
                             :object-type (lp/viewed-type desc row)
                             :id (:id row))]))))

(defn- standard-view
  [desc t hide-actions?]
  (when-let [info (lp/type-entry desc :standard-views t)]
    (sv/node (:type info) info
             (lp/type-entry desc :property-display t)
             {:actions? (not hide-actions?)
              :skip     (sv/placed-actions (:layout desc))
              :texts    {:actions   (i18n/t :ui/sv-actions)
                         :no-action (i18n/t :ui/sv-no-action)}})))

(defn- embedded-set-view
  [_view rows k {ctx :app}]
  (let [data* (r/atom {k rows vars/view-objects rows})
        scope {:descriptor (:descriptor ctx) :handles {} :derived {} :data* data*
               :routing? false :scenario-var nil}]
    (fn [view rows k {ctx :app :keys [scope-view]}]
      (when-not (identical? rows (get @data* k)) (reset! data* {k rows vars/view-objects rows}))
      [objectview-frame nil nil
       [scope-view view scope ctx]])))

(defn- set-panel
  [rows {:keys [object-type]} env]
  (let [desc (:descriptor (:app env))
        t    (or (some-> (first rows) :object-type-ref) object-type)
        k    (->kw (or (get (:object-names desc) t) t))
        view (or (some-> (lp/type-entry desc :object-views t) (get :panel-set)
                         lay/lower-view-body)
                 (when t
                   (sv/set-node (lp/type-entry desc :property-display t)
                                {:title-key (lp/type-entry desc :object-titles t)
                                 :texts     {:charts (i18n/t :ui/ov-set-charts)
                                             :list   (i18n/t :ui/ov-set-list)}})))]
    (if (or (nil? t) (empty? rows))
      [:div.capp-objectview__empty (i18n/t :ui/ov-set-empty)]
      ^{:key [k :panel-set]}
      [embedded-set-view view rows k env])))

(defn set-panel?
  [form-factor behavior n]
  (and (= :panel form-factor)
       (case (or behavior :instance)
         :set      true
         :adaptive (not= 1 n)
         false)))

(defn object-view-widget
  [{:keys [object form-factor hide-actions? panel-behavior mode empty] :as cfg} env]
  (let [rows       (lp/as-rows object)
        row        (first rows)
        ff         (or (->kw form-factor) :full)
        t          (:object-type-ref row)
        ot         (->kw t)
        desc       (:descriptor (:app env))
        type-label #(oc/type-label (oc/descriptor-meta desc t))
        configured (when-not (= :standard (->kw mode))
                     (some-> (lp/type-entry desc :object-views t) (get ff)
                             lay/lower-view-body))
        standard   (when-not configured (standard-view desc t hide-actions?))
        view       (or configured standard)]
    (cond
      (set-panel? ff (->kw panel-behavior) (count rows)) [set-panel rows cfg env]
      (and (nil? row) empty) [empty-state (if (map? empty) empty {:title empty})]
      (nil? row)  [:div.capp-objectview__empty (i18n/t :ui/property-list-no-object)]
      (nil? view) [ui/problem
                   (i18n/t :ui/capp-no-view {:type (type-label)})
                   (str "keine Object View für " (some-> ot name) " · " (name ff))]
      (not (lay/data-layout? view))
      [ui/problem
       (i18n/t :ui/capp-view-broken {:type (type-label)})
       (str "die Object View für " (some-> ot name) " · " (name ff)
            " ist kein Knotenbaum")]
      :else
      ^{:key [ot ff (boolean standard)]}
      [embedded-object-view view row (cond-> cfg standard (assoc ::standard? true)) env])))

(defn- attributes-widget [cfg {:keys [app scope]}]
  (let [desc (:descriptor app)
        row0 (:object desc)
        t    (lp/viewed-type desc row0)
        row  (or (some-> (:data* scope) deref (get t) first) row0)]
    (property-list-view row
                        (lp/type-entry desc :property-display (or (:object-type-ref row) t))
                        cfg)))

(def widgets
  {:attributes attributes-widget
   :relations  (fn [{:keys [title] :as cfg} env]
                 [:<>
                  (when title [:div.capp-table__title title])
                  ((host-widget :relations) cfg env)])
   :documents  (host-widget :documents)
   :matrix     (host-widget :matrix)
   :floorplan  (host-widget :floorplan)})
