(ns sutra-ui.apps.enta-editor.overview
  (:require [sutra-ui.ui.core :as ui :refer [link]]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.catalog :as cat]
            [sutra.capps.derived :as cd]
            [sutra.capps.variables :as vars]
            [sutra.i18n.core :as i18n]
            [sutra.types.link-side :as ls]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.form :as form]
            [sutra-ui.apps.enta-editor.fields :as f]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.apps.enta-editor.names :as names]
            [sutra-ui.apps.enta-editor.ui :as eui]
            [sutra-ui.apps.enta-editor.view-preview :as vp]
            [sutra-ui.graph-editor.shared :as gs]
            [sutra-ui.ui.form :as uf]
            [sutra-ui.ui.overlay :as overlay]))

(defn- type-fields
  [t]
  (let [v (get-in @catalog/schema [:objects (keyword t)])]
    (vec (sort (map :name (if (map? v) (:fields v) v))))))

(defn- link-table
  []
  (into {} (for [{:keys [name from to]} (:links @catalog/schema)]
             [name (merge {:from from :to to}
                          (select-keys (catalog/link-spec name)
                                       [:forward-api-name :reverse-api-name]))])))

(defn- links-of
  [t]
  (vec (sort-by (comp str :api-name)
                (for [{:keys [api-name link dir far]} (ls/table-sides (link-table) t)]
                  {:api-name api-name :far far
                   :label (:label (catalog/link-side link dir))}))))

(defn- ctx
  []
  {:props        (into {} (for [t (es/object-types)] [(keyword t) (set (type-fields t))]))
   :links        (link-table)
   :object-types (mapv keyword (es/object-types))})

(defn problems
  []
  (cd/explain (es/derived-module) (ctx)))

(defn- describe [type spec]
  (case (cd/kind-of spec)
    :aggregation (let [{:keys [over agg of by pick]} spec]
                   (str (name (or agg :?)) (when of (str " " (name of)))
                        (when by (str " nach " (name by)))
                        (when pick (str " → " (name pick)))
                        " über " (name (or over :?))
                        (when-let [far (cd/far-type (link-table) type over)]
                          (str " (" (f/type-label far true) ")"))))
    :column-math (pr-str (:expression spec))
    "—"))

(def ^:private blank
  {:aggregation {:kind :aggregation :agg :sum}
   :column-math {:kind :column-math :expression [:div]}})

(defn- operand-row [v on-change refs]
  (let [num? (number? v)]
    [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"
                   :margin-bottom "var(--sp-1)"}}
     [f/select (if num? "zahl" "prop") [["prop" "Eigenschaft"] ["zahl" "Zahl"]]
      #(on-change (if (= "zahl" %) 0 nil)) nil {:clearable? false}]
     [:div {:class "u-flex-1"}
      (if num?
        [form/number-field v on-change]
                [inputs/select-input nil
         {:value       (when (keyword? v) (name v))
          :fill        true
          :strict?     true
          :placeholder "Eigenschaft wählen…"
          :items       (concat
                        (for [p (:native refs)]
                          {:value (name p) :label (name p)
                           :group "Eigenschaften des Objekttyps"})
                        (for [p (:aggregation refs)]
                          {:value (name p) :label (name p)
                           :group "Aggregationen dieses Moduls"})
                        (for [p (:column-math refs)]
                          {:value (name p) :label (name p) :disabled? true
                           :group "Spaltenrechnungen — nicht verwendbar"}))
          :on-change   (fn [x _] (on-change (f/kw-or-nil x)))}])]]))

(defn- column-math-form [type spec on-change]
  (let [expr    (or (:expression spec) [:div])
        [op & args] (if (vector? expr) expr [:div])
        args    (vec args)
        refs    (cd/referenceable (es/derived-module) type (type-fields type))
        put!    (fn [e] (on-change (assoc spec :expression e)))]
    [:div
     [uf/field {:label "Rechenart"}
      [f/select (or op :div) (map name (sort cd/math-ops))
       #(put! (into [(keyword %)] args)) nil {:fill? true :clearable? false}]]
     [uf/field {:label "Werte"}
      (doall
       (for [[i a] (map-indexed vector args)]
         ^{:key i}
         [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
          [:div {:class "u-flex-1"}
           [operand-row a #(put! (into [op] (assoc args i %))) refs]]
          [f/row-button :cross "entfernen"
           #(put! (into [op] (vec (concat (subvec args 0 i) (subvec args (inc i))))))]]))
      [f/dashed-button "+ Wert" #(put! (into [op] (conj args nil)))]]
     [:div {:style {:color gs/grey :font-size "var(--fs-sm)" :margin-top "var(--sp-2)"
                    :line-height 1.45}}
      "Eine Spaltenrechnung verrechnet Werte EINES Objekttyps — dessen eigene "
      "Eigenschaften und die Aggregationen dieses Moduls. Eine andere "
      "Spaltenrechnung darf sie nicht lesen."]]))

(defn- aggregation-form [type spec on-change]
  (let [links (links-of type)
        far   (cd/far-type (link-table) type (:over spec))
        put!  (fn [k v] (on-change (if (nil? v) (dissoc spec k) (assoc spec k v))))
        fields (if far (type-fields far) [])]
    [:div
     [uf/field {:label (i18n/t :ui/app-builder-derived-over)}
      [f/select (:over spec)
       (for [{:keys [api-name label far]} links]
         [(name api-name)
          (str (or (not-empty (str label)) (name api-name)) " → " (f/type-label far true))])
       #(put! :over (f/kw-or-nil %)) (i18n/t :ui/app-builder-derived-over-pick) {:fill? true}]]
     [uf/field {:label "Aggregation"}
      [f/select (or (:agg spec) :sum) (map name cd/aggs)
       #(on-change (-> spec (assoc :agg (keyword %)) (dissoc :of :by :pick)))
       nil {:fill? true :clearable? false}]]
     (when (contains? #{:sum :max :min :avg} (:agg spec))
       [uf/field {:label "Eigenschaft"}
        [form/select-field (:of spec) (map name fields) #(put! :of %) "Eigenschaft…"]])
     (when (= :arg-max (:agg spec))
       [uf/field {:label "sortiert nach"}
        [form/select-field (:by spec) (map name fields) #(put! :by %) "Eigenschaft…"]])
     (when (#{:arg-max :one} (:agg spec))
       [uf/field {:label "übernimmt"}
        [form/select-field (:pick spec) (map name fields) #(put! :pick %) "Eigenschaft…"]])
     (when-not (= :one (:agg spec))
       [uf/field {:label "Filter (optional)"
                  :help  (str "Ein Prädikat über das VERKNÜPFTE Objekt, z. B. "
                              "[:= :jahr {:val :jahr}] — {:val …} liest eine Variable des Moduls.")}
        [form/edn-field (:where spec) #(put! :where %)]])]))

(defn- property-editor
  [_type editing _on-done]
  (let [init  (or (some-> editing val)
                  (:column-math blank))
        draft (r/atom {:name (some-> editing key) :spec init})]
    (fn [type editing on-done]
      (let [{:keys [name spec]} @draft
            kind    (cd/kind-of spec)
            module  (cond-> (es/derived-module)
                      (and name (seq (str name))) (cd/put type name spec))
            probs   (filter #(= (:name %) name) (cd/explain module (ctx)))
            save!   (fn [] (when (seq (str name))
                             (when (and editing (not= (key editing) name))
                               (es/rename-derived! type (key editing) name))
                             (es/put-derived! type name spec)
                             (on-done)))]
        [:div {:style {:border (str "1px solid " gs/blue) :border-radius "var(--radius)"
                       :padding "var(--sp-3) var(--pt-grid-size)" :margin "var(--sp-1) 0"
                       :background "var(--c-surface)"}}
         [uf/field {:label "Name" :required? true}
          [form/text-field name #(swap! draft assoc :name (f/kw-or-nil %))
           "z. B. produktiv-quote"]]
         [uf/field {:label "Beschriftung (optional)"}
          [form/text-field (:label spec)
           #(swap! draft update :spec (fn [s] (if % (assoc s :label %) (dissoc s :label))))
           "Anzeigename"]]
         [uf/field {:label "Art" :help (some #(when (= (:id %) kind) (:hint %)) cd/kinds)}
          [f/select (or kind :column-math)
           (for [{:keys [id label]} cd/kinds] [(clojure.core/name id) label])
           #(swap! draft assoc :spec (get blank (keyword %)))
           nil {:fill? true :clearable? false}]]
         (case kind
           :aggregation [aggregation-form type spec #(swap! draft assoc :spec %)]
           :column-math [column-math-form type spec #(swap! draft assoc :spec %)]
           nil)
         (when (seq probs)
           [:div {:style {:color "var(--c-danger-strong)" :font-size "var(--fs-sm)"
                          :margin-top "var(--sp-2)"}}
            (for [{:keys [error]} probs] ^{:key error} [:div error])])
         [:div {:class "ui-form__actions u-mt-12"}
          [overlay/form-actions {:on-cancel    on-done
                                 :ok-label     (if editing "Übernehmen" "Anlegen")
                                 :on-ok        save!
                                 :ok-disabled? (or (empty? (str name)) (seq probs))}]]]))))

(defn- editing [] (es/ui-get :derived-editing))

(defn- edit! [x] (es/ui-set! {:derived-editing x}))

(defn- type-block [t]
  (let [module (es/derived-module)
        props  (cd/properties module t)
        probs  (into {} (map (juxt :name identity)) (problems))]
    [:div {:style {:margin-bottom "var(--pt-grid-size)"}}
     [:div {:style {:font-weight 600 :font-size "var(--fs-sm)" :margin-bottom "var(--sp-1)"}
            :title (clojure.core/name t)}
      (f/type-label t)]
     (if (empty? props)
       [:div {:style {:color gs/grey :font-size "var(--fs-sm)" :margin-bottom "var(--sp-1)"}}
        "Noch keine abgeleiteten Eigenschaften."]
       (doall
        (for [[nm spec] props
              :let [prob (get probs nm)]]
          ^{:key (str t "/" nm)}
          [:div
           [:div {:style {:display "flex" :gap "var(--sp-2)" :align-items "center"
                          :padding "var(--sp-1) var(--sp-1)"
                          :border-radius "var(--radius)" :font-size "var(--fs-sm)"
                          :background (if prob "var(--c-danger-bg)" "transparent")}}
            [:span {:class "ab-derived__name"} (clojure.core/name nm)]
            [:span {:class "ab-derived__kind"}
             (case (cd/kind-of spec) :aggregation "Aggregation" :column-math "Spaltenrechnung" "?")]
            [:span {:style {:flex 1 :color "var(--c-text-2)" :overflow "hidden"
                            :text-overflow "ellipsis"
                            :white-space "nowrap"}}
             (describe t spec)]
            [f/row-button :edit "bearbeiten" #(edit! [t nm])]
            [f/row-button :cross "entfernen" #(es/drop-derived! t nm)]]
           (when prob
             [:div {:style {:color "var(--c-danger-strong)" :font-size "var(--fs-sm)"
                            :margin "0 0 var(--sp-1) var(--sp-1)"}}
              (:error prob)])
           (when (= (editing) [t nm])
             [property-editor t (find (get module t) nm) #(edit! nil)])])))
     (if (= (editing) [t :new])
       [property-editor t nil #(edit! nil)]
       [f/dashed-button "+ Neue abgeleitete Eigenschaft" #(edit! [t :new])])]))

(defn derived-panel
  [only]
  (let [ts (if only [only] (mapv keyword (es/object-types)))]
    [:div
     (if (empty? ts)
       [:div {:style {:color gs/grey :font-size "var(--fs-sm)"}}
        "Diese App bindet noch keinen Objekttyp — eine abgeleitete Eigenschaft "
        "braucht den Typ, den sie erweitert."]
       (doall (for [t ts] ^{:key (str t)} [type-block t])))]))

(defn- open? [k] (contains? (es/ui-get :data-open) k))

(defn- toggle! [k] (es/ui-flip! :data-open k))

(defn focus-derived! [t nm] (es/send! [:enta.data/focus-derived t nm]))

(defn- jump! [path]
  (es/select! path)
  (es/ui-set! {:pane :preview}))

(defn- jump-list [entries]
  (into [:span {:style {:display "inline-flex" :flex-wrap "wrap" :gap "var(--sp-0) var(--sp-3)"}}]
        (for [{:keys [path label]} entries]
          [link {:href "#" :underline :hover
                 :on-click #(do (.preventDefault %) (jump! path))}
           label])))

(defn- unbind! [t]
  (let [users (names/type-users (es/tree) t)]
    (es/send! [:enta.types/unbind users
               {:title (i18n/t :ui/app-builder-unbind-title {:name (f/type-label t)})
                :body (i18n/t :ui/app-builder-unbind-body
                              {:users (str/join ", " (map :label users))})
                :danger? true
                :confirm-label (i18n/t :ui/app-builder-unbind-ok)}
               (remove #(= % t) (es/object-types))
               (es/env)])))

(defn- object-type-row [module t]
  (let [users (names/type-users (es/tree) t)
        c     (count (cd/properties module (keyword t)))]
    [:div {:class "ab-list-row"}
     [:div {:style {:display "flex" :gap "var(--sp-2)" :align-items "center"}}
      [:span {:style {:flex 1 :font-weight 600} :title (name t)} (f/type-label t)]
      (when (pos? c)
        [:span {:class "ab-muted"} (i18n/t :ui/app-builder-derived-count {:n c})])
      [f/row-button :cross (i18n/t :ui/app-builder-unbind-ok) #(unbind! t)]]
     [:div {:class "ab-muted"}
      (if (seq users)
        [:<> (i18n/t :ui/app-builder-used-by) " " [jump-list users]]
        (i18n/t :ui/app-builder-unused))]]))

(defn- variables-section
  []
  (let [usage (names/var-usage (es/tree))
        kinds (into {} (map (juxt :id :label)) cat/var-kinds)]
    (if (empty? usage)
      [:div {:class "ab-muted"} (i18n/t :ui/app-builder-vars-none)]
      (into [:div]
            (for [[nm {:keys [declared writes reads]}] (sort-by (comp f/var-label key) usage)
                  :let [{:keys [path spec]} (first declared)
                        vread (filter :var reads)
                        wread (remove :var reads)]]
              [:div {:class "ab-list-row"}
               [:div {:style {:display "flex" :gap "var(--sp-2)" :align-items "baseline"}}
                [ui/tooltip {:content (str (name nm) " — bearbeiten")}
                 [link {:href "#" :underline :hover :color :inherit :style {:font-weight 600}
                        :on-click #(do (.preventDefault %) (jump! path))}
                  (f/var-label nm)]]
                [:span {:class "ab-muted u-flex-1"} (get kinds (cat/var-kind spec) "")]]
               [:div {:class "ab-muted"}
                [:div (i18n/t :ui/app-builder-vars-written-by) " "
                 (if (seq writes) [jump-list writes] "—")]
                [:div (i18n/t :ui/app-builder-vars-read-by) " "
                 (if (or (seq wread) (seq vread))
                   [:<> [jump-list wread]
                    (when (seq vread)
                      (str (when (seq wread) " · ")
                           (str/join ", " (map :label vread))))]
                   "—")]]])))))

(defn data-panel []
  (let [tree   (es/tree)
        module (es/derived-module)
        n      (count (cd/all-properties module))]
    (when tree
      [:div {:class "ab-panel"}
       [:div {:class "ab-outline__head"} [:span {:class "ab-panel-title"} "Daten"]]
       [eui/section {:title "Objekttypen" :count (count (es/object-types))
                     :open? (open? :object-types) :on-toggle #(toggle! :object-types)}
        (doall
         (for [t (es/object-types)]
           ^{:key (str t)} [object-type-row module t]))
        [:div {:style {:margin-top "var(--sp-2)"}}
         [form/select-field nil (remove (comp (set (map name (es/object-types))) first)
                                        (f/object-type-options))
          #(when % (es/set-object-types! (conj (es/object-types) %)))
          "+ Objekttyp binden…"]]]

       [eui/section {:title (i18n/t :ui/app-builder-vars) :count (count (cat/declared-vars tree))
                     :open? (open? :variables) :on-toggle #(toggle! :variables)}
        [variables-section]]

       [eui/section {:title "Abgeleitete Eigenschaften" :count n
                     :open? (open? :derived) :on-toggle #(toggle! :derived)}
        [ui/tooltip
         {:content (str "Wird für die Lesevorgänge DIESER App berechnet — kein Objekttyp "
      "der Ontologie bekommt die Eigenschaft. Die Vorschau rechnet sie "
      "schon im Entwurf über Live-Daten.")}
         [:div {:class "ab-muted" :style {:margin-bottom "var(--sp-2)"}}
          "Nur in dieser App, nicht in der Ontologie."]]
        (when-let [focus (es/ui-get :data-focus)]
          [:div {:style {:font-size "var(--fs-sm)" :margin-bottom "var(--sp-1)"}}
           [:span {:class "ab-muted"} "gefiltert auf " (f/type-label focus) " "]
           [f/row-button "alle zeigen" "Filter aufheben" #(es/ui-set! {:data-focus nil})]])
        [derived-panel (es/ui-get :data-focus)]]])))

(defn- watched-set-fields
  [spec plain put!]
  (let [gewaehlt (set (:object-sets spec))
        mengen   (sort (keep (fn [[nm s]] (when (or (:source s) (:from s)) nm))
                             (vars/all-var-specs (es/tree))))]
    (when (seq mengen)
      [uf/field {:label [ui/tooltip {:content (i18n/t :ui/app-builder-auto-refresh-sets-hint)}
                         [:span (i18n/t :ui/app-builder-auto-refresh-sets)]]}
       (for [nm mengen]
         ^{:key nm}
         [ui/checkbox
          {:checked (contains? gewaehlt nm)
           :label (name nm)
           :on-change
           (fn [e]
             (let [neu (if (.. e -target -checked)
                         (conj (vec (:object-sets spec)) nm)
                         (vec (remove #{nm} (:object-sets spec))))
                   m   (if (seq neu)
                         (assoc spec :object-sets neu)
                         (dissoc spec :object-sets))]
               (put! (or (not-empty m) plain))))}])])))

(defn- auto-refresh-fields
  [file]
  (let [live  (:live file)
        view? (some? (:form-factor file))
        on?   (if view? (or (true? live) (map? live)) (not (false? live)))
        spec  (when (map? live) live)
        plain (when view? true)
        put!  #(es/put-file-key! :live %)]
    [:div {:class "ab-param"}
     [ui/switch {:title (i18n/t :ui/app-builder-auto-refresh-hint)
                 :checked on?
                 :on-change #(put! (if (.. % -target -checked)
                                     (or spec plain)
                                     (when-not view? false)))
                 :label (i18n/t :ui/app-builder-auto-refresh)}]
     (when on?
       [:<>
        [uf/field {:label (i18n/t :ui/app-builder-auto-refresh-min)}
         ^{:key (str (:min-seconds spec))}
         [ui/numeric-input
          {:min 1 :placeholder "10"
           :default-value (:min-seconds spec)
           :on-blur (fn [e]
                      (let [n (js/parseInt (.. e -target -value))
                            m (if (js/isNaN n)
                                (dissoc spec :min-seconds)
                                (assoc spec :min-seconds (max 1 n)))]
                        (put! (or (not-empty m) plain))))}]]
        [watched-set-fields spec plain put!]])]))

(defn app-panel []
  (let [tree (es/tree)
        file (es/file)]
    (when tree
      [:div {:class "ab-panel"}
       [:div {:class "ab-outline__head"} [:span {:class "ab-panel-title"} "App"]]
       [uf/field {:label "Name"}
        [form/text-field (:display-name file) #(es/put-file-key! :display-name %)
         (i18n/t :ui/app-builder-app-name-placeholder)]]
       [uf/field {:label "Beschreibung"}
        [form/text-field (:description file) #(es/put-file-key! :description %)
         "Wofür ist diese App?"]]
       [:div {:class "ab-param"}
        [vp/auto-publish-switch (:auto-publish file)
         #(es/put-file-key! :auto-publish (when % true))]]
       [auto-refresh-fields file]
       [eui/section {:title (i18n/t :ui/app-builder-advanced)
                     :open? (open? :app-advanced) :on-toggle #(toggle! :app-advanced)}
        [uf/field {:label (i18n/t :ui/app-builder-app-api-name)
                   :help  (i18n/t :ui/app-builder-app-api-name-hint)}
         [form/text-field (:name file)
          #(es/put-file-key! :name (when % (keyword %))) "z. B. quoten"]]
        [uf/field {:label (i18n/t :ui/app-builder-app-heading)}
         [form/text-field (:title tree) #(es/put-param! [] :title %)
          (names/app-label file)]]]])))
