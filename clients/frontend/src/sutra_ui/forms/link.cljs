(ns sutra-ui.forms.link
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.forms.link-type :as ltf]
            [sutra-ui.object.card :as oc]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.forms.events]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.style :as style]
            [sutra.i18n.core :as i18n]
            [sutra-ui.forms.base :refer [entity-views named via]]
            [sutra-ui.forms.external :refer [column-select datasource-options ds-items
                                                    ext->link-external-source
                                                    ext-source->form-external]]))

(defn- link-fk-attrs
  [obj-kw]
  (ltf/fk-candidates @catalog/schema obj-kw))

(defn- pk-attr-of
  [obj-kw]
  (ltf/primary-key-of @catalog/canonical-schema obj-kw))

(defn- pair->cardinality [pair]
  ({[:one  :one] :one-to-one
    [:one  :many] :one-to-many
    [:many :one] :many-to-one
    [:many :many] :many-to-many} pair))

(defn- cardinality->pair [c]
  (case (some-> c keyword)
    :one-to-one   [:one  :one]
    :one-to-many  [:one  :many]
    :many-to-one  [:many :one]
    :many-to-many [:many :many]
    [:many :one]))

(defn- derive-fk-side
  [from-pos to-pos tiebreak]
  (cond
    (and (= from-pos :many) (= to-pos :one))  :from
    (and (= from-pos :one)  (= to-pos :many)) :to
    (and (= from-pos :one)  (= to-pos :one))  tiebreak
    :else                                     nil))

(defn- type-pill
  [obj-kw]
  (let [view (:object-type entity-views)
        {:keys [glyph bg fg]} (:icon view)]
    [:div
     {:class "form-typepill"
      :style {:background (if obj-kw bg "var(--c-surface-soft)")
              :color (if obj-kw "var(--c-text)" "var(--c-muted-2)")}}
     [:span {:class "form-typepill__glyph" :style {:color (if obj-kw fg "var(--c-muted-3)")}} glyph]
     [ui/tooltip {:content (some-> obj-kw clojure.core/name)}
      [:span
       (if obj-kw (oc/type-label obj-kw) (i18n/t :ui/pick-a-type-short))]]]))

(defn- one-many-radio
  [{:keys [pos selected? on-change]}]
  [ui/radio {:class "form-cardinality__radio"
             :style {:margin 0
                     :border (str "1px solid "
                                  (if selected? "var(--c-primary)" "var(--c-border)"))
                     :background (if selected? "var(--c-primary-bg)" "var(--c-surface)")
                     :color (if selected? "var(--c-primary)" "var(--c-text-2)")
                     :font-weight (if selected? "600" "500")}
             :checked selected? :on-change on-change
             :label (i18n/t (case pos :one :ui/link-side-one :ui/link-side-many))}])

(defn- endpoint-card
  [{:keys [obj-kw card-pos
           is-fk-side? fk-attribute
           can-take-fk? on-take-fk
           on-card-change on-fk-change]}]
  (let [pk        (pk-attr-of obj-kw)
        attrs     (link-fk-attrs obj-kw)
        no-type?  (not obj-kw)
        show-fk?  (boolean is-fk-side?)
        no-attrs? (and show-fk? (not no-type?) (empty? attrs))
        accent    (if show-fk? "var(--c-primary)" "var(--c-success)")
        bg        (if show-fk? "var(--c-primary-bg)" "var(--c-success-bg)")]
    [:div {:class "form-endpoint-card" :style {:border (str "1px solid " accent) :background bg}}
     [type-pill obj-kw]
     [form/field {:label (i18n/t :ui/cardinality)}
      [:div {:class "u-flex-gap-6"}
       [one-many-radio {:pos :one
                        :selected? (= card-pos :one)
                        :on-change #(on-card-change :one)}]
       [one-many-radio {:pos :many
                        :selected? (= card-pos :many)
                        :on-change #(on-card-change :many)}]]]
     (cond
       no-type?
       [:div {:class "form-hint"}
        (i18n/t :ui/pick-endpoint-type)]

       show-fk?
       [form/field {:label (i18n/t :ui/foreign-key)}
        (if no-attrs?
          [:div {:class "form-empty--error"}
           (i18n/t :ui/link-fk-no-properties)]
          [inputs/select-input (via (some-> fk-attribute keyword) on-fk-change)
           {:placeholder (i18n/t :ui/select-attribute)
            :strict?     true
            :class       "bp-fill"
            :items       (for [{a :name t :type} attrs]
                           {:value (keyword a)
                            :label (str (name a) (when t (str " · " (name t))))})}])]

       :else
       [:div nil
        [:div {:class "form-endpoint-card__label"}
         (i18n/t :ui/primary-key)]
        (if pk
          [:div {:class "u-mono-12" :style {:color "var(--c-text)"}} (clojure.core/name pk)]
          [:div {:class "form-hint"} (i18n/t :ui/link-pk-undeclared)])
        (when can-take-fk?
          [ui/button {:small    true
                      :outlined true
                      :intent   :primary
                      :on-click (fn [e] (.preventDefault e) (on-take-fk))}
           (i18n/t :ui/put-fk-here)])])]))

(defn- cardinality-phrase
  [pos type-kw]
  (let [n (if type-kw (oc/type-label type-kw (= pos :many)) "—")]
    (i18n/t (if (= pos :one) :ui/cardinality-one :ui/om-cardinality-many) {:type n})))

(defn link-sentence
  [from to cardinality]
  (let [[from-pos to-pos] (cardinality->pair cardinality)]
    (str (i18n/t :ui/cardinality-summary
                 {:a (cardinality-phrase :one from) :b (cardinality-phrase to-pos to)})
         "  ·  "
         (i18n/t :ui/cardinality-summary
                 {:a (cardinality-phrase :one to) :b (cardinality-phrase from-pos from)}))))

(defn side-phrase
  [cardinality dir far]
  (let [[from-pos to-pos] (cardinality->pair cardinality)]
    (cardinality-phrase (if (= dir :to) to-pos from-pos) far)))

(defn- join-table-block
  [{:keys [form-atom]}]
  (let [{:keys [external]} @form-atom
        ds-list (datasource-options)
        ext     external
        ds-key  (:datasource ext)
        table   (:table ext)
        tables  (when ds-key (get-in @data/ds-introspection [:tables ds-key]))
        set-ext! (fn [k v] (fs/edit! form-atom assoc-in [:external k] v))]
    (when ds-key
      (data/ensure-ds-tables! ds-key)
      (when (= :table (:source-kind ext))
        (data/ensure-ds-columns! ds-key table)))
    [:div {:class "form-jointable"}
     [:div {:class "form-jointable__title"}
      (i18n/t :ui/join-table-title)]
     [form/field {:label (i18n/t :ui/datasource)}
      (if (empty? ds-list)
        [:div {:class "form-empty--error"}
         (i18n/t :ui/datasources-none)
         (when (feature/command :manage-datasources)
           [:a {:href "#"
                :on-click (fn [e]
                            (.preventDefault e)
                            (feature/command! :manage-datasources))
                :class "u-underline" :style {:color "var(--c-primary)"}}
            (i18n/t :ui/datasources-manage)])]
        [inputs/select-input
         (via ds-key (fn [v]
                       (fs/edit! form-atom update :external
                                 assoc :datasource v :table nil
                                 :from-key-column nil :to-key-column nil)))
         {:strict?    true
          :clearable? false
          :class      "form-link__select"
          :items      (ds-items ds-list)}])]
     [ui/radio-group
      {:class "form-radio-row"
       :selected-value (some-> (:source-kind ext) name)
       :on-change #(if (= "query" (.. % -target -value))
                     (fs/edit! form-atom update :external assoc :source-kind :query :table nil)
                     (fs/edit! form-atom update :external assoc :source-kind :table :query nil))
       :options [{:value "table" :label (i18n/t :ui/table)}
                 {:value "query" :label (i18n/t :ui/sql-query)}]}]
     (if (= :table (:source-kind ext))
       (cond
         (nil? tables)
         (if ds-key
           [busy/block {:class "form-hint--italic"} (i18n/t :ui/tables-loading)]
           [:div {:class "form-hint--italic"} (i18n/t :ui/pick-datasource-first)])
         (empty? tables)
         [:div {:class "form-hint--italic"}
          (i18n/t :ui/tables-none-user)]
         :else
         [inputs/select-input
          (via table (fn [v]
                       (fs/edit! form-atom update :external
                                 assoc :table (or v "")
                                 :from-key-column nil :to-key-column nil)))
          {:placeholder (i18n/t :ui/select-table)
           :strict?     true
           :class       "form-link__select"
           :items       (named tables)}])
       [ui/text-area {:value (or (:query ext) "")
                      :placeholder (i18n/t :ui/join-query-placeholder)
                      :on-change #(set-ext! :query (.. % -target -value))
                      :rows 2
                      :fill true
                      :class ["bp-monospace-text" "form-link__textarea"]}])

     (for [[k label] [[:from-key-column :ui/from-key-column-hint]
                      [:to-key-column :ui/to-key-column-hint]]]
       ^{:key k}
       [form/field {:label (i18n/t label)}
        [column-select
         {:ds-key ds-key :table table
          :value (some-> (get ext k) name)
          :placeholder (i18n/t :ui/select-column)
          :extra-style {:width "100%" :flex 1}
          :on-change #(set-ext! k (when (seq %) (keyword %)))}]])]))

(defn- link-mapping-section
  [{:keys [form-atom from-kw to-kw]}]
  (let [m            @form-atom
        ext          (:external m)
        cardinality  (some-> (:cardinality m) keyword)
        [from-pos to-pos] (cardinality->pair cardinality)
        m-m?         (and (= from-pos :many) (= to-pos :many))
        both-one?    (and (= from-pos :one) (= to-pos :one))
        fk-side      (cond
                       m-m?      nil
                       both-one? (or (#{:from :to} (:fk-side ext)) :from)
                       :else     (derive-fk-side from-pos to-pos (:fk-side ext)))
        fk-attr      (:fk-attribute ext)
        from-fk-active? (= fk-side :from)
        to-fk-active?   (= fk-side :to)
        suggestion   (when (and fk-side (nil? fk-attr) (not (:fk-touched m))
                                (not= :in-system (:backing-mode m))
                                from-kw to-kw)
                       (let [[fk-t pk-t] (if (= fk-side :from) [from-kw to-kw] [to-kw from-kw])]
                         (ltf/suggest-fk @catalog/schema @catalog/canonical-schema fk-t pk-t)))
        _            (when suggestion
                       (fs/edit! form-atom update :external
                                 #(assoc % :fk-side fk-side :fk-attribute suggestion)))
        take-fk!     (fn [side]
                       (fs/edit! form-atom
                                 #(-> %
                                      (dissoc :fk-touched)
                                      (update :external assoc :fk-side side :fk-attribute nil))))
        set-card!    (fn [side pos]
                       (let [new-pair (case side
                                        :from [pos to-pos]
                                        :to   [from-pos pos])
                             new-card (pair->cardinality new-pair)
                             new-side (derive-fk-side (first new-pair)
                                                      (second new-pair)
                                                      (:fk-side ext))]
                         (fs/edit! form-atom
                                   #(-> %
                                        (assoc :cardinality (some-> new-card name))
                                        (update :external
                                                (fn [e]
                                                  (cond-> (assoc e :fk-side new-side)
                                                    (not= (:fk-side e) new-side)
                                                    (assoc :fk-attribute nil))))))))
        set-fk-attr! (fn [side a]
                       (fs/edit! form-atom
                                 #(-> %
                                      (assoc :fk-touched true :backing-mode :synced)
                                      (update :external assoc :fk-attribute a :fk-side side))))
        full-summary
        (when (and from-kw to-kw from-pos)
          (str (i18n/t :ui/cardinality-summary
                       {:a (cardinality-phrase :one from-kw)
                        :b (cardinality-phrase to-pos to-kw)})
               "  ·  "
               (i18n/t :ui/cardinality-summary
                       {:a (cardinality-phrase :one to-kw)
                        :b (cardinality-phrase from-pos from-kw)})))]
    [:div nil
     [:div {:class "form-mapping__cards"}
      [endpoint-card
       {:obj-kw         from-kw
        :card-pos       from-pos
        :is-fk-side?    from-fk-active?
        :fk-attribute   (when (= fk-side :from) fk-attr)
        :can-take-fk?   (and both-one? (not from-fk-active?))
        :on-take-fk     #(take-fk! :from)
        :on-card-change #(set-card! :from %)
        :on-fk-change   #(set-fk-attr! :from %)}]
      [:div {:class "form-mapping__arrow"}
       "→"]
      [endpoint-card
       {:obj-kw         to-kw
        :card-pos       to-pos
        :is-fk-side?    to-fk-active?
        :fk-attribute   (when (= fk-side :to) fk-attr)
        :can-take-fk?   (and both-one? (not to-fk-active?))
        :on-take-fk     #(take-fk! :to)
        :on-card-change #(set-card! :to %)
        :on-fk-change   #(set-fk-attr! :to %)}]]
     (when m-m?
       [join-table-block {:form-atom form-atom}])
     (when full-summary
       [:div {:class "form-mapping__summary"}
        full-summary])]))

(defn link-external-section
  [{:keys [form-atom from-kw to-kw]}]
  (let [form     @form-atom
        backing  (or (:backing-mode form) :synced)
        m-m?     (= "many-to-many" (some-> (:cardinality form) name))
        producer (get-in form [:loaded :backing :producer])
        set-backing! (fn [m] (fs/edit! form-atom assoc :backing-mode m))]
    (when (and m-m? (= backing :synced) (nil? (:datasource (:external form))))
      (let [ds-list (datasource-options)]
        (fs/edit! form-atom update :external merge
                  {:source-kind :table :datasource (some-> ds-list first first)})))
    [:div {:class "form-backing" :style {:border (str "1px solid " "var(--c-primary)")}}
     [:div {:class "form-backing__title" :style {:color "var(--c-primary)"}}
      (i18n/t :ui/link-backing-question)]
     (if (= backing :producer)
       [:div {:class "ontology-dataset__intro"}
        (i18n/t :ui/link-backing-producer {:producer (some-> producer name)})]
       [:<>
        [ui/segmented-control
         {:fill true :intent :primary :class "u-mb-8" :value (if m-m? :join :fk)
          :on-value-change
          (fn [v]
            (cond
              (and (= v :fk) m-m?)
              (fs/edit! form-atom
                        (fn [f]
                          (-> f
                              (assoc :cardinality "many-to-one" :backing-mode :synced)
                              (dissoc :fk-touched)
                              (update :external assoc :fk-side :from :fk-attribute nil))))

              (and (= v :join) (not m-m?))
              (fs/edit! form-atom
                        (fn [f]
                          (assoc f :cardinality "many-to-many" :backing-mode :synced)))))
          :options [{:value :fk :label (i18n/t :ui/link-rel-fk)
                     :title (i18n/t :ui/link-rel-fk-hint)}
                    {:value :join :label (i18n/t :ui/link-rel-join)
                     :title (i18n/t :ui/link-rel-join-hint)}]}]
        (when (and m-m? (= :in-system (get-in form [:loaded :mode])))
          [ui/radio-group
           {:class "form-radio-row"
            :selected-value (some-> backing name)
            :on-change #(set-backing! (keyword (.. % -target -value)))
            :options [{:value "in-system" :label (i18n/t :ui/link-join-in-system)}
                      {:value "synced" :label (i18n/t :ui/link-join-from-source)}]}])

        (cond
          (and m-m? (= backing :in-system))
          [:div {:class "ontology-dataset__intro"}
           (i18n/t :ui/link-backing-in-system-note)]

          :else
          [:<>
           (when (and (not m-m?) (= backing :in-system))
             [:div {:class "ontology-dataset__intro"}
              (i18n/t :ui/link-legacy-in-system)])
           [:div {:class "form-backing__hint"}
            (i18n/t :ui/cardinality-hint)]
           [link-mapping-section
            {:form-atom form-atom :from-kw from-kw :to-kw to-kw}]])])]))

(def ^:private flip-cardinality
  {"one-to-many" "many-to-one"
   "many-to-one" "one-to-many"
   "one-to-one"  "one-to-one"
   "many-to-many" "many-to-many"})

(def link-side-keys [:forward-api-name :reverse-api-name :forward-name :reverse-name])

(defn link-side-fields
  [form]
  (into {} (filter (comp seq val)) (select-keys form link-side-keys)))

(defn- link-side-inputs
  [form-atom near far api-k name-k]
  (let [nm #(if (seq %) % "…")]
    [form/field
     {:label (i18n/t :ui/link-side-to {:far (nm far) :near (nm near)}) :class "u-flex-1"}
     [inputs/text-input (fs/at form-atom name-k)
      {:placeholder (i18n/t :ui/link-side-display-name) :class "u-mb-4"}]
     [inputs/text-input (fs/at form-atom api-k)
      {:placeholder (str (i18n/t :ui/link-side-api-name) " · "
                         (or (not-empty (:name @form-atom)) "…"))}]]))

(defn- link-satisfies-section
  [form-atom]
  (let [{:keys [from to cardinality satisfies]} @form-atom
        opts   (when (and (seq from) (seq to))
                 (ltf/constraint-options @catalog/schema (keyword from) (keyword to)
                                         (some-> cardinality keyword)))
        chosen (set satisfies)
        known  (set (map (juxt :interface :constraint) opts))
        stale  (remove known chosen)
        toggle! (fn [k] (fs/edit! form-atom update :satisfies
                                  (fn [s]
                                    (let [s (set s)] (if (contains? s k) (disj s k) (conj s k))))))]
    (when (or (seq opts) (seq stale))
      [form/field {:label (i18n/t :ui/link-satisfies)}
       [:div {:class [style/small-muted "u-mb-4"]}
        (i18n/t :ui/link-satisfies-hint {:type from})]
       (for [{:keys [interface constraint target cardinality required fits? why]} opts
             :let [k [interface constraint]
                   on? (contains? chosen k)]]
         ^{:key (str k)}
         [:div {:class "form-radio-row"}
          [ui/checkbox
           {:checked on?
            :disabled (and (not fits?) (not on?))
            :on-change #(toggle! k)
            :label (str (name interface) " · " (name constraint) " → "
                        (some-> target name) " (" (some-> cardinality name) ")"
                        (when required (str " · " (i18n/t :ui/link-constraint-required))))}
           (when-not fits?
             [:span {:class "form-hint"} " "
              (i18n/t (if (= why :target) :ui/link-satisfies-no-target :ui/link-satisfies-card)
                      {:cardinality (some-> cardinality name)})])]])
       (for [[i c :as k] stale]
         ^{:key (str k)}
         [:div {:class "form-radio-row"}
          [ui/checkbox {:checked true :on-change #(toggle! k)
                        :label (str (name i) " · " (name c))}
           [:span {:class "form-hint"} " " (i18n/t :ui/link-satisfies-no-target)]]])])))

(defn link-form [{:keys [form-atom from-locked? to-locked?
                         on-create on-cancel save-label]}]
  (let [{cfrom :from cto :to} @form-atom
        from-kw (when (seq cfrom) (keyword cfrom))
        to-kw   (when (seq cto)   (keyword cto))
        obj-keys (sort (map name (keys (:objects @catalog/schema))))
        endpoint-select (fn [val locked? on-change]
                          [inputs/select-input (via val #(on-change (or % "")))
                           {:placeholder (i18n/t :ui/select-dashes)
                            :strict?     true
                            :disabled?   locked?
                            :class       "bp-fill"
                            :items       (for [k obj-keys]
                                           {:value k :label (oc/type-label (keyword k))})}])
        set-endpoint! (fn [k v]
                        (fs/edit! form-atom
                                  #(-> %
                                       (assoc k v)
                                       (dissoc :fk-touched)
                                       (update :external dissoc :fk-attribute))))]
    [:div {:class "form-root"}
     [:div {:class "form-row--end"}
      [form/field {:label (i18n/t :ui/from) :class "u-flex-1"}
       (endpoint-select cfrom from-locked? #(set-endpoint! :from %))]
      [ui/tooltip {:content (i18n/t :ui/swap-direction)}
       [ui/button {:minimal true :icon :swap-horizontal
             :on-click (fn [_]
                         (fs/edit! form-atom
                                   (fn [m]
                                     (let [ext (:external m)
                                           ext' (case (:fk-side ext)
                                                  :from (assoc ext :fk-side :to)
                                                  :to   (assoc ext :fk-side :from)
                                                  ext)]
                                       (-> m
                                           (assoc :from (:to m)
                                                  :to   (:from m)
                                                  :forward-api-name (:reverse-api-name m)
                                                  :reverse-api-name (:forward-api-name m)
                                                  :forward-name (:reverse-name m)
                                                  :reverse-name (:forward-name m))
                                           (update :cardinality
                                                   #(get flip-cardinality % %))
                                           (cond-> ext' (assoc :external ext')))))))
             :aria-label (i18n/t :ui/swap-direction)}]]
      [form/field {:label (i18n/t :ui/to) :class "u-flex-1"}
       (endpoint-select cto to-locked? #(set-endpoint! :to %))]]
     [link-external-section
      {:form-atom form-atom :from-kw from-kw :to-kw to-kw}]
     [form/field {:label (i18n/t :ui/name)}
      [inputs/text-input (fs/at form-atom :name)
       {:placeholder (i18n/t :ui/link-name-example)}]]
     [:div {:class "u-mb-8"}
      [:div {:class [style/small-muted "u-mb-4"]} (i18n/t :ui/link-side-hint)]
      [:div {:class "form-row--end"}
       [link-side-inputs form-atom cfrom cto :forward-api-name :forward-name]
       [link-side-inputs form-atom cto cfrom :reverse-api-name :reverse-name]]]
     [link-satisfies-section form-atom]
     [:div {:class "u-flex-gap-8"}
      [overlay/form-actions {:on-cancel on-cancel :on-ok on-create
                             :ok-label  (or save-label (i18n/t :ui/create))}]]]))

(defn submit-link!
  [form-atom create? on-done]
  (let [{:keys [spec error args]} (ltf/form->spec @form-atom @catalog/canonical-schema
                                                  ext->link-external-source)]
    (if error
      (shell/fail! (if args (i18n/t error args) (i18n/t error)))
      (db/send! [:shared.forms/stage-link spec create? on-done]))))

(defn load-link-form!
  [conn-id]
  (when-let [{nm :name :as conn}
             (some #(when (= (catalog/link-id %) conn-id) %)
                   (:links @catalog/schema))]
    (let [form (ltf/load-form @catalog/schema @catalog/canonical-schema nm)
          es   (:external-source conn)]
      (fs/set-form! :edit-link
                    (cond-> form
                      (and es (nil? (:external form)))
                      (assoc :external (ext-source->form-external es))))
      conn)))

(defn open-link-detail! [conn-id]
  (when-let [conn (load-link-form! conn-id)]
    (kompass/note-recent! {:kind :link-type :id conn-id
                           :label (clojure.core/name (:name conn))})
    (shell/close-detail-modal!)
    (catalog/open-resource! :link-types conn-id)))

(defn open-edit-link! [conn-id]
  (when (load-link-form! conn-id)
    (shell/open-detail-modal! :link-type conn-id)))
