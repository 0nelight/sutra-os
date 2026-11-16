(ns sutra-ui.apps.ontology.ot-view
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.state.forms :as fs]
            [reagent.ratom :as ratom]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.filter :as ui-filter]
            [sutra.kit.slug :refer [slug]]
            [sutra-ui.ui.core :as ui :refer [divider]]
            [sutra-ui.ui.table :as table]
            [clojure.string :as str]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.staging :as staging]
            [sutra.kit.remote :as rd]
            [sutra-ui.apps.ontology.fields :refer [via pick pairs]]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.ui.form :as ui-form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.forms.base :as forms]
            [sutra-ui.forms.external :as ext-forms]
            [sutra-ui.forms.group :as group-forms]
            [sutra-ui.forms.link :as link-forms]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.forms.property :as prop-forms]
            [sutra-ui.object.card :as oc]
            [sutra.types.value-types :as vt]
            [sutra.i18n.core :as i18n]))

(defn- t
  ([k] (i18n/t (keyword "ui" (str "otv-" (name k)))))
  ([k m] (i18n/t (keyword "ui" (str "otv-" (name k))) m)))

(def ^:private form forms/edit-object-form)

(def ^:private baseline (ratom/make-reaction #(:otv-baseline @ont-model/ui)))

(def ^:private otv (ratom/make-reaction #(:otv @ont-model/ui)))

(def ^:private send! ont-model/send!)

(def ^:private edit! ont-model/edit!)

(defn- clean [m] (dissoc m :selected-prop :attempted))

(defn- dirty? [] (and @baseline (not= (clean @form) (clean @baseline))))

(defn- load! [k] (send! [:ontology.otv/load k]))

(defn- ensure-loaded! [k]
  (let [known? (contains? (:object-types @catalog/canonical-schema) k)]
    (when (or (not= k (:loaded-as @baseline))
              (and known? (not (:known? @otv)) (not (dirty?))))
      (send! [:ontology.otv/load k known?]))))

(defn- blank? [v]
  (or (nil? v) (and (string? v) (str/blank? v)) (and (coll? v) (empty? v))))

(defn- tname [t] (if (or (string? t) (keyword? t)) (name t) (pr-str t)))

(defn- fid [f] (str/trim (str (:name f))))

(defn- fdisplay [f] (or (not-empty (get-in f [:opts :name])) (fid f)))

(defn- field-by-id [m id] (some #(when (= id (fid %)) %) (:fields m)))

(defn- synced? [m] (= :synced (:backing-mode m)))

(defn- pk [m]
  (some-> (or (get-in m [:external :primary-key]) (get-in m [:extra :primary-key])) name))

(defn- with-pk [m id]
  (let [kw (some-> id not-empty keyword)]
    (cond-> m
      (synced? m)
      (assoc-in [:external :primary-key] kw)
      (or (not (synced? m)) (contains? (:extra m) :primary-key))
      (update :extra #(if kw (assoc % :primary-key kw) (dissoc % :primary-key))))))

(defn- status [m] (or (some-> (get-in m [:extra :status]) name) "experimental"))

(defn- with-extra [m k v]
  (update m :extra #(if (blank? v) (dissoc % k) (assoc % k v))))

(defn- set-opt [f k v]
  (if-let [o (ontology/clean-prop-opts (if (blank? v) (dissoc (:opts f) k) (assoc (:opts f) k v)))]
    (assoc f :opts o)
    (dissoc f :opts)))

(defn- set-format [f k v]
  (let [fm (get-in f [:opts :format])
        fm (cond-> (if (blank? v) (dissoc fm k) (assoc fm k v))
             (and (= k :kind) (not= v :currency)) (dissoc :currency)
             (and (= k :kind) (not= v :unit))     (dissoc :unit))]
    (set-opt f :format (not-empty fm))))

(defn- update-props! [ids f]
  (fs/edit! form update :fields (fn [fs] (mapv #(if (contains? ids (fid %)) (f %) %) fs))))

(defn- rename-prop! [old new]
  (fs/edit! form
            (fn [m]
              (let [okw (keyword old) nkw (keyword new)
                    col (get-in m [:external :column-map okw])]
                (cond-> (update m :fields (fn [fs] (mapv #(if (= old (fid %)) (assoc % :name new) %)
                                                         fs)))
                  (= old (:title m)) (assoc :title new)
                  (= old (pk m))     (with-pk new)
                  col (update-in [:external :column-map] #(-> % (dissoc okw) (assoc nkw col)))))))
  (edit! [:otv] update :sel #(if (contains? % old) (-> % (disj old) (conj new)) %)))

(defn- remove-props! [ids]
  (fs/edit! form
            (fn [m]
              (cond-> (update m :fields #(vec (remove (fn [f] (contains? ids (fid f))) %)))
                (:external m) (update-in [:external :column-map]
                                         #(apply dissoc % (map keyword ids)))
                (contains? ids (:title m)) (assoc :title nil)
                (contains? ids (pk m))     (with-pk nil))))
  (edit! [:otv] update :sel #(apply disj % ids))
  (edit! [:otv] update :checked #(apply disj % ids)))

(defn- fresh-id [m]
  (let [taken (set (map fid (:fields m)))]
    (first (remove taken (cons "new-property" (map #(str "new-property-" %) (range 2 999)))))))

(defn- add-prop! []
  (let [id (fresh-id @form)]
    (fs/edit! form update :fields (fnil conj []) {:name id :type "string"
                                                  :opts {:name (t :new-property)}})
    (edit! [:otv] assoc :sel #{id} :page :editor)))

(def ^:private palette
  ["var(--blue3)" "var(--green3)" "var(--orange3)" "var(--red3)" "var(--violet3)"
   "var(--turquoise3)" "var(--rose3)" "var(--lime3)" "var(--sepia3)" "var(--cerulean3)"])

(defn- color-of [k]
  (palette (mod (reduce (fn [h c] (mod (+ (* 31 h) (.charCodeAt c 0)) 1000003)) 7 (name k))
                (count palette))))

(def ^:private icon-glyphs
  ["⬢" "⚠" "✈" "☺" "⚑" "★" "⚙" "✉" "☎" "⌂" "⛟" "⚓" "✚" "☂" "♜" "⚡" "▣" "◉" "✎" "€"])

(defn- type-glyph [t]
  (case (tname t)
    ("string" "text" "keyword" "uuid" "cipher")                          "❞"
    ("integer" "short" "long" "byte" "float" "double" "decimal" "number") "#"
    "boolean"                                                            "◐"
    "date"                                                               "▦"
    ("timestamp" "instant")                                              "◷"
    ("geopoint" "geoshape")                                              "⌖"
    ("attachment" "media-reference")                                     "⎘"
    "{}"))

(defn- show [v]
  (cond
    (blank? v)   "—"
    (keyword? v) (name v)
    (coll? v)    (str/join ", " (map #(if (keyword? %) (name %) (str %)) v))
    (true? v)    (t :yes)
    :else        (str v)))

(defn- tag [kind label]
  [ui/tag {:minimal true
           :intent  (case kind :pk :primary (:title :success) :success
                          :danger :danger :primary :primary nil)}
   label])

(defn- pill-select [{:keys [value options on-change kind disabled? title]}]
  [ui/tooltip {:content title}
   [:span
    [inputs/select-input nil
     {:class      (str "bp-minimal otv-pill--" (name kind))
      :value      value
      :disabled?  disabled?
      :clearable? false
      :strict?    true
      :on-change  (fn [v _] (on-change v))
      :items      (for [[v label] options] {:value v :label label})}]]])

(defn- editable [{:keys [value on-change placeholder class multiline? disabled?]}]
  [(if multiline? :textarea :input)
   (cond-> {:class (str "otv-editable " class) :value (or value "") :placeholder placeholder
            :disabled disabled? :on-change #(on-change (.. % -target -value))}
     multiline? (assoc :rows 2))])

(defn- type-icon [k glyph size]
  [:div {:class "otv-type-icon" :style {:background (color-of k) :width size :height size
                                        :font-size (* 0.5 size)}}
   glyph])

(defn- card [{:keys [title action on-action footer info class]} & body]
  [ui/section
   {:class         class
    :title         title
    :right-element (when (or info action)
                     [:<>
                      (when info
                        [ui/tooltip {:content info}
                         [:span [ui/icon {:icon :info-sign}]]])
                      (when action
                        [ui/button {:minimal true :small true :intent :primary
                                    :on-click on-action}
                         action])])}
   (into [ui/section-card {:class "otv-card__body"}] body)
   (when footer
     [ui/section-card {:padded false}
      [ui/button {:minimal true :intent :primary :icon :plus :fill true
                  :align-text :left :on-click (:on-click footer)}
       (:label footer)]])])

(def ^:private status-opts
  [["active" "Active"] ["experimental" "Experimental"] ["deprecated" "Deprecated"]
   ["example" "Example"]])

(def ^:private visibility-opts
  [["prominent" "Prominent"] ["normal" "Normal"] ["hidden" "Hidden"]])

(def ^:private meta-keys
  [[:display-name :name] [:plural :plural-name] [:description :description]
   [:aliases :aliases] [:symbol :icon] [:visibility :visibility] [:title :title-key]
   [:implements :interfaces] [:resolve-entity :entity-resolution]])

(defn- prop-diff [a b]
  (let [ka (assoc (:opts a) :type (tname (:type a)))
        kb (assoc (:opts b) :type (tname (:type b)))]
    (str/join "; " (for [k (sort (distinct (concat (keys ka) (keys kb))))
                         :when (not= (get ka k) (get kb k))]
                     (str (name k) ": " (show (get ka k)) " → " (show (get kb k)))))))

(defn- changes [old new]
  (let [by-id (fn [m] (into {} (map (juxt fid identity)) (:fields m)))
        o (by-id old) n (by-id new)]
    (vec
     (concat
      (for [[k lbl] meta-keys :when (not= (show (get old k)) (show (get new k)))]
        {:label (t lbl) :from (show (get old k)) :to (show (get new k))})
      (when (not= (status old) (status new))
        [{:label (t :status) :from (status old) :to (status new)
          :warn (when (= "active" (status old)) :status)}])
      (when (not= (pk old) (pk new))
        [{:label (t :primary-key) :from (show (pk old)) :to (show (pk new)) :warn :pk}])
      (for [[k lbl] [[:markings :markings] [:project :project]
                     [:requires-action-log :action-log]]
            :when (not= (get-in old [:extra k]) (get-in new [:extra k]))]
        {:label (t lbl) :from (show (get-in old [:extra k])) :to (show (get-in new [:extra k]))})
      (when (or (not= (:backing-mode old) (:backing-mode new))
                (not= (dissoc (:external old) :column-map :primary-key)
                      (dissoc (:external new) :column-map :primary-key)))
        [{:label (t :backing-datasource) :from (show (some-> (:backing-mode old) name))
          :to (show (some-> (:backing-mode new) name)) :warn :backing}])
      (for [id (keys n) :when (not (o id))]
        {:label (t :property-added {:id id}) :to (tname (:type (n id)))})
      (for [id (keys o) :when (not (n id))]
        {:label (t :property-deleted {:id id}) :warn :prop-deleted})
      (for [id (keys n)
            :let [a (o id) b (n id)
                  ca (get-in old [:external :column-map (keyword id)])
                  cb (get-in new [:external :column-map (keyword id)])]
            :when (and a (or (not= a b) (not= ca cb)))]
        {:label (t :property-changed {:id id})
         :to (str/join "; " (remove str/blank?
                                    [(prop-diff a b)
                                     (when (not= ca cb)
                                       (str (t :column) ": " (show ca) " → " (show cb)))]))
         :warn (when (not= (tname (:type a)) (tname (:type b))) :type)})))))

(defn- errors [m]
  (cond-> []
    (object-forms/object-form-problem m false nil)
    (conj (i18n/t (object-forms/object-form-problem m false nil)))
    (blank? (:title m))                     (conj (t :err-title))))

(defn- staged-label [i]
  (str (or (:verb i) "") " "
       (or (some-> (get-in i [:args :name]) name) (some-> (:type-name i) name) (:type i))))

(defn- save! [k] (send! [:ontology.otv/save k (boolean (dirty?))]))

(defn- discard! [k]
  (send! [:ontology.otv/discard k {:title (t :discard-title) :body (t :discard-body)
                                   :danger? true :confirm-label (t :discard)}]))

(defn- change-row [{:keys [label from to warn]}]
  [:div {:class (str "otv-change" (when warn " otv-change--warn"))}
   [:div {:class "otv-change__label"} (when warn [:span {:class "otv-change__warn"} "⚠"]) label]
   (when (or from to)
     [:div {:class "otv-change__diff"}
      (when from [:span {:class "otv-change__from"} from])
      (when (and from to) " → ")
      (when to [:span {:class "otv-change__to"} to])])
   (when warn [:div {:class "otv-change__hint"} (t (keyword (str "warn-" (name warn))))])])

(defn- review-dialog [k]
  (let [old @baseline new @form
        cs  (changes old new)
        es  (errors new)
        ws  (filter :warn cs)
        stg (staging/staged-list)
        tab (or (:review-tab @otv) :all)
        typed (:confirm-text @otv)
        label (oc/type-label k)
        need-confirm? (seq ws)
        can-save? (and (empty? es) (or (seq cs) (seq stg))
                       (or (not need-confirm?) (= typed label)))]
    [overlay/modal {:on-close #(edit! [:otv] assoc :review? false)
                    :class "otv-modal otv-review"
                    :title (t :review-edits)
                    :foot [:<>
                           [ui/button {:on-click #(edit! [:otv] assoc :review? false)}
                            (t :cancel)]
                           [ui/button {:intent :success :disabled (not can-save?)
                                       :on-click #(save! k)}
                            (t :save)]]
                    :foot-end? true}
     [:<>
      [ui/tabs {:id :otv-review :class "otv-tabs" :selected-tab-id tab
                :on-change (fn [id] (edit! [:otv] assoc :review-tab id))}
       (for [[id n] [[:all (+ (count cs) (count stg))] [:errors (count es)]
                     [:warnings (count ws)]]]
         [ui/tab {:id id :title (t id)}
          [ui/tag {:round true :minimal true
                   :intent ({:errors :danger :warnings :warning} id)} n]])]
      [:div {:class "otv-review__body"}
       (when (and (#{:all :errors} tab) (seq es))
         [:div {:class "otv-review__group"}
          (for [e es] ^{:key e} [:div {:class "otv-change otv-change--error"} "✕ " e])])
       (when (seq (if (= tab :warnings) ws (when (= tab :all) cs)))
         [:div {:class "otv-review__entity"}
          [:div {:class "otv-review__entity-head"}
           [type-icon k (or (not-empty (:symbol new)) "⬢") 22]
           [:span label] [:span {:class "otv-review__kind"} (t :object-type)]
           [ui/tooltip {:content (t :discard)}
            [ui/button
             {:minimal true :small true :icon :trash :on-click #(load! k)
              :aria-label (t :discard)}]]]
          (for [[i c] (map-indexed vector (if (= tab :warnings) ws cs))]
            ^{:key i} [change-row c])])
       (when (and (= tab :all) (seq stg))
         [:div {:class "otv-review__entity"}
          [:div {:class "otv-review__entity-head"} [:span (t :staged-elsewhere)]]
          (for [i stg]
            ^{:key (:seq i)}
            [:div {:class "otv-change"}
             [:div {:class "otv-change__label"} (staged-label i)]
             [ui/tooltip {:content (t :discard)}
              [ui/button
               {:minimal true :small true :icon :trash
                :on-click #(send! [:ontology.staged/discard (:seq i)])
                :aria-label (t :discard)}]]])])
       (when (and (empty? cs) (empty? stg) (empty? es))
         [:div {:class "otv-empty"} (t :no-changes)])]
      (when need-confirm?
        [:div {:class "otv-review__confirm"}
         [:div (t :confirm-warnings {:name label})]
         [inputs/text-input (via typed #(edit! [:otv] assoc :confirm-text %))
          {:fill true :placeholder label}]])
      (when-let [e (:save-error @otv)] [:div {:class "otv-change otv-change--error"} e])]]))

(defn header
  [k on-close & end]
  (let [n (+ (count (changes @baseline @form)) (count (staging/staged-list)))]
    (into [page/header {:title    (oc/type-label k)
                        :subtitle [ui/breadcrumbs {:items [{:text (t :ontology) :on-click on-close}
                                                           {:text (oc/type-label k)}]}]}]
          (concat end
                  [[ui/button {:disabled (zero? n) :on-click #(discard! k)} (t :discard)]
                   [ui/button {:intent :success :disabled (zero? n)
                               :on-click #(send! [:ontology.otv/review])}
                    (t :save) (when (pos? n) [ui/tag {:round true} n])]]))))

(def ^:private pages
  [[:overview :home] [:properties :properties] [:security :shield] [:datasources :database]
   [:capabilities :lightning] [:interfaces :layers] [:usage :share]])

(defn- icon-picker [k]
  [:<>
   [:div {:class "otv-popover__title"} (t :icon)]
   [:div {:class "otv-icon-picker__grid"}
    (for [g icon-glyphs]
      ^{:key g}
      [:button {:class "otv-icon-picker__cell"
                :style {:background (if (= g (:symbol @form)) (color-of k)
                                        "var(--c-surface-soft)")
                        :color (if (= g (:symbol @form)) "var(--c-on-accent)"
                                   "var(--c-text-2)")}
                :on-click #(do (fs/edit! form assoc :symbol g) (edit! [:otv] assoc :icon? false))}
       g])]
   [inputs/text-input (via (:symbol @form) #(fs/edit! form assoc :symbol %))
    {:fill true :maxLength 4 :placeholder (t :icon-custom)}]])

(defn- more-menu [k on-close]
  (let [active? (= "active" (status @baseline))
        rid (catalog/rid-of :object-types k)]
    (into [ui/menu]
          [[ui/menu-item {:text (t :view-lineage)
                          :on-click #(feature/command! :open-in-lineage [:object-type k])}]
           (when rid
             [ui/menu-item {:text (t :copy-rid)
                            :on-click #(some-> js/navigator .-clipboard (.writeText (str rid)))}])
           [ui/menu-item {:text (t :classic-editor)
                          :on-click #(object-forms/open-edit-object! k)}]
           [ui/menu-divider]
           [ui/menu-item
            {:text (t :delete)
             :intent :danger
             :disabled active?
             :html-title (when active? (t :delete-active))
             :on-click #(send! [:ontology.otv/delete k
                                {:title (t :delete-title {:name (oc/type-label k)})
                                 :body (t :delete-body) :danger? true
                                 :confirm-label (t :delete)}
                                on-close])}]])))

(defn- back-home [k on-close]
  (let [recents (remove #(and (= :object-type (:kind %)) (= k (:id %))) (ont-model/om-recents))
        sides   (catalog/link-sides @catalog/schema k)
        back    [ui/button {:minimal true :icon :arrow-left :on-click on-close} (t :back-home)]]
    (if (or (seq recents) (seq sides))
      [ui/popover
       {:interaction-kind :hover :placement :bottom-start :minimal true
        :content
        (into [ui/menu]
              (concat
               (when (seq recents)
                 (cons [ui/menu-divider {:title (t :recently-viewed)}]
                       (for [{:keys [kind id label]} (take 6 recents)]
                         [ui/menu-item
                          {:text (or label (name id))
                           :on-click #(when (= kind :object-type)
                                        (object-forms/open-object-detail! id))}])))
               (when (seq sides)
                 (cons [ui/menu-divider {:title (t :related)}]
                       (for [{:keys [far]} (distinct (take 8 sides))]
                         [ui/menu-item
                          {:text     (oc/type-label far)
                           :on-click #(object-forms/open-object-detail! far)}])))))}
       back]
      back)))

(defn sidebar
  [k on-close]
  (let [m @form page (or (:page @otv) :overview)]
    [:<>
     [back-home k on-close]
     [ui/entity-title
      {:ellipsize true
       :icon      [ui/popover
                   {:is-open            (boolean (:icon? @otv))
                    :on-interaction     (fn [o _] (edit! [:otv] assoc :icon? o))
                    :popover-class-name "otv-popover otv-icon-picker"
                    :content            [icon-picker k]}
                   [ui/tooltip {:content (t :icon)}
                    [ui/button {:minimal true}
                     [type-icon k (or (not-empty (:symbol m)) "⬢") 40]]]]
       :title     (or (not-empty (:display-name m)) (oc/type-label k))
       :subtitle  (t :n-objects
                     {:n (.toLocaleString (get (rd/value-or @data/object-counts {}) k 0))})
       :tags      [ui/popover {:is-open            (boolean (:menu? @otv))
                               :on-interaction     (fn [o _] (edit! [:otv] assoc :menu? o))
                               :placement          :bottom-end
                               :popover-class-name "otv-popover otv-menu"
                               :content            [more-menu k on-close]}
                   [ui/button {:minimal true :small true :icon :more}]]}]
     (into [ui/menu {:large true}]
           (for [[id icon] pages
                 :let [on? (or (= id page) (and (= id :properties) (= page :editor)))]]
             ^{:key id}
             [ui/menu-item {:icon     icon
                            :active   on?
                            :text     (t id)
                            :label    (when (= id :properties) (str (count (:fields m))))
                            :on-click #(edit! [:otv] assoc :page id)}]))]))

(defn- index-status [k]
  (case (:build-status (lineage-status/node-status :object-type k))
    ("succeeded" "success") [tag :success "Success"]
    ("failed" "error")      [tag :danger "Failed"]
    ("running" "queued")    [tag :primary "Running"]
    [tag :neutral "Not started"]))

(defn- metadata-card [k {:keys [actions]}]
  (let [m @form
        active? (= "active" (status @baseline))
        rid (catalog/rid-of :object-types k)]
    [:section {:class "otv-card otv-meta"}
     [:div {:class "otv-meta__main"}
      [:div {:class "otv-meta__left"}
       [:div {:class "otv-meta__row2"}
        [:div
         [:div {:class "otv-label"} (t :name)]
         [editable {:value (:display-name m) :placeholder (oc/type-label k)
                    :class "otv-editable--h"
                    :on-change #(fs/edit! form assoc :display-name %)}]]
        [:div
         [:div {:class "otv-label"} (t :plural-name)]
         [editable {:value (:plural m) :placeholder (t :plural-name)
                    :class "otv-editable--h"
                    :on-change #(fs/edit! form assoc :plural %)}]]]
       [:div {:class "otv-label"} (t :description)]
       [editable {:value (:description m) :multiline? true :placeholder (t :description-ph)
                  :on-change #(fs/edit! form assoc :description %)}]
       [:div {:class "otv-meta__row2"}
        [:div
         [:div {:class "otv-label"} (t :aliases)]
         [editable {:value (:aliases m) :placeholder (t :aliases-ph)
                    :on-change #(fs/edit! form assoc :aliases %)}]]
        [:div {:class "otv-meta__groups"}
         [:div {:class "otv-label"} (t :groups)]
         [group-forms/object-groups-chips k]]]]
      [:div {:class "otv-meta__right"}
       [:div {:class "otv-meta__kv"}
        [:span (t :status)]
        [pill-select {:kind (keyword (str "status-" (status m))) :value (status m)
                      :options status-opts
                      :on-change #(fs/edit! form with-extra :status (keyword %))}]]
       [:div {:class "otv-meta__kv"}
        [:span (t :visibility)]
        [pill-select {:kind :visibility :value (or (some-> (:visibility m) name) "normal")
                      :options visibility-opts
                      :on-change #(fs/edit! form assoc :visibility
                                            (when (not= % "normal") (keyword %)))}]]
       [divider]
       [:div {:class "otv-meta__kv"} [:span (t :index-status)] [index-status k]]
       [:div {:class "otv-meta__kv"}
        [:span (t :edits)]
        (if (seq (actions k)) [tag :success "Enabled"] [tag :neutral "Disabled"])]]]
     [:div {:class "otv-meta__ids"}
      [:span [:b (t :id)] [:code (name k)]]
      [:span [:b (t :api)]
       [ui/tooltip {:content (when active? (t :api-active))}
        [:code (:name m)]]]
      [:span {:class "otv-meta__rid"} [:b "RID"]
       [ui/tooltip {:content rid} [:code (or rid "—")]]]]]))

(defn- property-line [f {:keys [pk title]}]
  (let [id (fid f)]
    [:div {:class "otv-prop-line"
           :on-click #(edit! [:otv] assoc :page :editor :sel #{id})}
     [:span {:class "otv-prop-line__glyph"} (type-glyph (:type f))]
     [:span {:class "otv-prop-line__name"} (fdisplay f)]
     (when (= id pk) [tag :pk (t :primary-key)])
     (when (= id title) [tag :title (t :title)])]))

(defn- properties-card [_k]
  (let [m @form p (pk m) title (:title m)
        fs (sort-by #(cond (= (fid %) p) 0 (= (fid %) title) 1 :else 2) (:fields m))]
    [card {:title (str (t :properties) " (" (count fs) ")") :class "otv-card--list"
           :action (t :edit) :on-action #(edit! [:otv] assoc :page :properties)
           :footer {:label (t :create-new) :on-click add-prop!}}
     (if (empty? fs)
       [:div {:class "otv-empty"} (t :no-properties)]
       (for [[i f] (map-indexed vector fs)]
         ^{:key (str i (fid f))}
         [:<>
          [property-line f {:pk p :title title}]
          (when (and (zero? i) (= (fid f) p)) [:div {:class "otv-prop-line__rule"}])]))]))

(defn- action-glyph [spec]
  (let [op (some-> spec :rules first :op name)]
    (case op
      "create" ["+" "var(--orange4)"] ("delete" "delete-object") ["✕" "var(--rose2)"]
      ("function" "edit-function") ["ƒ" "var(--red4)"] ["✎" "var(--red3)"])))

(defn- action-types-card [k {:keys [actions new-action!]}]
  (let [acts (actions k)]
    [card {:title (str (t :action-types) " (" (count acts) ")") :class "otv-card--list"
           :info (t :action-types-info)
           :footer {:label (t :create-new) :on-click #(new-action! k)}}
     (if (empty? acts)
       [:div {:class "otv-empty"} (t :no-actions)]
       (for [[ak spec] acts
             :let [[g c] (action-glyph spec)]]
         ^{:key ak}
         [:div {:class "otv-action-line" :on-click #(catalog/open-resource! :actions ak)}
          [:span {:class "otv-action-line__icon" :style {:background c}} g]
          [:span (or (not-empty (:display-name spec)) (name ak))]]))]))

(defn- link-types-card [k {:keys [new-link!]}]
  (let [sides (catalog/link-sides @catalog/schema k)
        links (:links @catalog/schema)]
    [card {:title (str (t :link-types) " (" (count sides) ")") :class "otv-card--list"
           :footer {:label (t :create-new) :on-click #(new-link! k)}}
     (if (empty? sides)
       [:div {:class "otv-empty"} (t :no-links)]
       (for [{:keys [link dir far label]} sides
             :let [c (some #(when (= link (:name %)) %) links)]
             :when c]
         ^{:key (str (name link) dir)}
         [:div {:class    "otv-action-line"
                :on-click #(link-forms/open-link-detail! (catalog/link-id c))}
          [type-icon far (or (not-empty (get-in @catalog/canonical-schema
                                                [:object-types far :symbol])) "⬢") 22]
          [:span {:class "otv-action-line__name"} label]
          [:span {:class "otv-muted otv-action-line__sub"}
           (str (link-forms/side-phrase (:cardinality c) dir far) " · " (oc/type-label far))]]))]))

(defn- overview-page [k opts]
  [:div {:class "otv-page"}
   [metadata-card k opts]
   [:div {:class "otv-grid2"}
    [properties-card k]
    [action-types-card k opts]]
   [:div {:class "otv-grid2"}
    [link-types-card k opts]
    [(:dependents opts) k]]])

(defn- column-of [m id] (some-> (get-in m [:external :column-map (keyword id)]) name))

(defn- bulk-select
  [placeholder items on-pick]
  [inputs/select-input nil {:value nil :items items :placeholder placeholder :strict? true
                            :on-change (fn [v _] (when (some? v) (on-pick v)))}])

(defn- bulk-bar [ids]
  (let [ts (sort (distinct (map tname prop-forms/property-base-types)))]
    [:div {:class "otv-bulk"}
     [:span {:class "otv-bulk__n"} (t :n-selected {:n (count ids)})]
     [bulk-select (t :bulk-base-type) (for [x ts] {:value x :label x})
      (fn [v] (update-props! ids (fn [f] (assoc f :type v))))]
     [bulk-select (t :bulk-visibility) (pairs visibility-opts)
      (fn [v] (update-props! ids (fn [f] (set-opt f :visibility
                                                  (when (not= v "normal") (keyword v))))))]
     [bulk-select (t :bulk-status) (pairs status-opts)
      (fn [v] (update-props! ids (fn [f] (set-opt f :status (keyword v)))))]
     [bulk-select (t :bulk-render-hints)
      (for [h prop-forms/render-hint-options op ["add" "remove"]]
        {:value (str op ":" (name h)) :label (str (if (= op "add") "+ " "− ") (name h))})
      (fn [v]
        (let [[op h] (str/split v #":")]
          (update-props! ids
                         (fn [f]
                           (let [hs (set (get-in f [:opts :render-hints]))
                                 hs (if (= op "add") (conj hs (keyword h))
                                        (disj hs (keyword h)))]
                             (set-opt f :render-hints
                                      (vec (filter hs prop-forms/render-hint-options))))))))]
     [ui/button {:small true :minimal true
                 :on-click #(update-props! ids (fn [f] (set-opt f :format nil)))}
      (t :bulk-clear-format)]
     [ui/button {:small true :minimal true :intent :danger :on-click #(remove-props! ids)}
      (t :delete)]]))

(defn- properties-page [_k]
  (let [m @form p (pk m) title (:title m)
        q (str/lower-case (or (:q @otv) ""))
        fs (filter #(or (str/blank? q) (str/includes? (str/lower-case (str (fid %) " "
                                                                           (fdisplay %))) q))
                   (:fields m))
        checked (or (:checked @otv) #{})
        all? (and (seq fs) (every? #(checked (fid %)) fs))]
    [:div {:class "otv-page"}
     [page/toolbar
      [:h2 {:class "otv-h2"} (str (t :properties) " (" (count (:fields m)) ")")]
      [ui-filter/bar {:placeholder (t :filter-properties)
                   :value       (:q @otv)
                   :on-search   #(edit! [:otv] assoc :q %)}]
      [ui/button {:on-click add-prop!} "+ " (t :new-property)]
      [ui/button {:outlined true :intent :primary
                  :on-click #(edit! [:otv] assoc :page :editor)}
       (t :edit-mapping)]]
     (when (seq checked) [bulk-bar checked])
     [:div {:class "otv-card otv-table-card"}
      (let [pick! (fn [id] (edit! [:otv] update :checked
                                  (fn [s] (let [s (or s #{})]
                                            (if (s id) (disj s id) (conj s id))))))
            col   (fn [h render] {:key h :label (t h) :render render})]
        [table/data-table
         {:class        "otv-table"
          :scroll?      true
          :rows         fs
          :row-key      fid
          :on-row-click #(edit! [:otv] assoc :page :editor :sel #{(fid %)})
          :cols
          [{:key        :cb
            :cell-class "otv-table__cb"
            :label      [ui/checkbox
                         {:checked   (boolean all?) :class "u-m-0"
                          :on-change #(edit! [:otv] assoc :checked
                                             (if all? #{} (set (map fid fs))))}]
            :render     (fn [f]
                          [:span {:on-click #(.stopPropagation %)}
                           [ui/checkbox {:checked   (boolean (checked (fid f))) :class "u-m-0"
                                         :on-change #(pick! (fid f))}]])}
           (col :display-name
                (fn [f] [:<> [:span {:class "otv-prop-line__glyph"} (type-glyph (:type f))]
                         (fdisplay f)]))
           (col :id (fn [f] [:code (fid f)]))
           (col :base-type (fn [f] (tname (:type f))))
           (col :status (fn [f] (or (some-> (:opts f) :status name) "experimental")))
           (col :visibility (fn [f] (or (some-> (:opts f) :visibility name) "normal")))
           (col :keys (fn [f] (let [id (fid f)]
                                [:<> (when (= id p) [tag :pk (t :primary-key)])
                                 (when (= id title) [tag :title (t :title)])])))
           (col :column (fn [f] (if-let [c (column-of m (fid f))]
                                  [:code c]
                                  [:span {:class "otv-muted"} "—"])))]}])]]))

(defn- ds-columns [m]
  (let [ds (get-in m [:external :datasource]) tb (get-in m [:external :table])]
    (when (and (synced? m) ds (seq tb))
      (data/ensure-ds-columns! ds tb)
      (get-in @data/ds-introspection [:columns [ds tb]]))))

(defn- add-column! [c]
  (let [cn (ext-forms/col-name c)
        ot (or (when (map? c) (:ontology-type c)) "string")]
    (ext-forms/import-column! form cn ot)))

(defn- backing-dialog []
  (when (:backing? @otv)
    [overlay/modal {:on-close #(edit! [:otv] assoc :backing? false)
                    :close-on-backdrop? false
                    :class "otv-modal otv-modal--wide"
                    :title (t :backing-datasource)
                    :foot [ui/button {:intent :primary
                                      :on-click #(edit! [:otv] assoc :backing? false)}
                           (t :done)]
                    :foot-end? true}
     [:div {:class "otv-hint"} (t :backing-hint)]
     [ext-forms/object-external-section {:form-atom form}]]))

(defn- open-backing! []
  (fs/edit! form (fn [m] (if (synced? m) m
                             (assoc m :backing-mode :synced
                                    :external (or (:external m)
                                                  (ext-forms/ext-default
                                                   (ext-forms/datasource-options)))))))
  (edit! [:otv] assoc :backing? true))

(defn- datasource-pane [_k]
  (let [m @form cols (ds-columns m)
        mapped (set (map name (vals (get-in m [:external :column-map]))))
        unmapped (remove #(mapped (ext-forms/col-name %)) cols)]
    [:div {:class "otv-pe__pane otv-pe__ds"}
     [:div {:class "otv-pe__pane-head"}
      [:span (t :datasources)]
      (when (synced? m)
        [ui/button {:minimal true :small true :intent :primary :on-click open-backing!}
         (t :replace)])]
     (case (:backing-mode m)
       :synced
       [:div
        [:div {:class "otv-pe__ds-name"}
         [:span {:class "otv-pe__ds-icon"} "▦"]
         [:span (str (some-> (get-in m [:external :datasource]) name) " · "
                     (or (get-in m [:external :table]) (t :query)))]]
        (when (seq unmapped)
          [ui/button {:small true :minimal true :class "otv-pe__add-all"
                      :on-click #(doseq [c unmapped] (add-column! c))}
           (t :add-all-unmapped {:n (count unmapped)})])
        (for [c cols :let [cn (ext-forms/col-name c)]]
          ^{:key cn}
          [:div {:class (str "otv-pe__col" (when (mapped cn) " otv-pe__col--mapped"))}
           [:span {:class "otv-pe__col-name"} cn]
           [:span {:class "otv-muted"} (when (map? c) (str (:type c)))]
           (if (mapped cn)
             [:span {:class "otv-pe__col-to"}
              "→ " (some (fn [[a col]] (when (= cn (name col)) (name a)))
                         (get-in m [:external :column-map]))]
             [ui/button
              {:minimal true :small true :class "otv-pe__hover-btn" :on-click #(add-column! c)}
              (t :add-as-property)])])]
       :fed
       [:div
        [:div {:class "otv-hint"} (t :fed-by)]
        (for [x (:fed-by m)]
          ^{:key x}
          [:div {:class "otv-pe__ds-name"} [:span {:class "otv-pe__ds-icon"} "ƒ"] (name x)])]
       [ui/empty-state {:icon :database
                        :title (t :no-backing)
                        :hint (t :no-backing-hint)
                        :action [ui/button {:intent :primary :on-click open-backing!}
                                 (t :add-backing)]}])]))

(defn- map-to-column [m id cols]
  (let [cur (column-of m id)]
    [:span {:on-click #(.stopPropagation %)}
     [pick cur (for [c cols :let [cn (ext-forms/col-name c)]] {:value cn :label cn})
      (fn [v] (fs/edit! form update-in [:external :column-map]
                        (fn [cm] (if (str/blank? v) (dissoc cm (keyword id))
                                     (assoc cm (keyword id) (keyword v))))))
      {:placeholder (t :map-to-column)}]]))

(defn- properties-pane [_k]
  (let [m @form sel (or (:sel @otv) #{}) p (pk m) title (:title m)
        cols (ds-columns m)
        active-base? #(= "active" (some-> (get-in (field-by-id @baseline %) [:opts :status]) name))
        q (str/lower-case (or (:pq @otv) ""))]
    [:div {:class "otv-pe__pane otv-pe__props"}
     [:div {:class "otv-pe__pane-head"}
      [:span (str (t :properties) " (" (count (:fields m)) ")")]
      [ui/button {:small true :minimal true :on-click add-prop!} "+ " (t :add)]]
     [inputs/text-input (via (:pq @otv) #(edit! [:otv] assoc :pq %))
      {:fill true :placeholder (t :filter-properties)}]
     [:div {:class "otv-hint"} (t :multi-select-hint)]
     (for [f (:fields m)
           :let [id (fid f)]
           :when (or (str/blank? q) (str/includes? (str/lower-case (str id (fdisplay f))) q))]
       ^{:key id}
       [:div {:class (str "otv-pe__prop" (when (sel id) " otv-pe__prop--on"))
              :on-click #(edit! [:otv] assoc :sel
                                (if (or (.-metaKey %) (.-ctrlKey %))
                                  (if (sel id) (disj sel id) (conj sel id))
                                  #{id}))}
        [:span {:class "otv-prop-line__glyph"} (type-glyph (:type f))]
        [:div {:class "otv-pe__prop-names"}
         [:div (fdisplay f)
          (when (= id p) [tag :pk (t :primary-key)])
          (when (= id title) [tag :title (t :title)])]
         [:div {:class "otv-muted otv-mono"}
          id (when-let [c (column-of m id)] (str "  ← " c))]]
        [:div {:class "otv-pe__prop-actions"}
         (when (synced? m) [map-to-column m id cols])
         (when (and (synced? m) (column-of m id))
           [ui/tooltip {:content (t :unlink)}
            [ui/button
             {:minimal true :small true :class "otv-pe__hover-btn"
              :on-click #(do (.stopPropagation %)
                             (fs/edit! form update-in [:external :column-map]
                                       dissoc (keyword id)))} (t :unlink)]])
         [ui/tooltip
          {:content (if (active-base? id) (t :delete-prop-active) (t :delete-property))}
          [ui/button
           {:minimal true :small true :intent :danger :icon :trash :class "otv-pe__hover-btn"
            :disabled (active-base? id)
            :on-click #(do (.stopPropagation %) (remove-props! #{id}))
            :aria-label (if (active-base? id) (t :delete-prop-active) (t :delete-property))}]]]])]))

(def ^:private meta-tabs [:general :type :formatting :render-hints])

(defn- general-tab [id f]
  (let [m @form
        active? (= "active" (some-> (get-in (field-by-id @baseline id) [:opts :status]) name))
        ot-active? (= "active" (status @baseline))
        upd! (fn [g] (update-props! #{id} g))]
    [:div
     [ui-form/field {:label (t :display-name)}
      [inputs/text-input (via (get-in f [:opts :name]) (fn [v] (upd! #(set-opt % :name v))))
       {:fill true :placeholder id}]]
     [ui-form/field {:label (t :description)}
      [ui/text-area
       {:fill true :rows 2 :value (or (get-in f [:opts :description]) "")
        :on-change #(let [v (.. % -target -value)]
                      (upd! (fn [x] (set-opt x :description v))))}]]
     [ui-form/field {:label (t :id-api) :help (when active? (t :api-active))}
      [inputs/text-input
       (via id #(let [v (str/trim %)]
                  (when (and (seq v) (not (field-by-id m v))) (rename-prop! id v))))
       {:fill true :class "otv-mono" :disabled active?}]]
     [:div {:class "otv-field-row"}
      [ui-form/field {:label (t :status)}
       [pick (or (some-> (get-in f [:opts :status]) name) "experimental") (pairs status-opts)
        (fn [v] (upd! #(set-opt % :status (keyword v)))) {:clearable? false}]]
      [ui-form/field {:label (t :visibility)}
       [pick (or (some-> (get-in f [:opts :visibility]) name) "normal") (pairs visibility-opts)
        (fn [v] (upd! #(set-opt % :visibility (when (not= v "normal") (keyword v)))))
        {:clearable? false}]]]
     [ui-form/field {:label (t :keys)}
      [ui/checkbox {:inline true :checked (= id (pk m)) :disabled ot-active?
                    :on-change #(fs/edit! form with-pk (when (not= id (pk m)) id))
                    :label [ui/tooltip
                            {:content (when ot-active? (t :pk-active))}
                            [:span
                             (t :primary-key)]]}]
      [ui/checkbox {:inline true :checked (= id (:title m))
                    :disabled (not (prop-forms/title-key-type? (:type f)))
                    :on-change #(fs/edit! form assoc :title (when (not= id (:title m)) id))
                    :label (t :title-key)}]]]))

(defn- type-tab [id f]
  (let [vts (sort-by key (or (:value-types @catalog/canonical-schema) {}))
        cur (tname (:type f))
        ts (cond-> (vec prop-forms/property-base-types)
             (not (some #{cur} prop-forms/property-base-types)) (conj cur))
        upd! (fn [g] (update-props! #{id} g))]
    [:div
     [ui-form/field {:label (t :base-type)}
      [pick cur
       (for [x ts] {:value x :label (str x (when (#{"byte" "float" "short"} x)
                                             (str " · " (t :limited-support))))})
       (fn [v] (upd! #(assoc % :type v)))
       {:clearable? false :disabled? (not (or (string? (:type f)) (keyword? (:type f))))}]
      [:div {:class "otv-hint"} (t :base-type-hint)]]
     (when (seq vts)
       [ui-form/field {:label (t :value-type)}
        [pick (some-> (get-in f [:opts :value-type]) name)
         (for [[vk spec] vts] {:value (name vk) :label (str (name vk) " · " (tname (:type spec)))})
         (fn [s] (let [v    (some-> s not-empty keyword)
                       base (get-in @catalog/canonical-schema [:value-types v :type])]
                   (upd! #(cond-> (set-opt % :value-type v)
                            base (assoc :type (tname base))))))]])
     [ui/checkbox {:inline true :checked (boolean (get-in f [:opts :required]))
                   :on-change #(let [v (.. % -target -checked)]
                                 (upd! (fn [x] (set-opt x :required (when v true)))))
                   :label (t :required)}]]))

(defn- formatting-tab [id f]
  (let [cls (vt/class-of (keyword (tname (:type f))))
        kinds (get prop-forms/format-kinds-by-class cls)
        fm (get-in f [:opts :format])
        upd! (fn [k v] (update-props! #{id} #(set-format % k v)))
        preview (prop-forms/format-preview (tname (:type f)) fm)]
    (if-not kinds
      [:div {:class "otv-hint"} (t :no-formatting {:type (tname (:type f))})]
      [:div
       [ui-form/field {:label (t :value-formatting)}
        [pick (some-> (:kind fm) name)
         (for [k kinds] {:value (name k) :label (i18n/t (prop-forms/format-kind-labels k))})
         #(upd! :kind (some-> % not-empty keyword))]]
       (when (= :currency (:kind fm))
         [ui-form/field {:label (t :currency)}
          [inputs/text-input (via (:currency fm) #(upd! :currency (str/upper-case %)))
           {:fill true :maxLength 3 :placeholder "EUR"}]])
       (when (= :unit (:kind fm))
         [ui-form/field {:label (t :unit)}
          [inputs/text-input (via (:unit fm) #(upd! :unit %)) {:fill true :placeholder "kg"}]])
       (when (= :number cls)
         [:div {:class "otv-field-row"}
          [ui-form/field {:label (t :decimals)}
           [ui/numeric-input
            {:fill true :min 0 :max 10
             :value (str (or (:decimals fm) ""))
             :on-value-change (fn [_ v _]
                                (upd! :decimals (when (seq v) (js/parseInt v 10))))}]]
          [ui/checkbox {:inline true :checked (boolean (:grouping fm))
                        :on-change #(upd! :grouping (when (.. % -target -checked) true))
                        :label (t :grouping)}]])
       (when preview [:div {:class "otv-preview"} (t :preview) ": " [:b preview]])])))

(defn- render-hints-tab [ids]
  (let [fs (filter #(contains? ids (fid %)) (:fields @form))
        has? (fn [h] (every? #(some #{h} (get-in % [:opts :render-hints])) fs))]
    [:div
     [:div {:class "otv-hint"} (t :render-hints-hint)]
     (for [h prop-forms/render-hint-options]
       ^{:key h}
       [ui/checkbox
        {:checked (has? h)
         :label [:span [:b (name h)] [:span {:class "otv-muted"}
                                      " — " (t (keyword (str "hint-" (name h))))]]
         :on-change (fn [_]
                      (let [on? (has? h)]
                        (update-props!
                         ids (fn [f]
                               (let [hs (set (get-in f [:opts :render-hints]))
                                     hs (if on? (disj hs h) (conj hs h))]
                                 (set-opt
                                  f :render-hints
                                  (vec (filter hs prop-forms/render-hint-options))))))))}])]))

(defn- metadata-pane [_k]
  (let [sel (or (:sel @otv) #{})
        m @form
        tab (or (:meta-tab @otv) :general)]
    [:div {:class "otv-pe__pane otv-pe__meta"}
     (cond
       (empty? sel)
       [ui/empty-state {:icon :properties :title (t :select-property)}]

       (> (count sel) 1)
       [:div
        [:div {:class "otv-pe__pane-head"} [:span (t :n-selected {:n (count sel)})]]
        [bulk-bar sel]
        [render-hints-tab sel]]

       :else
       (let [id (first sel) f (field-by-id m id)]
         (when f
           [:div
            [:div {:class "otv-pe__meta-head"}
             [:span {:class "otv-prop-line__glyph"} (type-glyph (:type f))]
             [:span {:class "otv-pe__meta-title"} (fdisplay f)]]
            [ui/tabs {:id :otv-meta :class "otv-tabs" :selected-tab-id tab
                      :on-change (fn [x] (edit! [:otv] assoc :meta-tab x))}
             (for [x meta-tabs]
               [ui/tab {:id x :title (t (keyword (str "tab-" (name x))))}])]
            [:div {:class "otv-pe__meta-body"}
             (case tab
               :general      [general-tab id f]
               :type         [type-tab id f]
               :formatting   [formatting-tab id f]
               :render-hints [render-hints-tab #{id}])]])))]))

(defn- editor-page [k]
  [:div {:class "otv-pe"}
   [page/toolbar
    [ui/button {:minimal true :small true :intent :primary :icon :arrow-left
                :on-click #(edit! [:otv] assoc :page :properties)}
     (t :properties)]
    [:h2 {:class "otv-h2"} (t :edit-mapping)]]
   [:div {:class "otv-pe__panes"}
    [datasource-pane k]
    [properties-pane k]
    [metadata-pane k]]])

(defn- kv [label v] [:div {:class "otv-kv"} [:span {:class "otv-kv__k"} label] [:span v]])

(defn- datasources-page [k {:keys [actions view-dataset!]}]
  (let [m @form
        node (lineage-status/node-status :object-type k)
        imported (or (get-in (rd/value-or @data/import-status {}) [:per-type k :count]) 0)]
    [:div {:class "otv-page"}
     [card {:title (t :backing-datasource)}
      [:div {:class "otv-hint"} (t :backing-explain)]
      (case (:backing-mode m)
        :synced [kv (t :source) (str (some-> (get-in m [:external :datasource]) name) " · "
                                     (or (get-in m [:external :table]) (t :query)))]
        :fed    [kv (t :transforms) (str/join ", " (map name (:fed-by m)))]
        [kv (t :source) (t :no-backing)])
      [:div {:class "otv-row-gap"}
       [ui/button {:on-click open-backing!}
        (if (synced? m) (t :replace) (t :add-backing))]
       [ui/button {:on-click #(view-dataset! k)} (t :view-dataset)]
       [ui/button {:on-click #(edit! [:otv] assoc :page :editor)} (t :edit-mapping)]]]
     [card {:title (t :index)}
      [kv (t :index-status) [index-status k]]
      [kv (t :last-build) (show (:last-build node))]
      [kv (t :imported-rows) (str imported)]
      [kv (t :live-objects) (str (get (rd/value-or @data/object-counts {}) k 0))]]
     [card {:title (t :writeback)}
      [:div {:class "otv-hint"} (t :writeback-explain)]
      [kv (t :edits) (if (seq (actions k)) [tag :success "Enabled"] [tag :neutral "Disabled"])]
      [ui/checkbox {:inline true
                    :checked (boolean (get-in m [:extra :requires-action-log]))
                    :on-change #(fs/edit! form with-extra :requires-action-log
                                          (when (.. % -target -checked) true))
                    :label (t :action-log)}]]]))

(defn- project-field [form-handle projects]
  [ui-form/field {:label (t :project)}
   [pick (some-> (get-in @form-handle [:extra :project]) name)
    (for [p projects] {:value p :label p})
    #(fs/edit! form-handle with-extra :project (some-> % not-empty keyword))]])

(defn- security-page [_k]
  (let [m @form
        projects (sort (map name (keys (catalog/project-list))))]
    [:div {:class "otv-page"}
     [card {:title (t :permissions)}
      [:div {:class "otv-hint"} (t :permissions-explain)]
      [project-field form projects]]
     [card {:title (t :markings)}
      [:div {:class "otv-hint"} (t :markings-explain)]
      [inputs/text-input
       (via (str/join ", " (map name (get-in m [:extra :markings])))
            #(fs/edit! form with-extra :markings
                       (->> (str/split % #",") (map str/trim) (remove str/blank?) (mapv keyword))))
       {:fill true :placeholder "pii, finance"}]]
     [card {:title (t :granular)}
      [:div {:class "otv-hint"} (t :granular-explain)]]]))

(defn- capability [title desc on? & [toggle!]]
  [:div {:class "otv-cap"}
   [:div {:class "otv-cap__text"} [:div {:class "otv-cap__title"} title]
    [:div {:class "otv-hint"} desc]]
   (if toggle!
     [ui/switch {:checked (boolean on?) :on-change toggle! :class "u-m-0"}]
     (if on? [tag :success "Enabled"] [tag :neutral "Disabled"]))])

(defn- capabilities-page [k {:keys [actions]}]
  (let [m @form]
    [:div {:class "otv-page"}
     [card {:title (t :capabilities)}
      [capability (t :cap-edits) (t :cap-edits-desc) (seq (actions k))]
      [capability (t :cap-er) (t :cap-er-desc) (:resolve-entity m)
       #(fs/edit! form update :resolve-entity not)]
      [capability (t :action-log) (t :cap-log-desc) (get-in m [:extra :requires-action-log])
       #(fs/edit! form with-extra :requires-action-log
                  (when-not (get-in m [:extra :requires-action-log]) true))]
      [capability (t :cap-search) (t :cap-search-desc) (seq (get-in m [:extra :semantic-search]))]
      [capability (t :cap-checks) (t :cap-checks-desc {:n (count (get-in m [:extra :checks]))})
       (seq (get-in m [:extra :checks]))]]]))

(defn- interfaces-page [_k]
  (let [m @form
        impl (set (map keyword (:implements m)))
        ifs (sort-by key (:interfaces @catalog/canonical-schema))]
    [:div {:class "otv-page"}
     [card {:title (str (t :interfaces) " (" (count impl) ")")}
      (if (empty? ifs)
        [:div {:class "otv-empty"} (t :no-interfaces)]
        (for [[ik spec] ifs]
          ^{:key ik}
          [ui/checkbox
           {:checked (contains? impl ik)
            :on-change #(fs/edit! form update :implements
                                  (fn [v] (if (contains? impl ik)
                                            (vec (remove #{ik} (map keyword v)))
                                            (conj (vec v) ik))))
            :label [:span [:b (or (not-empty (:display-name spec)) (name ik))]
                    [:span {:class "otv-muted"} " " (name ik)]]}]))]]))

(defn object-type-view
  [k {:keys [usage] :as opts}]
  (ensure-loaded! k)
  (let [page (or (:page @otv) :overview)]
    [:<>
     (case page
       :overview     [overview-page k opts]
       :properties   [properties-page k]
       :editor       [editor-page k]
       :datasources  [datasources-page k opts]
       :security     [security-page k]
       :capabilities [capabilities-page k opts]
       :interfaces   [interfaces-page k]
       :usage        [:div {:class "otv-page"} [usage k]]
       [overview-page k opts])
     [backing-dialog]
     (when (:review? @otv) [review-dialog k])]))

(def ^:private nf forms/new-object-form)
(def ^:private wiz (ratom/make-reaction #(:otv-wizard @ont-model/ui)))

(def ^:private steps [:datasource :metadata :properties :actions :location])

(defn open-create! []
  (fs/put! nf {:name "" :plural "" :display-name "" :description "" :symbol "⬢"
               :fields [] :implements [] :backing-mode :in-system})
  (edit! [] assoc :otv-wizard {:step :datasource :actions #{} :groups #{}}))

(defn- type-name [s]
  (let [s (slug s)]
    (if (re-matches #"[a-z].*" s) s (str "t-" s))))

(defn- wz-datasource []
  (let [m @nf]
    [:div
     [:div {:class "otv-choice"}
      (for [[mode title desc] [[:synced (t :use-datasource) (t :use-datasource-desc)]
                               [:in-system (t :no-datasource) (t :no-datasource-desc)]]]
        ^{:key mode}
        [ui/radio-card
         {:checked   (= mode (:backing-mode m))
          :on-change #(fs/edit! nf (fn [x]
                                     (cond-> (assoc x :backing-mode mode)
                                       (and (= mode :synced) (nil? (:external x)))
                                       (assoc :external (ext-forms/ext-default
                                                         (ext-forms/datasource-options))))))}
         [:b title] [:div {:class "otv-hint"} desc]])]
     (when (synced? m) [ext-forms/object-external-section {:form-atom nf}])]))

(defn- wz-metadata []
  (let [m @nf w @wiz
        groups (sort (keys (catalog/object-type-groups)))
        k (keyword (or (not-empty (:name m)) "new"))]
    [:div
     [ui-form/field {:label (t :icon)}
      [:div {:class "otv-icon-picker__grid otv-icon-picker__grid--wide"}
       (for [g icon-glyphs]
         ^{:key g}
         [:button {:class "otv-icon-picker__cell"
                   :style {:background (if (= g (:symbol m)) (color-of k)
                                           "var(--c-surface-soft)")
                           :color (if (= g (:symbol m)) "var(--c-on-accent)"
                                      "var(--c-text-2)")}
                   :on-click #(fs/edit! nf assoc :symbol g)} g])]]
     [:div {:class "otv-field-row"}
      [ui-form/field {:label (t :name)}
       [inputs/text-input
        (via (:display-name m)
             (fn [v] (fs/edit! nf (fn [x]
                                    (cond-> (assoc x :display-name v)
                                      (not (:id-touched? w)) (assoc :name (type-name v))
                                      (not (:plural-touched? w))
                                      (assoc :plural (if (str/blank? v) "" (str v "s"))))))))
        {:fill true :placeholder "Flight Alert"}]]
      [ui-form/field {:label (t :plural-name)}
       [inputs/text-input (via (:plural m) #(do (edit! [:otv-wizard] assoc :plural-touched? true)
                                                (fs/edit! nf assoc :plural %)))
        {:fill true}]]]
     [ui-form/field {:label (t :description)}
      [ui/text-area {:fill true :rows 2 :value (:description m)
                     :on-change #(fs/edit! nf assoc :description (.. % -target -value))}]]
     [ui-form/field {:label (t :id) :help (t :id-hint)}
      [inputs/text-input (via (:name m) #(do (edit! [:otv-wizard] assoc :id-touched? true)
                                             (fs/edit! nf assoc :name %)))
       {:fill true :class "otv-mono"}]]
     (when (seq groups)
       [ui-form/field {:label (t :groups)}
        [:div {:class "otv-chips"}
         (for [g groups :let [on? (contains? (:groups w) g)]]
           ^{:key g}
           [ui/tag {:interactive true :minimal (not on?) :intent (when on? :primary)
                    :on-click #(edit! [:otv-wizard] update :groups (if on? disj conj) g)}
            (name g)])]])]))

(defn- wz-properties []
  (let [m @nf]
    [:div
     [:div {:class "otv-hint"} (t :wz-properties-hint)]
     [table/data-table
      {:class "otv-table"
       :rows  (map-indexed vector (:fields m))
       :cols
       [{:label (t :id)
         :render
         (fn [[i f]]
           (let [id (fid f)]
             [inputs/text-input
              (via (:name f)
                   (fn [v] (fs/edit! nf (fn [x]
                                          (cond-> (assoc-in x [:fields i :name] v)
                                            (= id (:title x)) (assoc :title v)
                                            (= id (pk x)) (with-pk v))))))
              {:fill true :class "otv-mono"}]))}
        {:label (t :display-name)
         :render (fn [[i f]]
                   [inputs/text-input
                    (via (get-in f [:opts :name])
                         #(fs/edit! nf update-in [:fields i] set-opt :name %))
                    {:fill true :placeholder (fid f)}])}
        {:label (t :base-type)
         :render (fn [[i f]]
                   [pick (tname (:type f))
                    (for [x prop-forms/property-base-types] {:value x :label x})
                    #(fs/edit! nf assoc-in [:fields i :type] %) {:clearable? false}])}
        {:label (t :primary-key)
         :render (fn [[_ f]] [ui/radio {:checked (= (fid f) (pk m)) :class "u-m-0"
                                        :on-change #(fs/edit! nf with-pk (fid f))}])}
        {:label (t :title-key)
         :render (fn [[_ f]] [ui/radio {:checked (= (fid f) (:title m)) :class "u-m-0"
                                        :on-change #(fs/edit! nf assoc :title (fid f))}])}
        {:label ""
         :render (fn [[i _]]
                   [ui/button
                    {:minimal true :small true :intent :danger :icon :trash
                     :class "otv-pe__hover-btn"
                     :on-click #(fs/edit! nf update :fields
                                          (fn [fs] (vec (concat (subvec fs 0 i)
                                                                (subvec fs (inc i))))))}])}]}]
     [ui/button {:small true :minimal true :class "u-mt-10"
                 :on-click #(fs/edit! nf (fn [x] (update x :fields (fnil conj [])
                                                         {:name (fresh-id x) :type "string"})))}
      "+ " (t :add-property)]]))

(defn- wz-actions []
  (let [w @wiz label (or (not-empty (:display-name @nf)) (:name @nf))]
    [:div
     [:div {:class "otv-hint"} (t :wz-actions-hint)]
     (for [[kind lbl] [["create" :act-create] ["update" :act-modify] ["delete" :act-delete]]]
       ^{:key kind}
       [ui/checkbox {:checked (contains? (:actions w) kind)
                     :on-change #(edit! [:otv-wizard] update :actions
                                        (if (contains? (:actions w) kind) disj conj) kind)
                     :label (t lbl {:name label})}])]))

(defn- wz-location []
  (let [projects (sort (map name (keys (catalog/project-list))))]
    [:div
     [:div {:class "otv-hint"} (t :wz-location-hint)]
     [project-field nf projects]
     [:div {:class "otv-hint"} (t :wz-staged-hint)]]))

(defn- create! []
  (let [m @nf k (keyword (str/trim (:name m)))
        props (sort (set (map fid (:fields m))))
        label (or (not-empty (:display-name m)) (name k))]
    (send! [:ontology.otv/create m
            (vec (for [a (:actions @wiz)]
                   (ont-model/->action-spec
                    (ont-model/wizard->form
                     {:display-name (t (keyword (str "act-name-" a)) {:name label})
                      :kind a :object-type (name k) :props props}))))])))

(defn create-dialog []
  (when-let [w @wiz]
    (let [step (:step w)
          i (.indexOf steps step)
          m @nf
          problem (when (:attempted m) (object-forms/object-form-problem m true nil))]
      [overlay/modal {:on-close #(edit! [] dissoc :otv-wizard)
                      :close-on-backdrop? false
                      :class "otv-modal otv-modal--wide otv-wizard"
                      :title (t :create-ot)
                      :foot [:<>
                             (when (pos? i)
                               [ui/button
                                {:on-click #(edit! [:otv-wizard] assoc :step (steps (dec i)))}
                                (t :back)])
                             (when (< i (dec (count steps)))
                               [ui/button
                                {:intent   :primary
                                 :on-click #(edit! [:otv-wizard] assoc :step (steps (inc i)))}
                                (t :next)])
                             [ui/button {:intent :success :on-click create!} (t :create)]]
                      :foot-end? true}
       [:<>
        [:div {:class "otv-wizard__layout"}
         [:ol {:class "otv-steps"}
          (for [[j s] (map-indexed vector steps)]
            ^{:key s}
            [:li {:class (str "otv-steps__item" (when (= s step) " otv-steps__item--on")
                              (when (< j i) " otv-steps__item--done"))
                  :on-click #(edit! [:otv-wizard] assoc :step s)}
             [:span {:class "otv-steps__n"} (if (< j i) "✓" (inc j))]
             (t (keyword (str "step-" (name s))))])]
         [:div {:class "otv-wizard__body"}
          [:h3 {:class "otv-h3"} (t (keyword (str "step-" (name step))))]
          (case step
            :datasource [wz-datasource]
            :metadata   [wz-metadata]
            :properties [wz-properties]
            :actions    [wz-actions]
            :location   [wz-location])]]
        (when (or problem (:error w))
          [:div {:class "otv-change otv-change--error"}
           (or (:error w) (i18n/t problem))])]])))
