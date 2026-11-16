(ns sutra-ui.apps.notebook.templates
  (:require [sutra-ui.ui.overlay :as overlay]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.table :as table]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra.types.submission :as csub]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.apps.notebook.state :refer [busy dialog docs err notebook open-doc send!
                                                 templates]]
            [sutra-ui.apps.notebook.ui :refer [action-btn field-label fmt-ts]]
            [sutra-ui.apps.notebook.doc :refer
             [open! target-project template-of template-title type-kw]]))

(defn pick-template!
  ([t] (pick-template! t {}))
  ([t opts] (send! [:notebook.template/pick t opts])))

(defn- close! [] (send! [:notebook.dialog/close]))

(defn- hide! [] (send! [:notebook/set :dialog nil]))

(defn option-pairs
  [{:keys [options-from]} rows]
  (let [prop  (some-> (:property options-from) keyword)
        lbl   (some-> (:label options-from) keyword)]
    (for [{:keys [id values]} rows
          :let [v (if prop (get values prop) id)
                l (or (get values (or lbl :name)) (and prop (get values prop)) id)]
          :when (some? v)]
      [(str v) (str l)])))

(def iso-date "yyyy-MM-dd")
(def iso-minute "yyyy-MM-dd'T'HH:mm")

(defn wire->date
  [v pattern]
  (let [s (str v)
        n (if (= pattern iso-date) 10 16)
        d (when (>= (count s) n) (ui/parse-date (subs s 0 n) pattern))]
    (when (and d (not (js/isNaN (.getTime d)))) d)))

(defn param-input
  [{:keys [type object-type] :as pspec} rows value on-change]
  (cond
    (and (seq rows) (not= type :object-set))
    [ui/html-select {:fill true :value (str (or value ""))
                     :on-change #(on-change (.. % -target -value))}
     [:option {:value ""} "—"]
     (for [[v l] (option-pairs pspec rows)]
       ^{:key v} [:option {:value v} l])]

    (and (seq rows) (= type :object-set))
    (let [chosen (set (if (sequential? value) value
                          (remove str/blank? (str/split (str value) #","))))]
      [:div {:class "np-checklist"}
       (for [[v l] (option-pairs pspec rows)]
         ^{:key v}
         [ui/checkbox
          {:class "np-check" :label l
           :checked (contains? chosen v)
           :on-change
           #(on-change (vec (if (.. % -target -checked) (conj chosen v)
                                (disj chosen v))))}])])

    :else
    (case type
      :boolean [ui/checkbox {:checked (boolean value)
                             :on-change #(on-change (.. % -target -checked))}]
      :number  [ui/input-group {:inputMode "decimal" :value (or value "")
                                :on-change #(on-change (.. % -target -value))}]
      :date    [ui/date-input
                {:fill true :value (wire->date value iso-date)
                 :on-change (fn [d _] (on-change (if d (ui/format-date d iso-date) "")))}]
      :timestamp [ui/date-input
                  {:fill true :time-precision :minute :value (wire->date value iso-minute)
                   :on-change (fn [d _] (on-change (if d (ui/format-date d iso-minute) "")))}]
      (:object :object-set)
      [ui/input-group
       {:value (if (sequential? value) (str/join "," value) (or value ""))
        :placeholder (str (some-> object-type name)
                          (if (= type :object-set) " — Ids, mit Komma getrennt" " — Id"))
        :on-change #(on-change (.. % -target -value))}]
      [ui/input-group {:value (or value "")
                       :on-change #(on-change (.. % -target -value))}])))

(defn ->wire-value
  [pspec v]
  (let [t (csub/template-term pspec)]
    (cond
      (= :boolean (get t 2))                       (boolean v)
      (and (sequential? v) (= [:object-set] t))    (when (seq v) (vec v))
      (str/blank? (str v))                         nil
      (#{:integral :fractional} (get t 1))
      (csub/decode-template-value pspec (str/replace (str v) "," "."))
      :else                                        (csub/decode-template-value pspec v))))

(defn wire-values [params values]
  (into {} (keep (fn [[pname pspec]]
                   (when-let [v (->wire-value pspec (get values pname))]
                     [pname v])))
        params))

(defn generate-dialog []
  (let [dg     @dialog
        t      (:template dg)
        params (:parameters t)
        opts   (:options t)
        values (:values dg)
        tname  (name (:template-name t))]
    [overlay/modal
     {:size (if (:preview dg) "960px" "480px")
      :on-close close!
      :title (or (:title t) tname)
      :foot [:<>
             [action-btn {:label "Abbrechen" :tone :ghost :on-click close!}]
             [action-btn
              {:label "Vorschau" :tone :ghost :disabled? @busy
               :title "Rendert ohne zu speichern; leere Parameter nehmen ihren Vorschauwert"
               :on-click #(send! [:notebook.template/preview tname
                                  (cond-> {:values (wire-values params values) :preview true}
                                    (:version dg) (assoc :version (:version dg)))])}]
             [action-btn
              {:label "Erzeugen" :disabled? @busy
               :on-click
               #(send! [:notebook.template/generate tname
                        (cond-> {:values (wire-values params values)}
                          (:version dg) (assoc :version (:version dg))
                          (target-project) (assoc :project (name (target-project)))
                          (not (str/blank? (:name dg))) (assoc :name (:name dg)))])}]]
      :foot-end? true
      :close-on-backdrop? false}
     [:div {:class "np-gen"}
      [:div {:class "np-gen__form"}
       [:div {:class "np-muted"} (or (:description t) "")]
       [field-label "Dokumentname"]
       [ui/input-group {:value (or (:name dg) "")
                        :placeholder (or (:title t) tname)
                        :on-change #(send! [:notebook.dialog/set [:name]
                                            (.. % -target -value)])}]
       (when (seq (:versions t))
         [:div
          [field-label "Vorlagen-Version"]
          [ui/html-select
           {:fill true :value (or (:version dg) "")
            :on-change #(send! [:notebook.dialog/set [:version]
                                (not-empty (.. % -target -value))])}
           [:option {:value ""} (str "neueste" (when (:version t) (str " (v" (:version t) ")")))]
           (for [v (reverse (:versions t))] ^{:key v} [:option {:value v} (str "v" v)])]])
       (for [[pname pspec] (sort-by key params)]
         ^{:key pname}
         [:div
          [field-label
           [:<> (tbl/humanize pname) (when (:required pspec) [:span {:class "np-req"} " *"])]]
          [param-input pspec (:rows (get opts pname)) (get values pname)
           #(send! [:notebook.dialog/set [:values pname] %])]
          (when (:description pspec)
            [:div {:class "np-muted"} (:description pspec)])])
       (when @err [:div {:class "np-error"} @err])]
      (when-let [html (:preview dg)]
        [:iframe {:src-doc html :sandbox "allow-same-origin" :title "Vorschau"
                  :class "np-gen__preview"}])]]))

(defn template-picker []
  [overlay/modal {:size "520px"
                  :on-close hide!
                  :title "Neu aus Vorlage"
                  :foot [:<>
                         [action-btn {:label "Schließen" :tone :ghost :on-click hide!}]]
                  :foot-end? true
                  :close-on-backdrop? false}
   (if (empty? @templates)
     [:div {:class "np-muted"}
      "Keine Vorlagen installiert. Ein Dokument wird über „Aktionen → Als Vorlage speichern“ "
      "zur Vorlage."]
     (for [t (sort-by #(str (or (:title %) (:template-name %))) @templates)]
       ^{:key (:template-name t)}
       [:div {:class "np-tpl-row" :on-click #(pick-template! t)}
        [:div {:class "np-tpl-row__title"} (or (:title t) (some-> (:template-name t) name))]
        (when (:description t) [:div {:class "np-tpl-row__desc"} (:description t)])
        [:div {:class "np-muted"}
         (str (count (:parameters t)) " Eingaben"
              (when (:version t) (str " · v" (:version t))))]]))])

(defn template-page
  []
  (let [tname (:template-page @dialog)
        t     (template-of tname)
        made  (filter #(= (some-> tname name) (:template %)) @docs)]
    [overlay/modal {:size "560px"
                    :on-close hide!
                    :title (template-title tname)
                    :foot [:<>
                           [action-btn {:label "Schließen" :tone :ghost :on-click hide!}]
                           (when t
                             [action-btn {:label (i18n/t :ui/notebook-template-new-doc)
                                          :on-click #(pick-template! t)}])]
                    :foot-end? true
                    :close-on-backdrop? false}
     [:div {:class "np-muted"}
      (str/join " · " (remove str/blank?
                              [(i18n/t :ui/notebook-template)
                               (some->> (:version t)
                                        (hash-map :v) (i18n/t :ui/notebook-template-version))
                               (some-> (:format t) name)]))]
     (when (seq (:description t))
       [:p (:description t)])
     (if-not t
       [:div {:class "np-error"}
        (i18n/t :ui/notebook-template-missing {:name (template-title tname)})]
       [:<>
        [:div {:class "np-section-title"} (i18n/t :ui/notebook-template-parameters)]
        (if (empty? (:parameters t))
          [:div {:class "np-muted"} (i18n/t :ui/notebook-none)]
          [table/data-table
           {:rows    (sort-by key (:parameters t))
            :row-key (comp name key)
            :cols    [{:label "Eingabe" :cell-class "np-strong"
                       :render (fn [[pname pspec]]
                                 [:<> (tbl/humanize pname)
                                  (when (:required pspec) [:span {:class "np-req"} " *"])])}
                      {:label "Typ" :cell-class "np-muted"
                       :render (fn [[_ pspec]]
                                 (if-let [ot (type-kw (:object-type pspec))]
                                   (oc/type-label ot (= :object-set
                                                        (some-> (:type pspec) keyword)))
                                   (some-> (:type pspec) name)))}
                      {:label "Beschreibung" :render (fn [[_ pspec]] (:description pspec))}]}])])
     [:div {:class "np-section-title"}
      (i18n/t :ui/notebook-template-documents {:n (count made)})]
     (if (empty? made)
       [:div {:class "np-muted"} (i18n/t :ui/notebook-none)]
       (for [d made]
         ^{:key (:id d)}
         [:div {:class "np-tpl-doc" :on-click #(do (hide!) (open! (:id d)))}
          (:name d) [:span {:class "np-muted"} (str " · " (fmt-ts (:updated-at d)))]]))]))

(defn- templatize-dialog*
  []
  (let [d   @open-doc
        dg  @dialog
        nm  (:templatize dg)
        pj  (:project dg)
        set #(send! [:notebook.dialog/set [%1] %2])]
    [overlay/modal {:size "520px"
                    :on-close close!
                    :title (i18n/t :ui/notebook-templatize-title)
                    :foot [:<>
                           [action-btn {:label (i18n/t :ui/close) :tone :ghost :on-click close!}]
                           (when-not (:saved dg)
                             [action-btn
                              {:label (i18n/t :ui/notebook-templatize-save)
                               :disabled? (or @busy (str/blank? nm) (str/blank? pj))
                               :on-click #(send! [:notebook.templatize/save (:id d) nm pj])}])]
                    :foot-end? true
                    :close-on-backdrop? false}
     [:p (i18n/t :ui/notebook-templatize-intro)]
     [field-label (i18n/t :ui/notebook-templatize-name)]
     [ui/input-group {:value (or nm "")
                      :on-change #(set :templatize
                                       (-> (.. % -target -value) str/lower-case
                                           (str/replace #"[^a-z0-9-]" "-")))}]
     [field-label (i18n/t :ui/notebook-templatize-project)]
     [ui/html-select {:fill true :value (or pj "")
                      :on-change #(set :project (.. % -target -value))}
      (for [p (:projects dg)
            :let [pn (name (:name p))]]
        ^{:key pn} [:option {:value pn} pn])]
     (when-let [s (:saved dg)]
       [:div {:class "np-ok"} (i18n/t :ui/notebook-templatize-staged {:name s})])
     (when @err [:div {:class "np-error"} @err])
     [:div {:class "np-muted"} (i18n/t :ui/notebook-templatize-placeholders)]]))

(def templatize-dialog
  (machine/entering notebook [:notebook.templatize/load] templatize-dialog*))
