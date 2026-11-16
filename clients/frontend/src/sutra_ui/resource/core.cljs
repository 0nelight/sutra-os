(ns sutra-ui.resource.core
  (:require [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra.types.transform-io :as tio]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.object.card :as oc]))

(def families
  [{:family :object-types      :label :ui/object-types      :kind :ui/kompass-kind-object-type
    :glyph "▢"}
   {:family :link-types        :label :ui/link-types        :kind :ui/kompass-kind-link-type
    :glyph "⇄"}
   {:family :interfaces        :label :ui/interfaces        :kind :ui/kind-interface
    :glyph "◇"}
   {:family :transforms        :label :ui/transforms        :kind :ui/kind-transform
    :glyph "ƒ"}
   {:family :functions         :label :ui/functions         :kind :ui/kind-function
    :glyph "▶"}
   {:family :shared-properties :label :ui/shared-properties :kind :ui/kind-shared-property
    :glyph "◈"}
   {:family :sources           :label :ui/sources           :kind :ui/kind-source
    :glyph "▤"}
   {:family :graphs            :label :ui/kompass-graphs    :kind :ui/kompass-kind-graph
    :glyph "◫"}])

(def schema-families
  (into [] (remove #(= :graphs (:family %))) families))

(def file-families
  [{:family :folders   :label :ui/kompass-folders   :kind :ui/kompass-kind-folder   :glyph "📁"}
   {:family :documents :label :ui/kompass-documents :kind :ui/kompass-kind-document :glyph "🗎"}
   {:family :widget-sets :label :ui/app-widget-sets :kind :ui/kompass-kind-widget-set
    :glyph "▣"}
   {:family :explorations :label :ui/kompass-explorations :kind :ui/kompass-kind-exploration
    :glyph "⌕"}
   {:family :files :label :ui/kompass-uploads :kind :ui/kompass-kind-file :glyph "⬚"}])

(defn file? [f] (contains? #{:documents :folders :widget-sets :explorations :files} f))

(defn- document-by-id [id]
  (some #(when (= (str id) (str (:id %))) %) (ks/rows :documents)))

(defn canonical-family [f] (if (= f :datasources) :sources f))

(defn protection-ref
  [family nm]
  (str (case (canonical-family family) :actions "action-types" (name (canonical-family family)))
       "/" (name nm)))

(def file-sections
  [{:family :datasets :label :ui/section-datasets :kind :ui/kompass-kind-dataset :glyph "▦"}])

(def ^:private by-family
  (into {} (map (juxt :family identity)) (concat families file-families file-sections)))

(defn- info [f] (get by-family (canonical-family f)))

(def ^:private sheet-info {:kind :ui/kompass-kind-sheet :glyph "▦"})

(defn- item-info
  [f nm]
  (if (and nm (= :documents (canonical-family f))
           (= "sheet" (some-> (document-by-id nm) :format name)))
    sheet-info
    (info f)))

(defn glyph ([f] (glyph f nil)) ([f nm] (:glyph (item-info f nm) "•")))
(def ^:private section-labels
  {:actions :ui/actions :applications :ui/applications :datasets :ui/section-datasets})

(defn family-label [f]
  (if-let [k (or (:label (info f)) (section-labels f))] (i18n/t k) (some-> f name)))
(defn kind-label
  ([f] (kind-label f nil))
  ([f nm] (if-let [k (:kind (item-info f nm))] (i18n/t k) (some-> f name))))

(defn spec-of [family nm]
  (let [f (canonical-family family)]
    (case f
      :documents (document-by-id nm)
      :folders   (some #(when (= (str nm) (:rid %)) %) (ks/kompass-node-list))
      :widget-sets (some #(when (= (str nm) (:name %)) %) (ks/rows :widget-sets))
      :explorations (some #(when (= (str nm) (str (:rid %))) %) (ks/rows :explorations))
      :files   (some #(when (= (str nm) (str (:id %))) %) (ks/rows :files))
      :graphs  (or (get @catalog/views (str nm))
                   (get-in @catalog/canonical-schema [:graphs nm]))
      :sources (let [k (keyword (name nm))]
                 (or (get-in @catalog/canonical-schema [:sources k])
                     (get (data/datasource-map) k)))
      (get-in @catalog/canonical-schema [f nm]))))

(defn display-name [family nm]
  (let [f    (canonical-family family)
        spec (spec-of f nm)]
    (cond
      (= f :object-types) (oc/type-label nm)
      (= f :graphs)       (or (not-empty (str (:name spec))) (str nm))
      (file? f)           (or (not-empty (str (:name spec))) (str nm))
      :else               (or (not-empty (:display-name spec)) (some-> nm name)))))

(defn- io-name [e]
  (cond
    (keyword? e) e
    (string? e)  (some-> (catalog/name-of e) name keyword)
    (map? e)     (some-> (or (:object-type e) (:dataset e) (:link-type e) (:ref e))
                         catalog/name-of name keyword)))

(defn transform-reads
  [spec]
  (into [] (keep io-name) (tio/inputs-of spec)))

(defn transform-writes
  [spec]
  (into [] (keep io-name) (tio/outputs-of spec)))

(defn sealed? [family nm]
  (and (= :sources (canonical-family family))
       (boolean (:sealed? (spec-of family nm)))))

(defn app-of
  [family spec]
  (let [f (canonical-family family)]
    (or (feature/opener-app f spec) (feature/opener-app :default spec))))

(defn open! [family nm]
  (let [f (canonical-family family)]
    (if (or (file? f) (= :graphs f))
      (when-let [o (feature/opener f (spec-of f nm))]
        ((:open! o) nm (spec-of f nm)))
      (catalog/open-resource! f (keyword (name nm))))))

(defn open-label
  [family nm]
  (i18n/t :ui/open-in {:app (feature/app-label (app-of family (spec-of family nm)))}))

(defn resource-link
  ([family nm] (resource-link family nm nil))
  ([family nm {:keys [class on-click]}]
   (let [txt (display-name family nm)]
     [ui/tooltip {:content (str (kind-label family nm) " · " txt)}
      [:a {:href "#"
           :class (cond-> "reslink" class (str " " class))
           :on-click (fn [e]
                       (.preventDefault e)
                       (.stopPropagation e)
                       (if on-click (on-click) (open! family nm)))}
       [:span {:class "reslink__icon"} (glyph family nm)]
       [:span {:class "reslink__name"} txt]
       (when (sealed? family nm)
         [ui/tooltip {:content (i18n/t :ui/kompass-sealed-source)}
          [:span {:class "reslink__lock"} "🔒"]])]])))

(defn project-select
  ([value on-change] (project-select value on-change true))
  ([value on-change homeless?]
   (into [ui/html-select {:value value :auto-focus true
                          :on-change #(on-change (.. % -target -value))}
          (if homeless?
            [:option {:value ""} (i18n/t :ui/kompass-default-scope)]
            [:option {:value "" :disabled true} (i18n/t :ui/kompass-pick-project)])]
         (for [pk (sort-by name (keys (catalog/project-list)))]
           [:option {:value (name pk)} (name pk)]))))
