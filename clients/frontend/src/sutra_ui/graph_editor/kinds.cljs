(ns sutra-ui.graph-editor.kinds
  (:require [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra.viz.lineage-projection :as lp]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.object.card :as oc]))

(def ^:private label-keys
  {:datasource  :ui/datasource
   :dataset     :ui/lin-kind-dataset
   :object-type :ui/object-type
   :transform   :ui/kind-transform
   :application :ui/application
   :action      :ui/kind-action
   :writeback   :ui/kind-writeback
   :function    :ui/kind-function
   :automation  :ui/automation
   :agent       :ui/ai-platform-agent
   :model       :ui/language-model
   :link-type   :ui/link-type
   :link        :ui/link-type})

(def ^:private aliases
  (into {:language-model :model :media-set :dataset}
        (map (fn [l] [l :transform]))
        lp/transform-language-aliases))

(defn kind-of [k]
  (let [k (keyword k)] (get aliases k k)))

(defn label [k]
  (if-let [key (label-keys (kind-of k))] (i18n/t key) (name k)))

(defn glyph
  [k kw]
  (let [k (kind-of k)]
    (or (when (= :object-type k) (some-> kw oc/type-meta :symbol))
        (colors/node-kind-icon k)
        (when (#{:link-type :link} k) "⇄")
        "•")))

(defn- schema-display [section kw]
  (not-empty (str (get-in @catalog/canonical-schema [section (keyword kw) :display-name]))))

(defn display-name [k kw]
  (when (some? kw)
    (case (kind-of k)
      :object-type (oc/type-label kw)
      :dataset     (or (schema-display :datasets kw) (name kw))
      :action      (or (schema-display :actions kw) (name kw))
      :application (or (schema-display :applications kw) (name kw))
      (name kw))))

(defn ident
  [k kw]
  [ui/tooltip {:content (str (label k) " · " (name kw))}
   [:span {:class "lineage-kind-ident"}
    [:span {:style {:color (colors/node-kind-color (kind-of k)) :margin-right "var(--sp-1)"}}
     (glyph k kw)]
    (display-name k kw)]])
