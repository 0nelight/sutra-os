(ns sutra-ui.resource.link
  (:require [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra-ui.object.card :as oc]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.catalog :as catalog]))

(defn- kw [x] (some-> (catalog/name-of x) name keyword))

(defn label [kind x]
  (let [k (kw x)]
    (case kind
      :object-type (oc/type-label k)
      :link-type   (or (not-empty (get-in @catalog/schema [:link-types k :display-name]))
                       (tbl/humanize k))
      :action      (or (not-empty (get-in @catalog/canonical-schema [:actions k :display-name]))
                       (tbl/humanize k))
      (some-> k name))))

(defn open! [kind x]
  (let [k (kw x)]
    (case kind
      :object-type (catalog/open-resource! :object-types k)
      :link-type   (catalog/open-resource! :link-types k)
      :transform   (catalog/open-resource! :transforms k)
      :source      (catalog/open-resource! :sources k)
      :action      (catalog/open-resource! :actions k)
      :dataset     (feature/command! :open-in-lineage [:dataset k])
      nil)))

(defn resource-link
  ([kind x] (resource-link kind x nil))
  ([kind x {:keys [class text]}]
   (when (some? x)
     (let [txt (or text (label kind x))]
       [:a {:href "#"
            :class (cond-> "sutra-objlink sutra-objlink--open" class (str " " class))
            :title (str (some-> (kw x) name))
            :aria-label (i18n/t :ui/oc-open-object {:name txt})
            :on-click (fn [e] (.preventDefault e) (.stopPropagation e) (open! kind x))}
        [:span {:class "sutra-objlink__title"} txt]]))))
