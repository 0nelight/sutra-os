(ns sutra-ui.apps.atlas.breadcrumb
  (:require [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra.types.rid :as rid]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.resource.core :as res]))

(defn- handle->resource
  [handle]
  (when-let [{:keys [family id]} (rid/parse-ref handle)]
    (case family
      :datasources {:family :datasources :nm (keyword id)}
      :graphs      {:family :graphs :nm id}
      {:family family :nm (catalog/name-of id)})))

(defn- node-by-rid [rid] (->> (kompass/kompass-node-list) (filter #(= rid (:rid %))) first))

(defn- folder-path
  [parent-rid]
  (loop [rid parent-rid acc ()]
    (if-let [f (and rid (node-by-rid rid))]
      (recur (:parent-rid f) (cons (:name f) acc))
      (vec acc))))

(defn- segments
  [handle]
  (let [{:keys [family nm]} (handle->resource handle)
        placement (->> (kompass/kompass-node-list) (filter #(= handle (:ref %))) first)
        leaf      (if family (res/display-name family nm) handle)]
    (cond
      placement (conj (folder-path (:parent-rid placement)) leaf)
      family    [(i18n/t :ui/kompass-unsorted) (res/family-label family) leaf]
      :else     (conj (folder-path (:parent-rid (node-by-rid handle)))
                      (:name (node-by-rid handle))))))

(defn breadcrumb
  [handle]
  [:div {:class "kompass-breadcrumb"}
   [ui/breadcrumbs {:items (into [{:text "Kompass"}]
                                 (map (fn [s] {:text (str s)}))
                                 (segments handle))}]])
