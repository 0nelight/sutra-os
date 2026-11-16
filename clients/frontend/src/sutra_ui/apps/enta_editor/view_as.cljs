(ns sutra-ui.apps.enta-editor.view-as
  (:require [sutra-ui.ui.core :as ui :refer [tag]]
            [clojure.string :as str]
            [sutra.capps.view-as :as va]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.fields :as f]))

(defn- options [] (va/audiences (es/tree)))

(defn active
  []
  (let [id (es/ui-get :view-as)]
    (when (and id (some #(= id (:id %)) (options))) id)))

(defn- provenance [{:keys [role? profile? view-as?]}]
  (let [parts (cond-> []
                role?    (conj "Rolle")
                profile? (conj "Profil")
                (and view-as? (not role?) (not profile?)) (conj "Viewing-as"))]
    (when (seq parts) (str " (" (str/join "/" parts) ")"))))

(defn descriptor
  []
  (let [id (active)]
    (cond-> (es/preview-descriptor)
      id (update :layout va/prune id)
      id (assoc :viewer {:role id}))))

(defn picker
  []
  (let [opts (options)
        id   (active)
        gone (va/hidden-panes (es/tree) id)]
    (when (seq opts)
      [ui/tooltip
       {:content "Nur eine Vorschau — der bearbeitete Baum bleibt unverändert."}
       [:span {:class (str "ab-canvas-bar__item" (when id " ab-canvas-bar__item--on"))}
        [:span {:class "ab-canvas-bar__label"} "Ansicht als"]
        [f/select (some (fn [[i o]] (when (= id (:id o)) i)) (map-indexed vector opts))
         (for [[i {:keys [label] :as o}] (map-indexed vector opts)]
           [(str i) (str label (provenance o))])
         #(es/ui-set! {:view-as (when % (:id (nth opts (js/parseInt % 10))))})
         "alle" {:class "ab-select"}]
        (when (and id (seq gone))
          [ui/tooltip {:content (str/join ", " (map :label gone))}
           [tag {:minimal true :intent :warning} (str (count gone) " ausgeblendet")]])]])))
