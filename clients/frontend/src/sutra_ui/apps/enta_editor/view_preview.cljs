(ns sutra-ui.apps.enta-editor.view-preview
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.storage :as storage]
            [sutra-ui.ui.core :as ui :refer [tag]]
            [sutra-ui.apps.enta-editor.events :as ev]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.fields :as f]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.object-view :as object-view]
            [sutra-ui.object.card :as oc]
            [sutra-ui.object.page :as object-page]
            [sutra-ui.apphost.registry :as capps]))

(def presets
  [{:id :free     :label "Frei (ganze Fläche)"}
   {:id :explorer :label "Explorer-Seitenleiste" :w 380 :h 760 :group "Anwendung"}
   {:id :map      :label "Karten-Panel"          :w 420 :h 800 :group "Anwendung"}
   {:id :graph    :label "Graph-Panel"           :w 460 :h 800 :group "Anwendung"}
   {:id :apphost :label "App Studio-Seitenpanel"  :w 560 :h 900 :group "Anwendung"}
   {:id :tablet   :label "Tablet 1024×768"       :w 1024 :h 768 :group "Auflösung"}
   {:id :laptop   :label "Laptop 1366×768"       :w 1366 :h 768 :group "Auflösung"}
   {:id :desktop  :label "Desktop 1920×1080"     :w 1920 :h 1080 :group "Auflösung"}
   {:id :manual   :label "Manuell…"}])

(defn size
  [app panel?]
  (or (get (es/ui-get :sizes) app)
      (storage/get-json (ev/size-key app))
      {:preset (if panel? :explorer :free)}))

(defn- set-size! [app s] (es/send! [:enta.preview/size app s]))

(defn- dims [{:keys [preset w h]}]
  (if (= "manual" (some-> preset name))
    (when (and w h) [w h])
    (some (fn [p] (when (= (name (:id p)) (some-> preset name)) (when (:w p) [(:w p) (:h p)])))
          presets)))

(defn size-picker
  [app panel?]
  (let [{:keys [preset w h] :as s} (size app panel?)
        manual? (= "manual" (some-> preset name))
        num     (fn [k v] [ui/numeric-input
                           {:value (or v "") :min 120 :small true :button-position :none
                            :class "ab-num"
                            :on-value-change (fn [_ t _]
                                               (set-size! app (assoc s k (js/parseInt t 10))))}])]
    [:span {:class "ab-canvas-bar__item"}
     [:span {:class "ab-canvas-bar__label"} "Größe"]
     [f/select (or preset :free)
      (for [{:keys [id label w h]} presets]
        [(name id) (if w (str label " (" w "×" h ")") label)])
      #(let [id (keyword %)
             p  (some (fn [p] (when (= id (:id p)) p)) presets)]
         (set-size! app (merge {:preset id} (select-keys (or p s) [:w :h]))))
      nil {:class "ab-select" :clearable? false}]
     (when manual? [:<> [num :w w] "×" [num :h h]])]))

(defn sized
  [app panel? child]
  (if-let [[w h] (dims (size app panel?))]
    [:div {:class "ab-sized"}
     [:div {:class "ab-sized__frame" :style {:width (str w "px") :height (str h "px")}}
      child]]
    child))

(defn- rows-for [ot]
  (let [c (get (es/ui-get :candidates) ot)]
    (when (nil? c) (js/setTimeout #(es/send! [:enta.objects/need ot]) 0))
    c))

(defn- archetype [rows k]
  (when (seq rows)
    (case k
      :leer    (first rows)
      :typisch (nth rows (quot (count rows) 2))
      :extrem  (peek rows))))

(defn pinned
  [app]
  (or (get (es/ui-get :pins) app)
      (:id (storage/get-json (ev/pin-key app)))))

(defn- pin! [app id] (es/send! [:enta.preview/pin app id]))

(defn- row-label [ot row]
  (let [tk (some-> (:title-key (oc/type-meta ot)) name keyword)]
    (str (or (some-> (get row tk) str not-empty) (:id row)))))

(defn object-picker
  [app ot]
  (let [{:keys [rows loading? error]} (rows-for ot)
        id   (pinned app)
        pick (fn [row] (when row (pin! app (str (:id row)))))]
    [:span {:class "ab-canvas-bar__item"}
     [:span {:class "ab-canvas-bar__label"} "Objekt"]
     (cond
       loading? [busy/inline {:size 12} "lädt…"]
       error    [ui/tooltip {:content error}
                 [tag {:minimal true :intent :danger} "Fehler"]]
       (empty? rows) [:span "keine Objekte"]
       :else
       [:<>
        [f/select id (for [row (sort-by #(row-label ot %) rows)]
                       [(str (:id row)) (row-label ot row)])
         #(pin! app %) "— Objekt anheften —" {:class "ab-select"}]
        (for [[k label title] [[:leer "leer" "das Objekt mit den wenigsten Werten"]
                               [:typisch "typisch" "ein Objekt aus der Mitte"]
                               [:extrem "extrem" "das Objekt mit den meisten und längsten Werten"]]]
          ^{:key k}
          [ui/tooltip
           {:content (str title " (unter den ersten " (count rows) ")")}
           [tag {:minimal true :on-click #(pick (archetype rows k))} label]])])]))

(defn pin-archetype!
  [app ot k]
  (when-let [row (archetype (:rows (rows-for ot)) k)]
    (pin! app (str (:id row)))))

(defonce ^:private opened (atom nil))

(defn object-view
  [app ot form-factor descriptor]
  (let [id    (pinned app)
        entry (when id (get @object-view/object-view-data [ot id]))]
    (when (and id (not= @opened [ot id]))
      (reset! opened [ot id])
      (object-view/refresh-object-view-data! ot id {:whole? true}))
    (cond
      (nil? id)
      [ui/empty-state {:icon :cube
                       :title "Mit welchem Objekt ansehen?"
                       :hint (str "Die Ansicht braucht ein echtes Objekt. Oben kannst du jederzeit "
                                  "ein anderes wählen.")
                       :action [ui/button {:intent   :primary
                                           :disabled (empty? (:rows (rows-for ot)))
                                           :on-click #(pin-archetype! app ot :typisch)}
                                "Typisches Objekt nehmen"]}]

      (or (nil? entry) (and (:loading? entry) (nil? (:data entry))))
      [busy/block "lädt…"]

      (:error entry)
      [:div {:style {:padding "var(--sp-7)" :color "var(--c-danger-strong)"}} (:error entry)]

      :else
      (let [app-spec (catalog/object-view-app ot form-factor)
            d        (merge (object-page/page-descriptor @catalog/canonical-schema
                                                         ot id entry app-spec (:layout descriptor))
                            (select-keys descriptor [:viewer]))]
        (capps/frame {:class "enta-inspect ab-canvas__app"}
                     ^{:key [ot id (hash (:layout descriptor)) (:viewer descriptor)]}
                     [capps/render d (:data entry)])))))

(defn auto-publish-switch
  [on? on-change]
  [ui/switch
   {:style {:margin "0 0 var(--sp-4)"}
    :title (str "An: „Speichern und veröffentlichen“ — jede gespeicherte Änderung"
                " sehen alle sofort. Aus: speichern, in der Vorschau prüfen, dann"
                " hier mit einer Beschreibung veröffentlichen.")
    :checked (boolean on?)
    :on-change #(on-change (.. % -target -checked))
    :label "Neue Versionen automatisch veröffentlichen (ab dem Speichern)"}])
