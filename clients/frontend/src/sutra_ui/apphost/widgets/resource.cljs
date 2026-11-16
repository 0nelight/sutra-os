(ns sutra-ui.apphost.widgets.resource
  (:require [sutra.i18n.core :as i18n]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.sel :as sel]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.resource-list :as rl]
            [sutra.kit.coerce :refer [->kw]]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.events :refer [run-effects!]]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(defn- personal-refs
  []
  {:favorites (kompass/om-favorites)
   :recents   (get-in @kompass/om-discover [(kompass/om-key) :recents] [])})

(defn- open!
  [entry {:keys [ev-ctx]} {:keys [selected-object-type selected-object-set]}]
  (let [[kind x] (rl/click-plan entry)
        row      {:object-type (:name entry) :title (:title entry) :rows (:rows entry)}]
    (when (:object-set? entry) (sel/wr! selected-object-set (:rows entry)))
    (when (= :object-types (->kw (:family entry))) (sel/wr! selected-object-type (:name entry)))
    (case kind
      :events        (run-effects! x row ev-ctx)
      :open-set      (run-effects! {:do :open-explorer
                                    :type (or (:object-type entry)
                                              (:object-type-ref (first (:rows entry))))
                                    :ids (mapv :id (:rows entry))}
                                   row ev-ctx)
      :open-type     (run-effects! {:do :open-explorer :type x} row ev-ctx)
      :open-document (run-effects! {:do :open-document :document (str x)} row ev-ctx)
      :open-app      (run-effects! {:do :open-app :app x} row ev-ctx)
      :open-resource (let [[family nm] x] (catalog/open-resource! family nm)))))

(defn- thumbnail
  [{:keys [thumbnail title]}]
  (when thumbnail
    [:img.capp-resources__thumb {:src thumbnail :alt (str title)}]))

(defn- entry-view
  [{:keys [title description icon icon-color style thumb-at] :as e}
   {:keys [format on-open]}]
  (let [card? (= :image-card (->kw format))]
    [:button {:type     "button"
              :class    (str "capp-resources__item capp-resources__item--" (name style)
                             (when card? (str " capp-resources__item--card-" (name thumb-at))))
              :on-click #(on-open e)}
     (when card? [thumbnail e])
     (when icon [:span.capp-resources__icon {:style (when icon-color {:color icon-color})} icon])
     [:span.capp-resources__text
      [:span.capp-resources__title title]
      (when description [:span.capp-resources__description description])]]))

(defn resource-list
  [{:keys [env format horizontal? class] :as cfg}]
  (let [{:keys [descriptor]} env
        {:keys [data error]} (when (rl/needs-answer? cfg)
                               (frame/read! env [::resources] descriptor
                                            [client/resources! descriptor]))
        entries (rl/items cfg (merge {:resources   (:resources data)
                                      :folder-refs (:folders data)}
                                     (personal-refs)))]
    (if error
      [ui/problem "Ressourcen nicht lesbar" error]
      (into [:div {:class (cond-> "capp-resources"
                            horizontal?  (str " capp-resources--row")
                            (= :image-card (->kw format)) (str " capp-resources--cards")
                            class        (str " " class))}]
            (map-indexed
             (fn [i e]
               ^{:key i}
               [entry-view e {:format  format
                              :on-open #(open! % env cfg)}]))
            entries))))

(defn- objects-of
  [rows]
  (into []
        (keep (fn [row]
                (when-let [t (or (:object-type-ref row) (:object-type row))]
                  {:object-type (if (keyword? t) (name t) (str t)) :id (str (:id row))})))
        rows))

(defn- read-file!
  [^js f on-text]
  (let [^js reader (js/FileReader.)]
    (set! (.-onload reader) #(on-text (.-result reader)))
    (.readAsText reader f)))

(defn- link-picker
  [{:keys [candidates on-pick on-close]}]
  [overlay/modal {:title "Vorhandene Ressource verknüpfen" :on-close on-close}
   (into [:div.capp-resources]
         (map (fn [{:keys [id name]}]
                ^{:key id}
                [:button.capp-resources__item.capp-resources__item--classic
                 {:type "button" :on-click #(on-pick id)}
                 [:span.capp-resources__title name]]))
         candidates)])

(machine/reg-event ::act
                   (fn [{:keys [db]} [_ id {:keys [ask ok]}]]
                     (rq/request (update db ::failed dissoc id)
                                 {:ask ask :then [ok] :error [::failed id]})))

(defn linked-resources
  [{:keys [env objects heading hide-upload? hide-link? class]}]
  (r/with-let [f    (frame/frame-for env)
               id   (random-uuid)
               held (atom nil)
               at   (fn [k] @(machine/subscribe [:capp/at [k id]] {:frame f}))]
    (let [{:keys [descriptor ev-ctx]} env
          objs   (objects-of objects)
          slot   [::linked objs]
          _      (when (not= slot @held)
                   (some->> @held (vector :capp/drop-read) (frame/send! f))
                   (reset! held slot))
          ask    (fn [extra] [client/linked-resources! descriptor (merge {:objects objs} extra)])
          change (fn [extra] (frame/send! f [::act id {:ask (ask extra) :ok [:capp/reread slot]}]))
          {:keys [data error]} (when (seq objs) (frame/read! env slot objs (ask {:op :read})))
          rows   (some-> data :documents vec)
          error  (or (at ::failed) error)
          shown  (some-> (at ::candidates) :documents vec)]
      [:div {:class (cond-> "capp-linked" class (str " " class))}
       [:div.capp-linked__head
        (when heading [:span.capp-linked__heading heading])
        (when-not hide-link?
          [ui/button
           {:disabled (empty? objs)
            :on-click #(frame/send! f [::act id {:ask (ask {:op :candidates})
                                                 :ok  [:capp/put [::candidates id]]}])}
           "Verknüpfen"])
        (when-not hide-upload?
          [:label.bp-button
           "Hochladen"
           [:input {:type      "file"
                    :style     {:display "none"}
                    :disabled  (empty? objs)
                    :on-change (fn [^js e]
                                 (when-let [^js file (aget (.. e -target -files) 0)]
                                   (read-file!
                                    file
                                    (fn [text]
                                      (change {:op   :upload
                                               :file {:name (.-name file) :mime (.-type file)
                                                      :text text}})))))}]])]
       (when error [ui/problem (str error) nil])
       (cond
         (empty? objs)  [:div.capp__hint "Kein Objekt gewählt."]
         (nil? rows)    [busy/block {:class "capp-objectview__loading"} (i18n/t :ui/capp-loading)]
         (empty? rows)  [:div.capp__hint "Keine verknüpften Ressourcen."]
         :else
         (into [:div.capp-resources]
               (map (fn [{:keys [id name updated-at]}]
                      ^{:key id}
                      [:button.capp-resources__item.capp-resources__item--classic
                       {:type     "button"
                        :on-click #(run-effects! {:do :open-document :document (str id)}
                                                 nil ev-ctx)}
                       [:span.capp-resources__text
                        [:span.capp-resources__title name]
                        (when-not (str/blank? (str updated-at))
                          [:span.capp-resources__description
                           (subs (str updated-at) 0 (min 16 (count (str updated-at))))])]]))
               rows))
       (when shown
         [link-picker {:candidates shown
                       :on-close   #(frame/send! f [:capp/put [::candidates id] nil])
                       :on-pick    (fn [doc]
                                     (frame/send! f [:capp/put [::candidates id] nil])
                                     (change {:op :link :document doc}))}])])
    (finally
      (frame/send! f [:capp/drop-read @held])
      (frame/send! f [:capp/update [] #(-> % (update ::failed dissoc id)
                                            (update ::candidates dissoc id)) []]))))
