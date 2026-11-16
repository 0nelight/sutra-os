(ns sutra-ui.apps.atlas.table
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.table :as table]
            [reagent.core :as r]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.apps.atlas.files :as files]
            [sutra-ui.apps.atlas.model :as model]
            [sutra-ui.feature :as feature]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.resource.core :as res]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.ui.style :as style]))

(defonce ^:private menu (r/atom nil))

(defn open-dialog! [kind opts] (model/send! [:kompass.dialog/open kind opts]))
(defn- close-dialog! [] (model/send! [:kompass.dialog/close]))

(defn enter-project!
  ([pkey] (enter-project! pkey nil))
  ([pkey folder] (model/send! [:kompass.project/enter pkey folder])))

(defn leave-project! [] (model/send! [:kompass.project/leave]))

(defn open-row!
  [{:keys [family nm project may-read?]}]
  (case family
    :projects (when (not= false may-read?) (enter-project! nm))
    :folders  (enter-project! (or project ks/default-project) nm)
    (res/open! family nm)))

(def ^:private units [[60 "second"] [60 "minute"] [24 "hour"] [30 "day"] [12 "month"]])

(defn- relative-time [iso]
  (let [secs (/ (- (.getTime (js/Date. iso)) (js/Date.now)) 1000)
        fmt  (js/Intl.RelativeTimeFormat. js/undefined #js {:numeric "auto"})]
    (loop [v secs [[n unit] & more] units]
      (if (and more (>= (js/Math.abs v) n))
        (recur (/ v n) more)
        (if (and (nil? more) (>= (js/Math.abs v) n))
          (.format fmt (js/Math.round (/ v n)) "year")
          (.format fmt (js/Math.round v) unit))))))

(defn- absolute-time [iso]
  (.toLocaleString (js/Date. iso) js/undefined
                   #js {:dateStyle "medium" :timeStyle "short"}))

(defn- time-cell [iso relative?]
  (if (and iso (not (js/isNaN (.getTime (js/Date. iso)))))
    [:span {:title (absolute-time iso)} (if relative? (relative-time iso) (absolute-time iso))]
    [:span {:class "kompass-files__unknown"} "—"]))

(defn- older-cell [floor]
  [ui/tooltip {:content (i18n/t :ui/kompass-time-older)}
   [:span
    (i18n/t :ui/kompass-time-before {:date (absolute-time floor)})]])

(defn- file-handle? [family] (not= :projects family))

(defn- schema-family? [family]
  (and (not (res/file? family)) (not (#{:graphs :projects} family))))

(defn- trashable? [family]
  (or (schema-family? family) (#{:documents :explorations :folders :files} family)))

(defn- movable? [family]
  (or (schema-family? family) (#{:documents :explorations :folders :files} family)))

(defn row-items
  [{:keys [family nm handle promoted? pinned? curated? private? may-read?] :as row}]
  (let [fav?   (ks/file-favorite? (files/fav-ref row))
        pref   (when (schema-family? family) (res/protection-ref family nm))
        locked (and pref (gov/protected-ref? pref))]
    [(when (not= false may-read?)
       [(if (#{:projects :folders} family)
          (i18n/t :ui/open)
          (i18n/t :ui/open-in
                  {:app (feature/app-label (res/app-of family (res/spec-of family nm)))}))
        #(open-row! row)])
     (when (= false may-read?)
       [(i18n/t :ui/kompass-request-access) #(open-dialog! :request-access {:row row})])
     (when (movable? family)
       [(i18n/t :ui/kompass-move) #(open-dialog! :move {:row row})])
     (when (file-handle? family)
       [(i18n/t :ui/kompass-share) #(open-dialog! :share {:row row})])
     [(i18n/t (if promoted? :ui/kompass-remove-status :ui/kompass-promote))
      #(model/set-promoted! handle (not promoted?))]
     [(i18n/t :ui/kompass-edit-tags) #(open-dialog! :tags {:row row})]
     (when (file-handle? family)
       [(i18n/t (if pinned? :ui/kompass-unpin :ui/kompass-pin)) #(model/toggle-pinned! handle)])
     [(i18n/t (if fav? :ui/kompass-unfavorite :ui/kompass-favorite))
      #(ks/toggle-file-favorite! (files/fav-ref row))]
     (when (file-handle? family)
       [(i18n/t (if curated? :ui/kompass-catalog-remove :ui/kompass-catalog-add))
        #(model/toggle-curated! handle)])
     (when (file-handle? family)
       [(i18n/t :ui/kompass-to-collection) #(open-dialog! :row-collections {:row row})])
     (when (= :projects family)
       [(i18n/t :ui/kompass-to-portfolio) #(open-dialog! :project-portfolio {:row row})])
     (when (= :folders family)
       [(i18n/t :ui/kompass-add-description) #(open-dialog! :describe {:row row})])
     (when (and pref (gov/protectable-ref? pref))
       [(i18n/t (if locked :ui/branch-unprotect :ui/branch-protect))
        #(gov/set-protection! [pref] (not locked))])
     (when (and (= :explorations family) (not private?)
                (:home-folders @ks/kompass-index))
       [(i18n/t :ui/kompass-make-private) #(ks/make-exploration-private! nm)])
     (when (trashable? family)
       [(i18n/t :ui/kompass-to-trash) #(model/trash-resource! handle) true])]))

(defn- context-menu []
  (when-let [{:keys [x y items]} @menu]
    [ui/context-menu-popover
     {:is-open       true
      :target-offset {:left x :top y}
      :on-close      #(reset! menu nil)
      :content       [overlay/menu-items items]}]))

(defn dropdown
  [attrs label items]
  [overlay/menu-button (assoc attrs :right-icon :caret-down) label items])

(defn- field [label & control]
  (into [:label {:class "kompass-modal__field"} [:span {:class style/label} label]] control))

(defn- unplace! [ref]
  (when-let [n (some #(when (= ref (:ref %)) %) (ks/kompass-node-list))]
    (model/kompass-delete-node! (:rid n))))

(defn- move-row!
  [{:keys [family nm project placed-ref]} target folder]
  (let [pk       (some-> (not-empty target) keyword)
        changed? (not= pk project)
        folder   (not-empty folder)]
    (case family
      :folders      (cond
                      folder   (model/kompass-move-node! nm folder)
                      changed? (ks/move-file-to-project! :folders nm pk)
                      :else    (model/kompass-move-node! nm nil))
      :explorations (if folder
                      (ks/kompass-place! placed-ref folder nil)
                      (ks/move-file-to-project! :explorations nm pk))
      :files        (do (when changed? (ks/move-file-to-project! :files nm pk))
                        (if folder (ks/kompass-place! placed-ref folder nil)
                            (unplace! placed-ref)))
      :documents    (do (when changed? (ks/move-file-to-project! :documents nm pk))
                        (if folder (ks/kompass-place! placed-ref folder nil)
                            (unplace! placed-ref)))
      (do (when changed? (model/move-resource-to-project! family nm pk))
          (if folder (ks/kompass-place! placed-ref folder nil) (unplace! placed-ref))))))

(defn- move-dialog [{:keys [row]}]
  (let [target (r/atom (some-> (:project row) name))
        folder (r/atom (str (:parent row)))
        nm     (r/atom (:name row))]
    (fn [{:keys [row]}]
      (let [renames? (#{:folders :explorations} (:family row))
            needs-project? (res/file? (:family row))]
        [overlay/modal {:on-close close-dialog!
                        :title (i18n/t :ui/kompass-move-title {:name (:name row)})
                        :class "kompass-modal"
                        :body-class "kompass-modal__body"
                        :ok-label (i18n/t :ui/kompass-move)
                        :ok-disabled? (or (and needs-project? (str/blank? @target))
                                          (and renames? (str/blank? @nm)))
                        :on-ok (fn []
                                 (when (and renames? (not= @nm (:name row)))
                                   (if (= :folders (:family row))
                                     (model/kompass-rename-node! (:nm row) @nm)
                                     (ks/rename-exploration! (:nm row) @nm)))
                                 (move-row! row @target @folder)
                                 (close-dialog!))}
         (when renames?
           [field (i18n/t :ui/kompass-file-name)
            [ui/input-group {:value @nm
                             :on-change #(reset! nm (.. % -target -value))}]])
         [field (i18n/t :ui/kompass-location)
          [res/project-select (str @target)
           #(do (reset! target %) (reset! folder "")) (not needs-project?)]]
         (when-let [fs (seq (remove #(= (:nm row) (first %))
                                    (some-> @target not-empty keyword files/folders-of)))]
           [field (i18n/t :ui/kompass-folder)
            (into [ui/html-select {:value @folder
                                   :on-change #(reset! folder (.. % -target -value))}
                   [:option {:value ""} (i18n/t :ui/kompass-project-top)]]
                  (for [[rid label] fs] [:option {:key rid :value rid} label]))])]))))

(defn- link-url [token] (str js/window.location.origin "/kompass?share=" token))

(defn- copy! [text] (some-> js/navigator .-clipboard (.writeText text)))

(defn- grant-rows [ref view]
  (into [:div {:class "kompass-modal__list"}]
        (for [{:keys [principal role via]} (:grants view)]
          [:div {:key principal :class "kompass-grant__row"}
           [:span {:class "kompass-grant__principal" :title principal}
            (principals/principal-label principal)]
           [:span {:class style/small-muted}
            (str (name role) (when (= "link" (some-> via name))
                               (str " · " (i18n/t :ui/kompass-via-link))))]
           [ui/button {:icon :cross :small    true
                       :on-click #(model/send! [:kompass.share/revoke ref principal])}]])))

(defn- grant-form [_ref]
  (let [principal (r/atom "")
        role      (r/atom "viewer")]
    (fn [ref]
      [:div {:class "u-flex-gap-6"}
       [ui/input-group {:fill true :value @principal
                        :placeholder (i18n/t :ui/kompass-grant-principal)
                        :on-change #(reset! principal (.. % -target -value))}]
       (into [ui/html-select {:value @role
                              :on-change #(reset! role (.. % -target -value))}]
             (for [o ["viewer" "editor" "owner"]] [:option {:key o :value o} o]))
       [ui/button
        {:small    true :disabled (str/blank? @principal)
         :on-click (fn []
                     (model/send! [:kompass.share/grant ref @role (str/trim @principal)])
                     (reset! principal ""))}
        (i18n/t :ui/kompass-grant-role)]])))

(def ^:private file-share-panel
  (machine/entering
   model/kompass #(vector :kompass.share/load (:placed-ref %))
   (fn [row]
     (let [ref                    (:placed-ref row)
           {:keys [view refused]} (get-in @model/ui [:share ref])]
       (cond
         refused      [:div {:class "kompass-notice"} refused]
         (nil? view)  [busy/inline nil]
         :else
         [:<>
          [:div {:class style/label} (i18n/t :ui/kompass-share-people)]
          [grant-rows ref view]
          [grant-form ref]
          [:div {:class style/label} (i18n/t :ui/kompass-share-link)]
          (if-let [token (get-in view [:link :token])]
            [:div {:class "u-flex-gap-6"}
             [ui/input-group {:fill true :read-only true :value (link-url token)}]
             [ui/button {:small true :on-click #(copy! (link-url token))}
              (i18n/t :ui/kompass-copy-link)]
             [ui/button {:small true :on-click #(model/send! [:kompass.share/link ref false])}
              (i18n/t :ui/kompass-link-off)]]
            [ui/button {:outlined true :on-click #(model/send! [:kompass.share/link ref true])}
             (i18n/t :ui/kompass-link-on)])
          [:div {:class style/small-muted} (i18n/t :ui/kompass-share-note)]])))))

(defn- share-dialog [{:keys [row]}]
  [overlay/modal {:on-close close-dialog!
                  :title (i18n/t :ui/kompass-share-title {:name (:name row)})
                  :class "kompass-modal"
                  :body-class "kompass-modal__body"
                  :foot [:<>
                         [ui/button {:outlined true :on-click #(copy! js/window.location.href)}
                          (i18n/t :ui/kompass-copy-link)]
                         (when-let [pk (:project row)]
                           [ui/button {:outlined true
                                       :on-click (fn []
                                                   (close-dialog!)
                                                   (enter-project! pk)
                                                   (model/send! [:kompass/details-tab :access]))}
                            (i18n/t :ui/kompass-open-access)])
                         [ui/button {:intent :primary :on-click close-dialog!} (i18n/t :ui/close)]]
                  :foot-end? true}
   (if (files/shareable? row)
     [file-share-panel row]
     [:div {:class style/small-muted} (i18n/t :ui/kompass-share-hint)])])

(defn- tags-dialog [_]
  (let [draft (r/atom "")]
    (fn [{:keys [row]}]
      (let [tags (model/resource-tags (:handle row))
            add! #(let [t (str/trim @draft)]
                    (when (seq t) (model/toggle-resource-tag! (:handle row) t))
                    (reset! draft ""))]
        [overlay/modal {:on-close close-dialog!
                        :title (i18n/t :ui/kompass-tags-title {:name (:name row)})
                        :class "kompass-modal"
                        :body-class "kompass-modal__body"
                        :foot [:<>
                               [ui/button {:intent :primary :on-click close-dialog!}
                                (i18n/t :ui/close)]]
                        :foot-end? true}
         [ui/input-group {:value @draft :auto-focus true :list "kompass-all-tags"
                          :placeholder (i18n/t :ui/kompass-tag-placeholder)
                          :on-change #(reset! draft (.. % -target -value))
                          :on-key-down #(when (= "Enter" (.-key %)) (add!))}]
         (into [:datalist {:id "kompass-all-tags"}]
               (for [t (model/all-tags)] [:option {:key t :value t}]))
         (into [:div {:class "kompass-files__tags"}]
               (for [t (sort tags)]
                 ^{:key t}
                 [ui/tag {:minimal true :round true
                          :on-remove (fn [_ _] (model/toggle-resource-tag! (:handle row) t))}
                  t]))]))))

(defn- describe-dialog [{:keys [row]}]
  (let [draft (r/atom (or (model/annotation-doc (:handle row)) ""))]
    (fn [{:keys [row]}]
      [overlay/modal {:on-close close-dialog!
                      :title (i18n/t :ui/kompass-add-description)
                      :class "kompass-modal"
                      :body-class "kompass-modal__body"
                      :ok-label (i18n/t :ui/save)
                      :ok-disabled? false
                      :on-ok #(do (model/set-annotation-doc! (:handle row) @draft) (close-dialog!))}
       [ui/text-area
        {:fill true :class ["bp-monospace-text" "kompass-textarea--long"] :value @draft :auto-focus true
         :on-change #(reset! draft (.. % -target -value))}]])))

(defn- request-dialog [_]
  (let [reason (r/atom "") role (r/atom "viewer")]
    (fn [{:keys [row]}]
      (let [pending (model/pending-access-request (:nm row))]
        [overlay/modal {:on-close close-dialog!
                        :title (i18n/t :ui/kompass-request-title {:name (:name row)})
                        :class "kompass-modal"
                        :body-class "kompass-modal__body"
                        :foot (if pending
                                [:<>
                                 [ui/button {:intent :primary :on-click close-dialog!}
                                  (i18n/t :ui/close)]]
                                [overlay/form-actions
                                 {:on-cancel    close-dialog!
                                  :ok-label     (i18n/t :ui/kompass-access-submit)
                                  :ok-disabled? (str/blank? @reason)
                                  :on-ok        #(do (model/request-project-access!
                                                      (:nm row) @role @reason)
                                                     (close-dialog!))}])
                        :foot-end? true}
         (if pending
           [:div {:class "kompass-access__pending"}
            (i18n/t :ui/kompass-access-pending {:role (name (:role pending))})]
           [:<>
            [field (i18n/t :ui/kompass-role)
             (into [ui/html-select {:value @role
                                    :on-change #(reset! role (.. % -target -value))}]
                   (for [r ["viewer" "editor" "owner"]] [:option {:key r :value r} r]))]
            [field (i18n/t :ui/kompass-access-reason)
             [ui/text-area
              {:fill true :class ["bp-monospace-text" "kompass-textarea--short"] :value @reason
               :on-change #(reset! reason (.. % -target -value))}]]])]))))

(defn- new-project-dialog [_]
  (let [nm (r/atom "") desc (r/atom "")]
    (fn [_]
      (let [spaces (keys (:spaces @catalog/canonical-schema))]
        [overlay/modal {:on-close close-dialog!
                        :title (i18n/t :ui/kompass-new-project)
                        :class "kompass-modal"
                        :body-class "kompass-modal__body"
                        :ok-label (i18n/t :ui/kompass-create)
                        :ok-disabled? (str/blank? @nm)
                        :on-ok #(do (model/create-project! (str/trim @nm) (str/trim @desc))
                                    (close-dialog!))}
         [field (i18n/t :ui/kompass-project-name)
          [ui/input-group {:value @nm :auto-focus true
                           :on-change #(reset! nm (.. % -target -value))}]]
         [field (i18n/t :ui/kompass-project-description)
          [ui/input-group {:value @desc
                           :on-change #(reset! desc (.. % -target -value))}]]
         [field (i18n/t :ui/kompass-space)
          (into [ui/html-select {:disabled true}
                 [:option (i18n/t :ui/kompass-space-this)]]
                (for [s spaces] [:option {:key s} (name s)]))]]))))

(defn- new-folder-dialog [_]
  (let [nm (r/atom "")]
    (fn [{:keys [pkey folder]}]
      [overlay/modal {:on-close close-dialog!
                      :title (i18n/t :ui/kompass-new-folder)
                      :class "kompass-modal"
                      :body-class "kompass-modal__body"
                      :ok-label (i18n/t :ui/kompass-create)
                      :ok-disabled? (str/blank? @nm)
                      :on-ok #(do (model/kompass-create-folder! (str/trim @nm) folder pkey)
                                  (close-dialog!))}
       [ui/input-group {:value @nm :auto-focus true
                        :placeholder (i18n/t :ui/kompass-new-folder-name)
                        :on-change #(reset! nm (.. % -target -value))}]])))

(defn- portfolio-dialog [_]
  (let [nm (r/atom "") desc (r/atom "")]
    (fn [_]
      [overlay/modal {:on-close close-dialog!
                      :title (i18n/t :ui/kompass-new-portfolio)
                      :class "kompass-modal"
                      :body-class "kompass-modal__body"
                      :ok-label (i18n/t :ui/kompass-create)
                      :ok-disabled? (str/blank? @nm)
                      :on-ok #(do (model/create-portfolio! (str/trim @nm) (str/trim @desc) [])
                                  (close-dialog!))}
       [field (i18n/t :ui/kompass-file-name)
        [ui/input-group {:value @nm :auto-focus true
                         :on-change #(reset! nm (.. % -target -value))}]]
       [field (i18n/t :ui/kompass-project-description)
        [ui/input-group {:value @desc
                         :on-change #(reset! desc (.. % -target -value))}]]])))

(defn- collection-dialog [_]
  (let [nm (r/atom "") desc (r/atom "")]
    (fn [_]
      [overlay/modal {:on-close close-dialog!
                      :title (i18n/t :ui/kompass-new-collection)
                      :class "kompass-modal"
                      :body-class "kompass-modal__body"
                      :ok-label (i18n/t :ui/kompass-create)
                      :ok-disabled? (str/blank? @nm)
                      :on-ok #(do (model/create-collection! (str/trim @nm) (str/trim @desc) [])
                                  (close-dialog!))}
       [field (i18n/t :ui/kompass-file-name)
        [ui/input-group {:value @nm :auto-focus true
                         :on-change #(reset! nm (.. % -target -value))}]]
       [field (i18n/t :ui/kompass-project-description)
        [ui/input-group {:value @desc
                         :on-change #(reset! desc (.. % -target -value))}]]])))

(defn- row-collections-dialog [{:keys [row]}]
  (let [ref    (files/fav-ref row)
        all    (:collections @ks/kompass-index)
        picked (r/atom (set (for [c all :when (some #{ref} (:refs c))] (:rid c))))]
    (fn [_]
      [overlay/modal {:on-close close-dialog!
                      :title (i18n/t :ui/kompass-to-collection)
                      :class "kompass-modal"
                      :body-class "kompass-modal__body"
                      :ok-label (i18n/t :ui/save)
                      :ok-disabled? false
                      :on-ok (fn []
                               (doseq [{:keys [rid refs]} all
                                       :let [in? (some #{ref} refs) want? (contains? @picked rid)]
                                       :when (not= (boolean in?) want?)]
                                 (model/change-collection!
                                  rid {:refs (if want?
                                               (conj (vec refs) ref)
                                               (vec (remove #{ref} refs)))}))
                               (close-dialog!))}
       (if (empty? all)
         [:div {:class style/small-muted} (i18n/t :ui/kompass-collections-empty)]
         (into [:div {:class "kompass-modal__list"}]
               (for [{:keys [rid] nm :name} all]
                 [ui/checkbox
                  {:key rid :checked (contains? @picked rid) :label nm
                   :on-change #(swap! picked (if (.. % -target -checked) conj disj) rid)}])))])))

(defn- dataset-name [fname]
  (let [n (-> (str/lower-case (str/replace fname #"\.[^.]+$" ""))
              (str/replace #"[^a-z0-9]+" "_")
              (str/replace #"^_+|_+$" ""))]
    (if (re-matches #"[a-z].*" n) n (str "datei_" n))))

(defn- upload-kinds
  [fname]
  (cond
    (re-find #"(?i)\.(csv|tsv|xlsx)$" fname) [:dataset :raw]
    (re-find #"(?i)\.(md|markdown|txt)$" fname) [:document :raw]
    :else [:raw]))

(defn- upload-dialog [{:keys [files]}]
  (let [choices (r/atom (vec (for [f files]
                               {:file f :kind (first (upload-kinds (.-name f)))
                                :dataset (dataset-name (.-name f))})))]
    (fn [{:keys [pkey folder]}]
      (let [{:keys [busy? problem]} (:upload @model/ui)]
        [overlay/modal {:on-close close-dialog!
                        :title (i18n/t :ui/kompass-upload-title {:n (count @choices)})
                        :class "kompass-modal"
                        :body-class "kompass-modal__body"
                        :foot [:<>
                               [ui/button {:outlined true :on-click close-dialog!}
                                (i18n/t :ui/cancel)]
                               [ui/button {:intent   :primary :disabled busy?
                                           :on-click #(model/send! [:kompass.upload/start
                                                                    pkey folder @choices])}
                                (i18n/t :ui/kompass-upload-go)]]
                        :foot-end? true}
         (doall
          (for [[i {:keys [file kind dataset]}] (map-indexed vector @choices)]
            ^{:key i}
            [:div {:class "kompass-upload__file"}
             [:strong (.-name file)]
             (into [ui/html-select {:value (name kind)
                                    :on-change #(swap! choices assoc-in [i :kind]
                                                       (keyword (.. % -target -value)))}]
                   (for [k (upload-kinds (.-name file))]
                     [:option {:key k :value (name k)}
                      (i18n/t (case k
                                :dataset  :ui/kompass-upload-dataset
                                :document :ui/kompass-upload-document
                                :ui/kompass-upload-raw))]))
             (when (= :dataset kind)
               [ui/input-group
                {:value dataset
                 :aria-label (i18n/t :ui/kompass-upload-dataset-name)
                 :on-change #(swap! choices assoc-in [i :dataset] (.. % -target -value))}])]))
         (when problem [:div {:class "kompass-notice"} problem])]))))

(defn- add-reference-dialog [{:keys [pkey declared]}]
  (let [target (r/atom "")]
    (fn [_]
      (let [rights (:rights @ks/kompass-index)
            others (for [pk (sort-by name (keys (catalog/project-list)))
                         :when (and (not= pk pkey) (not (contains? (set declared) pk))
                                    (get-in rights [(name pk) :read?]))]
                     pk)]
        [overlay/modal {:on-close close-dialog!
                        :title (i18n/t :ui/kompass-add-reference)
                        :class "kompass-modal"
                        :body-class "kompass-modal__body"
                        :ok-label (i18n/t :ui/kompass-add-reference)
                        :ok-disabled? (str/blank? @target)
                        :on-ok #(do (model/set-project-imports!
                                     pkey (conj (vec declared) (keyword @target)))
                                    (close-dialog!))}
         [:div {:class style/small-muted} (i18n/t :ui/kompass-add-reference-hint)]
         (into [ui/html-select {:value @target
                                :on-change #(reset! target (.. % -target -value))}
                [:option {:value "" :disabled true} (i18n/t :ui/kompass-pick-project)]]
               (for [pk others] [:option {:key pk :value (name pk)} (name pk)]))]))))

(defn- project-names-of [{:keys [projects]}]
  (let [by-rid (into {} (for [[pk spec] (catalog/project-list)] [(str (:id spec)) pk]))]
    (keep #(get by-rid (str %)) projects)))

(defn- portfolio-projects-dialog [{:keys [portfolio]}]
  (let [picked (r/atom (set (project-names-of portfolio)))]
    (fn [{:keys [portfolio]}]
      [overlay/modal {:on-close close-dialog!
                      :title (i18n/t :ui/kompass-add-projects)
                      :class "kompass-modal"
                      :body-class "kompass-modal__body"
                      :ok-label (i18n/t :ui/save)
                      :ok-disabled? false
                      :on-ok #(do (model/change-portfolio! (:rid portfolio)
                                                           {:projects (mapv name @picked)})
                                  (close-dialog!))}
       [:div {:class style/small-muted} (i18n/t :ui/kompass-add-projects-hint)]
       (into [:div {:class "kompass-modal__list"}]
             (for [pk (sort-by name (keys (catalog/project-list)))]
               [ui/checkbox
                {:key pk :checked (contains? @picked pk) :label (name pk)
                 :on-change #(swap! picked (if (.. % -target -checked) conj disj) pk)}]))])))

(defn- project-portfolio-dialog [{:keys [row]}]
  (let [current (:portfolio row)
        picked  (r/atom (str (:rid current)))]
    (fn [{:keys [row]}]
      [overlay/modal {:on-close close-dialog!
                      :title (i18n/t :ui/kompass-to-portfolio)
                      :class "kompass-modal"
                      :body-class "kompass-modal__body"
                      :ok-label (i18n/t :ui/save)
                      :ok-disabled? false
                      :on-ok (fn []
                               (let [pk (:nm row)
                                     target (some #(when (= @picked (:rid %)) %)
                                                  (:portfolios @ks/kompass-index))]
                                 (when (and current (not= (:rid current) (:rid target)))
                                   (model/change-portfolio!
                                    (:rid current)
                                    {:projects
                                     (mapv name (remove #{pk} (project-names-of current)))}))
                                 (when target
                                   (model/change-portfolio!
                                    (:rid target)
                                    {:projects
                                     (mapv name (distinct (conj (project-names-of target) pk)))})))
                               (close-dialog!))}
       (into [ui/html-select {:value @picked
                              :on-change #(reset! picked (.. % -target -value))}
              [:option {:value ""} (i18n/t :ui/kompass-no-portfolio)]]
             (for [p (:portfolios @ks/kompass-index)]
               [:option {:key (:rid p) :value (:rid p)} (:name p)]))])))

(defn dialogs
  []
  [:<>
   [context-menu]
   (when-let [{:keys [kind] :as d} (model/at :dialog)]
     ^{:key (str kind (:key (:row d)))}
     (case kind
       :move              [move-dialog d]
       :share             [share-dialog d]
       :tags              [tags-dialog d]
       :describe          [describe-dialog d]
       :request-access    [request-dialog d]
       :new-project       [new-project-dialog d]
       :new-folder        [new-folder-dialog d]
       :new-portfolio     [portfolio-dialog d]
       :portfolio-projects [portfolio-projects-dialog d]
       :project-portfolio [project-portfolio-dialog d]
       :new-collection    [collection-dialog d]
       :row-collections   [row-collections-dialog d]
       :upload            [upload-dialog d]
       :add-reference     [add-reference-dialog d]
       nil))])

(defn- tag-pill [t]
  (let [[cat tag] (str/split (str t) #":\s*" 2)]
    [ui/tag {:minimal true :round true :title t}
     (if tag [:<> [:strong cat ":"] " " tag] t)]))

(defn- row-icon [{:keys [family nm]}]
  (case family
    :projects [ui/icon {:icon :projects}]
    :folders  [ui/icon {:icon :folder-close :class "kompass-icon--folder"}]
    [:span (res/glyph family nm)]))

(defn- sort-value [col row]
  (case col
    :modified (str (:modified row))
    :portfolio (str/lower-case (str (get-in row [:portfolio :name])))
    (str/lower-case (str (:name row)))))

(defn- ordered [rows {:keys [col dir]} folders-first?]
  (if-not col
    rows
    (let [s (sort-by #(sort-value col %) rows)
          s (if (= :desc dir) (reverse s) s)]
      (if folders-first?
        (concat (filter #(= :folders (:family %)) s) (remove #(= :folders (:family %)) s))
        s))))

(defn- head-cell
  [sorting {:keys [label sort]}]
  (if-not sort
    label
    (let [{cur :col dir :dir} @sorting]
      [ui/button {:minimal true :small true
                  :right-icon (cond (not= cur sort) :double-caret-vertical
                                    (= :asc dir)    :caret-up
                                    :else           :caret-down)
                  :aria-sort (if (= cur sort) (if (= :asc dir) "ascending" "descending") "none")
                  :on-click #(swap! sorting (fn [s]
                                              {:col sort
                                               :dir (if (and (= sort (:col s)) (= :asc (:dir s)))
                                                      :desc :asc)}))}
       label])))

(defn- name-cell [{:keys [name path promoted? pinned? private? description] :as row} show-path?]
  (let [fav? (ks/file-favorite? (files/fav-ref row))]
    [ui/entity-title
     {:icon     [:<> [row-icon row]
                 (when promoted?
                   [ui/tooltip {:content (i18n/t :ui/kompass-promoted)}
                    [ui/icon {:icon :tick-circle :size 11 :class "kompass-icon--promoted"}]])]
      :title    [:a {:href "#" :on-click #(do (.preventDefault %) (open-row! row))} name]
      :tags     (when (or pinned? private? fav?)
                  [:<>
                   (when pinned?
                     [ui/tooltip {:content (i18n/t :ui/kompass-pinned)}
                      [ui/icon {:icon :pin :size 12}]])
                   (when private? [ui/tag {:minimal true} (i18n/t :ui/ox-save-private)])
                   (when fav? [ui/icon {:icon :star :size 12 :intent :warning}])])
      :subtitle (when show-path?
                  [:span {:title description} (str "/" (str/join "/" path))])}]))

(defn- when-cell [{:keys [modified may-read?] :as row} relative?]
  (cond
    (= false may-read?)
    [ui/button {:outlined true :on-click #(open-dialog! :request-access {:row row})}
     (i18n/t :ui/kompass-request-access)]
    (and (nil? modified) (:older-than row)) [older-cell (:older-than row)]
    :else [time-cell modified relative?]))

(defn- file-cols [{:keys [show-path? relative? portfolio?]} items]
  (cond-> [{:label (i18n/t :ui/kompass-col-name) :sort :name :width 360
            :render #(vector name-cell % show-path?)}
           {:label  (i18n/t (if relative? :ui/kompass-col-modified :ui/kompass-col-updated))
            :sort   :modified
            :render #(vector when-cell % relative?)}
           {:label  (i18n/t :ui/kompass-col-tags)
            :render (fn [{:keys [tags]}]
                      (into [:span] (for [t (sort tags)] ^{:key t} [tag-pill t])))}]
    portfolio?
    (conj {:label  (i18n/t :ui/kompass-col-portfolio) :sort :portfolio
           :render (fn [{:keys [portfolio]}]
                     (when portfolio
                       [:span [ui/icon {:icon :briefcase :size 14}] " " (:name portfolio)]))})
    :always
    (conj {:label  "" :align :right
           :render (fn [row] [overlay/more-menu (remove nil? (items row))])})))

(defn file-table
  [{:keys [default-sort]}]
  (let [sorting (r/atom (or default-sort {:col nil}))
        shown   (r/atom 100)]
    (fn [{:keys [rows selected folders-first? empty-text] items :row-items :as opts}]
      (let [items  (or items row-items)
            sorted (ordered rows @sorting folders-first?)
            total  (count sorted)]
        [table/object-table
         {:cols           (file-cols opts items)
          :rows           (take @shown sorted)
          :row-key        :key
          :wrap?          true
          :fit-columns?   true
          :multi-select?  (some? selected)
          :selected       selected
          :head-cell      #(head-cell sorting %)
          :on-row-context (fn [row e]
                            (.preventDefault e)
                            (reset! menu {:x (.-clientX e) :y (.-clientY e)
                                          :items (items row)}))
          :more           (when (> total @shown)
                            {:loaded @shown :total total :load! #(swap! shown + 200)})
          :empty          {:icon :folder-open :title empty-text}}]))))

(defn confirm-delete-forever!
  [{:keys [family nm name handle]}]
  (model/delete-file-forever-confirmed!
   {:title   (i18n/t :ui/kompass-delete-forever-title {:name name})
    :body    (i18n/t :ui/kompass-delete-forever-body)
    :danger? true}
   family nm handle))
