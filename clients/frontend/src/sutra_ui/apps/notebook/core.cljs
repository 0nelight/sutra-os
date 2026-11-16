(ns sutra-ui.apps.notebook.core
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.filter :as ui-filter]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra-ui.apphost.platform :as platform]
            [sutra.kit.slug :refer [slug]]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui :refer [link divider]]
            [reagent.core :as r]
            [sutra.capps.diagram :as dg]
            [sutra.capps.markup :as m]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra-ui.editor.code :as ce]
            [sutra-ui.object.card :as oc]
            [sutra-ui.resource.core :as res]
            [sutra-ui.apps.notebook.live :as live]
            [sutra-ui.editor.rich-text.editor :as rich]
            [sutra-ui.apps.notebook.diagram :as diagram]
            [sutra-ui.apps.notebook.markup :as markup]
            [sutra-ui.apps.notebook.sheet :as sheet]
            [sutra-ui.apps.notebook.use-case :as use-case]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.notebook.events]
            [sutra-ui.apps.notebook.state :refer
             [busy chain! dialog docs editor err frame-el frame-outline image-input layout-mode
              left-tab open-doc print-view proj-filter reload rendered save-state search
              seed-pending sel-widget send! session templates tick tpl-filter ui v-filter
              v-open versions wdialog]]
            [sutra-ui.apps.notebook.ui :refer [action-btn btn field-label fmt-ts]]
            [sutra-ui.apps.notebook.doc :as doc :refer
             [close! create! diagram-doc? layout-doc? linked-row live-state markup-doc? open!
              own-format?
              page-dims saves-here? sheet-doc? target-project template-of template-title
              type-kw version-title wrap-html]]
            [sutra-ui.apps.notebook.templates :refer
             [generate-dialog pick-template! template-page template-picker templatize-dialog]]
            [sutra-ui.apps.notebook.widget-dialog :refer
             [open-widget-dialog! resource-label widget-dialog widget-panel]]))

(defn- anchor [s] (or (not-empty (slug s)) "abschnitt"))

(defn- frame-heading [id]
  (some-> @frame-el .-contentDocument (.getElementById id)))

(defn- index-frame! []
  (when-let [doc (some-> @frame-el .-contentDocument)]
    (set! (.. @frame-el -style -height) (str (+ 24 (.. doc -documentElement -scrollHeight)) "px"))
    (let [seen (atom {})]
      (reset! frame-outline
              (vec (for [h (array-seq (.querySelectorAll doc "h1,h2,h3"))]
                     (let [base (anchor (.-textContent h))
                           n    (get (swap! seen update base (fnil inc 0)) base)
                           id   (if (= 1 n) base (str base "-" n))]
                       (set! (.-id h) id)
                       {:level (js/parseInt (subs (.-tagName h) 1)) :text (.-textContent h)
                        :id id})))))))

(defn- outline-items []
  @tick
  (if-let [ed (and (not (layout-doc? @open-doc)) @editor)]
    (let [seen (atom {})]
      (vec (for [{:keys [text] :as h} (rich/headings ed)]
             (let [base (anchor text)
                   n    (get (swap! seen update base (fnil inc 0)) base)]
               (assoc h :id (if (= 1 n) base (str base "-" n)))))))
    @frame-outline))

(defn- go-to! [{:keys [id pos]}]
  (if (and pos @editor)
    (rich/scroll-to-pos! @editor pos)
    (some-> (frame-heading id) (.scrollIntoView #js {:behavior "smooth" :block "start"}))))

(defn- scroll-to-hash! []
  (let [h (js/decodeURIComponent (subs (str (.-hash js/location)) 1))]
    (when (seq h)
      (js/setTimeout #(some (fn [it] (when (= h (:id it)) (go-to! it) true)) (outline-items))
                     300))))

(defn- copy-anchor! [id]
  (let [url (str (.-origin js/location) (.-pathname js/location) "#" (js/encodeURIComponent id))]
    (some-> js/navigator .-clipboard (.writeText url))))

(defn- outline-panel []
  (let [items (outline-items)]
    [:div {:class "np-panel"}
     (if (empty? items)
       [:div {:class "np-muted"} "Überschriften erscheinen hier als Gliederung."]
       (for [[i {:keys [level text id] :as it}] (map-indexed vector items)]
         ^{:key i}
         [:div {:class (str "np-outline__item np-outline__item--l" level)}
          [:span {:class "np-outline__text" :on-click #(go-to! it)} text]
          [ui/tooltip {:content "Ankerlink kopieren"}
           [:span {:class "np-outline__anchor"
                   :on-click #(copy-anchor! id)} "#"]]]))]))

(defn- page-settings []
  (let [p (or (:page @open-doc) {})
        set-p! (fn [k v]
                 (send! [:notebook/edit {:page (if (str/blank? (str v)) (dissoc p k)
                                                  (assoc p k v))}]))]
    [:div {:class "np-panel"}
     [:div {:class "np-muted"} "Gilt für Ansicht, Druck und Export."]
     [field-label "Seitengröße"]
     [ui/html-select {:fill true :value (name (or (:size p) :a4))
                      :on-change #(set-p! :size (keyword (.. % -target -value)))}
      (for [s ["a3" "a4" "a5" "letter" "legal" "tabloid"]]
        ^{:key s} [:option {:value s} (str/upper-case s)])]
     [field-label "Ausrichtung"]
     [ui/html-select {:fill true :value (name (or (:orientation p) :portrait))
                      :on-change #(set-p! :orientation (keyword (.. % -target -value)))}
      [:option {:value "portrait"} "Hochformat"]
      [:option {:value "landscape"} "Querformat"]]
     (for [[k label ph] [[:margin "Ränder" "18mm 16mm"] [:header "Kopfzeile" ""]
                         [:footer "Fußzeile" ""]]]
       ^{:key k}
       [:div
        [field-label label]
        [ui/input-group {:value (or (get p k) "") :placeholder ph
                         :on-change #(set-p! k (.. % -target -value))}]])]))

(defn- links-panel []
  (let [ls (:links @open-doc)]
    [:div {:class "np-panel"}
     [:div {:class "np-muted"}
      "Jedes Objekt, das beim Erzeugen gebunden oder als Widget eingebettet ist. "
      "Am Objekt findet man darüber dieses Dokument wieder."]
     (if (empty? ls)
       [:div {:class "np-muted"} "Keine."]
       (for [{:keys [object-type id resource via]} ls
             :let [t (type-kw object-type)]]
         ^{:key (str (or resource (str object-type "/" id)))}
         [:div {:class "np-link"}
          (if resource
            [res/resource-link :graphs resource]
            [oc/object-card t id {:row (linked-row t id) :compact? true
                                  :on-open shell/open-object!}])
          [:div {:class "np-muted np-link__via"}
           (if (= "embed" via) "eingebettet" "beim Erzeugen gebunden")]]))]))

(defn- named? [v] (or (:label v) (:endorsed v)))

(defn- versions-panel []
  (let [vs (cond->> @versions (= :named @v-filter) (filter named?))]
    [:div {:class "np-panel"}
     [:div {:class "np-row"}
      [ui/html-select {:fill true :value (name @v-filter)
                       :on-change #(send! [:notebook/set :v-filter
                                           (keyword (.. % -target -value))])}
       [:option {:value "named"} "Benannte Versionen"]
       [:option {:value "all"} "Alle Versionen"]]
      [btn {:label "+ Version" :tone :ghost
            :on-click #(send! [:notebook/set :dialog
                                {:save-version {:label "" :endorsed false}}])}]]
     (when @v-open
       [:div {:class "np-version np-version--current" :on-click #(send! [:notebook/set :v-open nil])}
        "← Aktuelle Sitzung"])
     (if (empty? vs)
       [:div {:class "np-muted"}
        (if (= :named @v-filter) "Keine benannten Versionen — „+ Version“ legt eine an."
            "Keine.")]
       (for [v vs]
         ^{:key (:seq v)}
         [:div {:class (str "np-version" (when (= (:seq v) (:seq @v-open)) " is-active"))}
          [ui/tooltip
           {:content (i18n/t :ui/notebook-version-seq {:n (:seq v)})}
           [:div {:class (str "np-version__title" (when (:endorsed v) " is-endorsed"))
                  :on-click #(send! [:notebook.version/open v false])}
            (when (:endorsed v)
              [ui/tooltip {:content (i18n/t :ui/notebook-version-endorsed)}
               [:span {:class "np-version__star"}
                "★ "]])
            (version-title v)]]
          [:div {:class "np-muted"}
           (str (fmt-ts (:created-at v))
                (when (:author v) (str " · " (principals/principal-label (:author v)))))]
          [:div {:class "np-row"}
           [link {:on-click #(send! [:notebook.version/open v false])} "Ansehen"]
           [link {:on-click #(send! [:notebook.version/open v true])} "Änderungen"]]]))]))

(defn- save-version-dialog []
  (let [sv    (:save-version @dialog)
        close #(send! [:notebook/set :dialog nil])]
    [overlay/modal
     {:size "380px"
      :on-close close
      :title "Version speichern"
      :foot [:<>
             [action-btn {:label "Abbrechen" :tone :ghost :on-click close}]
             [action-btn
              {:label "Speichern" :disabled? (or @busy (str/blank? (:label sv)))
               :on-click #(send! [:notebook.version/save (:label sv) (:endorsed sv)])}]]
      :foot-end? true
      :close-on-backdrop? false}
     [field-label "Name"]
     [ui/input-group {:value (:label sv) :auto-focus true
                      :placeholder "z. B. Freigabe an Kunde"
                      :on-change #(send! [:notebook.dialog/set [:save-version :label]
                                          (.. % -target -value)])}]
     [ui/checkbox
      {:class "np-check" :label "Bestätigt (markiert eine freigegebene Fassung)"
       :checked (boolean (:endorsed sv))
       :on-change #(send! [:notebook.dialog/set [:save-version :endorsed]
                           (.. % -target -checked)])}]]))

(defn- error-toast
  [_]
  (let [k       "notebook-error"
        dismiss #(send! [:notebook/set :err nil])
        show (fn [msg dismiss]
               (ui/show! (ui/default-toaster)
                         {:message msg :intent :danger :timeout 0 :on-dismiss dismiss}
                         k))]
    (r/create-class
     {:display-name "notebook-error-toast"
      :component-did-mount (fn [this] (show (second (r/argv this)) dismiss))
      :component-did-update (fn [this] (show (second (r/argv this)) dismiss))
      :component-will-unmount
      (fn [this]
        (when (some #(= k (:key %)) (ui/get-toasts (ui/default-toaster)))
          (show (second (r/argv this)) nil)
          (ui/dismiss! (ui/default-toaster) k)))
      :reagent-render (fn [_] nil)})))

(defn- print-mode []
  (let [fr (atom nil)]
    (fn []
      [ui/overlay {:is-open true :on-close #(send! [:notebook/set :print-view nil])
                   :has-backdrop false
                   :can-outside-click-close false :class "sutra-layer"}
       [:div {:class "np-print"}
        [:div {:class "np-print__bar"}
         [:span {:class "np-strong"} "Druckmodus"]
         [:span {:class "np-muted"}
          "Seitengröße im Druckdialog wie im Dokument wählen, Ränder „Standard“."]
         [:span {:class "np-grow"}]
         [btn {:label "Drucken…" :on-click #(some-> @fr .-contentWindow .print)}]
         [btn {:label "Schließen" :tone :ghost
               :on-click #(send! [:notebook/set :print-view nil])}]]
        [:iframe {:class "np-print__frame" :src-doc @print-view :ref #(reset! fr %)
                  :sandbox "allow-same-origin allow-modals" :title "Druckansicht"}]]])))

(defn- close-menu! [k] (send! [:notebook.dialog/close-menu k]))

(defn- dropdown
  [{:keys [k left?]} trigger & items]
  [ui/popover {:is-open            (= k (:menu @dialog))
               :on-interaction     (fn [o _]
                                     (if o (send! [:notebook/set :dialog {:menu k}])
                                         (close-menu! k)))
               :placement          (if left? :bottom-start :bottom-end)
               :autofocus          false
               :enforce-focus      false
               :class              "np-dropdown"
               :popover-class-name "np-dropdown__menu"
               :content            (into [:<>] items)}
   trigger])

(defn- menu-btn [k label items]
  [dropdown {:k k}
   [btn {:label (str label " ▾") :tone :ghost}]
   (into [ui/menu]
         (for [[l f d] (remove nil? items)]
           [ui/menu-item {:text l :on-click #(f) :intent (when d :danger)}]))])

(defn- export! [fmt] (send! [:notebook/export fmt]))

(defn- open-print-mode! [] (send! [:notebook/print]))

(defn- new-from-this! [] (send! [:notebook.template/from-doc]))

(defn- delete! [] (send! [:notebook/delete]))

(defn- export-menu []
  [menu-btn :export "Export"
   (if (sheet-doc? @open-doc)
     [["Als XLSX exportieren" #(export! :xlsx)]
      ["Als CSV exportieren (erstes Blatt)" #(export! :csv)]
      ["Als PDF exportieren" #(export! :pdf)]
      ["Als HTML herunterladen" #(export! :html)]
      ["Im Druckmodus öffnen" open-print-mode!]]
     [["Als PDF exportieren" #(export! :pdf)]
      ["Als DOCX exportieren" #(export! :docx)]
      ["Als HTML herunterladen" #(export! :html)]
      ["Im Druckmodus öffnen" open-print-mode!]])])

(defn- action-menu []
  (let [d @open-doc]
    [menu-btn :actions "Aktionen"
     [(when (:template d) ["Neu aus dieser Vorlage…" new-from-this!])
      ["Duplizieren" #(create! {:name (str (:name d) " (Kopie)") :format (:format d)
                                :body (:body d) :page (:page d)
                                :project (some-> (catalog/project-of (:project d)) name)})]
      [(i18n/t :ui/notebook-move) #(send! [:notebook/set :dialog {:move true}])]
      (when-not (own-format? d) ["Als Vorlage speichern…"
                                #(send! [:notebook/set :dialog {:templatize ""}])])
      (when-not (own-format? d) ["Widgets neu laden" rich/refresh-widgets!])
      ["Löschen" delete! true]]]))

(defn- presence
  []
  (when-let [st (live-state)]
    (let [names (live/others st)]
      [ui/tooltip {:content (case (:status st)
                              :live    (if (seq names)
                                         (str "Gemeinsam mit " (str/join ", " names))
                                         "Live — Änderungen anderer erscheinen sofort")
                              :offline "Keine Verbindung — Änderungen werden nachgereicht"
                              "Verbindet …")}
       [:span {:class (str "np-live np-live--" (name (:status st)))}
        [:span {:class "np-live__dot"}]
        (for [u names]
          ^{:key u}
          [:span {:class "np-avatar" :style {:background (live/color-of u)}}
           (str/upper-case (or (first u) "?"))])]])))

(defn- save-badge []
  (let [[txt cls] (case @save-state
                    :saved  ["Gespeichert" ""]
                    :dirty  ["Ungespeichert…" ""]
                    :saving ["Speichert…" ""]
                    :error  ["Nicht gespeichert" " is-error"])]
    [:span {:class (str "np-save" cls)} txt]))

(defn- param-value
  [pspec v]
  (let [t (type-kw (:object-type pspec))]
    (if (and t (#{:object :object-set} (some-> (:type pspec) keyword)))
      (into [:<>]
            (interpose ", "
                       (for [id (if (sequential? v) v [v])]
                         ^{:key (str id)}
                         [oc/object-link t id {:row (linked-row t id)
                                               :on-open shell/open-object!}])))
      (str (if (sequential? v) (str/join ", " v) v)))))

(defn- open-project! [pk] (send! [:notebook/open-project pk]))

(defn- move-to! [pk] (send! [:notebook/move pk]))

(defn- project-chip []
  (let [pk (catalog/project-of (:project @open-doc))]
    (if (:move @dialog)
      [:span {:class "np-chip"}
       [res/project-select "" #(when (seq %) (move-to! (keyword %))) false]
       [link {:on-click #(send! [:notebook/set :dialog nil])} (i18n/t :ui/cancel)]]
      [ui/tooltip {:content (i18n/t :ui/notebook-project)}
       [:span {:class "np-chip"}
        (if pk
          [link {:on-click #(open-project! pk)} (name pk)]
          [:span (i18n/t :ui/kompass-default-scope)])]])))

(defn- provenance []
  (let [d @open-doc
        t (template-of (:template d))]
    (when (:template d)
      [ui/callout {:intent :warning :compact true :icon nil}
       (i18n/t :ui/notebook-generated-from) " "
       [link {:on-click #(send! [:notebook/set :dialog {:template-page (:template d)}])}
        (template-title (:template d))]
       (when (:template-version d)
         (str " · " (i18n/t :ui/notebook-template-version {:v (:template-version d)})))
       (for [[k v] (sort-by key (:parameters d))]
         ^{:key (name k)}
         [:span " · " (tbl/humanize k) ": " [param-value (get-in t [:parameters k]) v]])
       " · " (i18n/t :ui/notebook-as-of {:at (fmt-ts (:updated-at d))}) " "
       [btn {:label "Daten aktualisieren" :tone :ghost :disabled? @busy
             :on-click #(send! [:notebook/regenerate])
             :title (str "Mit denselben Eingaben neu aus der Vorlage erzeugen — "
                         "der vorige Stand bleibt als Version")}]])))

(defn- tb
  [label title active? f & [disabled?]]
  [ui/tooltip {:content title}
   [ui/button (cond-> {:minimal true :small true :active active? :disabled disabled?
                       :aria-label title
                       :on-mouse-down (fn [e] (.preventDefault e) (f))}
                (keyword? label) (assoc :icon label))
    (when-not (keyword? label) label)]])

(defn- block-style [ed]
  (cond (.isActive ed "heading" #js {:level 1}) "h1"
        (.isActive ed "heading" #js {:level 2}) "h2"
        (.isActive ed "heading" #js {:level 3}) "h3"
        :else "p"))

(defn- link! [v]
  (when (some? v)
    (if (str/blank? v)
      (chain! (fn [^js c] (-> c (.extendMarkRange "link") (.unsetLink))))
      (chain! (fn [^js c] (-> c (.extendMarkRange "link") (.setLink #js {:href (str/trim v)})))))))

(defn- set-link! []
  (let [cur (some-> ^js @editor (.getAttributes "link") .-href)]
    (send! [:notebook.editor/prompt {:title "Link" :label "Adresse" :default (or cur "")
                                    :placeholder "https://… oder /notebook/…"}
            link!])))

(defn- pick-image! []
  (some-> @image-input .click))

(defn- toolbar []
  @tick
  (when-let [^js ed @editor]
    [page/toolbar
     (tb :undo "Rückgängig" false #(chain! (fn [^js c] (.undo c))) (not (.can ed)))
     (tb :redo "Wiederholen" false #(chain! (fn [^js c] (.redo c))))
     [divider]
     [ui/html-select
      {:minimal true :value (block-style ed)
       :on-change (fn [e]
                    (let [v (.. e -target -value)]
                      (if (= v "p")
                        (chain! (fn [^js c] (.setParagraph c)))
                        (chain! (fn [^js c]
                                  (.setHeading c #js {:level (js/parseInt (subs v 1))}))))))}
      [:option {:value "p"} "Text"]
      [:option {:value "h1"} "Überschrift 1"]
      [:option {:value "h2"} "Überschrift 2"]
      [:option {:value "h3"} "Überschrift 3"]]
     [divider]
     (tb :bold "Fett (Strg+B)" (.isActive ed "bold") #(chain! (fn [^js c] (.toggleBold c))))
     (tb :italic "Kursiv (Strg+I)" (.isActive ed "italic") #(chain! (fn [^js c] (.toggleItalic c))))
     (tb :underline "Unterstrichen (Strg+U)" (.isActive ed "underline")
         #(chain! (fn [^js c] (.toggleUnderline c))))
     (tb :strikethrough "Durchgestrichen" (.isActive ed "strike")
         #(chain! (fn [^js c] (.toggleStrike c))))
     (tb :code "Code" (.isActive ed "code") #(chain! (fn [^js c] (.toggleCode c))))
     (tb :link "Link" (.isActive ed "link") set-link!)
     [divider]
     (tb :properties "Aufzählung" (.isActive ed "bulletList")
         #(chain! (fn [^js c] (.toggleBulletList c))))
     (tb :numbered-list "Nummerierung" (.isActive ed "orderedList")
         #(chain! (fn [^js c] (.toggleOrderedList c))))
     (tb :citation "Zitat" (.isActive ed "blockquote") #(chain! (fn [^js c] (.toggleBlockquote c))))
     [divider]
     (tb :th "Tabelle einfügen" false
         #(chain! (fn [^js c] (.insertTable c #js {:rows 3 :cols 3 :withHeaderRow true}))))
     (tb :column-layout "Zwei Spalten" false #(rich/insert-columns! ed))
     (tb :media "Bild einfügen" false pick-image!)
     (tb :minus "Trennlinie" false #(chain! (fn [^js c] (.setHorizontalRule c))))
     (tb :page-layout "Seitenumbruch" false
         #(chain! (fn [^js c] (.insertContent c #js {:type "pageBreak"}))))
     [divider]
     [dropdown {:k :widget :left? true}
      [ui/button
       {:minimal true :small true :intent :primary :icon :plus
        :on-mouse-down #(.preventDefault %)}
       "Widget"]
      (into [ui/menu]
            (for [[k label hint] rich/widget-kinds]
              [ui/menu-item {:text [:<> [:div label] [:div {:class "sutra-menu__sub"} hint]]
                             :multiline true
                             :on-mouse-down #(.preventDefault %)
                             :on-click #(open-widget-dialog! k {} nil)}]))]
     (when (.isActive ed "table")
       [:<>
        [divider]
        [:span {:class "np-muted"} "Tabelle:"]
        (tb "+ Zeile" "Zeile darunter" false #(chain! (fn [^js c] (.addRowAfter c))))
        (tb "− Zeile" "Zeile löschen" false #(chain! (fn [^js c] (.deleteRow c))))
        (tb "+ Spalte" "Spalte rechts" false #(chain! (fn [^js c] (.addColumnAfter c))))
        (tb "− Spalte" "Spalte löschen" false #(chain! (fn [^js c] (.deleteColumn c))))
        (tb "Kopfzeile" "Kopfzeile ein/aus" false #(chain! (fn [^js c] (.toggleHeaderRow c))))
        (tb "Löschen" "Tabelle löschen" false #(chain! (fn [^js c] (.deleteTable c))))])]))

(defn- layout-toolbar []
  [page/toolbar
   [ui/segmented-control
    {:small true :value @layout-mode
     :on-value-change #(send! [:notebook/layout-mode %])
     :options [{:value :view :label "Ansicht"} {:value :source :label "Quelltext"}]}]
   [:span {:class "np-muted"}
    "Layout-Dokument: die Gestaltung kommt aus der Vorlage, bearbeitet wird der Quelltext."]])

(defn- mention-users []
  (let [me (:username @auth/auth)]
    (->> (vals @principals/principal-names)
         (remove #(or (str/blank? %) (= % me)))
         distinct sort
         (mapv (fn [u] {:user u :label u})))))

(defn- rich-surface [d]
  (let [el (atom nil)]
    (r/create-class
     {:display-name "notebook-rich"
      :component-did-mount
      (fn [_]
        (let [s    @session
              user (or (:username @auth/auth) "")
              ed   (rich/create!
                    {:el          @el
                     :host        {:open-object    shell/open-object!
                                   :mention-users  mention-users
                                   :resource-label resource-label
                                   :current-user   (:username @auth/auth)
                                   :widget-request (fn [_ed kind]
                                                     (if (= kind "image")
                                                       (pick-image!)
                                                       (open-widget-dialog! kind {} nil)))}
                     :content     (:body d)
                     :editable?   (or (nil? s) (boolean (:edit? (live-state))))
                     :live        s
                     :user        user
                     :color       (live/color-of user)
                     :placeholder
                     "Schreiben … „/“ fügt Blöcke und Widgets ein, „@“ erwähnt jemanden"
                     :on-update   (fn [html] (send! [:notebook/edit {:body html :format :html}]))
                     :on-select   (fn [ed] (reset! sel-widget (rich/selected-widget ed)))
                     :on-transaction (fn [_] (swap! tick inc))})]
          (reset! editor ed)
          (when (and s @seed-pending)
            (send! [:notebook.live/seeded])
            (.setContent (.-commands ^js ed) (or (:body d) ""))
            (live/set-meta! s :name (:name d))
            (live/set-meta! s :page (:page d))))
        (scroll-to-hash!))
      :component-will-unmount
      (fn [_] (some-> @editor .destroy) (reset! editor nil) (reset! sel-widget nil))
      :reagent-render
      (fn [_] [:div {:class "np-rich" :ref #(reset! el %)}])})))

(defn- sheet-surface [d]
  (let [s @session]
    [sheet/editor {:id        (:id d)
                   :body      (:body d)
                   :live      s
                   :edit?     (boolean (and s (:edit? (live-state))))
                   :user      (or (:username @auth/auth) "")
                   :seed?     #(when @seed-pending (send! [:notebook.live/seeded]) true)
                   :on-seeded #(live/set-meta! s :name (:name d))
                   :on-body   #(send! [:notebook/edit {:body % :format :sheet}])}]))

(defn- markup-surface [d]
  (let [s @session]
    [markup/editor {:id      (:id d)
                    :body    (:body d)
                    :live    s
                    :edit?   (boolean (and s (:edit? (live-state))))
                    :user    (or (:username @auth/auth) "")
                    :project (:project d)
                    :on-project #(move-to! (keyword %))
                    :on-body #(send! [:notebook/edit {:body % :format :markup}])}]))

(defn- diagram-surface [d]
  (let [s @session]
    [diagram/editor {:id      (:id d)
                     :name    (:name d)
                     :body    (:body d)
                     :live    s
                     :edit?   (boolean (and s (:edit? (live-state))))
                     :user    (or (:username @auth/auth) "")
                     :project (:project d)
                     :on-body #(send! [:notebook/edit {:body % :format :diagram}])}]))

(defn- live-gate
  [d surface]
  (let [s  @session
        st (live-state)]
    (if (and s (not (:synced? st)))
      [busy/block {:class "np-center"}
       (if (= :offline (:status st)) "Keine Verbindung — neuer Versuch …" "verbindet …")]
      ^{:key (str (:id d) "/" @reload "/" (some-> s ^js (:ydoc) .-clientID))} [surface d])))

(defn- rich-readonly [body]
  (let [el (atom nil) ed (atom nil)]
    (r/create-class
     {:component-did-mount
      (fn [_] (reset! ed (rich/create! {:el @el :content body :editable? false
                                               :host {:open-object shell/open-object!}})))
      :component-will-unmount (fn [_] (some-> @ed .destroy))
      :reagent-render (fn [_] [:div {:class "np-rich np-rich--readonly" :ref #(reset! el %)}])})))

(defn- paper [page & children]
  (let [[w h] (page-dims page)]
    (into [:div {:class "np-paper"
                 :style {:width (str w "mm") :min-height (str h "mm")
                         :padding (or (not-empty (:margin page)) "18mm 16mm")}}
           (when (seq (:header page)) [:div {:class "np-paper__header"} (:header page)])
           (when (seq (:footer page)) [:div {:class "np-paper__footer"} (:footer page)])]
          children)))

(defn- layout-surface []
  (let [d @open-doc]
    (case @layout-mode
      :source [:div {:class "np-source"}
               [ce/code-editor {:value (:body d) :min-height "70vh"
                                :on-change #(send! [:notebook/edit {:body %}])}]]
      (if-let [html @rendered]
        [paper (:page d)
         [:iframe {:src-doc html :sandbox "allow-same-origin" :title (:name d)
                   :class "np-frame" :ref #(reset! frame-el %) :on-load index-frame!}]]
        [busy/block {:class "np-center"} "wird gerendert…"]))))

(defn- version-surface []
  (let [v @v-open
        d @open-doc]
    [:div {:class "np-version-view"}
     [:div {:class "np-banner"}
      [:span {:class "np-grow"}
       (i18n/t (if (:diff? v) :ui/notebook-version-diff-banner :ui/notebook-version-banner)
               {:name (version-title v) :at (fmt-ts (:created-at v))})]
      [btn {:label "Aktuelle Sitzung" :tone :ghost
            :on-click #(send! [:notebook/set :v-open nil])}]
      [btn {:label "Als neues Dokument" :tone :ghost
            :on-click #(create! {:name (i18n/t :ui/notebook-version-copy-name
                                               {:name (:name d) :at (fmt-ts (:created-at v))})
                                 :format (:format d) :body (:body v) :page (:page d)})}]
      [btn
       {:label "Auf diese Version zurücksetzen" :disabled? @busy
        :on-click #(send! [:notebook.version/confirm-restore v])}]]
     (cond
       (:diff? v)
       [:div {:class "np-diff"}
        [:div {:class "np-muted"} "Rot: nur in der Version · Grün: nur im aktuellen Stand"]
        ^{:key (str (:seq v) "/" (hash (:body d)))}
        [ce/code-diff {:baseline (:body v) :value (:body d) :min-height "300px"}]]

       (sheet-doc? d)
       [sheet/static {:id (:id d) :body (:body v)}]

       (markup-doc? d)
       [markup/static {:id (:id d) :body (:body v)}]

       (diagram-doc? d)
       [diagram/static {:id (:id d) :body (:body v)}]

       (layout-doc? {:body (:body v)})
       [paper (:page d)
        [:iframe {:src-doc (wrap-html (:body v)) :sandbox "" :class "np-frame np-frame--tall"
                  :title (version-title v)}]]

       :else
       [paper (:page d) ^{:key (:seq v)} [rich-readonly (:body v)]])]))

(defn- left-panel []
  (let [sheet? (own-format? @open-doc)
        uc?    (use-case/use-case-doc? @open-doc)
        tabs   (cond->> [[:outline "Gliederung"] [:versions "Versionen"]
                         [:links (str "Verknüpft" (when (seq (:links @open-doc))
                                                    (str " " (count (:links @open-doc)))))]
                         [:page "Seite"]]
                 sheet? (filterv (comp #{:versions :links} first))
                 uc?    (into [[:use-case "Assistent"]] (filter (comp #{:versions :links} first))))
        tab    (if (some #(= @left-tab (first %)) tabs) @left-tab (if uc? :use-case :versions))]
    [:div {:class "np-tools"}
     [ui/tabs {:id :np-left :selected-tab-id tab
               :on-change #(send! [:notebook/tab %])}
      (for [[k label] tabs]
        [ui/tab {:id k :title label}])]
     (case tab
       :use-case [use-case/panel @open-doc]
       :versions [versions-panel]
       :links    [links-panel]
       :page     [page-settings]
       [outline-panel])]))

(defn- edit-instead! [] (send! [:notebook/edit-instead]))

(defn- read-only? [] (boolean (and @open-doc (:read-only? @ui))))

(defn- own-surface? [d] (or (own-format? d) (use-case/use-case-doc? d)))

(defn- back-button []
  [ui/tooltip {:content "Zur Notebook-Startseite"}
   [ui/button {:minimal true :icon :arrow-left :on-click close!} "Notebook"]])

(defn- doc-header [d]
  (if (read-only?)
    [page/header {:icon [back-button] :title (:name d)}
     [btn {:label    (i18n/t :ui/notebook-read-only-edit)
           :title    (i18n/t :ui/notebook-read-only-hint)
           :on-click edit-instead!}]]
    [page/header {:icon  [back-button]
                  :title [ui/editable-text {:value (:name d) :placeholder ""
                                            :on-change #(send! [:notebook/edit {:name %}])}]}
     (when (saves-here?) [save-badge])
     [presence]
     [project-chip]
     (when-not (or (markup-doc? d) (diagram-doc? d)) [export-menu])
     [action-menu]]))

(defn- doc-toolbar [d]
  (when-not (read-only?)
    [:<>
     [provenance]
     (cond (layout-doc? d) [layout-toolbar] (own-surface? d) nil :else [toolbar])]))

(defn- read-only-canvas
  [d]
  [:div {:class "np-canvas"}
   (cond
     (layout-doc? d) [layout-surface]
     (sheet-doc? d)  [sheet/static {:id (:id d) :body (:body d)}]
     (markup-doc? d) [markup/static {:id (:id d) :body (:body d)}]
     (diagram-doc? d) [diagram/static {:id (:id d) :body (:body d)}]
     (= :markdown (keyword (:format d)))
     [busy/block {:class "np-center"} "wird geöffnet…"]
     :else [paper (:page d) ^{:key (hash (:body d))} [rich-readonly (:body d)]])])

(defn- doc-canvas [d]
  (let [own? (own-surface? d)]
    [:div {:class (str "np-canvas" (when (and own? (not @v-open)) " np-canvas--sheet"))}
     (cond
       @v-open            [version-surface]
       (layout-doc? d)    [layout-surface]
       (sheet-doc? d)     [live-gate d sheet-surface]
       (markup-doc? d)    [live-gate d markup-surface]
       (diagram-doc? d)   [live-gate d diagram-surface]
       (use-case/use-case-doc? d) [use-case/surface d]
       (= :markdown (keyword (:format d)))
       [busy/block {:class "np-center"} "wird geöffnet…"]
       :else              [paper (:page d) [live-gate d rich-surface]])
     [rich/suggestion-menu]]))

(defn- linked-summary [d]
  (let [ls (filterv :object-type (:links d))]
    (when (seq ls)
      (let [{:keys [object-type id]} (first ls)
            t (type-kw object-type)]
        [:span
         [oc/object-link t id {:row (linked-row t id) :on-open shell/open-object!}]
         (when (> (count ls) 1) [:span {:class "np-muted"} (str " +" (dec (count ls)))])]))))

(defn- matches? [d]
  (let [q (str/lower-case (str/trim @search))
        t @tpl-filter
        p @proj-filter]
    (and (or (str/blank? q)
             (str/includes? (str/lower-case (str (:name d) " " (some-> (:template d)
                                                                       template-title)))
                            q))
         (case t
           ""       true
           "__frei" (nil? (:template d))
           (= t (:template d)))
         (or (= p "") (= p (some-> (catalog/project-of (:project d)) name))
             (and (= p "__none") (nil? (catalog/project-of (:project d))))))))

(defn- new-in-select []
  [:label {:class "np-newin"}
   (i18n/t :ui/notebook-new-in)
   (into [ui/html-select
          {:value (str (some-> (target-project) name))
           :on-change #(send! [:notebook/set :new-in
                                  (or (some-> (.. % -target -value) not-empty keyword)
                                      ::doc/none)])}
          [:option {:value ""} (i18n/t :ui/kompass-default-scope)]]
         (for [pk (sort-by name (keys (catalog/project-list)))]
           [:option {:value (name pk)} (name pk)]))])

(defn- new-card [icon title hint on-click]
  [:div {:class "np-newcard" :on-click on-click}
   [:div {:class "np-newcard__icon"} [ui/icon {:icon icon}]]
   [:div [:div {:class "np-strong"} title] [:div {:class "np-muted"} hint]]])

(def ^:private format-icon
  (fn [d] (cond (sheet-doc? d) :th (markup-doc? d) :annotation (diagram-doc? d) :graph
                (use-case/use-case-doc? d) :diagram-tree)))

(def ^:private doc-cols
  [{:label "Name" :cell-class "np-strong"
    :render (fn [d] [:<> (when-let [i (format-icon d)] [ui/icon {:icon i :size 14}]) " "
                     (:name d)])}
   {:label "Vorlage" :cell-class "np-muted"
    :render (fn [d]
              (if (:template d)
                (str (template-title (:template d))
                     (when (:template-version d) (str " v" (:template-version d))))
                (i18n/t :ui/notebook-free)))}
   {:label  "Verknüpft"
    :render (fn [d]
              [:span {:on-click #(when (some-> (.-target %) (.closest "a"))
                                   (.stopPropagation %))}
               [linked-summary d]])}
   {:label "Projekt" :cell-class "np-muted"
    :render (fn [d] (or (some-> (catalog/project-of (:project d)) name) "—"))}
   {:label "Geändert" :cell-class "np-muted"
    :render (fn [d] (str (fmt-ts (:updated-at d))
                         (when (:author d)
                           (str " · " (principals/principal-label (:author d))))))}])

(defn- home-page []
  (let [tpls  (sort (distinct (keep :template @docs)))
        shown (sort-by :updated-at #(compare %2 %1) (filter matches? @docs))]
    [:div {:class "np-home"}
     [:section {:class "np-home__new"}
      [new-card :document "Neues Dokument" "Leere Seite, „/“ für Widgets" #(create! {})]
      [new-card :th "Neue Tabelle" "Formeln, mehrere Blätter, gemeinsam bearbeiten"
       #(create! {:format :sheet :name "Unbenannte Tabelle"})]
      [new-card :annotation "Neues Aufmaß"
       "Fotos vom Ort, Maße und Notizen, gemeinsam besprechen"
       #(create! {:format :markup :name "Aufmaß" :body (m/body m/empty-model)})]
      [new-card :graph "Neue Zeichnung"
       "Schema mit Symbolen aus der Ontologie, Anschlüssen und Leitungen"
       #(create! {:format :diagram :name "Zeichnung" :body (dg/body dg/empty-model)})]
      [new-card :diagram-tree "Neuer Use-Case-Entwurf"
       "Objektmodell und Lebenszyklen, mit Assistent"
       #(create! {:format :use-case :name "Use-Case-Entwurf" :body use-case/starter})]
      [new-card :duplicate "Neu aus Vorlage" (str (count @templates) " Vorlagen")
       #(send! [:notebook/set :dialog {:pick? true}])]
      [new-in-select]]
     (when (seq @templates)
       [:section
        [:div {:class "np-section-title"} "Vorlagen"]
        [:div {:class "np-tpl-grid"}
         (for [t (sort-by #(str (or (:title %) (:template-name %))) @templates)]
           ^{:key (:template-name t)}
           [:div {:class "np-tpl-card"}
            [:div {:class "np-tpl-card__title"
                   :on-click #(send! [:notebook/set :dialog
                                          {:template-page (:template-name t)}])}
             (or (:title t) (tbl/humanize (:template-name t)))]
            [:div {:class "np-muted np-tpl-card__desc"} (or (:description t) "")]
            [:div {:class "np-tpl-card__foot"}
             [:span {:class "np-muted"}
              (str (count (:parameters t)) " Eingaben"
                   (when (:version t) (str " · v" (:version t))))]
             [link {:on-click #(pick-template! t)}
              "Neu aus dieser Vorlage"]]])]])
     [:section
      [:div {:class "np-row np-home__filters"}
       [:div {:class "np-section-title np-grow"} "Dokumente"]
       (when (seq tpls)
         [ui/html-select {:value @tpl-filter
                          :on-change #(send! [:notebook/set :tpl-filter
                                              (.. % -target -value)])}
          [:option {:value ""} "Alle Vorlagen"]
          [:option {:value "__frei"} "Frei geschrieben"]
          (for [t tpls] ^{:key t} [:option {:value t} (template-title t)])])
       [ui/html-select {:value @proj-filter
                        :on-change #(send! [:notebook/set :proj-filter
                                            (.. % -target -value)])}
        [:option {:value ""} "Alle Projekte"]
        [:option {:value "__none"} (i18n/t :ui/kompass-default-scope)]
        (for [pk (sort-by name (keys (catalog/project-list)))]
          ^{:key pk} [:option {:value (name pk)} (name pk)])]]
      (if (empty? shown)
        (if (seq @docs)
          [ui/empty-state {:icon :search :title "Nichts gefunden."}]
          [ui/empty-state {:icon :document
                           :title "Noch keine Dokumente — „Neues Dokument“ legt eins an."}])
        [table/data-table {:cols doc-cols :rows shown :row-key :id :class "np-doctable"
                           :on-row-click #(open! (:id %))}])]]))

(defn- follow-route! [_ _ old new]
  (let [doc (fn [u] (:doc (feature/slice u :notebook)))
        id  (doc new)]
    (when (and (not= (doc old) id) (not= id (:id @open-doc)))
      (if id (open! id) (close!)))))

(defn- flush-on-leave! [_] (send! [:notebook/flush]))

(defn- image-file-input []
  [:input {:type "file" :accept "image/*" :hidden true :ref #(reset! image-input %)
           :on-change (fn [e]
                        (when-let [f (some-> e .-target .-files (aget 0))]
                          (when-let [ed @editor] (rich/read-image! ed f)))
                        (set! (.. e -target -value) ""))}])

(defn- header-view [_ _]
  (if-let [d @open-doc]
    [doc-header d]
    [page/header {:title "Notebook"}
     [ui-filter/bar {:value @search :placeholder "Dokumente suchen…"
                     :on-search #(send! [:notebook/set :search %])}]]))

(defn- toolbar-view [_ _] (when-let [d @open-doc] [doc-toolbar d]))

(defn- canvas-view [_ _]
  (if-let [d @open-doc]
    (if (read-only?) [read-only-canvas d] [doc-canvas d])
    [home-page]))

(def ^:private tools-open?
  (reify IDeref (-deref [_] (boolean (and @open-doc (not (read-only?)))))))

(def ^:private widget-open?
  (reify IDeref
    (-deref [_]
      (let [d @open-doc]
        (boolean (and d @sel-widget (not (read-only?)) (not (layout-doc? d))
                      (not (own-surface? d)) (not @v-open)))))))

(page/view! :notebook/header header-view)
(page/view! :notebook/toolbar toolbar-view)
(page/view! :notebook/tools (fn [_ _] [left-panel]))
(page/view! :notebook/canvas canvas-view)
(page/view! :notebook/widget (fn [_ _] [widget-panel]))

(def ^:private tree
  (page/dokument
   {:header  :notebook/header
    :toolbar :notebook/toolbar
    :flow?   false
    :body    (page/werkzeug
              {:sidebar       :notebook/tools
               :sidebar-label "Dokument"
               :sidebar-when  tools-open?
               :sidebar-w     250
               :main          {:kind  :columns
                               :items [{:kind :rows :box {:grow? true}
                                        :items [(page/view :notebook/canvas)]}
                                       {:kind :rows :scroll? true :when widget-open?
                                        :padding :compact :box {:w 270}
                                        :items [(page/view :notebook/widget)]}]}})}))

(defn notebook-app []
  (r/create-class
   {:display-name "notebook"
    :component-did-mount
    (fn [_]
      (add-watch shell/ui-state ::notebook-route follow-route!)
      (.addEventListener js/window "beforeunload" flush-on-leave!)
      (send! [:notebook/enter]))
    :component-will-unmount
    (fn [_]
      (remove-watch shell/ui-state ::notebook-route)
      (.removeEventListener js/window "beforeunload" flush-on-leave!)
      (send! [:notebook/leave]))
    :reagent-render
    (fn []
      [:<>
       [platform/module :notebook tree]
       [image-file-input]
       (when @print-view [print-mode])
       (when (:pick? @dialog) [template-picker])
       (when (:template @dialog) [generate-dialog])
       (when (:template-page @dialog) [template-page])
       (when (:save-version @dialog) [save-version-dialog])
       (when (contains? @dialog :templatize) [templatize-dialog])
       (when @wdialog [widget-dialog])
       (when (and @err (not (:template @dialog)))
         [error-toast @err])])}))
