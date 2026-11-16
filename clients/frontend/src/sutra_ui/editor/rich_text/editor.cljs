(ns sutra-ui.editor.rich-text.editor
  (:require ["@tiptap/core" :refer [Editor Node Extension]]
            ["@tiptap/starter-kit" :default StarterKit]
            ["@tiptap/extension-table" :refer [TableKit]]
            ["@tiptap/extension-image" :default Image]
            ["@tiptap/extension-collaboration" :refer [Collaboration]]
            ["@tiptap/extension-collaboration-caret" :refer [CollaborationCaret]]
            ["@tiptap/extensions" :refer [Placeholder]]
            ["@tiptap/suggestion" :default Suggestion]
            ["@tiptap/pm/state" :refer [PluginKey]]
            [clojure.string :as str]
            [goog.string :as gstr]
            [sutra.kit.url :as url]
            [reagent.core :as r]
            [sutra-ui.api :as api]
            [sutra-ui.ui.core :as ui]))

(def widget-attrs
  ["kind" "type" "id" "property" "properties" "ids" "where" "order-by" "limit" "show-name"
   "format" "tz" "auto" "value" "user" "label" "href" "function" "args" "output"
   "no-link" "hover" "locked" "snapshot" "locked-at" "locked-by"
   "graph" "types" "derived" "use-case"])

(def inline-kinds #{"object-property" "current-date" "mention" "resource-link"})

(defonce menu (r/atom nil))

(defonce ^:private hosts (js/WeakMap.))

(defn- host
  [^js ed k]
  (when ed (get (.get hosts ed) k)))

(def ^:private kept-tags
  #{"a" "abbr" "b" "blockquote" "br" "caption" "code" "col" "colgroup" "dd" "del" "div" "dl"
    "dt" "em" "figcaption" "figure" "h1" "h2" "h3" "h4" "h5" "h6" "hr" "i" "img" "ins" "kbd"
    "li" "mark" "ol" "p" "pre" "q" "s" "small" "span" "strong" "sub" "sup" "table" "tbody" "td"
    "tfoot" "th" "thead" "time" "tr" "u" "ul"})

(def ^:private dropped-tags
  #{"script" "style" "iframe" "frame" "frameset" "object" "embed" "applet" "template"
    "noscript" "noembed" "noframes" "xmp" "plaintext" "svg" "math" "link" "meta" "base"
    "title" "head" "form" "input" "button" "textarea" "select" "option" "audio" "video"
    "source" "track" "canvas" "portal"})

(def ^:private kept-attrs
  #{"class" "title" "lang" "dir" "colspan" "rowspan" "scope" "alt" "width" "height"
    "datetime" "start" "reversed"})

(defn- clean-attr [tag n v]
  (cond
    (= "href" n)  (when (= "a" tag) (url/safe-url v))
    (= "src" n)   (when (= "img" tag) (url/image-src v))
    (kept-attrs n) v))

(defn- copy-clean!
  [^js doc ^js from ^js to]
  (doseq [^js n (array-seq (.-childNodes from))]
    (case (.-nodeType n)
      3 (.appendChild to (.createTextNode doc (.-data n)))
      1 (let [tag (str/lower-case (.-localName n))]
          (cond
            (dropped-tags tag) nil
            (kept-tags tag)
            (let [e (.createElement doc tag)]
              (doseq [^js a (array-seq (.-attributes n))
                      :let [an (str/lower-case (.-name a))
                            v  (clean-attr tag an (.-value a))]
                      :when (some? v)]
                (.setAttribute e an v))
              (copy-clean! doc n e)
              (.appendChild to e))
            :else (copy-clean! doc n to)))
      nil)))

(defn sanitize
  [html]
  (let [doc (.parseFromString (js/DOMParser.) (str html) "text/html")
        out (.createElement doc "div")]
    (copy-clean! doc (.-body doc) out)
    (.-innerHTML out)))

(defn attrs-map [^js node]
  (into {} (keep (fn [a] (when-let [v (aget (.-attrs node) a)] [(keyword a) v])))
        widget-attrs))

(defn- cache-key [attrs] (pr-str (into (sorted-map) (dissoc attrs :locked :snapshot
                                                            :locked-at :locked-by))))

(defonce ^:private cache (atom {}))
(defonce ^:private live (atom #{}))

(defn widget-html
  [attrs]
  (let [k (cache-key attrs)]
    (or (get @cache k)
        (let [p (-> (api/post! :document-widget {:attrs (dissoc attrs :locked :snapshot
                                                                :locked-at :locked-by)})
                    (.then (fn [r] (sanitize (:html r))))
                    (.catch (fn [e] (str "<span class=\"np-missing\">"
                                         (gstr/htmlEscape (or (.-message e) "Widget nicht ladbar"))
                                         "</span>"))))]
          (swap! cache assoc k p)
          p))))

(def object-kinds #{"object-property" "object-card" "markdown-property"})
(def ^:private data-kinds #{"object-property" "object-card" "object-table" "markdown-property"
                            "function"})

(defn- el [tag cls & [text]]
  (let [e (.createElement js/document tag)]
    (set! (.-className e) cls)
    (when text (set! (.-textContent e) text))
    e))

(defn- button [label title f]
  (let [b (el "button" "np-wbtn" label)]
    (.setAttribute b "type" "button")
    (when title (.setAttribute b "title" title))
    (.addEventListener b "mousedown" (fn [e] (.preventDefault e) (.stopPropagation e) (f)))
    b))

(defn- fmt-ts [t]
  (let [d (js/Date. (str t))]
    (if (js/isNaN (.getTime d)) (str t)
        (.toLocaleString d "de-AT" #js {:dateStyle "medium" :timeStyle "short"}))))

(defn- open-object! [ed {:keys [type id]}]
  (when-let [f (host ed :open-object)] (f (keyword type) id)))

(defn- set-attrs-at! [^js editor get-pos attrs]
  (let [^js tr (.. editor -state -tr)
        pos    (get-pos)]
    (.setNodeMarkup tr pos js/undefined
                    (clj->js (into {} (for [a widget-attrs
                                            :let [v (get attrs (keyword a))]]
                                        [a (when (some? v) (str v))]))))
    (.dispatch (.-view editor) tr)))

(defn- lock-at! [editor get-pos attrs]
  (-> (widget-html attrs)
      (.then (fn [html]
               (set-attrs-at! editor get-pos
                              (assoc attrs :locked "true" :snapshot html
                                     :locked-at (.toISOString (js/Date.))
                                     :locked-by (host editor :current-user)))))))

(defn- update-data-at! [editor get-pos attrs]
  (swap! cache dissoc (cache-key attrs))
  (set-attrs-at! editor get-pos (dissoc attrs :locked :snapshot :locked-at :locked-by)))

(defn- link-click [^js editor e]
  (when-let [a (some-> (.-target e) (.closest "a[href]"))]
    (.preventDefault e)
    (when-not (.-isEditable editor)
      (let [href (.getAttribute a "href")]
        (if-let [[_ t id] (re-matches #"/object/([^/]+)/(.+)" (str href))]
          (open-object! editor {:type t :id (js/decodeURIComponent id)})
          (when (re-matches #"(?i)(/|https?://).*" (str href))
            (.open js/window href "_blank" "noopener")))))))

(defn- paint-content! [content attrs]
  (if (= "true" (:locked attrs))
    (set! (.-innerHTML content) (sanitize (:snapshot attrs)))
    (do (when (str/blank? (.-innerHTML content))
          (set! (.-innerHTML content) "<span class=\"np-widget__loading\">lädt…</span>"))
        (-> (widget-html attrs)
            (.then (fn [html] (set! (.-innerHTML content) html)))))))

(defn- paint-chrome!
  [{:keys [bar foot]} ^js editor get-pos attrs]
  (set! (.-innerHTML bar) "")
  (set! (.-innerHTML foot) "")
  (when (and (contains? object-kinds (:kind attrs)) (:type attrs) (:id attrs))
    (.appendChild bar (button "Öffnen" "Objekt in der Objektansicht öffnen"
                              #(open-object! editor attrs))))
  (when (and (.-isEditable editor) (contains? data-kinds (:kind attrs)))
    (if (= "true" (:locked attrs))
      (do (.appendChild foot (el "span" "np-wfoot__lock"
                                 (str "🔒 Gesperrt · " (fmt-ts (:locked-at attrs))
                                      (when (:locked-by attrs) (str " · " (:locked-by attrs))))))
          (.appendChild foot (button "Daten aktualisieren"
                                     "Entsperrt und lädt die Daten neu — der Snapshot geht verloren"
                                     #(update-data-at! editor get-pos attrs))))
      (.appendChild foot (button "Daten sperren" "Hält den jetzigen Stand als Snapshot fest"
                                 #(lock-at! editor get-pos attrs))))))

(defn- hover-card! [pop attrs]
  (when (and (= "object-property" (:kind attrs)) (= "true" (:hover attrs))
             (:type attrs) (:id attrs))
    (when (str/blank? (.-innerHTML pop))
      (-> (widget-html {:kind "object-card" :type (:type attrs) :id (:id attrs)
                        :no-link (:no-link attrs)})
          (.then (fn [html] (set! (.-innerHTML pop) html)))))))

(defn- node-view [inline?]
  (fn [^js props]
    (let [node    (atom (.-node props))
          editor  (.-editor props)
          get-pos (.-getPos props)
          dom     (el (if inline? "span" "div")
                      (str "np-widget " (if inline? "np-widget--inline" "np-widget--block")))
          content (el (if inline? "span" "div") "np-widget__content")
          parts   {:bar  (el (if inline? "span" "div") "np-wbar")
                   :foot (el (if inline? "span" "div") "np-wfoot")
                   :pop  (el (if inline? "span" "div") "np-wpop")}
          entry   {:content content :node node}
          paint   (fn [n]
                    (let [attrs (attrs-map n)]
                      (.toggle (.-classList dom) "np-widget--locked" (= "true" (:locked attrs)))
                      (set! (.-innerHTML (:pop parts)) "")
                      (paint-content! content attrs)
                      (paint-chrome! parts editor get-pos attrs)))]
      (.setAttribute dom "contenteditable" "false")
      (.setAttribute dom "data-kind" (str (aget (.-attrs ^js @node) "kind")))
      (doseq [c [content (:bar parts) (:foot parts) (:pop parts)]] (.appendChild dom c))
      (.addEventListener dom "click" (partial link-click editor))
      (.addEventListener dom "mouseenter" #(hover-card! (:pop parts) (attrs-map @node)))
      (swap! live conj entry)
      (paint @node)
      #js {:dom dom
           :update (fn [n]
                     (if (not= (.-type n) (.-type @node))
                       false
                       (do (when (not= (attrs-map n) (attrs-map @node))
                             (reset! node n)
                             (paint n))
                           true)))
           :selectNode #(.add (.-classList dom) "is-selected")
           :deselectNode #(.remove (.-classList dom) "is-selected")
           :stopEvent (fn [e] (boolean (some-> (.-target e) (.closest ".np-wbtn"))))
           :ignoreMutation (fn [] true)
           :destroy #(swap! live disj entry)})))

(defn refresh-widgets!
  []
  (reset! cache {})
  (doseq [{:keys [content node]} @live] (paint-content! content (attrs-map @node))))

(defn- attr-spec []
  (clj->js
   (into {} (for [a widget-attrs]
              [a {:default nil
                  :parseHTML (fn [el] (.getAttribute el (str "data-" a)))
                  :renderHTML (fn [attrs]
                                (let [v (aget attrs a)]
                                  (if (some? v) (js-obj (str "data-" a) v) #js {})))}]))))

(defn- widget-node [nm inline?]
  (.create Node
           #js {:name nm
                :group (if inline? "inline" "block")
                :inline inline?
                :atom true
                :selectable true
                :draggable true
                :addAttributes attr-spec
                :parseHTML (fn []
                             #js [#js {:tag "sutra-widget"
                                       :getAttrs (fn [el]
                                                   (if (= inline? (contains? inline-kinds
                                                                             (.getAttribute
                                                                              el "data-kind")))
                                                     nil
                                                     false))}])
                :renderHTML (fn [^js p] #js ["sutra-widget" (.-HTMLAttributes p)])
                :addNodeView (fn [] (node-view inline?))}))

(def ^:private page-break
  (.create Node
           #js {:name "pageBreak" :group "block" :atom true :selectable true
                :parseHTML (fn [] #js [#js {:tag "div.page-break"}])
                :renderHTML (fn [] #js ["div" #js {:class "page-break"}])}))

(def ^:private columns
  (.create Node
           #js {:name "npColumns" :group "block" :content "npColumn{2,3}" :defining true
                :parseHTML (fn [] #js [#js {:tag "div.np-cols"}])
                :renderHTML (fn [] #js ["div" #js {:class "np-cols"} 0])}))

(def ^:private column
  (.create Node
           #js {:name "npColumn" :content "block+" :isolating true
                :parseHTML (fn [] #js [#js {:tag "div.np-col"}])
                :renderHTML (fn [] #js ["div" #js {:class "np-col"} 0])}))

(defn- suggestion-render []
  (fn []
    #js {:onStart (fn [^js p] (reset! menu {:items (vec (.-items p)) :index 0
                                            :rect ((.-clientRect p)) :command (.-command p)}))
         :onUpdate (fn [^js p] (swap! menu assoc :items (vec (.-items p)) :index 0
                                      :rect ((.-clientRect p)) :command (.-command p)))
         :onKeyDown (fn [p]
                      (let [k (.. p -event -key) {:keys [items index command]} @menu
                            n (count items)]
                        (case k
                          "ArrowDown" (do (swap! menu assoc :index (mod (inc index) (max 1 n)))
                                          true)
                          "ArrowUp"   (do (swap! menu assoc :index (mod (dec index) (max 1 n)))
                                          true)
                          "Enter"     (if (pos? n) (do (command (nth items index)) true) false)
                          "Escape"    (do (reset! menu nil) true)
                          false)))
         :onExit (fn [] (reset! menu nil))}))

(defn- chain [^js ed range] (-> (.chain ed) (.focus) (.deleteRange range)))

(defn insert-widget!
  [^js ed attrs]
  (let [kind (str (:kind attrs))
        nm   (if (contains? inline-kinds kind) "sutraWidget" "sutraWidgetBlock")]
    (-> (.chain ed) (.focus)
        (.insertContent #js {:type nm :attrs (clj->js (into {} (for [[k v] attrs
                                                                     :when (some? v)]
                                                                 [(name k) (str v)])))})
        (.run))))

(defn insert-columns! [^js ed]
  (-> (.chain ed) (.focus)
      (.insertContent (clj->js {:type "npColumns"
                                :content [{:type "npColumn" :content [{:type "paragraph"}]}
                                          {:type "npColumn" :content [{:type "paragraph"}]}]}))
      (.run)))

(def widget-kinds
  [["object-property" "Objekt-Property" "Ein Wert eines Objekts im Text"]
   ["object-card" "Objektkarte" "Titel und Eigenschaften eines Objekts"]
   ["object-table" "Objekttabelle" "Eine Zeile je Objekt eines Typs"]
   ["markdown-property" "Markdown-Property" "Eine Markdown-Eigenschaft als Text"]
   ["function" "Function" "Das Ergebnis einer Function"]
   ["current-date" "Aktuelles Datum" "Datum oder Uhrzeit"]
   ["resource-link" "Ressourcen-Link" "Link auf Dokument, Graph, App, Objekt"]
   ["graph" "Data-Lineage-Graph" "Ein gespeicherter Data-Lineage-Graph, gezeichnet"]
   ["object-model" "Objektmodell" "Skizze: Kern, abgeleitet, Use Case"]
   ["lifecycle" "Lebenszyklus" "Zustände einer Eigenschaft und ihre Actions"]
   ["use-case-draft" "Use-Case-Entwurf"
    "Objektmodell, Lebenszyklus oder Plan aus einem Entwurf, bevor es die Typen gibt"]])

(defn- block-items []
  [{:label "Text" :hint "Absatz" :run #(-> (chain %1 %2) (.setParagraph) (.run))}
   {:label "Überschrift 1" :hint "Groß" :run #(-> (chain %1 %2) (.setHeading #js {:level 1})
                                                  (.run))}
   {:label "Überschrift 2" :hint "Mittel" :run #(-> (chain %1 %2) (.setHeading #js {:level 2})
                                                    (.run))}
   {:label "Überschrift 3" :hint "Klein" :run #(-> (chain %1 %2) (.setHeading #js {:level 3})
                                                   (.run))}
   {:label "Aufzählung" :hint "• Liste" :run #(-> (chain %1 %2) (.toggleBulletList) (.run))}
   {:label "Nummerierung" :hint "1. Liste" :run #(-> (chain %1 %2) (.toggleOrderedList) (.run))}
   {:label "Zitat" :hint "Block" :run #(-> (chain %1 %2) (.toggleBlockquote) (.run))}
   {:label "Code" :hint "Block" :run #(-> (chain %1 %2) (.toggleCodeBlock) (.run))}
   {:label "Tabelle" :hint "3 × 3"
    :run #(-> (chain %1 %2) (.insertTable #js {:rows 3 :cols 3 :withHeaderRow true}) (.run))}
   {:label "Spalten" :hint "Zwei Spalten"
    :run (fn [^js ed range] (-> (chain ed range) (.run)) (insert-columns! ed))}
   {:label "Bild" :hint "Datei wählen"
    :run (fn [^js ed range] (-> (chain ed range) (.run))
           (when-let [f (host ed :widget-request)] (f ed "image")))}
   {:label "Linie" :hint "Trennlinie" :run #(-> (chain %1 %2) (.setHorizontalRule) (.run))}
   {:label "Seitenumbruch" :hint "Druck"
    :run #(-> (chain %1 %2) (.insertContent #js {:type "pageBreak"}) (.run))}])

(defn- widget-items []
  (for [[k label hint] widget-kinds]
    {:label label :hint hint :widget? true
     :run (fn [^js ed range]
            (-> (chain ed range) (.run))
            (when-let [f (host ed :widget-request)] (f ed k)))}))

(defn- matching [items q]
  (let [q (str/lower-case (str q))]
    (into-array (filter #(str/includes? (str/lower-case (str (:label %) " " (:hint %))) q)
                        items))))

(defn- suggestion-ext [nm ch items-fn]
  (.create Extension
           #js {:name nm
                :addProseMirrorPlugins
                (fn []
                  (this-as this
                           #js [(Suggestion
                                 #js {:editor (.-editor ^js this)
                                      :char ch
                                      :pluginKey (PluginKey. nm)
                                      :allowSpaces false
                                      :items (fn [^js p]
                                               (matching (items-fn (.-editor p)) (.-query p)))
                                      :command (fn [^js p]
                                                 ((:run (.-props p)) (.-editor p) (.-range p)))
                                      :render (suggestion-render)})]))}))

(def ^:private slash
  (suggestion-ext "npSlash" "/" (fn [_] (concat (block-items) (widget-items)))))

(def ^:private mentions
  (suggestion-ext "npMention" "@"
                  (fn [ed]
                    (for [{:keys [user label]} (when-let [f (host ed :mention-users)] (f))]
                      {:label label :hint (str "@" user)
                       :run (fn [^js ed range]
                              (-> (chain ed range) (.run))
                              (insert-widget! ed {:kind "mention" :user user :label label})
                              (-> (.chain ed) (.insertContent " ") (.run)))}))))

(defn read-image!
  [^js ed file]
  (let [rd (js/FileReader.)]
    (set! (.-onload rd) #(-> (.chain ed) (.focus) (.setImage #js {:src (.-result rd)}) (.run)))
    (.readAsDataURL rd file)))

(defn- same-origin-path [text]
  (let [t (str/trim (str text))]
    (when (and (str/starts-with? t (.-origin js/location)) (not (re-find #"\s" t)))
      (subs t (count (.-origin js/location))))))

(defn- handle-paste [ed]
  (fn [_view event]
    (let [dt    (.-clipboardData event)
          files (some->> dt .-files array-seq (filter #(str/starts-with? (.-type %) "image/")))
          text  (some-> dt (.getData "text/plain"))
          html  (some-> dt (.getData "text/html"))]
      (cond
        (seq files) (do (doseq [f files] (read-image! @ed f)) true)
        (and (str/blank? html) (same-origin-path text))
        (let [path (same-origin-path text)]
          (insert-widget! @ed {:kind "resource-link" :href path
                               :label ((or (host @ed :resource-label) identity) path)})
          true)
        :else false))))

(defn- handle-drop [ed]
  (fn [_view event _slice moved?]
    (let [files (some->> event .-dataTransfer .-files array-seq
                         (filter #(str/starts-with? (.-type %) "image/")))]
      (if (and (not moved?) (seq files))
        (do (.preventDefault event) (doseq [f files] (read-image! @ed f)) true)
        false))))

(defn- live-extensions
  [{:keys [ydoc aw]} user color]
  [(.configure Collaboration #js {:document ydoc})
   (.configure CollaborationCaret #js {:provider #js {:awareness aw}
                                       :user #js {:name user :color color}})])

(defn create!
  [{:keys [el content editable? placeholder on-update on-select on-transaction live user color]
    lent :host}]
  (let [ed (atom nil)
        e  (Editor.
            #js {:element el
                 :editable (boolean editable?)
                 :extensions
                 (into-array
                  (concat
                   [(.configure StarterKit
                                (clj->js (cond-> {:heading {:levels [1 2 3]}
                                                  :link {:openOnClick false :autolink true}}
                                           live (assoc :undoRedo false))))
                    (.configure TableKit #js {:table #js {:resizable false}})
                    (.configure Image #js {:allowBase64 true})
                    (.configure Placeholder #js {:placeholder (or placeholder "")})
                    (widget-node "sutraWidget" true)
                    (widget-node "sutraWidgetBlock" false)
                    page-break columns column slash mentions]
                   (when live (live-extensions live user color))))
                 :content (if live nil (or content ""))
                 :editorProps #js {:handlePaste (handle-paste ed) :handleDrop (handle-drop ed)}
                 :onUpdate (fn [^js p] (when on-update (on-update (.getHTML ^js (.-editor p)))))
                 :onSelectionUpdate (fn [^js p] (when on-select (on-select (.-editor p))))
                 :onTransaction (fn [^js p] (when on-transaction (on-transaction (.-editor p))))})]
    (.set hosts e lent)
    (reset! ed e)
    e))

(defn selected-widget
  [^js ed]
  (let [^js sel (.. ed -state -selection)
        ^js n   (.-node sel)]
    (when (and n (#{"sutraWidget" "sutraWidgetBlock"} (.. n -type -name)))
      {:pos (.-from sel) :attrs (attrs-map n)})))

(defn set-widget-attrs!
  [^js ed pos attrs]
  (let [^js tr (.. ed -state -tr)
        ^js n  (.nodeAt (.. ed -state -doc) pos)]
    (when n
      (.setNodeMarkup tr pos js/undefined
                      (clj->js (into {} (for [a widget-attrs
                                              :let [v (get attrs (keyword a))]]
                                          [a (when (some? v) (str v))]))))
      (.dispatch (.-view ed) tr))))

(defn headings
  [^js ed]
  (let [out (array)]
    (.descendants (.. ed -state -doc)
                  (fn [^js n pos]
                    (when (= "heading" (.. n -type -name))
                      (.push out {:level (.. n -attrs -level) :text (.-textContent n) :pos pos}))
                    true))
    (vec out)))

(defn scroll-to-pos! [^js ed pos]
  (when-let [dom (.nodeDOM (.-view ed) pos)]
    (.scrollIntoView dom #js {:behavior "smooth" :block "start"})))

(defn suggestion-menu
  []
  (when-let [{:keys [items index rect command]} @menu]
    (when rect
      [ui/popover
       {:is-open            true
        :on-interaction     (fn [o _] (when-not o (reset! menu nil)))
        :placement          :bottom-start
        :minimal            true
        :autofocus          false
        :enforce-focus      false
        :position-strategy  :fixed
        :popover-class-name "np-menu"
        :render-target
        (fn [{:keys [ref]}]
          [ui/portal
           [:div {:style {:position "fixed" :left (.-left rect) :top (.-top rect)
                          :width (.-width rect) :height (.-height rect)}
                  :ref   ref}]])
        :content
        (if (empty? items)
          [:div {:class "np-menu__empty"} "Nichts gefunden"]
          (for [[i it] (map-indexed vector items)]
            ^{:key i}
            [:div {:class (str "np-menu__item" (when (= i index) " is-active")
                               (when (and (:widget? it)
                                          (not (:widget? (get items (dec i)))))
                                 " np-menu__item--first-widget"))
                   :on-mouse-down (fn [e] (.preventDefault e) (command it))}
             [:span {:class "np-menu__label"} (:label it)]
             [:span {:class "np-menu__hint"} (:hint it)]]))}])))
