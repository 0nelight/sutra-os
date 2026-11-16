(ns sutra-ui.apps.notebook.markup
  (:require ["yjs" :as Y]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.image-annotation :as ia]
            [sutra.capps.markup :as m]
            [sutra.types.paths :as paths]
            [sutra-ui.api :as api]
            [sutra-ui.download :as dl]
            [sutra-ui.apps.notebook.events]
            [sutra-ui.apps.notebook.live :as live]
            [sutra-ui.apps.notebook.ymodel :as ym]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.resource.core :as res]
            [sutra-ui.state.catalog :as catalog]))

(def ^:private max-side 2400)
(def ^:private finger-px 22)
(def ^:private body-ms 400)
(def ^:private presence-ms 60)

(def ^:private tool-labels
  {:select ["Wählen" :hand-up] :dim ["Maß" :arrows-horizontal] :note ["Notiz" :annotation]
   :area ["Bereich" :widget]})

(defn- y-entries [ydoc] (ym/entries ydoc m/collections))

(defn- seed! [ydoc model] (ym/seed! ydoc (m/entries model)))

(defn- clock
  [iso]
  (when iso
    (let [d (js/Date. iso)]
      (if (= (.toDateString d) (.toDateString (js/Date.)))
        (.toLocaleTimeString d "de-AT" #js {:hour "2-digit" :minute "2-digit"})
        (.toLocaleString d "de-AT" #js {:day "numeric" :month "numeric" :hour "2-digit"
                                        :minute "2-digit"})))))

(defn- shrink
  [file]
  (js/Promise.
   (fn [ok fail]
     (let [url (js/URL.createObjectURL file)
           img (js/Image.)]
       (set! (.-onerror img) (fn [_] (js/URL.revokeObjectURL url) (fail (js/Error. "kein Bild"))))
       (set! (.-onload img)
             (fn [_]
               (let [w0 (.-naturalWidth img) h0 (.-naturalHeight img)
                     k  (min 1 (/ max-side (max w0 h0)))
                     w  (js/Math.round (* k w0)) h (js/Math.round (* k h0))
                     cv (.createElement js/document "canvas")]
                 (set! (.-width cv) w)
                 (set! (.-height cv) h)
                 (.drawImage (.getContext cv "2d") img 0 0 w h)
                 (js/URL.revokeObjectURL url)
                 (ok {:b64 (second (str/split (.toDataURL cv "image/jpeg" 0.85) #","))
                      :w w :h h}))))
       (set! (.-src img) url)))))

(defn- src-of [file] (paths/url :kompass-file {:id (str file)}))

(defn- data-url [file]
  (-> (api/fetch-url (src-of file) #js {:credentials "same-origin"})
      (.then #(.blob %))
      (.then (fn [b] (js/Promise. (fn [ok _]
                                    (let [fr (js/FileReader.)]
                                      (set! (.-onload fr) #(ok (.-result fr)))
                                      (.readAsDataURL fr b))))))))

(defn- title-of [file]
  (let [n (str/replace (.-name file) #"\.[^.]+$" "")]
    (if (re-matches #"(?i)img_?\d+|image|photo|\d+" n) "Foto" n)))

(defn- seg [[[x1 y1] [x2 y2]] attrs]
  [:line (merge {:x1 x1 :y1 y1 :x2 x2 :y2 y2} attrs)])

(defn- mark-svg
  [{:keys [kind a b box] :as mk} {:keys [stroke font tick]} sel?]
  (let [c   (m/color-of mk)
        txt (m/caption mk)
        cls {:class (str "np-mk__mark" (when sel? " is-sel"))}]
    (case kind
      :dim
      (let [{:keys [line ticks label angle]} (m/dim-geometry a b tick)
            [lx ly] label]
        [:g cls
         [:g {:stroke c :stroke-width stroke}
          (seg line {}) (seg (first ticks) {}) (seg (second ticks) {})]
         [:text {:x lx :y ly :fill c :font-size font :text-anchor "middle"
                 :transform (str "rotate(" angle " " lx " " ly ")")} txt]])
      :note
      (let [[x y] a]
        [:g cls
         [:circle {:cx x :cy y :r tick :fill c}]
         [:text {:x (+ x (* 1.5 tick)) :y (+ y (* 0.35 font)) :fill c :font-size font} txt]])
      :area
      (let [{:keys [x1 y1 x2 y2]} box]
        [:g cls
         [:rect {:x x1 :y y1 :width (- x2 x1) :height (- y2 y1) :fill c :fill-opacity 0.12
                 :stroke c :stroke-width stroke}]
         [:text {:x x1 :y (- y1 (* 0.4 font)) :fill c :font-size font} txt]])
      nil)))

(defn- handles-svg [mk {:keys [tick]}]
  (for [[k [x y]] (m/handles mk)]
    ^{:key (name k)} [:circle {:class "np-mk__handle" :cx x :cy y :r (* 1.4 tick)}]))

(defn- others-svg [others marks {:keys [stroke font tick]}]
  (for [{:keys [name color cursor line sel]} others]
    ^{:key name}
    [:g {:class "np-mk__other" :style {:color color}}
     (when-let [mk (some #(when (= sel (:id %)) %) marks)]
       (when-let [[x y] (or (:a mk) (some-> (:box mk) ((juxt :x1 :y1))))]
         [:circle {:cx x :cy y :r (* 2.2 tick) :fill "none" :stroke color
                   :stroke-width stroke}]))
     (when line
       (seg line {:stroke color :stroke-width stroke :stroke-dasharray (str (* 2 tick))}))
     (when-let [[x y] cursor]
       [:g
        [:circle {:cx x :cy y :r (* 0.8 tick) :fill color}]
        [:text {:x (+ x tick) :y (- y tick) :fill color :font-size (* 0.8 font)} name]])]))

(defn- box-of [^js rc]
  {:left (.-left rc) :top (.-top rc) :width (.-width rc) :height (.-height rc)})

(defn- image-point
  [{:keys [x y w h]} {:keys [left top width height]} [cx cy]]
  [(+ x (* w (/ (- cx left) (max 1 width))))
   (+ y (* h (/ (- cy top) (max 1 height))))])

(defn- midpoint [[ax ay] [bx by]] [(/ (+ ax bx) 2) (/ (+ ay by) 2)])

(defn- distance [[ax ay] [bx by]] (js/Math.hypot (- bx ax) (- by ay)))

(defn- pinched
  [{v0 :view [mx0 my0] :mid d :d} a b {:keys [left top width]} size]
  (let [k  (/ (:w v0) (max 1 width))
        c0 [(+ (:x v0) (* k (- mx0 left))) (+ (:y v0) (* k (- my0 top)))]
        v1 (m/zoom v0 c0 (/ (distance a b) (max 1 d)) size)
        k1 (/ (:w v1) (max 1 width))
        [mx my] (midpoint a b)]
    (m/pan v1 (* k1 (- mx mx0)) (* k1 (- my my0)))))

(defn- panned [{[x0 y0] :pan v :view} [x y] width]
  (let [k (/ (:w v) (max 1 width))]
    (m/pan v (* k (- x x0)) (* k (- y y0)))))

(defn- client-xy [^js e] [(.-clientX e) (.-clientY e)])

(defn- toast! [msg]
  (ui/show! (ui/default-toaster) {:message msg :intent :warning :timeout 4000}))

(defn- model [{:keys [ydoc base]}]
  (let [y (when ydoc (m/from-entries (y-entries ydoc)))]
    (if (and y (not (m/blank? y))) y base)))

(defn- writable!
  [{:keys [can? ydoc base]}]
  (when can?
    (when (and (m/blank? (m/from-entries (y-entries ydoc))) (not (m/blank? base)))
      (seed! ydoc base))
    true))

(defn- put! [{:keys [ydoc] :as c} coll id v]
  (when (writable! c) (.set (ym/ymap ydoc coll) id (pr-str v))))

(defn- del! [{:keys [ydoc] :as c} coll id]
  (when (writable! c) (.delete (ym/ymap ydoc coll) id)))

(defn- photo-of [{:keys [ui]} mdl]
  (let [ps (m/photos mdl) pid (:photo @ui)]
    (or (some #(when (= pid (:id %)) %) ps) (first ps))))

(defn- size [p] {:w (:w p) :h (:h p)})

(defn- stage-box [{:keys [stage]}] (some-> ^js @stage .getBoundingClientRect))

(defn- view-of [{:keys [ui] :as c} p]
  (or (get-in @ui [:views (:id p)])
      (let [^js rc (stage-box c)]
        (m/fit (size p) (if rc (/ (.-width rc) (max 1 (.-height rc))) 1.5)))))

(defn- set-view! [{:keys [ui]} p v] (swap! ui assoc-in [:views (:id p)] v))

(defn- to-img [c p ^js e]
  (image-point (view-of c p) (box-of (stage-box c)) (client-xy e)))

(defn- tol
  [c p]
  (* finger-px (/ (:w (view-of c p)) (max 1 (some-> (stage-box c) .-width)))))

(defn- publish! [{:keys [^js aw ui last-pub] :as c} extra force?]
  (when (and aw (or force? (> (- (js/Date.now) @last-pub) presence-ms)))
    (reset! last-pub (js/Date.now))
    (let [{:keys [st]} @ui p (photo-of c (model c))]
      (.setLocalStateField
       aw "markup"
       (clj->js (merge {:photo (:id p) :sel (:sel st)
                        :line  (m/draft-line st (m/marks-on (model c) (:id p)))}
                       extra))))))

(defn- select! [{:keys [ui] :as c} id]
  (swap! ui #(-> % (assoc-in [:st :sel] id) (dissoc :mm-error)))
  (publish! c {} true))

(defn- effect! [{:keys [ui user] :as c} p [kind a b]]
  (case kind
    :select (select! c a)
    :create (let [id (ym/new-id)]
              (put! c :marks id (assoc a :photo (:id p) :by user :at (ym/now)))
              (select! c id)
              (when (= :dim (:kind a)) (swap! ui assoc :focus-mm id)))
    :update (when-let [mk (get-in (model c) [:marks a])]
              (put! c :marks a (merge mk b)))))

(defn- event!
  [{:keys [ui can?] :as c} p ev]
  (let [[st fx] (m/step (:st @ui) ev {:marks (m/marks-on (model c) (:id p))
                                      :size  (size p)
                                      :tol   (tol c p)})]
    (swap! ui assoc :st st)
    (run! #(effect! c p %) (if can? fx (filter (comp #{:select} first) fx)))))

(defn- patch! [c id f & args]
  (when-let [mk (get-in (model c) [:marks id])]
    (put! c :marks id (apply f mk args))))

(defn- drop-mark! [c mdl id]
  (doseq [cm (m/thread mdl id)] (del! c :comments (:id cm)))
  (del! c :marks id))

(defn- remove-mark! [{:keys [^js ydoc] :as c} id]
  (when (writable! c)
    (let [mdl (model c)]
      (.transact ydoc #(drop-mark! c mdl id)))
    (select! c nil)))

(defn- to! [{:keys [frame]} event] (machine/dispatch event {:frame frame}))

(defn- remove-photo! [{:keys [^js ydoc ui] :as c} p]
  (to! c [:notebook.editor/confirm
          {:title         "Foto entfernen?"
           :body          (str "„" (:title p) "“ mit " (count (m/marks-on (model c) (:id p)))
                               " Markierungen. Eine Version hält es.")
           :confirm-label "Entfernen"}
          #(when (writable! c)
             (let [mdl (model c)]
               (.transact ydoc (fn []
                                 (doseq [mk (m/marks-on mdl (:id p))]
                                   (drop-mark! c mdl (:id mk)))
                                 (del! c :photos (:id p)))))
             (swap! ui assoc :photo nil))]))

(defn- upload-photo!
  [proj f]
  (-> (shrink f)
      (.then (fn [{:keys [b64 w h]}]
               (-> (api/post! :kompass-files {:name (str (title-of f) ".jpg") :project proj
                                              :mime "image/jpeg" :bytes-b64 b64})
                   (.then (fn [res] {:w w :h h :title (title-of f) :file (:id res)})))))))

(defn- upload-one! [{:keys [ui user] :as c} proj f]
  (to! c [:rf/request
          {:ask  [upload-photo! proj f]
           :busy [[:uploading f] true]
           :then [[:rf/invoke
                   (fn [photo]
                     (let [id (ym/new-id)]
                       (put! c :photos id
                             (assoc photo :pos (m/next-pos (model c)) :by user :at (ym/now)))
                       (swap! ui assoc :photo id)))]]
           :else [(fn [s] [:rf/invoke toast! (str "Foto nicht hochgeladen: " s)])]}]))

(defn- upload! [{:keys [project] :as c} files]
  (if-let [proj (some-> (catalog/project-of project) name)]
    (doseq [f files] (upload-one! c proj f))
    (toast! "Das Dokument liegt in keinem Projekt — verschiebe es erst in eins.")))

(defn- svg! [c p]
  (to! c [:notebook.editor/call [data-url (:file p)]
          #(dl/text! (m/photo-svg (model c) (:id p) %) "image/svg+xml" (str (:title p) ".svg"))
          #(toast! (str "SVG nicht erzeugt: " %))]))

(defn- comment! [{:keys [user] :as c} mid text]
  (when-not (str/blank? text)
    (put! c :comments (ym/new-id) {:mark mid :by user :at (ym/now) :text (str/trim text)})))

(defn- body-changed! [{:keys [last-body on-body] :as c}]
  (let [mdl (model c) b (m/body mdl)]
    (when (and (not= b @last-body) (not (m/blank? mdl)))
      (reset! last-body b)
      (on-body b))))

(defn- down! [{:keys [ui ptrs gesture] :as c} p ^js e]
  (.setPointerCapture (.-currentTarget e) (.-pointerId e))
  (swap! ptrs assoc (.-pointerId e) (client-xy e))
  (if (= 2 (count @ptrs))
    (let [[a b] (vals @ptrs)]
      (event! c p [:cancel])
      (reset! gesture {:d (distance a b) :mid (midpoint a b) :view (view-of c p)}))
    (let [pt (to-img c p e)]
      (event! c p [:down pt])
      (when (and (= :select (get-in @ui [:st :tool]))
                 (nil? (get-in @ui [:st :grab]))
                 (nil? (m/hit (m/marks-on (model c) (:id p)) pt (tol c p))))
        (reset! gesture {:pan (client-xy e) :view (view-of c p)})))))

(defn- move! [{:keys [ptrs gesture] :as c} p ^js e]
  (swap! ptrs #(if (contains? % (.-pointerId e)) (assoc % (.-pointerId e) (client-xy e)) %))
  (let [g @gesture]
    (cond
      (and (:d g) (= 2 (count @ptrs)))
      (let [[a b] (vals @ptrs)]
        (set-view! c p (pinched g a b (box-of (stage-box c)) (size p))))

      (:pan g)
      (set-view! c p (panned g (client-xy e) (.-width (stage-box c))))

      :else
      (let [pt (to-img c p e)]
        (event! c p [:move pt])
        (publish! c {:cursor pt} false)))))

(defn- up! [{:keys [ptrs gesture] :as c} p ^js e]
  (let [g @gesture]
    (swap! ptrs dissoc (.-pointerId e))
    (cond
      (:d g)   (when (empty? @ptrs) (reset! gesture nil))
      (:pan g) (do (reset! gesture nil)
                   (event! c p (if (> (distance (:pan g) (client-xy e)) 5)
                                 [:cancel]
                                 [:up (to-img c p e)])))
      :else    (event! c p [:up (to-img c p e)]))
    (publish! c {} true)))

(defn- wheel! [c p ^js e]
  (.preventDefault e)
  (set-view! c p (m/zoom (view-of c p) (to-img c p e)
                         (js/Math.pow 1.0015 (- (.-deltaY e))) (size p))))

(defn- context
  [{:keys [body live edit? user project on-project on-body]}]
  (let [ydoc (some-> live :ydoc)
        ui   (r/atom {:photo nil :st {:tool :select} :views {} :tick 0 :uploading {}})
        c    {:ydoc ydoc :aw (some-> live :aw) :base (m/read-body body)
              :edit? edit? :can? (and ydoc edit?) :user user :project project
              :on-project on-project :on-body on-body
              :ui ui :frame (machine/frame ui {:name :notebook-markup :lifetime :view})
              :stage (atom nil) :ro (atom nil) :ptrs (atom {}) :gesture (atom nil)
              :last-body (atom body) :body-timer (atom nil) :last-pub (atom 0)
              :undo (when ydoc
                      (Y/UndoManager. (into-array (map #(ym/ymap ydoc %) m/collections))))}]
    (assoc c
           :on-y  (fn [_ _]
                    (swap! (:ui c) update :tick inc)
                    (when (and edit? on-body)
                      (js/clearTimeout @(:body-timer c))
                      (reset! (:body-timer c) (js/setTimeout #(body-changed! c) body-ms))))
           :on-aw (fn [_ _] (swap! (:ui c) update :tick inc)))))

(defn- listen! [{:keys [ydoc ^js aw on-y on-aw]}]
  (when ydoc (doseq [k m/collections] (.observe (ym/ymap ydoc k) on-y)))
  (when aw (.on aw "change" on-aw)))

(defn- unmount! [{:keys [ydoc ^js aw ^js undo ro body-timer on-y on-aw frame]}]
  (machine/leave! frame)
  (js/clearTimeout @body-timer)
  (some-> ^js @ro .disconnect)
  (when ydoc (doseq [k m/collections] (.unobserve (ym/ymap ydoc k) on-y)))
  (when aw (.off aw "change" on-aw) (.setLocalStateField aw "markup" nil))
  (some-> undo .destroy))

(defn- stage-ref!
  [{:keys [stage ro ui]} el]
  (when (and el (not= el @stage))
    (reset! stage el)
    (some-> ^js @ro .disconnect)
    (reset! ro (doto (js/ResizeObserver. (fn [_] (swap! ui assoc :views {})))
                 (.observe el)))))

(defn- photo-strip [{:keys [ui can?] :as c} mdl p busy]
  [:div {:class "np-mk__strip"}
   (for [q (m/photos mdl)]
     ^{:key (:id q)}
     [:button {:class (str "np-mk__thumb" (when (= (:id q) (:id p)) " is-on"))
               :title (:title q)
               :on-click #(do (swap! ui assoc :photo (:id q))
                              (swap! ui assoc-in [:st :sel] nil))}
      [:img {:src (src-of (:file q)) :alt (:title q) :loading "lazy"}]
      (let [n (m/open-count mdl (:id q))]
        (when (pos? n) [:span {:class "np-mk__badge"} n]))])
   (when can?
     [:label {:class "np-mk__add"}
      [:input {:type "file" :accept "image/*" :multiple true
               :on-change (fn [^js e]
                            (upload! c (array-seq (.. e -target -files)))
                            (set! (.. e -target -value) ""))}]
      (if (pos? busy) "lädt …" "+ Foto")])])

(defn- no-photo [{:keys [can? project on-project]}]
  [:div {:class "np-mk__empty"}
   [:div {:class "np-strong"} "Noch kein Foto"]
   (cond
     (not can?) [:div {:class "np-muted"} "In diesem Aufmaß ist nichts aufgenommen."]
     (and (nil? (catalog/project-of project)) on-project)
     [:<>
      [:div {:class "np-muted"} "Die Fotos liegen im Projekt des Aufmaßes — welches?"]
      [res/project-select "" #(when (seq %) (on-project %)) false]]
     :else
     [:div {:class "np-muted"} "„+ Foto“ nimmt eins mit der Kamera auf oder aus der Mediathek."])])

(defn- tools [{:keys [^js undo] :as c} p st]
  [:div {:class "np-mk__tools"}
   (for [t m/tools :let [[l icon] (tool-labels t)]]
     ^{:key t}
     [ui/button {:icon icon :large true :text l
                 :active (= t (:tool st))
                 :on-click #(event! c p [:tool t])}])
   [:span {:class "np-grow"}]
   [ui/button {:icon :undo :large true :minimal true :title "Rückgängig"
               :on-click #(some-> undo .undo)}]
   [ui/button {:icon :redo :large true :minimal true :title "Wiederholen"
               :on-click #(some-> undo .redo)}]])

(defn- stage-view [{:keys [ui can?] :as c} {:keys [p st marks sel sc others]}]
  (let [{vx :x vy :y vw :w vh :h} (view-of c p)]
    [:div {:class (str "np-mk__stage np-mk__stage--" (name (:tool st :select)))
           :ref   #(stage-ref! c %)}
     [:svg {:view-box              (str vx " " vy " " vw " " vh)
            :preserve-aspect-ratio "none"
            :on-pointer-down       #(down! c p %)
            :on-pointer-move       #(move! c p %)
            :on-pointer-up         #(up! c p %)
            :on-pointer-cancel     #(up! c p %)
            :on-wheel              #(wheel! c p %)}
      [:image {:href (src-of (:file p)) :width (:w p) :height (:h p)}]
      (for [mk marks]
        ^{:key (:id mk)}
        [mark-svg (m/moved mk st) sc (= (:id mk) (:sel st))])
      (when sel (handles-svg (m/moved sel st) sc))
      (when-let [l (m/draft-line st marks)]
        (seg l {:class "np-mk__draft" :stroke-width (:stroke sc)}))
      (when-let [[x y] (first (:draft st))]
        [:circle {:class "np-mk__handle" :cx x :cy y :r (* 1.4 (:tick sc))}])
      (when-let [a (some-> (:area st) ia/drawing-box)]
        [:rect (merge a {:class "np-mk__draft" :stroke-width (:stroke sc)})])
      (others-svg others marks sc)]
     [:div {:class "np-mk__corner"}
      [ui/button {:icon :zoom-to-fit :minimal true :title "Ganzes Foto"
                  :on-click #(swap! ui update :views dissoc (:id p))}]
      [ui/button {:icon :download :minimal true :title "Als SVG herunterladen"
                  :on-click #(svg! c p)}]
      (when can?
        [ui/button {:icon :trash :minimal true :title "Foto entfernen"
                    :on-click #(remove-photo! c p)}])]]))

(declare photo-panel mark-panel)

(defn editor
  [opts]
  (let [{:keys [ui ^js aw ^js ydoc can?] :as c} (context opts)
        patch*  (partial patch! c)
        remove* (partial remove-mark! c)
        comment* (partial comment! c)
        put*    (partial put! c)
        select* (partial select! c)]
    (listen! c)
    (r/create-class
     {:component-will-unmount (fn [_] (unmount! c))
      :reagent-render
      (fn [_]
        (let [{:keys [st tick uploading]} @ui
              _      tick
              mdl    (model c)
              p      (photo-of c mdl)
              marks  (when p (m/marks-on mdl (:id p)))
              sel    (some #(when (= (:sel st) (:id %)) %) marks)]
          [:div {:class "np-mk"}
           [photo-strip c mdl p (count uploading)]
           (if-not p
             [no-photo c]
             [:<>
              (when can? [tools c p st])
              [stage-view c {:p p :st st :marks marks :sel sel :sc (m/scale-of (size p))
                             :others (when aw (m/others-on (ym/awareness-states aw)
                                                           (.-clientID ydoc) (:id p)))}]
              [:div {:class "np-mk__panel"}
               (if sel
                 [mark-panel {:mk sel :model mdl :can? can? :ui ui
                              :patch! patch* :remove! remove* :comment! comment*
                              :close! #(select* nil)}]
                 [photo-panel {:p p :marks marks :mdl mdl :can? can? :put! put*
                               :pick! select*}])]])]))})))

(defn- photo-panel [{:keys [p marks mdl can? put! pick!]}]
  [:div {:class "np-mk__list"}
   (if can?
     [:input {:class "np-mk__title" :default-value (:title p) :key (:id p)
              :on-blur #(let [t (str/trim (.. % -target -value))]
                          (when (and (seq t) (not= t (:title p)))
                            (put! :photos (:id p) (assoc (dissoc p :id) :title t))))}]
     [:div {:class "np-strong"} (:title p)])
   (if (empty? marks)
     [:div {:class "np-muted"}
      (when can? "„Maß“: von Kante zu Kante ziehen oder zweimal tippen, dann die Länge tippen.")]
     (for [mk marks :let [n (count (m/thread mdl (:id mk)))]]
       ^{:key (:id mk)}
       [:button {:class (str "np-mk__row" (when (:done? mk) " is-done"))
                 :on-click #(pick! (:id mk))}
        [:span {:class "np-mk__dot" :style {:background (m/color-of mk)}}]
        [:span {:class "np-grow"} (m/caption mk)]
        (when (pos? n) [:span {:class "np-mk__count"} (str n " 💬")])]))])

(defn- mark-panel [{:keys [mk model can? ui patch! remove! comment! close!]}]
  (let [id (:id mk)]
    [:div {:class "np-mk__detail"}
     [:div {:class "np-mk__head"}
      [:span {:class "np-mk__dot" :style {:background (m/color-of mk)}}]
      [:span {:class "np-strong np-grow"} (m/caption mk)]
      (when can?
        [ui/button {:small true :minimal (not (:done? mk)) :intent (when (:done? mk) :success)
                    :icon :tick :text (if (:done? mk) "Geklärt" "Offen")
                    :on-click #(patch! id update :done? not)}])
      [ui/button {:small true :minimal true :icon :cross :title "Schließen"
                  :on-click close!}]]
     (when (= :dim (:kind mk))
       [:div {:class "np-mk__mm"}
        [:input {:class "np-mk__mm-input" :key (str id "/" (:mm mk))
                 :input-mode "decimal" :placeholder "Länge, z. B. 2345 oder 2,345 m"
                 :read-only (not can?)
                 :auto-focus (= id (:focus-mm @ui))
                 :default-value (some-> (:mm mk) str)
                 :on-key-down #(when (= "Enter" (.-key %)) (.blur (.-target %)))
                 :on-blur (fn [e]
                            (swap! ui dissoc :focus-mm)
                            (let [t (.. e -target -value)
                                  v (m/parse-length t)]
                              (cond
                                (str/blank? t) (patch! id dissoc :mm)
                                v (when (not= v (:mm mk)) (patch! id assoc :mm v))
                                :else (swap! ui assoc :mm-error t))))}]
        [:span {:class "np-muted"} "mm"]
        (when-let [t (:mm-error @ui)]
          [:span {:class "np-mk__error"} (str "„" t "“ ist keine Länge")])])
     (when (= :dim (:kind mk))
       [:div {:class "np-mk__chips"}
        (for [l m/labels]
          ^{:key l}
          [:button {:class (str "np-mk__chip" (when (= l (:label mk)) " is-on"))
                    :disabled (not can?)
                    :on-click #(patch! id (fn [x] (if (= l (:label x))
                                                    (dissoc x :label) (assoc x :label l))))}
           l])])
     [:textarea {:class "np-mk__note" :key (str id "/note/" (hash (:note mk)))
                 :placeholder "Notiz für die Konstruktion"
                 :read-only (not can?)
                 :default-value (:note mk)
                 :on-blur #(let [t (.. % -target -value)]
                             (when (not= t (str (:note mk))) (patch! id assoc :note t)))}]
     [:div {:class "np-mk__thread"}
      (for [{:keys [by at text] cid :id} (m/thread model id)]
        ^{:key cid}
        [:div {:class "np-mk__comment"}
         [:span {:class "np-mk__who" :style {:color (live/color-of by)}} by]
         [:span {:class "np-muted"} (clock at)]
         [:div text]])
      (when can?
        [:input {:class "np-mk__say" :placeholder "Antworten …" :enter-key-hint "send"
                 :on-key-down (fn [e]
                                (when (= "Enter" (.-key e))
                                  (comment! id (.. e -target -value))
                                  (set! (.. e -target -value) "")))}])]
     [:div {:class "np-mk__foot np-muted"}
      (str (:by mk) (when (:at mk) (str " · " (clock (:at mk)))))
      [:span {:class "np-grow"}]
      (when can?
        [ui/button {:small true :minimal true :intent :danger :icon :trash :text "Löschen"
                    :on-click #(remove! id)}])]]))

(defn static
  [{:keys [id body]}]
  ^{:key (hash body)} [editor {:id id :body body :edit? false}])
