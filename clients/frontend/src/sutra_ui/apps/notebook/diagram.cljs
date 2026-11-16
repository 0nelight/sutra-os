(ns sutra-ui.apps.notebook.diagram
  (:require ["yjs" :as Y]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.diagram :as d]
            [sutra.capps.markup :as mk]
            [sutra.types.paths :as paths]
            [sutra-ui.download :as dl]
            [sutra-ui.apps.notebook.state :refer [diagram-rows send!]]
            [sutra-ui.apps.notebook.ymodel :as ym]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.shell :as shell]))

(def ^:private finger-px 9)
(def ^:private body-ms 400)
(def ^:private presence-ms 60)
(def ^:private obj-mime "application/x-sutra-object")
(def ^:private stencil-mime "application/x-sutra-stencil")

(defn- model [{:keys [ydoc base]}]
  (let [y (when ydoc (d/from-entries (ym/entries ydoc d/collections)))]
    (if (and y (or (not (d/blank? y)) (seq (:settings y)))) y base)))

(defn- writable! [{:keys [can? ydoc base]}]
  (when can?
    (when (and (d/blank? (d/from-entries (ym/entries ydoc d/collections)))
               (not (d/blank? base)))
      (ym/seed! ydoc (d/entries base)))
    true))

(defn- write!
  [{:keys [^js ydoc] :as c} f]
  (when (writable! c)
    (.transact ydoc
               #(f (fn [coll id v] (.set (ym/ymap ydoc coll) id (pr-str v)))
                   (fn [coll id] (.delete (ym/ymap ydoc coll) id))))))

(defn- put1! [c coll id v] (write! c (fn [put _] (put coll id v))))

(defn- setting [mdl k] (get-in mdl [:settings k]))

(defn- pname [x] (some-> (or (catalog/name-of x) x) name))

(defn- type-spec [t] (get-in @catalog/schema [:object-types (keyword t)]))

(defn- type-fields [t] (mapv (comp name :name) (:fields (type-spec t))))

(defn- row-title [t row]
  (catalog/instance-display-name row (:title-key (type-spec t))))

(defn- project-types
  [p]
  (when p
    (sort (for [[k v] (:object-types @catalog/canonical-schema)
                :when (= p (pname (:project v)))]
            (name k)))))

(defn- palette-types [{:keys [project]} mdl]
  (or (:types (setting mdl "palette"))
      (vec (project-types (pname project)))))

(defn- load-rows! [_ t]
  (when-not (contains? @diagram-rows t) (send! [:notebook.diagram/rows t])))

(defn- rows-of [_ t]
  (let [rs (get @diagram-rows t)] (when (vector? rs) rs)))

(defn- row-of [c t oid]
  (some #(when (= (str oid) (str (:id %))) %) (rows-of c t)))

(defn- labeller
  [c]
  (fn [s]
    (when-let [{:keys [type oid field cached]} (:obj s)]
      (if-let [row (row-of c type oid)]
        (str (if field (d/field-val row field) (row-title type row)))
        cached))))

(defn- object-shape
  [t row p]
  (let [title (row-title t row)
        st    (d/stencil-for t row title)
        field (d/label-field (type-fields t))
        text  (str (if field (d/field-val row field) title))]
    (assoc (d/new-shape st p)
           :obj {:type t :oid (str (:id row)) :field field :title title :cached text})))

(defn- stage-box [{:keys [stage]}] (some-> ^js @stage .getBoundingClientRect))

(defn- view-of [{:keys [ui] :as c} [w h]]
  (or (:view @ui)
      (let [^js rc (stage-box c)
            v (mk/fit {:w (+ w 20) :h (+ h 20)}
                      (if rc (/ (.-width rc) (max 1 (.-height rc))) 1.4))]
        (-> v (update :x - 10) (update :y - 10)))))

(defn- px->mm [c size]
  (/ (:w (view-of c size)) (max 1 (some-> (stage-box c) .-width))))

(defn- client-xy [^js e] [(.-clientX e) (.-clientY e)])

(defn- to-page [c size [cx cy]]
  (let [{:keys [x y w h]} (view-of c size)
        ^js rc (stage-box c)]
    [(+ x (* w (/ (- cx (.-left rc)) (max 1 (.-width rc)))))
     (+ y (* h (/ (- cy (.-top rc)) (max 1 (.-height rc)))))]))

(defn- tol [c size] (* finger-px (px->mm c size)))

(defn- moved-model
  [mdl {:keys [kind] :as drag}]
  (case kind
    :move (let [{:keys [orig delta vias]} drag
                [dx dy] (or delta [0 0])
                shift   (fn [pts] (mapv (fn [[x y]] [(+ x dx) (+ y dy)]) pts))]
            (-> mdl
                (update :shapes #(reduce-kv (fn [ss id {:keys [x y]}]
                                              (cond-> ss (get ss id)
                                                (update id assoc :x (+ x dx) :y (+ y dy))))
                                            % orig))
                (update :edges #(reduce-kv (fn [es id via]
                                             (cond-> es (get es id)
                                               (assoc-in [id :via] (shift via))))
                                           % vias))))
    :seg (if (:via drag) (assoc-in mdl [:edges (:edge drag) :via] (:via drag)) mdl)
    :end (assoc-in mdl [:edges (:edge drag) (:which drag)]
                   (or (:target drag) {:at (:p drag)}))
    :label (assoc-in mdl [:shapes (:id drag) :label-at] (:at drag))
    :resize (update-in mdl [:shapes (:id drag)] merge (:wh drag))
    mdl))

(defn- publish! [{:keys [^js aw last-pub ui]} extra force?]
  (when (and aw (or force? (> (- (js/Date.now) @last-pub) presence-ms)))
    (reset! last-pub (js/Date.now))
    (.setLocalStateField aw "diagram" (clj->js (merge {:sel (vec (get-in @ui [:st :sel]))}
                                                      extra)))))

(defn- select! [{:keys [ui] :as c} ids edge]
  (swap! ui update :st assoc :sel (set ids) :edge edge)
  (publish! c {} true))

(defn- add-shapes!
  [c shs]
  (write! c (fn [put _] (doseq [[id s] shs] (put :shapes id s))))
  (select! c (keys shs) nil))

(defn- place-object! [c t row p]
  (add-shapes! c {(ym/new-id) (object-shape t row p)}))

(defn- place-stencil! [{:keys [ui] :as c} st p]
  (let [id (ym/new-id)]
    (add-shapes! c {id (cond-> (d/new-shape st p)
                         (#{:text :schriftfeld} st) (assoc :label (if (= :text st) "Text"
                                                                      "Titel\nErsteller")))})
    (when (#{:text :schriftfeld} st) (swap! ui assoc :focus-label id))))

(defn- delete-sel! [{:keys [ui] :as c}]
  (let [mdl (model c) {:keys [sel edge]} (:st @ui)
        drop-e (into (set (mapcat #(d/edges-at mdl %) sel)) (when edge [edge]))]
    (write! c (fn [_ del]
                (doseq [id sel] (del :shapes id))
                (doseq [id drop-e] (del :edges id))))
    (select! c nil nil)))

(defn- patch-shape! [c id f & args]
  (when-let [s (get-in (model c) [:shapes id])]
    (put1! c :shapes id (apply f s args))))

(defn- patch-edge! [c id f & args]
  (when-let [e (get-in (model c) [:edges id])]
    (put1! c :edges id (apply f e args))))

(defn- each-sel! [{:keys [ui] :as c} f]
  (let [mdl (model c)]
    (write! c (fn [put _]
                (doseq [id (get-in @ui [:st :sel]) :let [s (get-in mdl [:shapes id])] :when s]
                  (put :shapes id (f s)))))))

(defn- rotate-sel! [c deg] (each-sel! c #(update % :rot (fn [r] (mod (+ (or r 0) deg) 360)))))

(defn- nudge! [c dx dy] (each-sel! c #(-> % (update :x + dx) (update :y + dy))))

(defn- copy! [{:keys [ui] :as c}]
  (let [mdl (model c) sel (get-in @ui [:st :sel])]
    (when (seq sel)
      (swap! ui assoc :clip
             {:shapes (select-keys (:shapes mdl) sel)
              :edges  (into {} (filter (fn [[_ e]] (and (contains? sel (get-in e [:from :shape]))
                                                         (contains? sel (get-in e [:to :shape])))))
                            (:edges mdl))}))))

(defn- paste! [{:keys [ui] :as c}]
  (when-let [{:keys [shapes edges]} (:clip @ui)]
    (let [ids  (into {} (map (fn [k] [k (ym/new-id)])) (keys shapes))
          off  (* 2 d/grid)
          ends #(cond-> % (:shape %) (update :shape ids))]
      (write! c (fn [put _]
                  (doseq [[id s] shapes]
                    (put :shapes (ids id) (-> s (update :x + off) (update :y + off)
                                              (dissoc :obj))))
                  (doseq [[_ e] edges]
                    (put :edges (ym/new-id)
                         (-> e (update :from ends) (update :to ends)
                             (update :via #(mapv (fn [[x y]] [(+ x off) (+ y off)]) %)))))))
      (select! c (vals ids) nil)
      (copy! c))))

(defn- align! [{:keys [ui] :as c} how]
  (let [mdl (model c)
        shs (filter #(contains? (get-in @ui [:st :sel]) (:id %)) (d/shapes mdl))]
    (when (> (count shs) 1)
      (let [to (d/align shs how)]
        (write! c (fn [put _] (doseq [[id xy] to]
                                (put :shapes id (merge (get-in mdl [:shapes id]) xy)))))))))

(defn- routes [mdl] (into {} (keep (fn [e] (some->> (d/edge-route mdl e) (vector (:id e)))))
                          (d/edges mdl)))

(defn- start-move [mdl sel p]
  {:kind  :move :start p
   :orig  (into {} (for [id sel :let [s (get-in mdl [:shapes id])] :when s]
                     [id (select-keys s [:x :y])]))
   :vias  (into {} (keep (fn [[id e]]
                           (when (and (seq (:via e))
                                      (or (contains? sel (get-in e [:from :shape]))
                                          (contains? sel (get-in e [:to :shape]))))
                             [id (:via e)])))
                (:edges mdl))})

(defn- edge-handles
  [pts]
  (concat [{:end :from :p (first pts)} {:end :to :p (peek pts)}]
          (for [i (range 1 (- (count pts) 2))
                :let [[ax ay] (nth pts i) [bx by] (nth pts (inc i))]]
            {:seg i :p [(/ (+ ax bx) 2) (/ (+ ay by) 2)]})))

(defn- near [p q t] (<= (js/Math.hypot (- (first p) (first q)) (- (second p) (second q))) t))

(defn- down-select [{:keys [ui] :as c} mdl p t ^js e]
  (let [{:keys [sel edge]} (:st @ui)
        shs   (d/shapes mdl)
        label (some-> (.-target e) (.closest "[data-label]") (.getAttribute "data-label"))
        eh    (when edge (some #(when (near p (:p %) t) %)
                               (edge-handles (d/edge-route mdl (get-in mdl [:edges edge])))))
        one   (when (= 1 (count sel)) (get-in mdl [:shapes (first sel)]))
        grip  (when (and one (not (d/point-shape? one)))
                (let [{:keys [x2 y2]} (d/bbox one)] (near p [x2 y2] t)))
        port  (d/hit-port (remove d/point-shape? shs) p (* 0.6 t) #{})
        hit   (d/hit-shape shs p (* 0.4 t))
        eg    (when-not hit (d/hit-edge (routes mdl) p (* 0.6 t)))]
    (cond
      (and eh (:end eh)) {:kind :end :edge edge :which (:end eh) :p p}
      eh {:kind :seg :edge edge :i (:seg eh) :start p
          :pts (d/edge-route mdl (get-in mdl [:edges edge]))}
      grip {:kind :resize :id (first sel) :start p :orig (select-keys one [:w :h :rot])}
      label (let [s (get-in mdl [:shapes label])
                  [cx cy] (d/center s)]
              (select! c [label] nil)
              {:kind :label :id label :start p
               :orig (or (:label-at s) (:text (d/stencil-of s)) [0 0])
               :c [cx cy]})
      (and port (not (.-shiftKey e))) {:kind :connect :from (select-keys port [:shape :port])
                                       :a (:p port) :p p}
      hit (let [id  (:id hit)
                sel (cond (.-shiftKey e) (if (contains? sel id) (disj sel id) (conj sel id))
                          (contains? sel id) sel
                          :else #{id})]
            (select! c sel nil)
            (start-move mdl sel p))
      eg (do (select! c nil (:id eg)) nil)
      :else (do (when-not (.-shiftKey e) (select! c nil nil))
                {:kind :band :a p :b p}))))

(defn- down-connect [mdl p t]
  (let [shs  (d/shapes mdl)
        port (d/hit-port shs p t #{})]
    {:kind :connect :from (if port (select-keys port [:shape :port]) {:at (d/snap-pt p)})
     :a (if port (:p port) (d/snap-pt p)) :p p}))

(defn- connect-target
  [mdl from p t]
  (let [shs (d/shapes mdl)]
    (if-let [port (d/hit-port shs p t (if (:shape from) #{(:shape from)} #{}))]
      {:end (select-keys port [:shape :port])}
      (if-let [eg (d/hit-edge (routes mdl) p (* 0.6 t))]
        (let [id (ym/new-id) [x y] (d/snap-pt (:p eg))
              k  (d/new-shape :knoten [x y])
              k  (assoc k :x (- x (/ (:w k) 2)) :y (- y (/ (:h k) 2)))]
          {:end {:shape id :port :c} :knoten [id k]})
        {:end {:at (d/snap-pt p)}}))))

(defn- finish! [{:keys [ui] :as c} mdl drag p t ^js e]
  (case (:kind drag)
    :move (let [moved (moved-model mdl drag)]
            (when (not= [0 0] (or (:delta drag) [0 0]))
              (write! c (fn [put _]
                          (doseq [id (keys (:orig drag))]
                            (put :shapes id (get-in moved [:shapes id])))
                          (doseq [id (keys (:vias drag))]
                            (put :edges id (get-in moved [:edges id])))))))
    :band (let [ids (map :id (d/in-box (d/shapes mdl) (:a drag) (:b drag)))]
            (select! c (if (.-shiftKey e) (into (get-in @ui [:st :sel]) ids) ids) nil))
    :connect (when (> (js/Math.hypot (- (first p) (first (:a drag)))
                                     (- (second p) (second (:a drag)))) (* 0.5 t))
               (let [{:keys [end knoten]} (connect-target mdl (:from drag) p t)
                     {:keys [kind arrow]} (:last-edge @ui)
                     id (ym/new-id)]
                 (write! c (fn [put _]
                             (when-let [[kid k] knoten] (put :shapes kid k))
                             (put :edges id {:from (:from drag) :to end
                                             :kind (or kind :rohr) :arrow (or arrow :none)})))
                 (select! c nil id)))
    :seg (when (:via drag) (patch-edge! c (:edge drag) assoc :via (:via drag)))
    :end (let [e (get-in mdl [:edges (:edge drag)])
               other (get e (if (= :from (:which drag)) :to :from))
               {:keys [end knoten]} (connect-target mdl (if (:shape other) other {}) p t)]
           (write! c (fn [put _]
                       (when-let [[kid k] knoten] (put :shapes kid k))
                       (put :edges (:edge drag) (assoc e (:which drag) end)))))
    :label (patch-shape! c (:id drag) assoc :label-at (:at drag))
    :resize (when (:wh drag) (patch-shape! c (:id drag) merge (:wh drag)))
    nil))

(defn- drag-move [mdl drag p t ^js e]
  (case (:kind drag)
    :move (let [[sx sy] (:start drag)
                raw [(- (first p) sx) (- (second p) sy)]]
            (assoc drag :delta (if (.-altKey e) raw (mapv d/snap raw))))
    :band (assoc drag :b p)
    :connect (assoc drag :p p :hover (d/hit-port (d/shapes mdl) p t
                                                 (if-let [s (get-in drag [:from :shape])]
                                                   #{s} #{})))
    :seg (let [pts (:pts drag) i (:i drag)
               h?  (== (second (nth pts i)) (second (nth pts (inc i))))
               [sx sy] (:start drag)
               dd  (if h? (- (second p) sy) (- (first p) sx))]
           (assoc drag :via (d/drag-segment pts i dd)))
    :end (let [port (d/hit-port (d/shapes mdl) p t #{})]
           (assoc drag :p (d/snap-pt p) :target (some-> port (select-keys [:shape :port]))))
    :label (let [[sx sy] (:start drag) [ox oy] (:orig drag)]
             (assoc drag :at [(+ ox (- (first p) sx)) (+ oy (- (second p) sy))]))
    :resize (let [{:keys [w h rot]} (:orig drag)
                  [sx sy] (:start drag)
                  [dx dy] (mapv d/snap [(- (first p) sx) (- (second p) sy)])
                  [dx dy] (if (#{90 270} (mod (or rot 0) 360)) [dy dx] [dx dy])]
              (assoc drag :wh {:w (max d/grid (+ w dx)) :h (max d/grid (+ h dy))}))
    drag))

(defn- down! [{:keys [ui can?] :as c} size ^js e]
  (.focus (.-currentTarget e))
  (.setPointerCapture (.-currentTarget e) (.-pointerId e))
  (let [mdl (model c) p (to-page c size (client-xy e)) t (tol c size)
        tool (get-in @ui [:st :tool] :select)
        drag (cond
               (or (#{1 2} (.-button e)) (:space @ui))
               {:kind :pan :client (client-xy e) :view (view-of c size)}
               (not can?) (do (down-select c mdl p t e) {:kind :pan :client (client-xy e)
                                                         :view (view-of c size)})
               (= tool :connect) (down-connect mdl p t)
               (= tool :text) (do (place-stencil! c :text p)
                                  (swap! ui assoc-in [:st :tool] :select)
                                  nil)
               :else (down-select c mdl p t e))]
    (swap! ui assoc :drag drag)))

(defn- move! [{:keys [ui] :as c} size ^js e]
  (let [drag (:drag @ui) p (to-page c size (client-xy e))]
    (cond
      (= :pan (:kind drag))
      (let [[x0 y0] (:client drag) [x y] (client-xy e)
            k (px->mm c size)]
        (swap! ui assoc :view (mk/pan (:view drag) (* k (- x x0)) (* k (- y y0)))))
      drag (swap! ui assoc :drag (drag-move (model c) drag p (tol c size) e))
      :else (let [h (d/hit-shape (d/shapes (model c)) p (tol c size))]
              (when (not= (:id h) (:hover @ui)) (swap! ui assoc :hover (:id h)))))
    (publish! c {:cursor p} false)))

(defn- up! [{:keys [ui] :as c} size ^js e]
  (let [drag (:drag @ui)]
    (swap! ui dissoc :drag)
    (when (and drag (not= :pan (:kind drag)))
      (finish! c (model c) drag (to-page c size (client-xy e)) (tol c size) e))))

(defn- wheel! [{:keys [ui] :as c} size ^js e]
  (.preventDefault e)
  (let [v (view-of c size)]
    (swap! ui assoc :view
           (if (or (.-ctrlKey e) (.-metaKey e) (zero? (.-deltaX e)))
             (mk/zoom v (to-page c size (client-xy e)) (js/Math.pow 1.0018 (- (.-deltaY e)))
                      {:w (* 3 (first size)) :h (* 3 (second size))})
             (let [k (px->mm c size)] (mk/pan v (* k (- (.-deltaX e))) (* k (- (.-deltaY e)))))))))

(defn- drop! [{:keys [ui] :as c} size ^js e]
  (.preventDefault e)
  (let [dt (.-dataTransfer e) p (to-page c size (client-xy e))]
    (if-let [o (not-empty (.getData dt obj-mime))]
      (let [{:keys [t id]} (js->clj (js/JSON.parse o) :keywordize-keys true)]
        (when-let [row (row-of c t id)] (place-object! c t row p)))
      (when-let [st (not-empty (.getData dt stencil-mime))]
        (place-stencil! c (keyword st) p)))
    (swap! ui assoc-in [:st :tool] :select)))

(defn- key! [{:keys [ui ^js undo can?] :as c} ^js e]
  (let [k (.-key e) mod? (or (.-ctrlKey e) (.-metaKey e))
        step (if (.-shiftKey e) (* 4 d/grid) d/grid)
        stop #(.preventDefault e)]
    (when-not (some-> (.-target e) .-tagName #{"INPUT" "TEXTAREA" "SELECT"})
      (cond
        (= k " ") (do (stop) (swap! ui assoc :space true))
        (not can?) nil
        (and mod? (= "z" (str/lower-case k))) (do (stop) (if (.-shiftKey e) (.redo undo)
                                                             (.undo undo)))
        (and mod? (= "y" k)) (do (stop) (.redo undo))
        (and mod? (= "c" k)) (copy! c)
        (and mod? (= "v" k)) (paste! c)
        (and mod? (= "d" k)) (do (stop) (copy! c) (paste! c))
        (and mod? (= "a" k)) (do (stop) (select! c (keys (:shapes (model c))) nil))
        (#{"Delete" "Backspace"} k) (do (stop) (delete-sel! c))
        (= "Escape" k) (do (swap! ui update :st assoc :tool :select) (swap! ui dissoc :drag)
                           (select! c nil nil))
        (= "r" k) (rotate-sel! c (if (.-shiftKey e) 270 90))
        (= "f" k) (each-sel! c #(update % :flip? not))
        (= "c" k) (swap! ui assoc-in [:st :tool] :connect)
        (= "v" k) (swap! ui assoc-in [:st :tool] :select)
        (= "t" k) (swap! ui assoc-in [:st :tool] :text)
        (= "ArrowLeft" k) (do (stop) (nudge! c (- step) 0))
        (= "ArrowRight" k) (do (stop) (nudge! c step 0))
        (= "ArrowUp" k) (do (stop) (nudge! c 0 (- step)))
        (= "ArrowDown" k) (do (stop) (nudge! c 0 step))
        :else nil))))

(defn- file-name [{:keys [doc-name]} ext] (str (or (not-empty doc-name) "Zeichnung") "." ext))

(defn- svg! [c] (dl/text! (d/svg-file (model c) (labeller c)) "image/svg+xml"
                          (file-name c "svg")))

(defn- png! [c]
  (let [mdl (model c) [w h] (d/page-size mdl) k 8
        src (str "data:image/svg+xml;charset=utf-8,"
                 (js/encodeURIComponent (d/svg-file mdl (labeller c))))
        img (js/Image.)]
    (set! (.-onload img)
          (fn [_]
            (let [cv (.createElement js/document "canvas")]
              (set! (.-width cv) (* k w))
              (set! (.-height cv) (* k h))
              (.drawImage (.getContext cv "2d") img 0 0 (* k w) (* k h))
              (.toBlob cv #(dl/blob! % (file-name c "png")) "image/png"))))
    (set! (.-src img) src)))

(defn- src-of [file] (paths/url :kompass-file {:id (str file)}))

(defn- toast! [msg]
  (ui/show! (ui/default-toaster) {:message msg :intent :warning :timeout 4000}))

(defn- upload-underlay! [{:keys [project] :as c} ^js f]
  (if-let [proj (pname project)]
    (let [fr (js/FileReader.)]
      (set! (.-onload fr)
            (fn [_]
              (let [b64 (second (str/split (.-result fr) #"," 2))]
                (send! [:notebook.diagram/upload
                        {:name (.-name f) :project proj
                         :mime (or (not-empty (.-type f)) "image/png") :bytes-b64 b64}
                        (fn [res]
                          (let [[w h] (d/page-size (model c))]
                            (put1! c :settings "underlay"
                                   {:file (:id res) :name (.-name f) :opacity 0.35
                                    :x 0 :y 0 :w w :h h})))
                        #(toast! (str "Hintergrund nicht hochgeladen: " %))]))))
      (.readAsDataURL fr f))
    (toast! "Die Zeichnung liegt in keinem Projekt — verschiebe sie erst in eins.")))

(defn- port-dots [s r cls]
  (for [{:keys [id p]} (d/ports s) :let [[x y] p]]
    ^{:key (name id)} [:circle {:class cls :cx x :cy y :r r}]))

(defn- others-svg [states self k]
  (for [[client {:keys [user diagram]}] states
        :when (and (not= client self) (:name user) (:cursor diagram))
        :let [[x y] (:cursor diagram)]]
    ^{:key client}
    [:g {:class "dg-other" :style {:color (:color user)}}
     [:circle {:cx x :cy y :r (* 4 k) :fill (:color user)}]
     [:text {:x (+ x (* 6 k)) :y (- y (* 6 k)) :font-size (* 12 k) :fill (:color user)}
      (:name user)]]))

(defn- sheet [{:keys [ui ^js aw ^js ydoc can?] :as c} mdl size]
  (let [{:keys [st drag hover]} @ui
        [pw ph] size
        shown (moved-model mdl drag)
        shs   (d/shapes shown)
        lbl   (labeller c)
        {vx :x vy :y vw :w vh :h} (view-of c size)
        k     (px->mm c size)
        sel   (:sel st)
        under (setting mdl "underlay")
        conn? (or (= :connect (:tool st)) (#{:connect :end} (:kind drag)))]
    [:svg {:view-box (str vx " " vy " " vw " " vh) :preserve-aspect-ratio "none"
           :tab-index 0
           :class (str "dg-svg dg-svg--" (name (:tool st :select)))
           :on-pointer-down #(down! c size %)
           :on-pointer-move #(move! c size %)
           :on-pointer-up #(up! c size %)
           :on-pointer-cancel #(up! c size %)
           :on-wheel #(wheel! c size %)
           :on-key-down #(key! c %)
           :on-key-up #(when (= " " (.-key %)) (swap! ui dissoc :space))
           :on-double-click #(when-let [id (first sel)] (swap! ui assoc :focus-label id))
           :on-context-menu #(.preventDefault %)
           :on-drag-over #(.preventDefault %)
           :on-drop #(drop! c size %)}
     [:defs
      [:pattern {:id "dg-grid" :width 5 :height 5 :pattern-units "userSpaceOnUse"}
       [:path {:d "M 5 0 L 0 0 0 5" :fill "none" :class "dg-grid"}]]]
     [:rect {:class "dg-page" :x 0 :y 0 :width pw :height ph}]
     (when-not (:hide-grid? (setting mdl "page"))
       [:rect {:x 0 :y 0 :width pw :height ph :fill "url(#dg-grid)" :pointer-events "none"}])
     (when (and under (not (:hidden? under)))
       [:image {:href (src-of (:file under)) :x (:x under 0) :y (:y under 0)
                :width (:w under pw) :height (:h under ph) :opacity (:opacity under 0.35)
                :preserve-aspect-ratio "xMinYMin meet" :pointer-events "none"}])
     [:g {:stroke-linecap "round" :font-family "var(--font-sans)"}
      (for [s shs :when (#{:rahmen :bereich :speicher :waermetauscher} (keyword (:stencil s)))]
        ^{:key (:id s)} [:g (d/shape-hiccup s)])
      (for [e (d/edges shown) :let [pts (d/edge-route shown e)] :when pts]
        ^{:key (:id e)}
        [:g {:class (str "dg-edge" (when (= (:id e) (:edge st)) " is-sel"))}
         (d/edge-hiccup e pts)])
      (for [s shs :when (not (#{:rahmen :bereich :speicher :waermetauscher}
                               (keyword (:stencil s))))]
        ^{:key (:id s)} [:g {:class (when (:obj s) "dg-obj")} (d/shape-hiccup s)])
      (for [s shs :let [h (d/label-hiccup s (d/label-of s lbl))] :when h]
        ^{:key (str "l" (:id s))} [:g {:data-label (:id s) :class "dg-label-hit"} h])]
     (when (= :connect (:kind drag))
       (let [a {:p (:a drag) :dir (some #(when (= (keyword (get-in drag [:from :port])) (:id %))
                                           (:dir %))
                                        (some-> (get-in shown
                                                        [:shapes (get-in drag [:from :shape])])
                                                d/ports))}
             b (if-let [h (:hover drag)]
                 (d/end-at shown (select-keys h [:shape :port]))
                 {:p (:p drag)})]
         [:polyline {:class "dg-draft" :fill "none"
                     :points (str/join " " (map #(str/join "," %) (d/route a b nil)))}]))
     (for [s shs :when (contains? sel (:id s)) :let [{:keys [x1 y1 x2 y2]} (d/bbox s)]]
       ^{:key (str "s" (:id s))}
       [:g
        [:rect {:class "dg-sel" :x (- x1 (* 3 k)) :y (- y1 (* 3 k))
                :width (+ (- x2 x1) (* 6 k)) :height (+ (- y2 y1) (* 6 k))}]
        (when (and (= 1 (count sel)) (not (d/point-shape? s)) can?)
          [:rect {:class "dg-grip" :x (- x2 (* 4 k)) :y (- y2 (* 4 k))
                  :width (* 8 k) :height (* 8 k)}])])
     (when can?
       (for [s shs :when (or conn? (and (= hover (:id s)) (not (d/point-shape? s)))
                             (and (contains? sel (:id s)) (= 1 (count sel))))]
         ^{:key (str "p" (:id s))} [:g (port-dots s (* 3 k) "dg-port")]))
     (when-let [h (:hover drag)]
       (let [[x y] (:p h)] [:circle {:class "dg-port is-hot" :cx x :cy y :r (* 5 k)}]))
     (when-let [eid (:edge st)]
       (when-let [pts (d/edge-route shown (get-in shown [:edges eid]))]
         (for [{:keys [p end seg]} (edge-handles pts) :let [[x y] p]]
           ^{:key (str (or end seg))}
           [:rect {:class (str "dg-handle" (when seg " dg-handle--seg"))
                   :x (- x (* 4 k)) :y (- y (* 4 k)) :width (* 8 k) :height (* 8 k)}])))
     (when (= :band (:kind drag))
       (let [[ax ay] (:a drag) [bx by] (:b drag)]
         [:rect {:class "dg-band" :x (min ax bx) :y (min ay by)
                 :width (js/Math.abs (- bx ax)) :height (js/Math.abs (- by ay))}]))
     (when aw (others-svg (ym/awareness-states aw) (.-clientID ydoc) k))]))

(def ^:private tool-labels
  [[:select "Wählen (V)" :select] [:connect "Verbinder (C)" :flows] [:text "Text (T)" :font]])

(defn- toolbar [{:keys [ui ^js undo can?] :as c}]
  (let [st (:st @ui)]
    [:div {:class "dg-tools"}
     (when can?
       [:<>
        (for [[t l icon] tool-labels]
          ^{:key t}
          [ui/button {:icon icon :minimal (not= t (:tool st :select)) :title l
                      :active (= t (:tool st :select))
                      :on-click #(swap! ui assoc-in [:st :tool] t)}])
        [:span {:class "dg-sep"}]
        [ui/button {:icon :undo :minimal true :title "Rückgängig (Strg+Z)"
                    :on-click #(some-> undo .undo)}]
        [ui/button {:icon :redo :minimal true :title "Wiederholen (Strg+Y)"
                    :on-click #(some-> undo .redo)}]
        [:span {:class "dg-sep"}]
        [ui/button {:icon :rotate-page :minimal true :title "Drehen (R)"
                    :disabled (empty? (:sel st)) :on-click #(rotate-sel! c 90)}]
        [ui/button {:icon :swap-horizontal :minimal true :title "Spiegeln (F)"
                    :disabled (empty? (:sel st))
                    :on-click #(each-sel! c (fn [s] (update s :flip? not)))}]
        [ui/button {:icon :trash :minimal true :title "Löschen (Entf)"
                    :disabled (and (empty? (:sel st)) (nil? (:edge st)))
                    :on-click #(delete-sel! c)}]])
     [:span {:class "np-grow"}]
     [ui/button {:icon :zoom-to-fit :minimal true :title "Ganzes Blatt"
                 :on-click #(swap! ui dissoc :view)}]
     [ui/button {:minimal true :icon :download :text "SVG" :on-click #(svg! c)}]
     [ui/button {:minimal true :icon :media :text "PNG" :on-click #(png! c)}]]))

(defn- stencil-tile [{:keys [ui] :as c} st]
  (let [{:keys [label]} (get d/stencils st)]
    [:div {:class "dg-tile" :title label :draggable true
           :on-drag-start #(.setData (.-dataTransfer %) stencil-mime (name st))
           :on-click #(let [[w h] (d/page-size (model c))
                            v (or (:view @ui) {:x 0 :y 0 :w w :h h})]
                        (place-stencil! c st [(+ (:x v) (/ (:w v) 2)) (+ (:y v) (/ (:h v) 2))]))}
     (d/icon-hiccup st 34)
     [:span label]]))

(defn- object-row [{:keys [ui] :as c} t row placed-id]
  (let [title (row-title t row)
        st    (d/stencil-for t row title)
        f     (d/label-field (type-fields t))
        tag   (when f (str (d/field-val row f)))]
    [:div {:class (str "dg-objrow" (when placed-id " is-placed"))
           :draggable (not placed-id)
           :title (if placed-id "Steht schon auf dem Blatt — klicken zeigt es" "Aufs Blatt ziehen")
           :on-drag-start #(.setData (.-dataTransfer %) obj-mime
                                     (js/JSON.stringify #js {:t t :id (str (:id row))}))
           :on-click #(if placed-id
                        (select! c [placed-id] nil)
                        (let [[w h] (d/page-size (model c))
                              v (or (:view @ui) {:x 0 :y 0 :w w :h h})]
                          (place-object! c t row [(+ (:x v) (/ (:w v) 2))
                                                  (+ (:y v) (/ (:h v) 2))])))}
     (d/icon-hiccup st 22)
     (when (seq tag) [:span {:class "dg-objrow__tag"} tag])
     [:span {:class "dg-objrow__title"} title]
     (when placed-id [ui/icon {:icon :tick :size 12}])]))

(defn- matching-rows
  [c t q]
  (let [text #(str/lower-case (str (row-title t %) " " (str/join " " (vals (:values %)))))]
    (->> (rows-of c t)
         (filter #(or (str/blank? q) (str/includes? (text %) q)))
         (sort-by #(row-title t %)))))

(defn- palette [{:keys [ui can?] :as c} mdl]
  (let [types  (palette-types c mdl)
        q      (str/lower-case (str (:q @ui)))
        placed (d/placed mdl)
        all    (sort (map name (keys (:object-types @catalog/schema))))]
    (doseq [t types] (load-rows! c t))
    [:div {:class "dg-palette"}
     [:div {:class "dg-pal-head"} "Objekte"]
     [:input {:class "dg-search" :placeholder "Suchen …" :value (:q @ui)
              :on-change #(swap! ui assoc :q (.. % -target -value))}]
     (for [t types :let [spec (type-spec t) rows (matching-rows c t q)]]
       ^{:key t}
       [:details {:class "dg-group" :open true}
        [:summary
         [:span (str (or (:symbol spec) "") " " (or (:plural spec) t))]
         [:span {:class "np-muted"} (count rows)]
         (when can?
           [:button {:class "dg-x" :title "Aus der Palette nehmen"
                     :on-click (fn [e] (.preventDefault e)
                                 (put1! c :settings "palette"
                                        {:types (vec (remove #{t} types))}))}
            "×"])]
        (if (= :loading (get @diagram-rows t))
          [:div {:class "np-muted dg-pad"} "lädt …"]
          (for [row rows]
            ^{:key (str (:id row))}
            [object-row c t row (get placed [t (str (:id row))])]))])
     (when can?
       [:select {:class "dg-addtype" :value ""
                 :on-change #(let [v (.. % -target -value)]
                               (when (seq v)
                                 (put1! c :settings "palette" {:types (conj (vec types) v)})))}
        [:option {:value ""} "+ Objekttyp …"]
        (for [t all :when (not (some #{t} types))] ^{:key t} [:option {:value t} t])])
     (when can?
       [:<>
        [:div {:class "dg-pal-head"} "Schablone"]
        (for [[g sts] d/stencil-groups]
          ^{:key g}
          [:details {:class "dg-group" :open true}
           [:summary [:span g]]
           [:div {:class "dg-tiles"} (for [st sts] ^{:key st} [stencil-tile c st])]])])]))

(defn- field [label & kids] (into [:label {:class "dg-field"} [:span label]] kids))

(defn- num-input [v on]
  [:input {:type "number" :step d/grid :default-value v :key (str v)
           :on-blur #(when-let [x (parse-double (.. % -target -value))] (when (not= x v) (on x)))}])

(defn- swatches [v on]
  [:div {:class "dg-swatches"}
   (for [col [nil "#1c2127" "#e01e1e" "#1f8f1f" "#2563c9" "#d9822b" "#7d5bbe" "#8a9ba8"]]
     ^{:key (str col)}
     [:button {:class (str "dg-swatch" (when (= col v) " is-on"))
               :style {:background (or col "transparent")}
               :title (or col "Vorgabe") :on-click #(on col)}])])

(defn- shape-panel [{:keys [ui can?] :as c} s]
  (let [id (:id s) {:keys [type oid] lfield :field} (:obj s)
        row (when type (row-of c type oid))
        p!  (partial patch-shape! c id)]
    [:div {:class "dg-props"}
     [:div {:class "dg-props__head"} (d/icon-hiccup (keyword (:stencil s)) 28)
      [:span {:class "np-strong"} (if type (or (some->> row (row-title type)) (:title (:obj s)))
                                      (:label (d/stencil-of s)))]]
     (when type
       [:div {:class "dg-obj"}
        [:div {:class "np-muted"} (str (or (:symbol (type-spec type)) "") " " type)]
        (when row
          [:table {:class "dg-vals"}
           [:tbody
            (for [[k v] (sort-by (comp str key) (:values row)) :when (some? v)]
              ^{:key (str k)} [:tr [:td (name k)] [:td (str v)]])]])
        [ui/button {:small true :icon :share :text "Objekt öffnen"
                    :on-click #(shell/open-object! (keyword type) oid {:new-tab? true})}]])
     (when can?
       [:<>
        (when type
          [field "Beschriftung aus"
           [:select {:value (or lfield "") :on-change #(p! assoc-in [:obj :field]
                                                          (not-empty (.. % -target -value)))}
            [:option {:value ""} "Titel"]
            (for [f (type-fields type)] ^{:key f} [:option {:value f} f])]])
        [field (if type "Eigener Text (statt Feld)" "Text")
         [:textarea {:rows 2 :default-value (:label s) :key (str id "/" (:label s))
                     :auto-focus (= id (:focus-label @ui))
                     :on-focus #(swap! ui dissoc :focus-label)
                     :on-blur #(let [v (.. % -target -value)]
                                 (when (not= v (str (:label s)))
                                   (p! (fn [x] (if (str/blank? v) (dissoc x :label)
                                                   (assoc x :label v))))))}]]
        [field "Symbol"
         [:select {:value (name (:stencil s))
                   :on-change #(p! assoc :stencil (keyword (.. % -target -value)))}
          (for [[k {:keys [label]}] d/stencils] ^{:key k} [:option {:value (name k)} label])]]
        [:div {:class "dg-row2"}
         [field "Breite" [num-input (:w s) #(p! assoc :w %)]]
         [field "Höhe" [num-input (:h s) #(p! assoc :h %)]]]
        [:div {:class "dg-row2"}
         [field "X" [num-input (:x s) #(p! assoc :x %)]]
         [field "Y" [num-input (:y s) #(p! assoc :y %)]]]
        [field "Schriftgröße" [num-input (or (:font s) 4.5) #(p! assoc :font %)]]
        [field "Farbe Linie" [swatches (:color s) #(p! (fn [x] (if % (assoc x :color %)
                                                                 (dissoc x :color))))]]
        [field "Farbe Text" [swatches (:label-color s)
                             #(p! (fn [x] (if % (assoc x :label-color %)
                                            (dissoc x :label-color))))]]
        [:div {:class "dg-btns"}
         [ui/button {:small true :icon :rotate-page :text "Drehen" :on-click #(rotate-sel! c 90)}]
         [ui/button {:small true :icon :swap-horizontal :text "Spiegeln"
                     :on-click #(p! update :flip? not)}]
         [ui/button {:small true :text "Text zurücksetzen" :on-click #(p! dissoc :label-at)}]
         [ui/button {:small true :text "Nach vorn" :on-click #(p! update :z (fnil inc 0))}]
         [ui/button {:small true :text "Nach hinten" :on-click #(p! update :z (fnil dec 0))}]
         [ui/button {:small true :minimal true :intent :danger :icon :trash :text "Löschen"
                     :on-click #(delete-sel! c)}]]])]))

(defn- reversed [e]
  (-> e (assoc :from (:to e) :to (:from e)) (update :via (comp vec reverse))))

(defn- edge-panel [{:keys [ui can?] :as c} e]
  (let [p! (fn [k v] (patch-edge! c (:id e) assoc k v)
             (swap! ui assoc-in [:last-edge k] v))]
    [:div {:class "dg-props"}
     [:div {:class "dg-props__head np-strong"} "Verbinder"]
     (when can?
       [:<>
        [field "Art"
         [:select {:value (name (or (:kind e) :rohr))
                   :on-change #(p! :kind (keyword (.. % -target -value)))}
          (for [[k {:keys [label]}] d/edge-kinds] ^{:key k} [:option {:value (name k)} label])]]
        [field "Flusspfeil"
         [:select {:value (name (or (:arrow e) :none))
                   :on-change #(p! :arrow (keyword (.. % -target -value)))}
          (for [[k l] d/arrows] ^{:key k} [:option {:value (name k)} l])]]
        [:label {:class "dg-check"}
         [:input {:type "checkbox" :checked (boolean (:straight? e))
                  :on-change #(patch-edge! c (:id e) assoc :straight? (.. % -target -checked))}]
         "Gerade (schräg, ohne Knicke)"]
        [field "Farbe" [swatches (:color e) #(patch-edge! c (:id e) (fn [x] (if % (assoc x :color %)
                                                                              (dissoc x :color))))]]
        [:div {:class "dg-btns"}
         [ui/button {:small true :icon :swap-horizontal :text "Richtung umkehren"
                     :on-click #(patch-edge! c (:id e) reversed)}]
         [ui/button {:small true :text "Weg zurücksetzen"
                     :on-click #(patch-edge! c (:id e) dissoc :via)}]
         [ui/button {:small true :minimal true :intent :danger :icon :trash :text "Löschen"
                     :on-click #(delete-sel! c)}]]
        [:div {:class "np-muted dg-pad"}
         "Die Quadrate an den Abschnitten ziehen verschiebt sie; die Enden lassen sich auf einen
          anderen Anschluss legen."]])]))

(defn- multi-panel [c n]
  [:div {:class "dg-props"}
   [:div {:class "dg-props__head np-strong"} (str n " Formen")]
   [field "Ausrichten"
    [:div {:class "dg-btns"}
     (for [[k icon t] [[:left :alignment-left "Links"] [:center :alignment-vertical-center "Mitte"]
                       [:right :alignment-right "Rechts"] [:top :alignment-top "Oben"]
                       [:middle :alignment-horizontal-center "Mitte"]
                       [:bottom :alignment-bottom "Unten"]]]
       ^{:key k} [ui/button {:small true :icon icon :title t :on-click #(align! c k)}])]]
   [:div {:class "dg-btns"}
    [ui/button {:small true :icon :rotate-page :text "Drehen" :on-click #(rotate-sel! c 90)}]
    [ui/button {:small true :minimal true :intent :danger :icon :trash :text "Löschen"
                :on-click #(delete-sel! c)}]]])

(defn- underlay-panel [c under]
  (let [u! #(put1! c :settings "underlay" (merge under %))]
    (if under
      [:<>
       [:div {:class "np-muted"} (:name under)]
       [field "Deckkraft"
        [:input {:type "range" :min 0.05 :max 1 :step 0.05 :value (:opacity under 0.35)
                 :on-change #(u! {:opacity (parse-double (.. % -target -value))})}]]
       [:div {:class "dg-row2"}
        [field "Breite mm"
         [num-input (:w under) #(u! {:w % :h (* (:h under) (/ % (max 1 (:w under))))})]]
        [field "X" [num-input (:x under 0) #(u! {:x %})]]
        [field "Y" [num-input (:y under 0) #(u! {:y %})]]]
       [:div {:class "dg-btns"}
        [ui/button {:small true :text (if (:hidden? under) "Zeigen" "Ausblenden")
                    :on-click #(u! {:hidden? (not (:hidden? under))})}]
        [ui/button {:small true :minimal true :intent :danger :text "Entfernen"
                    :on-click #(write! c (fn [_ del] (del :settings "underlay")))}]]]
      [:label {:class "dg-upload"}
       [:input {:type "file" :accept "image/*,.svg"
                :on-change (fn [^js e]
                             (some->> (.. e -target -files) array-seq first (upload-underlay! c))
                             (set! (.. e -target -value) ""))}]
       "Bild oder SVG hochladen …"])))

(defn- page-panel [{:keys [can?] :as c} mdl]
  (let [pg (setting mdl "page") under (setting mdl "underlay")
        set-pg! #(put1! c :settings "page" (merge pg %))]
    [:div {:class "dg-props"}
     [:div {:class "dg-props__head np-strong"} "Blatt"]
     (when can?
       [:<>
        [:div {:class "dg-row2"}
         [field "Format"
          [:select {:value (name (or (:size pg) :a4))
                    :on-change #(set-pg! {:size (keyword (.. % -target -value))})}
           (for [k (keys d/page-mm)]
             ^{:key k} [:option {:value (name k)} (str/upper-case (name k))])]]
         [field "Lage"
          [:select {:value (if (:portrait? pg) "hoch" "quer")
                    :on-change #(set-pg! {:portrait? (= "hoch" (.. % -target -value))})}
           [:option {:value "quer"} "Quer"] [:option {:value "hoch"} "Hoch"]]]]
        [:label {:class "dg-check"}
         [:input {:type "checkbox" :checked (not (:hide-grid? pg))
                  :on-change #(set-pg! {:hide-grid? (not (.. % -target -checked))})}]
         "Raster zeigen"]
        [:div {:class "dg-pal-head"} "Hintergrund zum Nachzeichnen"]
        [underlay-panel c under]])
     [:div {:class "np-muted dg-help"}
      [:div [:b "Objekt"] " aus der linken Liste aufs Blatt ziehen — es wird sein Symbol."]
      [:div [:b "Verbinden"] ": vom Anschlusspunkt einer Form ziehen (oder Werkzeug C)."
       " Auf einer Leitung losgelassen entsteht ein Abzweig."]
      [:div [:b "Tasten"] ": R drehen, F spiegeln, Pfeile schieben, Strg+C/V/D, Entf, "
       "Leertaste+Ziehen verschiebt das Blatt, Strg+Rad zoomt."]]]))

(defn- props [{:keys [ui] :as c} mdl]
  (let [{:keys [sel edge]} (:st @ui)]
    (cond
      edge (if-let [e (get-in mdl [:edges edge])]
             [edge-panel c (assoc e :id edge)]
             [page-panel c mdl])
      (= 1 (count sel)) (if-let [s (get-in mdl [:shapes (first sel)])]
                          ^{:key (first sel)} [shape-panel c (assoc s :id (first sel))]
                          [page-panel c mdl])
      (seq sel) [multi-panel c (count sel)]
      :else [page-panel c mdl])))

(defn- body-changed! [{:keys [last-body on-body] :as c}]
  (let [mdl (model c) b (d/body mdl)]
    (when (and (not= b @last-body) (not (d/blank? mdl)))
      (reset! last-body b)
      (on-body b))))

(defn- context [{:keys [body live edit? user project on-body] doc-name :name}]
  (let [ydoc (some-> live :ydoc)
        ui   (r/atom {:st {:tool :select :sel #{}} :tick 0})
        c    {:ydoc ydoc :aw (some-> live :aw) :base (d/read-body body) :doc-name doc-name
              :edit? edit? :can? (boolean (and ydoc edit?)) :user user :project project
              :on-body on-body :ui ui :stage (atom nil) :ro (atom nil)
              :last-body (atom body) :body-timer (atom nil) :last-pub (atom 0)
              :undo (when ydoc
                      (Y/UndoManager. (into-array (map #(ym/ymap ydoc %) d/collections))))}]
    (assoc c
           :on-y  (fn [_ _]
                    (swap! ui update :tick inc)
                    (when (and edit? on-body)
                      (js/clearTimeout @(:body-timer c))
                      (reset! (:body-timer c) (js/setTimeout #(body-changed! c) body-ms))))
           :on-aw (fn [_ _] (swap! ui update :tick inc)))))

(defn- listen! [{:keys [ydoc ^js aw on-y on-aw]}]
  (when ydoc (doseq [k d/collections] (.observe (ym/ymap ydoc k) on-y)))
  (when aw (.on aw "change" on-aw)))

(defn- unmount! [{:keys [ydoc ^js aw ^js undo ro body-timer on-y on-aw]}]
  (js/clearTimeout @body-timer)
  (some-> ^js @ro .disconnect)
  (when ydoc (doseq [k d/collections] (.unobserve (ym/ymap ydoc k) on-y)))
  (when aw (.off aw "change" on-aw) (.setLocalStateField aw "diagram" nil))
  (some-> undo .destroy))

(defn- stage-ref! [{:keys [stage ro ui]} el]
  (when (and el (not= el @stage))
    (reset! stage el)
    (some-> ^js @ro .disconnect)
    (reset! ro (doto (js/ResizeObserver. (fn [_] (swap! ui update :tick inc))) (.observe el)))))

(defn editor
  [opts]
  (let [{:keys [ui] :as c} (context opts)]
    (listen! c)
    (r/create-class
     {:component-will-unmount (fn [_] (unmount! c))
      :reagent-render
      (fn [_]
        (let [{:keys [tick]} @ui
              _    tick
              mdl  (model c)
              size (d/page-size mdl)]
          (doseq [t (distinct (keep #(get-in % [:obj :type]) (vals (:shapes mdl))))]
            (load-rows! c t))
          [:div {:class "dg"}
           [toolbar c]
           [palette c mdl]
           [:div {:class "dg-stage" :ref #(stage-ref! c %)}
            (when @(:stage c) [sheet c mdl size])]
           [:div {:class "dg-side"} [props c mdl]]]))})))

(defn static
  [{:keys [id body]}]
  ^{:key (hash body)} [editor {:id id :body body :edit? false}])
