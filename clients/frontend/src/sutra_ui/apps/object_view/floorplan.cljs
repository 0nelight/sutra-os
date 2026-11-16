(ns sutra-ui.apps.object-view.floorplan
  (:require [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.types.decimal :as d]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.apps.object-view.model :as ov-model]
            [sutra-ui.ui.page :as page]
            [sutra-ui.apps.object-view.registry :as reg]
            ["reactflow" :default ReactFlow
             :refer [Background Controls applyNodeChanges NodeResizer]]
            ["react" :as react]))

(defn- sel-floorplan-id []
  (ov-model/focused-instance-id))

(declare device-types)

(defn- objs-of [t] (ov-model/complete-rows-of t))
(defn- conns []
  (ov-model/complete-links-of (into [:floorplan :placement :location] device-types)))

(defn- floorplans [] (objs-of :floorplan))

(defn- by-id [t id] (some #(when (= (:id %) id) %) (objs-of t)))

(defn- placements-on
  [floorplan-id]
  (let [pl-ids (->> (conns)
                    (filter #(and (= (:link-type-ref %) :on-floorplan)
                                  (= (:to-id %) floorplan-id)))
                    (map :from-id)
                    set)]
    (filter #(pl-ids (:id %)) (objs-of :placement))))

(defn- placement-location-id [placement-id]
  (->> (conns)
       (filter #(and (= (:link-type-ref %) :depicts-location)
                     (= (:from-id %) placement-id)))
       first
       :to-id))

(defn- placed-location-ids
  [floorplan-id]
  (into #{} (keep #(placement-location-id (:id %)) (placements-on floorplan-id))))

(def ^:private device-types [:host :switch :printer :phone :firewall])

(defn- devices-in-location
  [loc-id]
  (let [dev-ids (->> (conns)
                     (filter #(and (= (:link-type-ref %) :located-in)
                                   (= (:to-id %) loc-id)))
                     (map :from-id)
                     set)]
    (vec (for [t device-types
               o (objs-of t)
               :when (dev-ids (:id o))]
           {:id     (:id o)
            :name   (or (get-in o [:values :name]) "?")
            :type   (name t)
            :status (some-> (get-in o [:values :live-status]) name)}))))

(defn- device-placement-id
  [floorplan-id device-id]
  (let [on-fp (set (map :id (placements-on floorplan-id)))]
    (->> (conns)
         (filter #(and (= (:link-type-ref %) :depicts-device)
                       (= (:to-id %) device-id)
                       (on-fp (:from-id %))))
         first :from-id)))

(defn- location-placement-id
  [floorplan-id loc-id]
  (let [on-fp (set (map :id (placements-on floorplan-id)))]
    (->> (conns)
         (filter #(and (= (:link-type-ref %) :depicts-location)
                       (= (:to-id %) loc-id)
                       (on-fp (:from-id %))))
         first :from-id)))

(defn- placement-by-id [pid]
  (some #(when (= (:id %) pid) %) (objs-of :placement)))

(defn- save-geometry! [pid m] (ov-model/send! [:object-view.floorplan/geometry pid m]))

(defn- add-room!
  ([floorplan-id loc-id] (add-room! floorplan-id loc-id 40 40))
  ([floorplan-id loc-id x y]
   (ov-model/send! [:object-view.floorplan/place floorplan-id {:x x :y y :w 220 :h 150}
                    [:connect-depicts-location loc-id]])))

(defn- add-device!
  [floorplan-id device-id x y]
  (ov-model/send! [:object-view.floorplan/place floorplan-id {:x x :y y :w 110 :h 34}
                   [:connect-depicts-device device-id]]))

(defn- status-color [status]
  (case status
    "up"   "#238551"
    "down" "#cd4246"
    "#abb3bf"))

(def ^:private depth-color ["#9d3f9d" "#2d72d2" "#00a396" "#7a542e"])

(defn- loc-node-inner [props]
  (let [data  (.-data props)
        label (.-label ^js data)
        pid   (.-pid ^js data)
        depth (or (.-depth ^js data) 0)
        color (nth depth-color (min depth (dec (count depth-color))))]
    [:div {:class "explorer-floorplan__loc-node" :style {:border (str "1.5px solid " color)}}
     [:> NodeResizer {:minWidth 80 :minHeight 50 :color color
                      :onResizeEnd
                      (fn [_ p]
                        (save-geometry!
                         (uuid pid)
                         {:x (Math/round (.-x p)) :y (Math/round (.-y p))
                          :w (Math/round (.-width p)) :h (Math/round (.-height p))}))}]
     [:div {:class "explorer-floorplan__loc-label" :style {:background color}}
      label]]))

(defn- device-node-inner [props]
  (let [data   (.-data props)
        label  (.-label ^js data)
        status (.-status ^js data)]
    [:div {:class "explorer-floorplan__device-node"}
     [:span {:class "explorer-floorplan__device-dot" :style {:background (status-color status)}}]
     [:span {:class "u-truncate"} label]]))

(def ^:private node-types
  #js {"loc"    (fn [p] (r/as-element [loc-node-inner p]))
       "device" (fn [p] (r/as-element [device-node-inner p]))})

(defn- ->n
  [v dflt]
  (or (d/lossy-number v) dflt))

(defn- within-parent-loc-id
  [loc-id]
  (->> (conns)
       (filter #(and (= (:link-type-ref %) :within)
                     (= (:from-id %) loc-id)))
       first :to-id))

(defn- within-depth
  [loc-id]
  (loop [d 0, cur loc-id, seen #{}]
    (let [p (within-parent-loc-id cur)]
      (if (and p (not (seen p)))
        (recur (inc d) p (conj seen cur))
        d))))

(defn- build-nodes [floorplan-id]
  (let [pls       (placements-on floorplan-id)
        loc-pls   (filter #(placement-location-id (:id %)) pls)
        loc->node (into {} (for [pl loc-pls]
                             [(placement-location-id (:id pl)) (str (:id pl))]))
        loc-nodes (->> loc-pls
                       (sort-by #(within-depth (placement-location-id (:id %))))
                       (mapv
                        (fn [pl]
                          (let [v          (:values pl)
                                loc-id     (placement-location-id (:id pl))
                                loc        (by-id :location loc-id)
                                parent-loc (within-parent-loc-id loc-id)
                                parent-nd  (get loc->node parent-loc)]
                            (cond-> {:id       (str (:id pl))
                                     :type     "loc"
                                     :position {:x (->n (:x v) 20) :y (->n (:y v) 20)}
                                     :style    {:width (->n (:w v) 220) :height (->n (:h v) 150)}
                                     :data     {:kind  "loc"
                                                :pid   (str (:id pl))
                                                :label
                                                (or (get-in loc [:values :name]) "(unlinked)")
                                                :depth (within-depth loc-id)}}
                              parent-nd (assoc :parentNode parent-nd :extent "parent"))))))
        dev-nodes (vec
                   (for [pl   loc-pls
                         :let [room-node (str (:id pl))
                               room-w    (->n (:w (:values pl)) 220)
                               cols      (max 1 (int (/ (- room-w 12) 96)))
                               devs      (devices-in-location (placement-location-id (:id pl)))]
                         [i d] (map-indexed vector devs)
                         :let [did  (:id d)
                               dpid (device-placement-id floorplan-id did)
                               dv   (:values (when dpid (placement-by-id dpid)))]]
                     {:id         (if dpid (str dpid) (str "dev:" did))
                      :type       "device"
                      :parentNode room-node
                      :extent     "parent"
                      :position   {:x (->n (:x dv) (+ 6 (* (mod i cols) 96)))
                                   :y (->n (:y dv) (+ 26 (* (quot i cols) 34)))}
                      :style      {:width (->n (:w dv) 92) :height (->n (:h dv) 30)}
                      :data       {:kind   "device"
                                   :pid    (if dpid (str dpid) "")
                                   :did    did
                                   :label  (:name d)
                                   :status (:status d)}}))]
    (into loc-nodes dev-nodes)))

(defn- canvas-inner [props]
  (let [fid       (.-fid ^js props)
        iv        (.-iv ^js props)
        nodes-state  (react/useState #js [])
        nodes     (aget nodes-state 0)
        set-nodes (aget nodes-state 1)
        rf-ref    (react/useRef nil)
        menu-state   (react/useState nil)
        menu      (aget menu-state 0)
        set-menu  (aget menu-state 1)]
    (react/useEffect
     (fn []
       (set-nodes (clj->js (build-nodes fid)))
       js/undefined)
     #js [fid iv])
    (r/as-element
     [:div {:class "explorer-floorplan__canvas"
            :on-drag-over (fn [e]
                            (.preventDefault e)
                            (set! (.. e -dataTransfer -dropEffect) "move"))
            :on-drop (fn [e]
                       (.preventDefault e)
                       (let [loc-id (.. e -dataTransfer (getData "application/sutra-loc"))
                             inst   (.-current rf-ref)]
                         (when (and (seq loc-id) inst)
                           (let [flow       (.screenToFlowPosition
                                             inst #js {:x (.-clientX e) :y (.-clientY e)})
                                 parent-pid (some->> (within-parent-loc-id loc-id)
                                                     (location-placement-id fid))
                                 parent-nd  (when parent-pid (.getNode inst (str parent-pid)))
                                 pabs       (some-> parent-nd .-positionAbsolute)
                                 ox         (if pabs (.-x pabs) 0)
                                 oy         (if pabs (.-y pabs) 0)]
                             (add-room! (uuid fid) loc-id
                                        (Math/round (- (.-x flow) ox))
                                        (Math/round (- (.-y flow) oy)))))))}
      [:> ReactFlow
       {:nodes         nodes
        :nodeTypes     node-types
        :onInit        (fn [inst] (set! (.-current rf-ref) inst))
        :onNodeContextMenu
        (fn [event node]
          (.preventDefault event)
          (let [data (.-data node)
                pid  (.-pid ^js data)]
            (when (seq pid)
              (set-menu #js {:x     (.-clientX event)
                             :y     (.-clientY event)
                             :pid   pid
                             :label (.-label ^js data)}))))
        :onNodesChange (fn [changes]
                         (set-nodes (fn [prev] (applyNodeChanges changes prev))))
        :onNodeDragStop
        (fn [_e node]
          (let [pos  (.-position node)
                x    (Math/round (.-x pos))
                y    (Math/round (.-y pos))
                data (.-data node)
                pid  (.-pid ^js data)]
            (if (and (= "device" (.-kind ^js data)) (not (seq pid)))
              (add-device! (uuid fid) (.-did ^js data) x y)
              (save-geometry! (uuid pid) {:x x :y y}))))
        :minZoom 0.2
        :fitView true
        :fitViewOptions #js {:padding 0.2}}
       [:> Background {:variant "dots" :gap 24 :size 1}]
       [:> Controls]]
      (when menu
        [ui/context-menu-popover
         {:is-open       true
          :target-offset {:left (.-x ^js menu) :top (.-y ^js menu)}
          :on-close      #(set-menu nil)
          :content
          (into [ui/menu]
                [[ui/menu-divider {:title (or (.-label ^js menu) "placement")}]
                 [ui/menu-item
                  {:text "Remove from floorplan"
                   :intent :danger
                   :on-click
                     #(ov-model/send! [:object-view.floorplan/remove
                                       (uuid (.-pid ^js menu))])}]])}])])))

(def ^:private level-label ["Buildings" "Floors" "Rooms" "Deeper"])

(defn- palette-entry [floorplan-id loc color]
  [:div {:draggable true
         :on-drag-start (fn [e]
                          (.. e -dataTransfer
                              (setData "application/sutra-loc" (:id loc)))
                          (set! (.. e -dataTransfer -effectAllowed) "move"))
         :on-click #(add-room! (uuid floorplan-id) (:id loc))
         :style {:padding "var(--sp-1) var(--sp-2)" :font-size "var(--fs-sm)" :cursor "grab"
                 :border-radius "var(--radius)" :color "var(--c-text)"
                 :border "1px solid var(--c-border-line)" :border-left (str "3px solid " color)
                 :margin-bottom "var(--sp-1)" :background "var(--c-surface-soft)"}}
   (or (get-in loc [:values :name]) "(unnamed)")])

(defn- palette [floorplan-id]
  (let [placed (placed-location-ids floorplan-id)
        avail  (->> (objs-of :location)
                    (remove #(placed (:id %)))
                    (sort-by #(get-in % [:values :name])))
        by-lvl (group-by #(min (within-depth (:id %)) 3) avail)]
    [:div {:class "explorer-floorplan__palette"}
     [:div {:class "explorer-floorplan__palette-hint"}
      "Drag onto the canvas to place — or click to add."]
     (if (empty? avail)
       [:div {:class "explorer-floorplan__palette-empty"} "All locations placed."]
       (into [:div]
             (for [lvl  (sort (keys by-lvl))
                   :let [color (nth depth-color (min lvl (dec (count depth-color))))]]
               ^{:key lvl}
               [:div
                [:div {:class "explorer-floorplan__palette-group" :style {:color color}}
                 (str (nth level-label (min lvl 3)) " (" (count (by-lvl lvl)) ")")]
                (into [:div]
                      (for [loc (by-lvl lvl)]
                        ^{:key (str (:id loc))}
                        (palette-entry floorplan-id loc color)))])))]))

(defn root [{:keys [id]}]
  (let [fid    (or id (sel-floorplan-id))
        _insts @instances/instances
        iv     @instances/instances-version
        is-fp? (and fid (some #(= (:id %) fid) (floorplans)))]
    (cond
      (empty? (floorplans))
      [ui/empty-state {:icon :map :title (i18n/t :ui/ov-floorplan-none)}]

      (not is-fp?)
      [ui/empty-state {:icon :map :title (i18n/t :ui/ov-floorplan-not-one)}]

      :else
      [:div {:class "u-flex-gap-8"}
       [:div {:class "u-flex-1-min-0"}
        [:> canvas-inner {:fid (str fid) :iv iv}]]
       [palette (str fid)]])))

(page/view! :floorplan root)

(defmethod reg/config-form :floorplan [_ _ _]
  [:div {:class "explorer-floorplan__config-none"}
   (i18n/t :ui/ov-floorplan-config-none)])

(defmethod reg/default-config :floorplan [_] {})
