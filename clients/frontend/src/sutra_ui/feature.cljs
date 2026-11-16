(ns sutra-ui.feature
  (:require [sutra.i18n.core :as i18n]))

(defonce ^:private registry (atom {:apps {} :screens {} :contributions {}}))

(defn- check!
  [what m required allowed]
  (let [missing (remove #(contains? m %) required)
        unknown (remove (into (set required) allowed) (keys m))]
    (when (or (seq missing) (seq unknown))
      (throw (ex-info (str "feature: " what " " (:id m) " — fehlt " (vec missing)
                           ", unbekannt " (vec unknown))
                      {:spec m :missing (vec missing) :unknown (vec unknown)})))
    m))

(def ^:private app-keys [:branching? :mobile-tab :on-branch-change])

(def ^:private screen-keys
  [:order :segment :aliases :label-key :glyph :hidden? :view :canvas
   :params :enter :tail :parse-tail :parse-entry :embed-aliases :output :on-enter :subject
   :page-title])

(def ^:private canvas-keys
  [:top-bar :overlays :capabilities :header :settings :detail])

(defn icon
  [{:keys [width cap join stroked?] :or {width 1.4 stroked? true}} & shapes]
  (fn [colour]
    (into [:svg (cond-> {:width 20 :height 20 :viewBox "0 0 20 20" :fill "none"
                         :style {:color colour}}
                  stroked? (assoc :stroke "currentColor" :stroke-width width)
                  cap      (assoc :stroke-linecap cap)
                  join     (assoc :stroke-linejoin join))]
          shapes)))

(defn register-app!
  [app]
  (check! "app" app [:id :order :label-key :glyph] app-keys)
  (swap! registry assoc-in [:apps (:id app)] app)
  app)

(defn register-screen!
  [screen]
  (check! "screen" screen [:id :app] screen-keys)
  (when-let [c (:canvas screen)]
    (check! "canvas" c [:mode :positions :graph] canvas-keys))
  (when-not (or (:view screen) (:canvas screen))
    (throw (ex-info (str "feature: screen " (:id screen) " braucht :view oder :canvas")
                    {:spec screen})))
  (when-let [p (:parse-tail screen)]
    (let [extra (remove (set (:params screen)) (keys (p [])))]
      (when (seq extra)
        (throw (ex-info (str "feature: screen " (:id screen) " liest " (vec extra)
                             " aus der Adresse, nimmt sie aber nicht als :params")
                        {:spec screen})))))
  (swap! registry assoc-in [:screens (:id screen)] screen)
  screen)

(defn contribute!
  [slot id item]
  (swap! registry assoc-in [:contributions slot id] (assoc item :id id))
  item)

(defn contributions
  [slot]
  (->> (get-in @registry [:contributions slot]) vals (sort-by (juxt :order (comp str :id)))))

(defn apps
  []
  (sort-by :order (vals (:apps @registry))))

(defn app
  [id]
  (get-in @registry [:apps id]))

(defn screen
  [nav]
  (get-in @registry [:screens nav]))

(defn screens
  []
  (sort-by (juxt :order (comp str :id)) (vals (:screens @registry))))

(defn app-of
  [nav]
  (or (:app (screen nav)) (some-> (first (apps)) :id)))

(defn app-navs
  [app-id]
  (into [] (comp (filter #(= app-id (:app %))) (remove :hidden?) (map :id)) (screens)))

(defn app-default-nav
  [app-id]
  (first (app-navs app-id)))

(defn home-nav
  []
  (some-> (first (apps)) :id app-default-nav))

(defn current-nav
  [ui]
  (or (:nav ui) (home-nav)))

(defn app-label
  [app-id]
  (some-> (app app-id) :label-key i18n/t))

(defn screen-label
  [nav]
  (if-let [k (:label-key (screen nav))] (i18n/t k) (some-> nav name)))

(defn opener
  [family spec]
  (some (fn [c] (when (and (= family (:family c))
                           ((:accepts? c (constantly true)) spec))
                  c))
        (contributions :resource-opener)))

(defn command
  [id]
  (:run! (get-in @registry [:contributions :command id])))

(defn command!
  [id & args]
  (when-let [f (command id)] (apply f args)))

(defn- as-set
  [{:keys [kind object-type id] :as value}]
  (if (= :object kind)
    {:kind :object-set :object-type object-type :query {:ids [(str id)]}}
    value))

(defn open-in-targets
  [value]
  (when value
    (into []
          (keep (fn [c]
                  (let [v (cond (= (:accepts c) (:kind value)) value
                                (= :object-set (:accepts c))   (as-set value))]
                    (when (and v ((:accepts? c (constantly true)) v))
                      (assoc c :value v)))))
          (concat (contributions :open-in)
                  (mapcat #((:targets %)) (contributions :open-in-source))))))

(defn opener-app
  [family spec]
  (when-let [a (:app (opener family spec))]
    (if (fn? a) (a spec) a)))

(defn branching?
  [app-id]
  (boolean (:branching? (app app-id))))

(defn slice-path
  [app-id]
  [:slices app-id])

(defn slice
  [ui app-id]
  (get-in ui (slice-path app-id)))

(defn screen-slice
  [ui nav]
  (slice ui (app-of nav)))

(defn with-params
  [ui nav params]
  (if-let [s (screen nav)]
    (do (when-let [unknown (seq (remove (set (:params s)) (keys params)))]
          (throw (ex-info (str "feature: screen " nav " nimmt " (vec unknown) " nicht an")
                          {:screen nav :params params})))
        (cond-> ui
          (seq params) (update-in (slice-path (:app s)) (:enter s merge) params)))
    ui))

(defn arrive
  [ui nav params]
  (if (screen nav)
    (-> ui (assoc :nav nav) (with-params nav params))
    ui))

(defn address-tail
  [ui nav]
  (when-let [f (:tail (screen nav))]
    (f (screen-slice ui nav))))

(defn address-params
  [nav segments]
  (if-let [f (:parse-tail (screen nav))] (f segments) {}))

(defn subject
  [ui nav]
  (when-let [f (:subject (screen nav))]
    (f (screen-slice ui nav))))

(defn output
  [ui nav]
  (when-let [f (:output (screen nav))]
    (f (screen-slice ui nav))))

(defn page-title
  [ui nav]
  (when-let [f (:page-title (screen nav))]
    (f (screen-slice ui nav))))

(defn canvas
  [mode]
  (when-let [m (some-> mode name keyword)]
    (some (fn [s] (when (= m (get-in s [:canvas :mode]))
                    (assoc (:canvas s) :screen (:id s))))
          (vals (:screens @registry)))))

(defn canvas-settings
  [ui mode]
  (when-let [{f :settings nav :screen} (canvas mode)]
    (when f (f (screen-slice ui nav)))))

(defn nav->mode
  ([nav] (nav->mode nav nil))
  ([nav default] (get-in (screen nav) [:canvas :mode] default)))

(defn mode->nav
  [mode]
  (:screen (canvas mode)))

(defn canvas-navs
  []
  (into #{} (comp (filter :canvas) (map :id)) (screens)))

(defn capabilities
  [mode]
  (:capabilities (canvas mode) #{}))

(defn capable?
  [mode k]
  (contains? (capabilities mode) k))

(defn schema-positions?
  [mode]
  (= :schema (:positions (canvas mode))))
