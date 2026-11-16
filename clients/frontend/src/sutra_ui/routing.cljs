(ns sutra-ui.routing
  (:require [clojure.string :as str]
            [sutra.types.rid :as rid]
            [sutra.i18n.core :as i18n]
            [sutra-ui.shell.desktop-nav :as desktop-nav]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]))

(def ^:private default-branch
  "main")

(def portal-segment
  "apps")

(def portal-alias "app")

(def sticky-params
  ["embedded"])

(defn- segment-of
  [nav ui]
  (let [s (:segment (feature/screen nav))]
    (if (fn? s) (s (feature/screen-slice ui nav)) s)))

(defn- str->nav
  [seg]
  (some (fn [{:keys [id segment aliases embed-aliases]}]
          (when (or (= seg (if (fn? segment) (segment {}) segment))
                    (some #{seg} aliases)
                    (some #{seg} embed-aliases))
            id))
        (feature/screens)))

(defn- embed-alias?
  [nav seg]
  (boolean (some #{seg} (:embed-aliases (feature/screen nav)))))

(def ^:private canvas-kind->str
  {:object-type    "object-type"
   :interface "interface"
   :transform "transform"})

(def ^:private str->canvas-kind (into {} (map (juxt val key)) canvas-kind->str))

(def ^:private detail-kind->kinds-key
  {:object-type     :object-types
   :object-type-group :object-type-groups
   :link-type :link-types
   :interface  :interfaces
   :function        :functions
   :transform  :transforms})

(defn enc [s] (js/encodeURIComponent (str s)))
(defn dec' [s] (js/decodeURIComponent s))

(defn open-app-slug
  [open]
  (some-> open :card :name name))

(defn workspace-slug
  [ws]
  (let [nm (:name ws)]
    (if (keyword? nm) (name nm) (str nm))))

(defn workspace-of-slug
  [slug wss]
  (when (seq (str slug))
    (let [s (str/lower-case (str slug))]
      (or (some #(when (= s (str/lower-case (workspace-slug %))) %) wss)
          (some #(when (= s (str/lower-case (str (:display-name %)))) %) wss)
          (some #(when (= (str slug) (str (:id %))) %) wss)))))

(def ^:private dev-segment "dev")

(defn- dev-tail
  [version]
  (when (= dev-segment version) [dev-segment]))

(defn- read-dev
  [seg]
  (when (= dev-segment seg) dev-segment))

(defn- version-suffix
  []
  (apply str (map #(str "/" %) (dev-tail @pstate/consumer-app-version))))

(defn workspace-path
  [ws open]
  (str "/workspace/" (enc (workspace-slug ws))
       (when-let [slug (open-app-slug open)]
         (str "/app/" (enc slug) (version-suffix)))))

(def editor-path
  "/workspace/workspace-frame/edit")

(defn editor-path? [path-str]
  (= editor-path (str/replace (or path-str "") #"/+$" "")))

(defn parse-workspace-path
  [path-str]
  (let [s     (-> (or path-str "") (str/replace #"^/" ""))
        parts (when (seq s) (str/split s #"/"))]
    (when (and (= "workspace" (first parts)) (second parts)
               (not (editor-path? path-str)))
      (cond-> {:workspace-slug (dec' (second parts))
               :open-app-slug  (when (= "app" (nth parts 2 nil))
                                 (some-> (nth parts 3 nil) dec'))}
        (read-dev (nth parts 4 nil)) (assoc :version (read-dev (nth parts 4 nil)))))))

(defn detail-id->str
  [kind id]
  (cond
    (= kind :link-type) (str id)
    (keyword? id) (or (some-> (catalog/rid-of (detail-kind->kinds-key kind) id) str)
                      (name id))
    :else (str id)))

(defn detail-id->name-str
  [id]
  (cond
    (keyword? id) (name id)
    (rid/rid? id) (let [n (catalog/name-of id)]
                    (if (keyword? n) (name n) (str id)))
    :else (str id)))

(defn str->detail-id
  [kind s]
  (cond
    (= kind :link-type) s
    (rid/rid? s) (or (catalog/name-of s) s)
    :else (keyword s)))

(defn- canvas-tail
  [ui]
  (let [{:keys [kind id]} (:detail-modal ui)
        k-str (canvas-kind->str kind)]
    (if (and k-str id)
      [k-str (enc (detail-id->str kind id))]
      [])))

(defn- nav-tail
  [nav ui]
  (let [s (feature/screen nav)]
    (cond (:tail s)   (feature/address-tail ui nav)
          (:canvas s) (canvas-tail ui)
          :else       [])))

(defn graph-path
  [{:keys [id mode]}]
  (str "/view/" (enc id) "/"
       (or (segment-of (feature/mode->nav mode) {}) (segment-of (feature/home-nav) {}))))

(defn- graph-canvas?
  [nav]
  (contains? (feature/canvas-navs) nav))

(defn- branch-head
  [branch]
  (if (and branch (not= branch default-branch)) ["branch" (enc branch)] []))

(defn dev-app-href
  [branch slug]
  (str "/" (str/join "/" (concat (branch-head branch) [portal-segment (enc slug)]
                                 (dev-tail dev-segment)))))

(defn app-tail
  [slug version]
  (into [(enc slug)] (dev-tail version)))

(defn parse-app-tail
  [[slug version]]
  {:open-app-slug (some-> slug dec')
   :app-version   (read-dev version)})

(defn- shell-path
  [ui]
  (let [{:keys [current-branch current-view-id nav]} ui
        head (cond-> (branch-head current-branch)
               (and (not (str/blank? current-view-id)) (graph-canvas? nav))
               (into ["view" (enc current-view-id)])
               nav               (conj (or (segment-of nav ui) (name nav))))
        tail  (when nav (nav-tail nav ui))
        parts (when-not (= :pending tail) (into head tail))]
    (cond (= :pending tail) nil
          (seq parts)       (str "/" (str/join "/" parts))
          :else             "/")))

(defonce ^:private mode
  (atom :builder))

(defn- consumer-state->path [open]
  (if-let [ws @auth/current-workspace]
    (workspace-path ws open)
    (if-let [slug (open-app-slug open)]
      (str "/" portal-segment "/" (enc slug) (version-suffix))
      "/")))

(defn state->path
  [ui]
  (cond
    @auth/current-workspace (workspace-path @auth/current-workspace @pstate/current-consumer-app)
    (= :consumer @mode)     (consumer-state->path @pstate/current-consumer-app)
    :else                   (shell-path ui)))

(defn href
  [nav params]
  (shell-path (feature/arrive (select-keys @shell/ui-state [:current-branch]) nav params)))

(defn- parse-canvas-tail
  [[kind-str id]]
  (let [k (str->canvas-kind kind-str)]
    (if (and k id)
      {:detail-modal {:kind k :id (str->detail-id k (dec' id))}}
      {})))

(defn- parse-nav-tail [nav segs query]
  (let [s     (feature/screen nav)
        entry (when-let [f (:parse-entry s)] (f (vec segs) query))]
    (cond entry           {:params (merge (feature/address-params nav []) entry)
                           :entry-form? true}
          (:parse-tail s) {:params (feature/address-params nav segs)}
          (:canvas s)     (parse-canvas-tail segs)
          :else           {})))

(defn query-map
  [search]
  (let [p    (js/URLSearchParams. (or search ""))
        skip (set sticky-params)
        m    (transient {})]
    (.forEach p (fn [v k] (when-not (skip k) (assoc! m k v))))
    (persistent! m)))

(defn- parse-shell-path
  [path-str query]
  (let [s     (-> (or path-str "")
                  (str/replace #"^/" "")
                  (str/replace #"^index\.html$" ""))
        parts (when (seq s) (str/split s #"/"))
        [head tail]
        (loop [m {} xs parts]
          (let [[k v & more] xs]
            (cond
              (nil? k) [m []]
              (and v (= k "branch"))
              (recur (assoc m :current-branch (dec' v)) more)
              (and (= k "view") (some? v))
              (recur (cond-> m (seq v) (assoc :current-view-id (dec' v))) more)
              :else [m xs])))
        nav (str->nav (first tail))]
    (cond-> (assoc head :nav nav)
      nav                         (merge (parse-nav-tail nav (rest tail) query))
      (embed-alias? nav (first tail)) (assoc :embedded? true))))

(defn- parse-editor-path
  [path-str _]
  (when (editor-path? path-str) {:editor? true}))

(defn- parse-portal-path
  [path-str _]
  (let [s     (-> (or path-str "") (str/replace #"^/" ""))
        parts (when (seq s) (str/split s #"/"))]
    (when (#{portal-segment portal-alias} (first parts))
      (cond-> {:open-app-slug (some-> (second parts) dec')}
        (read-dev (nth parts 2 nil)) (assoc :version (read-dev (nth parts 2 nil)))))))

(def ^:private route-table
  [{:route :editor    :modes #{:consumer}          :parse parse-editor-path}
   {:route :workspace :modes #{:builder :consumer} :parse #(parse-workspace-path %1)}
   {:route :portal    :modes #{:consumer}          :parse parse-portal-path}
   {:route :shell     :modes #{:builder}           :parse parse-shell-path}])

(defn- match
  [mode path-str query]
  (or (some (fn [{:keys [route modes parse]}]
              (when (modes mode)
                (when-let [r (parse path-str query)] [route r])))
            route-table)
      [:portal {}]))

(defn parse-path
  ([path-str] (parse-path path-str nil))
  ([path-str search]
   (second (match :builder path-str (query-map search)))))

(def ^:private suppress-write? (atom false))

(defn sticky-query
  []
  (when (exists? js/window)
    (let [in  (js/URLSearchParams. (.-search js/location))
          out (js/URLSearchParams.)]
      (doseq [k sticky-params]
        (when-let [v (.get in k)] (.set out k v)))
      (let [s (.toString out)]
        (when-not (str/blank? s) (str "?" s))))))

(defonce ^:private replace-next?
  (atom false))

(defn- write-path! [p]
  (let [target (or p "/")]
    (when (not= (.-pathname js/location) target)
      (if (compare-and-set! replace-next? true false)
        (.replaceState js/history nil "" (str target (sticky-query)))
        (.pushState js/history nil "" (str target (sticky-query)))))))

(defn- location-match
  []
  (match @mode (.-pathname js/location) (query-map (.-search js/location))))

(defn- location-route [] (second (location-match)))

(defn- canonical-entry!
  [route]
  (when (or (:entry-form? route) (:embedded? route))
    (if-let [p (state->path @shell/ui-state)]
      (let [kept (if (:entry-form? route)
                   (or (some-> (sticky-query) (subs 1)) "")
                   (.-search js/location))
            q    (.toString (cond-> (js/URLSearchParams. kept)
                              (:embedded? route) (doto (.set "embedded" "true"))))]
        (.replaceState js/history nil "" (str p (when (seq q) (str "?" q)))))
      (reset! replace-next? true))))

(defn- recompute! []
  (when-not @suppress-write?
    (when-let [p (state->path @shell/ui-state)]
      (write-path! p))))

(defn apply-open-app!
  [slug]
  (when (or (not= slug (open-app-slug @pstate/current-consumer-app))
            (and slug (nil? (get-in @pstate/current-consumer-app [:card :id]))))
    (if slug
      (if-let [card (pstate/consumer-app-of-name slug)]
        (pstate/open-consumer-app! card)
        (pstate/consumer-app-failed! {:name slug}
                                     (i18n/t :ui/app-slug-unknown {:slug slug})))
      (when @pstate/current-consumer-app
        (pstate/close-consumer-app!)))))

(defn- apply-workspace!
  [slug]
  (let [cur (some-> @auth/current-workspace workspace-slug)]
    (cond
      (nil? slug)
      (do (when @auth/current-workspace (pstate/leave-workspace!))
          (js/Promise.resolve nil))

      (and cur (= (str/lower-case slug) (str/lower-case cur)))
      (js/Promise.resolve nil)

      :else
      (-> (pstate/ensure-workspaces!)
          (.then (fn [_]
                   (when-let [ws (workspace-of-slug slug (pstate/workspace-list))]
                     (pstate/enter-workspace! ws))))))))

(defn- on-screen
  [ui {:keys [nav params]}]
  (if nav (feature/arrive ui nav params) (assoc ui :nav nil)))

(defn route->ui
  [ui route]
  (cond-> (merge ui
                 (select-keys route [:current-branch :current-view-id])
                 {:detail-modal  (:detail-modal route)
                  :selected-type (:selected-type route)})
    (contains? route :nav) (on-screen route)))

(defn- apply-shell-route!
  [route]
  (let [checkout-promise (atom nil)]
    (when (seq route)
      (reset! suppress-write? true)
      (try
        (when-let [b (or (:current-branch route)
                         (when (:current-branch @shell/ui-state) default-branch))]
          (when (not= b (:current-branch @shell/ui-state))
            (reset! checkout-promise (sync/checkout-branch! b))))
        (shell/adopt-route! route->ui (dissoc route :current-branch :current-view-id))
        (let [{:keys [kind id]} (:detail-modal route)]
          (when (= kind :object-type)
            (panel/open-bottom-panel! kind id)))
        (when-let [v (and (graph-canvas? (:nav route)) (:current-view-id route))]
          (graph/load-view! v))
        (when-let [f (and (contains? route :nav)
                          (:on-enter (feature/screen (feature/current-nav route))))]
          (f (:params route)))
        (finally (reset! suppress-write? false))))
    (or @checkout-promise (js/Promise.resolve nil))))

(defn- open-surface!
  [workspace-slug app-slug version]
  (reset! suppress-write? true)
  (pstate/set-consumer-app-version! version)
  (-> (apply-workspace! workspace-slug)
      (.then (fn [_] (apply-open-app! app-slug)))
      (.finally (fn [] (reset! suppress-write? false)))))

(defn- apply-route!
  [[route-id route]]
  (case route-id
    :shell     (do (when @auth/current-workspace (pstate/leave-workspace!))
                   (apply-shell-route! route))
    :editor    (do (pstate/open-workspace-editor!)
                   (open-surface! nil nil nil))
    (open-surface! (:workspace-slug route) (:open-app-slug route) (:version route))))

(defonce ^:private history-hooks (atom {}))

(defn on-history!
  [k phase f]
  (swap! history-hooks assoc k [phase f]))

(defn- run-hooks! [phase]
  (doseq [[p f] (vals @history-hooks) :when (= p phase)] (f)))

(defn- follow-address! []
  (let [m (location-match)
        p (apply-route! m)]
    (canonical-entry! (second m))
    p))

(defn- on-popstate [_]
  (run-hooks! :before)
  (follow-address!)
  (run-hooks! :after))

(defn visit!
  [url]
  (.pushState js/history nil "" url)
  (on-popstate nil))

(defn resolve-deep-links!
  []
  (let [route (location-route)]
    (when (or (and (:nav route)
                   (not= (:params route) (feature/address-params (:nav route) [])))
              (:detail-modal route) (:selected-type route))
      (reset! suppress-write? true)
      (try
        (shell/adopt-route!
         (fn [ui route]
           (cond-> (assoc ui
                          :detail-modal  (:detail-modal route)
                          :selected-type (:selected-type route))
             (:nav route) (feature/with-params (:nav route) (:params route))))
         route)
        (let [{:keys [kind id]} (:detail-modal route)]
          (when (= kind :object-type)
            (panel/open-bottom-panel! kind id)))
        (finally (reset! suppress-write? false))))))

(defonce ^:private installed? (atom false))

(defn reapply!
  []
  (when @installed? (follow-address!)))

(defn install!
  [m]
  (reset! mode m)
  (let [p (follow-address!)]
    (when (compare-and-set! installed? false true)
      (add-watch shell/ui-state ::routing (fn [_ _ _ _] (recompute!)))
      (add-watch pstate/current-consumer-app ::routing-apps (fn [_ _ _ _] (recompute!)))
      (add-watch auth/current-workspace ::routing-workspace (fn [_ _ _ _] (recompute!)))
      (.addEventListener js/window "popstate" on-popstate)
      (desktop-nav/install!))
    p))
