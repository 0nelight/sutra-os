(ns sutra-ui.client-log
  (:require [sutra-ui.storage :as storage]
            [sutra-ui.api :as api]
            [sutra-ui.debug :as dbg]
            [sutra-ui.routing :as routing]))

(def ^:private always-shipped
  #{:console-error :console-warn :uncaught :promise-rejection
    :api-error :render-error :invisible :navigation})

(def ^:private MAX-BUFFER 500)
(def ^:private FLUSH-MS  1500)
(def ^:private FLUSH-AT  40)
(def ^:private LS-KEY    "sutra-clientlog-mode")

(defonce ^:private mode (atom :errors))
(defonce ^:private buf (atom []))
(defonce ^:private flush-scheduled? (atom false))
(defonce ^:private shipping? (atom false))
(defonce ^:private installed? (atom false))

(defn- ship? [level]
  (or (= :all @mode) (contains? always-shipped level)))

(defn- entry->wire [{:keys [level src msg stack t]}]
  {:level   (name level)
   :message msg
   :stack   stack
   :src     src
   :ts      (str t)
   :url     (.. js/window -location -href)
   :ua      (.. js/navigator -userAgent)})

(defn flush!
  []
  (reset! flush-scheduled? false)
  (let [entries @buf]
    (when (seq entries)
      (reset! buf [])
      (reset! shipping? true)
      (-> (api/post! :client-log {:entries (mapv entry->wire entries)})
          (.then  (fn [_] (reset! shipping? false)))
          (.catch (fn [_] (reset! shipping? false)))))))

(defn map-stack!
  [{:keys [label message stack]}]
  (-> (api/post! :client-log {:entries [{:level   "render-error"
                        :message (or message label "render error")
                        :stack   stack
                        :url     (.. js/window -location -href)
                        :ua      (.. js/navigator -userAgent)}]})
      (.then  (fn [resp] (:mapped resp)))
      (.catch (fn [_] nil))))

(def ^:private PHASE-MS 2000)

(defn- ship-phases! []
  (when @api/profile-armed?
    (when-let [evs (seq (dbg/drain-events!))]
      (-> (api/post! :profile {:events (vec evs)})
          (.catch (fn [_] nil))))))

(defn- schedule-flush! []
  (when (compare-and-set! flush-scheduled? false true)
    (js/setTimeout flush! FLUSH-MS)))

(defn- on-entry [entry]
  (when (and (or (not @shipping?) (= :navigation (:level entry))) (ship? (:level entry)))
    (let [n (count (swap! buf (fn [b] (if (>= (count b) MAX-BUFFER) b (conj b entry)))))]
      (if (>= n FLUSH-AT) (flush!) (schedule-flush!)))))

(defn set-mode!
  [m]
  (let [m (keyword m)
        m (if (#{:errors :all} m) m :errors)]
    (reset! mode m)
    (storage/set-item! LS-KEY (name m))
    m))

(defn- query-mode []
  (try
    (let [p (js/URLSearchParams. (.. js/window -location -search))]
      (when-let [v (.get p "clientlog")] (keyword v)))
    (catch :default _ nil)))

(def ^:private AFTER-POPSTATE-MS [2000 15000])

(defonce ^:private last-path (atom nil))

(defn- current-path []
  (str (.. js/window -location -pathname) (.. js/window -location -search)))

(defn- track-history-writes! []
  (doseq [method ["pushState" "replaceState"]]
    (let [original (aget js/history method)]
      (aset js/history method
            (fn [& args]
              (let [result (.apply original js/history (to-array args))]
                (reset! last-path (current-path))
                result))))))

(defn- navigation! [msg]
  (dbg/record! :navigation "history" msg))

(defn- measure-after-popstate! [delay-ms]
  (let [started (.now js/Date)]
    (js/setTimeout
     (fn []
       (let [app (.getElementById js/document "app")]
         (navigation! (str "after-popstate " delay-ms "ms"
                           " path=" (current-path)
                           " text=" (count (some-> app .-textContent))
                           " error-panel=" (some? (.querySelector js/document ".errboundary"))
                           " lag=" (- (.now js/Date) started delay-ms) "ms"))))
     delay-ms)))

(defn- on-popstate []
  (navigation! (str "popstate from=" @last-path " to=" (current-path)))
  (reset! last-path (current-path))
  (run! measure-after-popstate! AFTER-POPSTATE-MS))

(defn- on-pageshow [e]
  (when (.-persisted e)
    (navigation! (str "pageshow from-bfcache path=" (current-path)))))

(defn- note-document-arrival! []
  (when-let [entry (first (.getEntriesByType js/performance "navigation"))]
    (when (= "back_forward" (.-type entry))
      (navigation! (str "document-load back-forward path=" (current-path))))))

(defn- install-navigation-trace! []
  (reset! last-path (current-path))
  (track-history-writes!)
  (routing/on-history! ::trace :before on-popstate)
  (.addEventListener js/window "pageshow" on-pageshow)
  (note-document-arrival!))

(defn install!
  []
  (when (compare-and-set! installed? false true)
    (set-mode! (or (query-mode) (some-> (storage/get-item LS-KEY) keyword) :errors))
    (dbg/add-sink! on-entry)
    (install-navigation-trace!)
    (js/setInterval ship-phases! PHASE-MS)
    (set! (.. js/window -__sutra_clientlog)
          #js {:mode    (fn [m] (name (set-mode! m)))
               :flush   (fn [] (flush!) true)
               :phases  (fn [] (ship-phases!) true)
               :current (fn [] (name @mode))}))
  true)
