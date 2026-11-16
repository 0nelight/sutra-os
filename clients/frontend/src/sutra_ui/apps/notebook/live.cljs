(ns sutra-ui.apps.notebook.live
  (:require [sutra-ui.storage :as storage]
            ["yjs" :as Y]
            ["y-protocols/awareness" :as awareness]
            ["lib0/buffer" :as buf]
            [cljs.reader :as reader]
            [reagent.core :as r]
            [sutra.types.live-doc :as live]
            [sutra.types.paths :as paths]
            [sutra-ui.api :as api]))

(def ^:private palette
  ["#2d72d2" "#238551" "#c87619" "#cd4246" "#9d3f9d" "#00a396" "#db2c6f" "#946638"])

(defn color-of
  [user]
  (nth palette (mod (reduce #(+ (* 31 %1) (.charCodeAt %2 0)) 0 (str user)) (count palette))))

(defn absorb
  [s {:keys [type] :as m}]
  (case type
    "sync"  (assoc s :status :live :synced? true :peer (:peer m) :edit? (:edit? m))
    "peers" (assoc s :leader (:leader m) :peers (:peers m))
    "reset" (assoc s :status :reset)
    s))

(defn leader?
  [s]
  (and (= :live (:status s)) (some? (:peer s)) (= (:peer s) (:leader s))))

(defn others
  [s]
  (->> (:peers s) (remove #(= (:peer s) (:peer %))) (map :user) distinct vec))

(def ^:private remote "sutra-live-remote")
(def ^:private empty-update-bytes 2)

(defn- b64 [u8] (buf/toBase64 u8))
(defn- from-b64 [s] (buf/fromBase64 s))

(defn- blank? [ydoc] (<= (.-length (Y/encodeStateAsUpdate ydoc)) empty-update-bytes))

(defn- missing-on-server
  [ydoc updates]
  (let [diff (if (seq updates)
               (Y/encodeStateAsUpdate ydoc (-> (into-array (map from-b64 updates))
                                               Y/mergeUpdates
                                               Y/encodeStateVectorFromUpdate))
               (Y/encodeStateAsUpdate ydoc))]
    (when (> (.-length diff) empty-update-bytes) diff)))

(defn- stash-key [id]
  (str "sutra-live-pending:"
       (api/on-page-branch (paths/address :document-live {:id (str id) :client 0}))))

(defn- stashed [id]
  (try (some-> (storage/get-item (stash-key id)) from-b64)
       (catch :default _ nil)))

(defn- unstash! [id]
  (storage/remove-item! (stash-key id)))

(defn- stash!
  [id u]
  (let [all (if-let [old (stashed id)] (Y/mergeUpdates #js [old u]) u)]
    (when-not (storage/set-item! (stash-key id) (b64 all))
      (js/console.warn "notebook live: nicht zwischengespeichert"))))

(defn- socket-url [id client]
  (str (if (= "https:" (.-protocol js/location)) "wss://" "ws://") (.-host js/location)
       (api/on-page-branch (paths/address :document-live {:id (str id) :client client}))))

(defn meta-of
  [{:keys [ydoc]}]
  (let [m (.getMap ^js ydoc "meta")]
    (not-empty (into {} (keep (fn [k] (when-let [v (.get m (name k))]
                                        [k (reader/read-string v)])))
                     [:name :page]))))

(defn set-meta!
  [{:keys [ydoc]} k v]
  (let [m (.getMap ^js ydoc "meta")
        t (pr-str v)]
    (when (not= t (.get m (name k))) (.set m (name k) t))))

(defn- handle!
  [{:keys [id ydoc aw send!]} {:keys [on-sync on-reset]} {:keys [type] :as m}]
  (case type
    "sync"      (let [ups (:updates m)
                      seed? (and (:seed? m) (blank? ydoc))]
                  (when (seq ups)
                    (Y/applyUpdate ydoc (Y/mergeUpdates (into-array (map from-b64 ups))) remote))
                  (when-let [u (and (:edit? m) (not seed?) (stashed id))]
                    (Y/applyUpdate ydoc u remote))
                  (doseq [a (:awareness m)]
                    (awareness/applyAwarenessUpdate aw (from-b64 a) remote))
                  (when-let [d (and (:edit? m) (missing-on-server ydoc ups))]
                    (send! {:type "update" :update (b64 d)}))
                  (when (:edit? m) (unstash! id))
                  (send! {:type   "awareness"
                          :update (b64 (awareness/encodeAwarenessUpdate
                                        aw #js [(.-clientID ^js ydoc)]))})
                  (when on-sync (on-sync seed?)))
    "update"    (Y/applyUpdate ydoc (from-b64 (:update m)) remote)
    "awareness" (awareness/applyAwarenessUpdate aw (from-b64 (:update m)) remote)
    "left"      (awareness/removeAwarenessStates aw #js [(:client m)] remote)
    "compact"   (let [s (b64 (Y/encodeStateAsUpdate ydoc))]
                  (when (<= (count s) live/max-update)
                    (send! {:type "compacted" :through (:through m) :state s})))
    "reset"     (do (unstash! id) (when on-reset (on-reset)))
    "refused"   (js/console.warn "notebook live: abgewiesen" (:reason m))
    nil))

(defn- connect!
  [{:keys [id ydoc state ws closing?] :as s} opts]
  (let [sock (js/WebSocket. (socket-url id (.-clientID ^js ydoc)))]
    (reset! ws sock)
    (set! (.-onmessage sock)
          (fn [e]
            (let [m (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true)]
              (swap! state absorb m)
              (handle! s opts m))))
    (set! (.-onclose sock)
          (fn [_]
            (when (= sock @ws)
              (cond
                @closing?                   (swap! state assoc :status :closed)
                (= :reset (:status @state)) nil
                :else (do (swap! state assoc :status :offline :leader nil)
                          (js/setTimeout #(when-not @closing? (connect! s opts)) 2000))))))))

(defn open!
  [id user opts]
  (let [ydoc  (Y/Doc.)
        aw    (awareness/Awareness. ydoc)
        ws    (atom nil)
        send! (fn [m] (let [^js w @ws]
                        (when (and w (= 1 (.-readyState w)))
                          (.send w (js/JSON.stringify (clj->js m)))
                          true)))
        warn  (fn [^js e] (when (stashed id) (.preventDefault e) (set! (.-returnValue e) "")))
        s     {:id id :ydoc ydoc :aw aw :ws ws :send! send! :warn warn
               :state (r/atom {:status :connecting}) :closing? (atom false)}]
    (.addEventListener js/window "beforeunload" warn)
    (.on ydoc "update"
         (fn [u origin]
           (when (and (not= origin remote) (not (send! {:type "update" :update (b64 u)})))
             (stash! id u))))
    (.on aw "update"
         (fn [^js ch origin]
           (when (not= origin remote)
             (let [ids (.concat (.-added ch) (.-updated ch) (.-removed ch))]
               (send! {:type   "awareness"
                       :update (b64 (awareness/encodeAwarenessUpdate aw ids))})))))
    (.observe (.getMap ydoc "meta")
              (fn [_ ^js tr]
                (when (and (:on-meta opts) (not (.-local tr)))
                  ((:on-meta opts) (meta-of s)))))
    (.setLocalStateField aw "user" #js {:name user :color (color-of user)})
    (connect! s opts)
    s))

(defn close!
  [{:keys [ydoc aw ws closing? warn]}]
  (reset! closing? true)
  (.removeEventListener js/window "beforeunload" warn)
  (awareness/removeAwarenessStates aw #js [(.-clientID ^js ydoc)] "close")
  (some-> ^js @ws .close)
  (.destroy aw)
  (.destroy ydoc))
