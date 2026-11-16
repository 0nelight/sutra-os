(ns sutra-ui.apphost.client
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [sutra-ui.apphost.interop :as interop :refer [->js ->clj]]
            [sutra-ui.apphost.toast :refer [toast!]]
            [sutra-ui.debug :as dbg]
            [sutra-ui.download :as dl]
            [sutra.capps.layout :as lay]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]))

(defn- property-types [descriptor]
  (let [t (:property-types descriptor)]
    (into t (keep (fn [[r n]] (some->> (get t r) (vector (keyword (name n))))))
          (:object-names descriptor))))

(defn- read-for
  [descriptor]
  (fn [resp] (interop/read-response resp (property-types descriptor))))

(defn- fetch*
  [url opts]
  (let [u (api/on-page-branch url)]
    (api/fetch-url u opts)))

(defn app-url
  [descriptor & segs]
  (let [base (apply str (or (:api-root descriptor) (str "/api/my-application/" (:id descriptor)))
                    segs)]
    (if-let [v (not-empty (str (get-in descriptor [:version :ask])))]
      (str base "?v=" (js/encodeURIComponent v))
      base)))

(defn- sep [url] (if (str/includes? url "?") "&" "?"))

(defn- with-scenario
  [url scenario]
  (if (str/blank? (str scenario))
    url
    (str url (sep url) "scenario=" (js/encodeURIComponent (str scenario)))))

(defn- with-params
  [url params]
  (let [qs (->> params
                (keep (fn [[k v]]
                        (when-not (str/blank? (str v))
                          (str (name k) "=" (js/encodeURIComponent (str v))))))
                (str/join "&"))]
    (if (str/blank? qs) url (str url (sep url) qs))))

(defn- fail-msg
  [r fallback]
  (or (some-> ^js (:body r) .-error)
      (let [s (:status r)]
        (js/console.warn "app/client:" fallback (str "HTTP " s))
        (str fallback
             (cond (contains? #{401 403} s) " — dafür fehlt die Berechtigung"
                   (= 404 s)                " — nicht gefunden"
                   (and s (>= s 500))       " — der Server hat einen Fehler gemeldet"
                   :else                    "")))))

(defn- active-scenario
  [descriptor scenario]
  (if (str/blank? (str scenario)) (:scenario descriptor) scenario))

(defn door!
  [descriptor path {:keys [body method params fail] :as opts}]
  (let [body? (contains? opts :body)
        url   (cond-> (app-url descriptor path)
                (contains? opts :scenario)
                (with-scenario (active-scenario descriptor (:scenario opts)))
                (seq params) (with-params params))]
    (-> (fetch* url (->js (cond-> {:method      (or method (if body? "POST" "GET"))
                                   :credentials "same-origin"
                                   :headers     (cond-> {"Accept" "application/json"}
                                                  body? (assoc "Content-Type"
                                                               "application/json"))}
                            body? (assoc :body (js/JSON.stringify (->js body))))))
        (.then (read-for descriptor)
               (fn [e]
                 (throw (ex-info (str "Netzwerkfehler: " (.-message e)) (or (ex-data e) {}) e))))
        (.then (fn [r]
                 (if (:ok r)
                   (:body r)
                   (throw (ex-info (fail-msg r (or fail "Anfrage fehlgeschlagen"))
                                   {:status (:status r) :answer (:body r)}))))))))

(defn refresh!
  [descriptor {:keys [scenario vars held more]}]
  (let [t0 (dbg/now)]
    (.then (door! descriptor ""
                  {:params {:scenario (active-scenario descriptor scenario)
                            :vars     (when (seq vars) (js/JSON.stringify (->js vars)))
                            :held     (when (and (seq vars) (map? held) (empty? more))
                                        (js/JSON.stringify (->js held)))
                            :more     (when (seq more) (js/JSON.stringify (->js more)))
                            :data     (lay/answer-asking #{:rows})}
                   :fail   "Laden fehlgeschlagen"})
           (fn [^js b]
             (let [d (dbg/timed :app/->clj #(some-> b .-data ->clj) interop/rows-note)]
               (dbg/measure! :app/refresh t0)
               d)))))

(defn evaluate-params!
  [descriptor action params {:keys [scenario search]}]
  (let [body (cond-> {:parameters params}
               action      (assoc :action action)
               (seq search) (assoc :search search))]
    (.then (door! descriptor "/action-params" {:body body :scenario scenario}) ->clj)))

(def ^:private stale-message "Objekt wurde zwischenzeitlich geändert — bitte neu laden")

(defn- held-back-note
  [kinds]
  (when (seq kinds)
    (let [label #(i18n/t (keyword "ui" (str "branch-effect-" %)))]
      (i18n/t :ui/branch-effects-held {:kinds (str/join ", " (map label kinds))}))))

(defn- stale
  [descriptor scenario ^js answer]
  (let [base {:conflicts (some-> answer .-conflicts ->clj)
              :message   (or (some-> answer .-error) stale-message)}]
    (.then (refresh! descriptor {:scenario scenario})
           (fn [data] {:stale (assoc base :data data)})
           (fn [_] {:stale (assoc base :data nil)}))))

(defn submit!
  [descriptor params {:keys [action scenario]}]
  (let [body (cond-> {:parameters params} action (assoc :action action))
        vars (some-> (:app/input-vals descriptor) (apply []))]
    (.then (door! descriptor "/submit"
                  {:body     body
                   :scenario scenario
                   :params   {:vars (when (seq vars) (js/JSON.stringify (->js vars)))}
                   :fail     "Speichern abgelehnt"})
           (fn [^js b]
             (some-> (some-> b (aget "held-back") ->clj) held-back-note toast!)
             {:data (let [d (some-> b .-data ->clj)
                          c (some-> b (aget "created") ->clj)]
                      (cond-> d
                        (and c (map? d))    (vary-meta assoc :created (vec c))
                        (and (map? d) (:app/input-vals descriptor))
                        (vary-meta assoc :app/whole-page true)))})
           (fn [e]
             (let [{:keys [status answer]} (ex-data e)]
               (if (= 409 status) (stale descriptor scenario answer) (throw e)))))))

(defn submit-strict!
  [descriptor params opts]
  (.then (submit! descriptor params opts)
         (fn [{:keys [data stale]}]
           (if stale (throw (js/Error. (:message stale))) data))))

(defn upload!
  [descriptor payload {:keys [branch]}]
  (.then (door! descriptor "/upload"
                {:body payload :params {:branch branch} :fail "Upload abgelehnt"})
         (fn [^js b]
           {:data    (some-> b .-data ->clj)
            :hash    (some-> b .-hash)
            :build   (some-> b .-build ->clj)
            :counts  (some-> b .-counts ->clj)
            :action  (some-> b .-action ->clj)
            :already (true? (some-> b .-already))})))

(defn- read-door!
  [descriptor path fail spec scenario]
  (.then (door! descriptor path {:body spec :scenario scenario :fail fail}) ->clj))

(defn aggregate!
  [descriptor spec & [{:keys [scenario]}]]
  (.then (door! descriptor "/aggregate"
                {:body spec :scenario scenario :fail "Auswertung fehlgeschlagen"})
         (fn [b]
           (update (->clj b) :keys
                   (fn [k] (-> k
                               (update :group #(mapv keyword %))
                               (update :metrics #(mapv keyword %))))))))

(defn series!
  [descriptor spec & [{:keys [scenario]}]]
  (read-door! descriptor "/series" "Zeitreihe konnte nicht gelesen werden" spec scenario))

(defn function!
  [descriptor spec & [{:keys [scenario]}]]
  (read-door! descriptor "/function" "Funktion konnte nicht ausgeführt werden" spec scenario))

(defn function-key
  [descriptor spec scenario]
  [(app-url descriptor "/function") spec scenario])

(defn page-generation
  [descriptor]
  (some-> (:app/generation descriptor) deref))

(defn edit-history!
  [descriptor spec & [{:keys [scenario]}]]
  (read-door! descriptor "/edit-history" "Änderungs-Historie nicht lesbar" spec scenario))

(defn data-freshness!
  [descriptor & [{:keys [scenario]}]]
  (read-door! descriptor "/data-freshness" "Datenaktualität nicht lesbar" {} scenario))

(defn- post-json!
  [descriptor path spec what]
  (.then (door! descriptor path {:body spec :fail what}) ->clj))

(defn resources!
  [descriptor]
  (post-json! descriptor "/resources" {} "Ressourcen nicht lesbar"))

(defn comments!
  [descriptor spec]
  (post-json! descriptor "/comments" spec "Kommentare nicht lesbar"))

(defn comment-attachment!
  [descriptor spec]
  (-> (fetch* (app-url descriptor "/comments")
              (->js {:method "POST" :credentials "same-origin"
                     :headers {"Content-Type" "application/json"}
                     :body (js/JSON.stringify (->js spec))}))
      (.then (fn [^js r]
               (if (.-ok r) (.blob r) (throw (js/Error. "Anhang nicht lesbar"))))
             (fn [e] (throw (ex-info (str "Netzwerkfehler: " (.-message e))
                                     (or (ex-data e) {}) e))))))

(defn linked-resources!
  [descriptor spec]
  (post-json! descriptor "/linked-resources" spec "Verknüpfte Ressourcen nicht lesbar"))

(defn socket-oeffnen
  [path]
  (js/WebSocket. (str (if (= "https:" (.-protocol js/location)) "wss://" "ws://")
                      (.-host js/location) (api/on-page-branch path))))

(defn refresh-page!
  []
  (or (not (exists? js/window))
      (.dispatchEvent js/window (js/CustomEvent. "sutra:data-changed"
                                                 #js {:cancelable true}))))

(defonce ^:private auto-refresh-off? (atom false))

(defn auto-refresh!
  [on?]
  (reset! auto-refresh-off? (not on?)))

(defn watch!
  [descriptor {:keys [on-change types object-sets min-seconds keep-paused? verdeckt]}]
  (let [typen   (vec (or (seq types)
                         (when-not (seq object-sets)
                           (map name (vals (:object-names descriptor))))))
        mengen  (into (mapv (fn [t] {:type "base" :objectType (name t)}) typen) object-sets)
        floor   (* 1000 (or min-seconds 10))
        hoerer  (random-uuid)
        zuletzt (atom 0)
        offen   (atom nil)
        tot?    (atom false)
        sock    (atom nil)
        neustart (atom nil)
        versuche (atom 0)
        verpasst (atom {})
        menge-von (atom {})
        gehalten? (fn [m] (contains? (some-> verdeckt deref) (get @menge-von (:id m))))
        rufen   (fn rufen [m]
                  (if (or (.-hidden js/document) @auto-refresh-off? (gehalten? m))
                    (swap! verpasst assoc (:id m) m)
                    (let [jetzt (.now js/Date)
                          warte (- floor (- jetzt @zuletzt))]
                      (if (pos? warte)
                        (when-not @offen
                          (reset! offen (js/setTimeout #(do (reset! offen nil) (rufen m)) warte)))
                        (do (reset! zuletzt jetzt) (on-change m))))))
        sichtbar (fn [_]
                   (when-not (or (.-hidden js/document) @auto-refresh-off?)
                     (let [frei (remove gehalten? (vals @verpasst))]
                       (when-let [m (first frei)]
                         (swap! verpasst #(apply dissoc % (map :id frei)))
                         (rufen m)))))
        aufmachen
        (fn aufmachen []
          (when-not @tot?
            (let [ws (socket-oeffnen "/api/ontology/watch")]
              (reset! sock ws)
              (set! (.-onopen ws)
                    (fn [_]
                      (let [wieder? (pos? @versuche)]
                        (reset! versuche 0)
                        (.send ws (js/JSON.stringify
                                   (->js {:id (str (random-uuid))
                                          :requests (mapv (fn [s] {:objectSet s}) mengen)})))
                        (when wieder? (rufen {:type "refreshObjectSet"})))))
              (set! (.-onmessage ws)
                    (fn [ev]
                      (let [m (js/JSON.parse (.-data ev))]
                        (when (= "subscribeResponses" (.-type m))
                          (let [antworten (:responses (->clj m))]
                            (reset! menge-von (zipmap (map :id antworten) mengen))
                            (doseq [a antworten :when (= "error" (:type a))]
                              (js/console.warn "Auto-Refresh: Abo abgelehnt" (pr-str a)))))
                        (when (#{"objectSetChanged" "refreshObjectSet"} (.-type m))
                          (rufen (->clj m))))))
              (set! (.-onclose ws)
                    (fn [_]
                      (when-not @tot?
                        (let [ms (+ (min 30000 (* 1000 (js/Math.pow 2 @versuche)))
                                    (rand-int 1000))]
                          (swap! versuche inc)
                          (reset! neustart (js/setTimeout aufmachen ms)))))))))]
    (when (seq mengen)
      (when-not keep-paused? (auto-refresh! true))
      (add-watch auto-refresh-off? hoerer (fn [_ _ _ _] (sichtbar nil)))
      (when verdeckt (add-watch verdeckt hoerer (fn [_ _ _ _] (sichtbar nil))))
      (.addEventListener js/document "visibilitychange" sichtbar)
      (aufmachen))
    (fn []
      (reset! tot? true)
      (remove-watch auto-refresh-off? hoerer)
      (when verdeckt (remove-watch verdeckt hoerer))
      (.removeEventListener js/document "visibilitychange" sichtbar)
      (when-let [ws @sock] (try (.close ws) (catch :default _ nil)))
      (when-let [t @neustart] (js/clearTimeout t))
      (when-let [t @offen] (js/clearTimeout t)))))

(defonce ^:private preloaded (atom {}))

(defn- around-key [app-id {:keys [type id standard]}]
  [(str app-id) (str type) (str id) (boolean standard)])

(defn forget-preloaded!
  []
  (reset! preloaded {}))

(defn preload-around!
  [app-id arounds]
  (swap! preloaded into (map (fn [{:keys [spec answer]}] [(around-key app-id spec) answer]))
         arounds))

(defn- take-preloaded! [descriptor spec]
  (let [k (around-key (:id descriptor) spec)]
    (when-let [a (get @preloaded k)]
      (swap! preloaded dissoc k)
      a)))

(defn around!
  [descriptor spec & [{:keys [scenario]}]]
  (if-let [a (and (str/blank? (str (active-scenario descriptor scenario)))
                  (take-preloaded! descriptor spec))]
    (js/Promise.resolve a)
    (read-door! descriptor "/around" "Objekt-Umfeld nicht lesbar" spec scenario)))

(defn facets!
  [descriptor spec & [{:keys [scenario]}]]
  (read-door! descriptor "/facets" "Filterwerte konnten nicht geladen werden" spec scenario))

(defn users!
  [descriptor spec & [{:keys [scenario]}]]
  (read-door! descriptor "/users" "Nutzer konnten nicht geladen werden" spec scenario))

(defn search!
  [descriptor spec & [{:keys [scenario]}]]
  (read-door! descriptor "/search" "Suche fehlgeschlagen" spec scenario))

(defn objects!
  [descriptor spec & [{:keys [scenario]}]]
  (read-door! descriptor "/objects" "Objekte konnten nicht gelesen werden" spec scenario))

(defn widget-set!
  [descriptor set-name version]
  (.then (door! descriptor (str "/widget-set/" set-name)
                {:params {:version version} :fail "Widget konnte nicht geladen werden"})
         #(reader/read-string (:bundle-edn (->clj %)))))

(defn blob-url
  [descriptor hash]
  (app-url descriptor "/blob/" hash))

(defn export!
  [descriptor params {:keys [filename mime]}]
  (.then (door! descriptor "/export" {:body params :fail "Export abgelehnt"})
         (fn [^js b]
           (let [nm (cond (fn? filename) (filename b)
                          (some? filename) filename
                          :else (or (some-> b .-filename) "export"))]
             (dl/b64! (.-file_b64 b) (or mime "application/octet-stream") nm)
             nm))))

(defn document!
  [descriptor body]
  (.then (door! descriptor "/document" {:body body :fail "Beleg konnte nicht erzeugt werden"})
         (fn [^js b]
           (if-let [b64 (some-> b .-file_b64 not-empty)]
             {:url      (-> b64 (dl/b64->blob "application/pdf") (js/URL.createObjectURL))
              :filename (or (some-> b .-filename) "beleg.pdf")
              :media    (some-> b .-media)
              :document (some-> b .-document ->clj)}
             (do (js/console.error
                  "app/document: der Server antwortete 2xx ohne PDF (file_b64 fehlt)"
                  (some-> b .-document))
                 (throw (js/Error. (str "der Beleg wurde erzeugt, kam aber ohne PDF"
                                        " zurück — es gibt nichts zu zeigen"))))))))

(defn upload-status!
  [descriptor since]
  (.then (door! descriptor "/upload-status" {:params {:since since} :fail "Status nicht lesbar"})
         #(some-> % ->clj)))

(defn scenarios-list
  [descriptor]
  (.then (door! descriptor "/scenarios" {:fail "Szenarien laden fehlgeschlagen"})
         #(vec (some-> ^js % .-scenarios ->clj))))

(defn scenario-create!
  [descriptor name]
  (post-json! descriptor "/scenarios" {:name name} "Szenario anlegen abgelehnt"))

(defn scenario-apply!
  [descriptor seg & [{:keys [again?]}]]
  (post-json! descriptor (str "/scenarios/" seg "/apply") {:again (boolean again?)}
              "Szenario anwenden abgelehnt"))

(defn generate!
  [descriptor spec]
  (post-json! descriptor "/generate" spec "Inhalt konnte nicht erzeugt werden"))

(defn scenario-duplicate!
  [descriptor seg name]
  (post-json! descriptor (str "/scenarios/" seg "/duplicate") {:name name}
              "Szenario kopieren abgelehnt"))

(defn scenario-rebuild!
  [descriptor seg body]
  (post-json! descriptor (str "/scenarios/" seg "/rebuild") body
              "Szenario neu aufbauen abgelehnt"))

(defn scenario-actions
  [descriptor seg]
  (.then (door! descriptor (str "/scenarios/" seg "/actions")
                {:fail "Szenario lesen fehlgeschlagen"})
         ->clj))

(defn scenario-drop!
  [descriptor seg]
  (.then (door! descriptor (str "/scenarios/" seg)
                {:method "DELETE" :fail "Szenario löschen abgelehnt"})
         (constantly true)))
