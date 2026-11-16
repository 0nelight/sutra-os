(ns sutra-ui.shell.feedback.recording
  (:require [sutra-ui.storage :as storage]
            [clojure.string :as str]
            [sutra.kit.decisions :as decisions]
            [sutra.types.feedback :as b]
            [sutra.types.paths :as paths]
            [sutra-ui.api :as api]
            [sutra-ui.apphost.view-state :as vs]
            [sutra-ui.state.loading :as loading]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.portal :as pstate]))

(def ^:private LS-KEY "sutra-feedback")
(def ^:private SAMMEL-MS 1500)
(def ^:private SEITEN-MS 500)

(def ^:private pfad [:shared.feedback])

(defonce aktiv
  (db/cursor (conj pfad :aktiv) nil))
(defonce ^:private ausgang (db/cursor (conj pfad :ausgang) []))
(defonce ereignisse
  (db/cursor (conj pfad :ereignisse) {}))
(defonce fehler
  (db/cursor (conj pfad :fehler) nil))

(def ^:private bedienbar-sel
  (str "button,a[href],input,select,textarea,summary,tr,li,"
       "[role=button],[role=link],[role=tab],[role=option],[role=menuitem],[role=checkbox],"
       "[role=radio],[role=switch],[role=row],[role=gridcell],[role=combobox],"
       "[tabindex]:not([tabindex='-1'])"))

(def ^:private feld-sel "select,input,textarea,[role=listbox],[role=combobox]")
(def ^:private dialog-sel "[role=dialog],dialog[open],[aria-modal=true]")
(def ^:private bereich-sel (str dialog-sel ",[role=region],section,form"))

(defn eigenes? [el] (some-> el (.closest "[data-feedback]") some?))

(defn- text-von [el] (some-> el .-textContent str/trim not-empty))

(defn- erste-zeile [s]
  (some->> (some-> s str (str/split #"\n")) (map str/trim) (remove str/blank?) first))

(defn- eigener-text
  [el]
  (let [w (.createTreeWalker js/document el js/NodeFilter.SHOW_TEXT)
        im-feld? (fn [n] (when-let [f (some-> (.-parentElement n) (.closest feld-sel))]
                           (.contains el f)))]
    (loop [acc []]
      (if-let [n (.nextNode w)]
        (recur (if (im-feld? n) acc (conj acc (.-nodeValue n))))
        (some-> (str/join " " acc) str/trim not-empty)))))

(defn- objekt-von
  [el]
  (when-let [[_ typ id] (some->> (.getAttribute el "data-objekt") (re-find #"^([^/]+)/(.+)$"))]
    {:typ typ :id id :titel (.getAttribute el "data-objekt-titel")}))

(defn- sichtbar? [el]
  (let [r (.getBoundingClientRect el)]
    (and (pos? (.-width r)) (pos? (.-height r))
         (< (.-top r) (.-innerHeight js/window)) (pos? (.-bottom r)))))

(defn- objekte-im-blick
  []
  (->> (.querySelectorAll js/document "[data-objekt]")
       js/Array.from
       (remove eigenes?)
       (filter sichtbar?)
       reverse
       (keep objekt-von)))

(defn- dialog-name [d]
  (or (not-empty (.getAttribute d "aria-label"))
      (some-> (.querySelector d "h1,h2,h3,h4,[class*=title]") text-von)))

(defn- klickbar?
  [el]
  (= "pointer" (.-cursor (js/getComputedStyle el))))

(defn- zeiger-flaeche
  [el]
  (loop [e el oben nil n 0]
    (if (and e (< n 8) (klickbar? e)) (recur (.-parentElement e) e (inc n)) oben)))

(defn- fakten
  [el]
  (let [r (.getBoundingClientRect el)
        tag (.-tagName el)]
    {:tag tag
     :typ (.getAttribute el "type")
     :rolle (.getAttribute el "role")
     :aria (.getAttribute el "aria-label")
     :beschriftung (or (some->> (.getAttribute el "aria-labelledby") (re-find #"\S+")
                                (.getElementById js/document) text-von)
                       (some-> (.-labels el) (.item 0) eigener-text))
     :text (when-not (#{"INPUT" "TEXTAREA" "SELECT"} tag) (erste-zeile (.-innerText el)))
     :wahl (when (= "SELECT" tag) (some-> (.-selectedOptions el) (.item 0) text-von))
     :klickbar? (klickbar? el)
     :titel (.getAttribute el "title")
     :platzhalter (.getAttribute el "placeholder")
     :bereich (some-> (.closest el bereich-sel) dialog-name)
     :objekt (some-> (.closest el "[data-objekt]") objekt-von)
     :rechteck [(.-left r) (.-top r) (.-width r) (.-height r)]}))

(defn- ziel-von
  [el]
  (let [label (.closest el "label")
        bedien (.closest el bedienbar-sel)
        flaeche (zeiger-flaeche el)]
    (b/ziel (fakten (cond
                      (and label bedien (.contains label bedien)) bedien
                      (some-> label .-control)                    (.-control label)
                      (and bedien flaeche (not= bedien flaeche) (.contains bedien flaeche))
                      flaeche
                      :else (or bedien flaeche el))))))

(defn- anker
  [el]
  (if-let [id (not-empty (.-id el))]
    (str "#" (js/CSS.escape id))
    (when-let [o (.getAttribute el "data-objekt")]
      (str "[data-objekt=\"" (js/CSS.escape o) "\"]"))))

(defn- glied
  [el]
  (or (anker el)
      (let [tag (str/lower-case (.-tagName el))
            basis (->> (js/Array.from (.-classList el))
                       (remove #(re-find #"^(is-|has-)|--" %))
                       (take 2)
                       (map #(str "." (js/CSS.escape %)))
                       (apply str tag))
            geschw (some-> (.-parentElement el) .-children js/Array.from)]
        (if (< 1 (count (filter #(.matches % basis) geschw)))
          (let [n (count (take-while #(not= % el)
                                     (filter #(= (.-tagName %) (.-tagName el)) geschw)))]
            (str basis ":nth-of-type(" (inc n) ")"))
          basis))))

(defn- selektor
  [el]
  (let [glieder (loop [e el acc ()]
                  (if (or (nil? e) (= e (.-body js/document)))
                    (vec acc)
                    (let [acc (conj acc (glied e))]
                      (if (anker e) (vec acc) (recur (.-parentElement e) acc)))))
        eindeutig? (fn [s] (let [ts (.querySelectorAll js/document s)]
                             (and (= 1 (.-length ts)) (= el (aget ts 0)))))]
    (or (->> (range (dec (count glieder)) -1 -1)
             (map #(str/join " > " (subvec glieder %)))
             (filter eindeutig?)
             first)
        (str/join " > " glieder))))

(defn- knoten
  [el]
  (let [an (fn [attr] (some-> (.closest el (str "[" attr "]")) (.getAttribute attr)))
        k  (.closest el "[data-capp-pfad]")]
    {:app    (an "data-capp-app")
     :pfad   (some-> k (.getAttribute "data-capp-pfad"))
     :art    (some-> k (.getAttribute "data-capp-art"))
     :aktion (an "data-aktion")
     :param  (an "data-param")}))

(defn- klassen
  [el]
  (->> (js/Array.from (.-classList el))
       (remove #(re-find #"^(is-|has-)" %))
       (take 12)
       vec
       not-empty))

(defn referenz
  [el nr]
  (cond-> {:nr nr :knoten (knoten el) :selektor (selektor el) :ziel (b/ziel (fakten el))}
    (klassen el) (assoc :klassen (klassen el))))

(defn- ausloeser-von [el] {:knoten (b/knoten* (knoten el)) :selektor (selektor el)})

(defn- offener-dialog []
  (some-> (.querySelectorAll js/document dialog-sel) js/Array.from last dialog-name))

(defn- app-titel []
  (let [{:keys [card descriptor]} @pstate/current-consumer-app]
    (when card
      (or (get-in descriptor [:layout :header :title])
          (pstate/app-label (merge card (select-keys descriptor [:display-name])))))))

(defn- ort []
  (let [w js/window]
    (b/ort {:pfad (str (.. w -location -pathname) (.. w -location -search))
            :app (some-> @pstate/current-consumer-app :card :name name)
            :fenster [(.-innerWidth w) (.-innerHeight w)]
            :scroll [(.-scrollX w) (.-scrollY w)]
            :zustand (vs/stand)
            :objekte (objekte-im-blick)
            :dialog (offener-dialog)})))

(defn- ls-lesen [] (storage/get-edn LS-KEY))
(defn- ls-setzen! [v] (storage/set-edn! LS-KEY v))

(defn- seit-beginn [] (max 0 (- (js/Date.now) (:beginn @aktiv 0))))

(defn- abschicken! [e]
  (swap! ereignisse assoc (:id e) e)
  (swap! ausgang conj e)
  (db/send! [:shared.feedback/planen]))

(defn- merke!
  [e]
  (when (and @aktiv (not (:beendet? @aktiv)))
    (let [id (str (random-uuid))]
      (abschicken! (assoc e :id id :t (or (:t e) (seit-beginn))))
      id)))

(defn- nachtragen!
  [id k v]
  (when (some #(= id (:id %)) @ausgang)
    (swap! ausgang (fn [es] (mapv #(cond-> % (= id (:id %)) (assoc k v)) es)))
    (swap! ereignisse #(cond-> % (contains? % id) (assoc-in [id k] v)))))

(defn melde!
  [m]
  (when @aktiv
    (abschicken! (b/meldung (str (random-uuid)) (seit-beginn) (update m :ort #(or % (ort)))))))

(defn- zum-schluss!
  []
  (when-let [{:keys [id]} @aktiv]
    (when (seq @ausgang)
      (.sendBeacon js/navigator (paths/url :feedback-ereignisse)
                   (js/Blob. #js [(js/JSON.stringify
                                   (clj->js {:sitzung id :ereignisse @ausgang}))]
                             #js {:type "application/json"})))))

(defonce ^:private letzter-klick (atom nil))

(defonce ^:private klicks (atom {}))

(defn- klick-merken! [ts id]
  (when id (swap! klicks #(assoc (if (> (count %) 50) {} %) ts id))))

(defn- unter-zeiger
  [ev]
  (let [t (.-target ev)
        p (when (or (pos? (.-clientX ev)) (pos? (.-clientY ev)))
            (.elementFromPoint js/document (.-clientX ev) (.-clientY ev)))]
    (if (and p (instance? js/Element t) (.contains t p)) p t)))

(defn- bei-klick
  [ev]
  (let [el (unter-zeiger ev)
        z (when (and (instance? js/Element el) (not (eigenes? el))) (ziel-von el))]
    (when z
      (let [a (ausloeser-von el)
            ent (b/entscheidungen (decisions/records))]
        (reset! letzter-klick (assoc a :wann (js/Date.now)))
        (klick-merken!
         (.-timeStamp ev)
         (merke! (cond-> {:art :klick :ort (ort) :punkt [(.-clientX ev) (.-clientY ev)] :ziel z
                         :selektor (:selektor a)}
                  (:knoten a) (assoc :knoten (:knoten a))
                  ent (assoc :entscheidungen ent)
                  (klassen el) (assoc :klassen (klassen el)))))))))

(defn- markiert
  []
  (let [feld (.-activeElement js/document)
        in-feld? (and feld (some? (.-selectionStart feld)) (not= "" (.-value feld)))
        sel (js/getSelection)
        n (some-> sel .-anchorNode)]
    (if in-feld?
      [feld (- (.-selectionEnd feld) (.-selectionStart feld))]
      [(if (= 3 (some-> n .-nodeType)) (.-parentElement n) n) (count (str sel))])))

(defn- bei-kopie [_]
  (let [[el n] (markiert)]
    (when (and el (pos? n) (not (eigenes? el)))
      (merke! {:art :kopie :ort (ort) :ziel (ziel-von el) :laenge n}))))

(defn- bei-einfuegen [ev]
  (let [el (.-target ev)
        n (count (some-> (.-clipboardData ev) (.getData "text")))]
    (when (and (pos? n) (not (eigenes? el)))
      (merke! {:art :einfuegen :ort (ort) :ziel (ziel-von el) :laenge n}))))

(defonce ^:private tastung
  (js/WeakMap.))

(defn- tastung! [el f]
  (.set tastung el (f (or (.get tastung el) {:tasten 0 :dauer {}}))))

(def ^:private tasten-ereignisse #{"keydown" "keypress" "keyup" "beforeinput" "input"})

(defn- zeiten-beobachten!
  []
  (when (and (exists? js/PerformanceObserver)
             (some #{"event"} (js->clj (.-supportedEntryTypes js/PerformanceObserver))))
    (.observe (js/PerformanceObserver.
               (fn [^js liste _]
                 (doseq [^js e (.getEntries liste)
                         :let [iid (.-interactionId e) el (.-target e)]]
                   (cond
                     (and el (pos? iid) (tasten-ereignisse (.-name e)))
                     (tastung! el #(update-in % [:dauer iid] (fnil max 0) (.-duration e)))

                     (= "click" (.-name e))
                     (when-let [id (get @klicks (.-startTime e))]
                       (swap! klicks dissoc (.-startTime e))
                       (nachtragen! id :antwort-ms (js/Math.round (.-duration e))))))))
              #js {:type "event" :durationThreshold 16})))

(defn- verzoegerung-nehmen!
  [el]
  (when-let [{:keys [tasten dauer]} (.get tastung el)]
    (.delete tastung el)
    (when (pos? tasten) (b/verzoegerung tasten (vals dauer)))))

(defn- bei-aenderung [ev]
  (let [el (.-target ev)]
    (when (and (not (eigenes? el)) (not (#{"checkbox" "radio"} (.-type el))))
      (let [v (verzoegerung-nehmen! el)]
        (merke! (cond-> {:art :eingabe :ort (ort) :ziel (ziel-von el)
                         :laenge (count (str (.-value el)))}
                  v (assoc :verzoegerung v)))))))

(defonce ^:private letzte-eingabe (atom nil))

(defn- bei-druck [ev]
  (let [el (.-target ev)]
    (when (and (instance? js/Element el) (not (eigenes? el)))
      (reset! letzte-eingabe
              {:wann (js/Date.now)
               :ursache (cond-> {:art :zeiger :ziel (ziel-von el) :selektor (selektor el)
                                 :punkt [(js/Math.round (.-clientX ev))
                                         (js/Math.round (.-clientY ev))]}
                          (klassen el) (assoc :klassen (klassen el)))}))))

(defn- bei-taste [ev]
  (when-not (eigenes? (.-target ev))
    (reset! letzte-eingabe {:wann (js/Date.now)
                            :ursache (cond-> {:art :taste}
                                       (b/steuertasten (.-key ev)) (assoc :taste (.-key ev)))})
    (tastung! (.-target ev) #(update % :tasten inc)))
  (when (and (b/steuertasten (.-key ev)) (not (eigenes? (.-target ev))))
    (let [el (.-activeElement js/document)]
      (merke! (cond-> {:art :taste :ort (ort) :taste (.-key ev)}
                (and el (not= el (.-body js/document))) (assoc :ziel (ziel-von el)))))))

(defonce ^:private letzte-seite (atom nil))

(defn- seite-pruefen! []
  (let [h (str (.. js/window -location -pathname) (.. js/window -location -search))]
    (when (not= h @letzte-seite)
      (reset! letzte-seite h)
      (merke! (cond-> {:art :seite :ort (ort)} (app-titel) (assoc :titel (app-titel)))))))

(def ^:private fehler-sel
  "[role=alert],.capp-actionform__note,[class*='error']:not([class*='errors'])")
(def ^:private getippt-sel "input:not([type=checkbox]):not([type=radio]),textarea")

(def ^:private absagen-max 8)
(def ^:private ausloeser-ms 5000)
(def ^:private zuordnung-ms 3000)

(defonce ^:private absagen
  (atom ()))

(defn- stapeltext []
  (let [limit (.-stackTraceLimit js/Error)]
    (set! (.-stackTraceLimit js/Error) 40)
    (let [t (.-stack (js/Error.))]
      (set! (.-stackTraceLimit js/Error) limit)
      t)))

(defn- herkunft-lesen [res]
  (when-let [h (.get (.-headers res) "X-Sutra-Herkunft")]
    (try (js->clj (js/JSON.parse (js/decodeURIComponent h)) :keywordize-keys true)
         (catch :default _ nil))))

(defn- absage-merken!
  [methode pfad spur res]
  (let [{:keys [text server aktion fehlt ref-art gesendet]} (herkunft-lesen res)
        {:keys [wann] :as klick} @letzter-klick
        jetzt (js/Date.now)
        a (cond-> {:methode methode :pfad pfad :status (.-status res) :spur (b/spur spur)}
            (seq server) (assoc :server (vec server))
            aktion (assoc :aktion aktion)
            (seq fehlt) (assoc :fehlt (vec fehlt))
            ref-art (assoc :ref-art ref-art)
            (seq gesendet) (assoc :gesendet (update-keys gesendet name))
            (and wann (< (- jetzt wann) ausloeser-ms)) (assoc :ausloeser (dissoc klick :wann)))]
    (swap! absagen #(take absagen-max (conj % {:wann jetzt :text text :absage a})))))

(defn- absage-zu
  [text]
  (let [jetzt (js/Date.now)
        rs @absagen]
    (if-let [r (first (filter #(and (:text %) (str/includes? (str text) (:text %))) rs))]
      (assoc (:absage r) :zuordnung :text)
      (when-let [r (first (filter #(< (- jetzt (:wann %)) zuordnung-ms) rs))]
        (assoc (:absage r) :zuordnung :zeit)))))

(defn- anfrage-pfad
  [url]
  (when (string? url)
    (let [p (.-pathname (js/URL. url (.. js/window -location -href)))]
      (when (and (str/includes? p "/api/") (not (str/includes? p "/api/feedback"))) p))))

(defn- mit-feedback-kopf
  [opts sitzung]
  (let [h (js/Headers. (some-> opts .-headers))]
    (.set h "X-Sutra-Feedback" (str sitzung))
    (js/Object.assign #js {} opts #js {:headers h})))

(defn- fetch-umhuellen!
  []
  (let [orig js/window.fetch]
    (set! js/window.fetch
          (fn [url opts]
            (if-let [pfad (when (:id @aktiv) (anfrage-pfad url))]
              (let [spur (stapeltext)
                    methode (str/upper-case (or (some-> opts .-method) "GET"))]
                (-> (.call orig js/window url (mit-feedback-kopf opts (:id @aktiv)))
                    (.then (fn [res]
                             (when-not (.-ok res) (absage-merken! methode pfad spur res))
                             res))))
              (.call orig js/window url opts))))))

(defonce ^:private gezeigte-fehler (atom #{}))

(defn- fehler-pruefen!
  []
  (let [els (->> (.querySelectorAll js/document fehler-sel)
                 js/Array.from
                 (remove eigenes?)
                 (filter sichtbar?)
                 (remove #(some-> (.-parentElement %) (.closest fehler-sel))))
        werte (when (seq els)
                (->> (.querySelectorAll js/document getippt-sel)
                     js/Array.from
                     (remove eigenes?)
                     (mapv #(.-value %))))
        jetzt (into {}
                    (keep #(when-let [e (b/fehler {:text (.-innerText %) :werte werte
                                                   :knoten (knoten %)
                                                   :selektor (selektor %)})]
                             [(:text e) (cond-> e (klassen %) (assoc :klassen (klassen %)))]))
                    els)]
    (doseq [[text e] jetzt :when (not (@gezeigte-fehler text))]
      (merke! (cond-> (assoc e :ort (ort))
                (absage-zu text) (assoc :absage (absage-zu text)))))
    (reset! gezeigte-fehler (set (keys jetzt)))))

(defonce ^:private offene
  (atom {}))

(defonce ^:private schliessen
  (atom nil))

(defn- bei-fenster-zu [ev]
  (when @aktiv
    (let [wie (keyword (.-detail ev))
          jetzt (js/Date.now)
          {vorher :wie wann :wann} @schliessen]
      (when-not (and (= :zustand wie) (= :befehl vorher) (< (- jetzt wann) b/ursache-ms))
        (reset! schliessen (cond-> {:wann jetzt :wie wie}
                             (= :befehl wie) (assoc :spur (b/spur (stapeltext)))))))))

(defn- geschlossen? [el]
  (boolean (re-find #"overlay-exit" (str (.getAttribute el "class")))))

(defn- fenster-jetzt []
  (into {}
        (comp (remove eigenes?)
              (remove geschlossen?)
              (map (fn [el] [el {:name (dialog-name el)
                                 :objekt (some-> (.querySelector el "[data-objekt]")
                                                 objekt-von)}])))
        (js/Array.from (.querySelectorAll js/document dialog-sel))))

(defn- juengst [a]
  (let [{:keys [wann] :as v} @a]
    (when (and wann (< (- (js/Date.now) wann) b/ursache-ms)) v)))

(defn- fenster-pruefen! []
  (let [vorher @offene
        jetzt (fenster-jetzt)
        ursache (let [{:keys [wann] u :ursache} (juengst letzte-eingabe)]
                  (when u (assoc u :vor-ms (- (js/Date.now) wann))))]
    (reset! offene (into {} (map (fn [[el i]] [el (merge-with #(or %2 %1) (vorher el) i)]))
                         jetzt))
    (doseq [[el i] jetzt :when (not (contains? vorher el))]
      (merke! (b/fenster (assoc i :ort (ort) :wechsel :auf :ursache ursache))))
    (doseq [[el i] vorher :when (not (contains? jetzt el))]
      (let [{:keys [wie spur]} (juengst schliessen)]
        (merke! (b/fenster (assoc i :ort (ort) :wechsel :zu :ursache ursache
                                  :wie wie :spur spur)))))))

(defonce ^:private pruefung (atom nil))

(defn- fenster-beobachten! []
  (.observe (js/MutationObserver.
             (fn [_ _]
               (when (and @aktiv (nil? @pruefung))
                 (reset! pruefung
                         (js/requestAnimationFrame
                          #(do (reset! pruefung nil) (fenster-pruefen!)))))))
            (.-body js/document)
            #js {:childList true :subtree true :attributes true :attributeFilter #js ["class"]}))

(defonce ^:private gehoert? (atom false))

(defn- zuhoeren! []
  (when (compare-and-set! gehoert? false true)
    (doto js/document
      (.addEventListener "click" bei-klick true)
      (.addEventListener "pointerdown" bei-druck true)
      (.addEventListener "change" bei-aenderung true)
      (.addEventListener "keydown" bei-taste true)
      (.addEventListener "copy" bei-kopie true)
      (.addEventListener "paste" bei-einfuegen true))
    (fetch-umhuellen!)
    (zeiten-beobachten!)
    (fenster-beobachten!)
    (.addEventListener js/window "sutra:fenster-zu" bei-fenster-zu)
    (.addEventListener js/window "pagehide" zum-schluss!)
    (js/setInterval #(when @aktiv (seite-pruefen!) (fehler-pruefen!)) SEITEN-MS)))

(defn- roh
  [verlauf]
  (into {} (comp (mapcat #(cons % (:meldungen %)))
                 (map #(dissoc % :satz :stand :meldungen :herkunft))
                 (map (juxt :id identity)))
        verlauf))

(def ^:private ansichten
  [:view/portal :view/object-page])

(defn- verbinden! [s]
  (reset! aktiv s)
  (decisions/start! ansichten)
  (ls-setzen! s)
  (zuhoeren!)
  (reset! letzte-seite nil)
  (reset! gezeigte-fehler #{})
  (reset! offene (fenster-jetzt))
  (seite-pruefen!))

(defn aktiv? [] (some? @aktiv))

(defn- in [s k] (get-in s (conj pfad k)))

(db/reg :shared.feedback/aus
        (fn [s _]
          {:db (update-in s pfad assoc :aktiv nil :ausgang [] :ereignisse {})
           :fx [[:invoke [decisions/stop!]] [:invoke [ls-setzen! nil]]]}))

(db/reg :shared.feedback/planen
        (fn [s _]
          (when-not (in s :geplant?)
            {:db (assoc-in s (conj pfad :geplant?) true)
             :fx [[:after {:key ::senden :ms SAMMEL-MS :event [:shared.feedback/senden]}]]})))

(db/reg :shared.feedback/senden
        (fn [s _]
          (let [{:keys [id]} (in s :aktiv) stapel (in s :ausgang)]
            {:db (assoc-in s (conj pfad :geplant?) false)
             :fx [(when (and id (seq stapel))
                    [:call {:call [loading/or-failure api/post! :feedback-ereignisse
                                   {:sitzung id :ereignisse stapel}]
                            :ok [:shared.feedback/gesendet (into #{} (map :id) stapel)]
                            :err [:sutra/ignored]}])]})))

(db/reg :shared.feedback/gesendet
        (fn [s [_ ids antwort]]
          (if-let [{:keys [status]} (loading/failure antwort)]
            (when (= 409 status) {:fx [[:dispatch [:shared.feedback/aus]]]})
            {:db (update-in s (conj pfad :ausgang) #(filterv (comp not ids :id) %))})))

(db/reg :shared.feedback/starten
        (fn [_ _]
          {:fx [[:call {:call [api/post! :feedback {}]
                        :ok [:shared.feedback/begonnen] :err [:shared.feedback/fehler]}]]}))

(db/reg :shared.feedback/begonnen
        (fn [s [_ {:keys [id begonnen]}]]
          {:db (update-in s pfad assoc :ereignisse {} :fehler nil)
           :fx [[:invoke [verbinden! {:id id :beginn (.getTime (js/Date. begonnen))}]]]}))

(db/reg :shared.feedback/fehler
        (fn [s [_ satz]] {:db (assoc-in s (conj pfad :fehler) satz)}))

(db/reg :shared.feedback/beenden
        (fn [s [_ dann sonst]]
          (if-let [{:keys [id]} (in s :aktiv)]
            (let [stapel (in s :ausgang)
                  ende   [:shared.feedback/ende id dann sonst]]
              {:db (update-in s pfad #(-> % (assoc :fehler nil) (assoc-in [:aktiv :beendet?] true)))
               :fx [(if (seq stapel)
                      [:call {:call [api/post! :feedback-ereignisse
                                     {:sitzung id :ereignisse stapel}]
                              :ok ende :err sonst}]
                      [:dispatch ende])]})
            {:db (assoc-in s (conj pfad :fehler) nil) :fx [[:dispatch dann]]})))

(db/reg :shared.feedback/ende
        (fn [s [_ id dann sonst]]
          {:db (assoc-in s (conj pfad :ausgang) [])
           :fx [[:call {:call [api/post! :feedback-ende {:sitzung id}] :ok dann :err sonst}]]}))

(db/reg :shared.feedback/holen
        (fn [_ [_ id]]
          {:fx [[:call {:call [loading/or-failure api/get! :feedback {:id id}]
                        :ok [:shared.feedback/geholt] :err [:sutra/ignored]}]]}))

(db/reg :shared.feedback/geholt
        (fn [s [_ antwort]]
          (let [{:keys [status] :as f} (loading/failure antwort)]
            (cond
              (or (#{403 404} status) (and (not f) (:beendet (:sitzung antwort))))
              {:fx [[:dispatch [:shared.feedback/aus]]]}
              (not f)
              {:db (update-in s (conj pfad :ereignisse) #(merge (roh (:verlauf antwort)) %))}))))

(defn starten!
  []
  (db/send! [:shared.feedback/starten]))

(defn neu!
  []
  (db/send! [:shared.feedback/beenden [:shared.feedback/starten] [:shared.feedback/fehler]]))

(defn schliessen!
  []
  (db/send! [:shared.feedback/beenden [:shared.feedback/aus] [:shared.feedback/aus]]))

(defn- ms [x] (when (and x (pos? x)) (js/Math.round x)))

(defn- paket
  []
  (into [] (comp (filter #(and (= "script" (.-initiatorType ^js %))
                               (str/includes? (.-name ^js %) "/js/")))
                 (take 8)
                 (map (fn [^js r]
                        {:pfad (.-pathname (js/URL. (.-name r)))
                         :cache? (and (zero? (.-transferSize r)) (pos? (.-decodedBodySize r)))
                         :ms (js/Math.round (.-duration r))})))
        (.getEntriesByType js/performance "resource")))

(defn- geladen!
  []
  (let [^js nav (aget (.getEntriesByType js/performance "navigation") 0)
        ^js fcp (aget (.getEntriesByName js/performance "first-contentful-paint") 0)
        skript (js/Math.round (.now js/performance))
        t (max 0 (- (js/Math.round (.-timeOrigin js/performance)) (:beginn @aktiv 0)))
        fertig! (fn []
                  (let [p (paket)]
                    (merke! (cond-> {:art :laden :t t :ort (ort)
                                     :typ (keyword (str/replace (or (some-> nav .-type) "navigate")
                                                                "_" "-"))
                                     :ms (cond-> {:skript skript}
                                           (ms (some-> nav .-responseStart))
                                           (assoc :antwort (ms (.-responseStart nav)))
                                           (ms (some-> fcp .-startTime))
                                           (assoc :sichtbar (ms (.-startTime fcp)))
                                           (not (.getElementById js/document "boot-status"))
                                           (assoc :bereit (js/Math.round (.now js/performance))))}
                              (seq p) (assoc :paket p)))))
        bis (+ (js/Date.now) 20000)]
    ((fn warten []
       (if (and (.getElementById js/document "boot-status") (< (js/Date.now) bis))
         (js/requestAnimationFrame warten)
         (fertig!))))))

(defn install!
  []
  (when-let [s (ls-lesen)]
    (verbinden! s)
    (geladen!)
    (db/send! [:shared.feedback/holen (:id s)])))
