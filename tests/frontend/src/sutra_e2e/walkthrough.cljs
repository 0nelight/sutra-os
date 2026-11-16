(ns sutra-e2e.walkthrough
  (:require ["playwright-core" :refer [chromium]]
            ["fs" :as fs]
            ["http" :as http]
            ["module" :as module]
            ["path" :as path]
            [cljs.reader :as edn]
            [clojure.string :as str]
            [shadow.resource :as rc]
            [sutra-e2e.async :refer [fang nacheinander plet]]
            [sutra-e2e.walkthrough.befund :as befund]
            [sutra-e2e.walkthrough.bericht :refer [bericht]]))

(def ^:private seite (js/eval (rc/inline "sutra_e2e/walkthrough_seite.js")))
(defn- probe [k] (unchecked-get seite k))
(defn- clj [x] (js->clj x :keywordize-keys true))
(defn- kappe [s n] (let [s (str s)] (subs s 0 (min n (count s)))))
(defn- ende-von [s n] (let [s (str s)] (subs s (max 0 (- (count s) n)))))
(defn- erste-zeile [e] (kappe (first (str/split-lines (str (or (.-message e) e)))) 200))

(defn- passwort
  [user]
  (let [datei (path/resolve js/__dirname "../../.run/cli-creds.edn")
        creds (edn/read-string (str (fs/readFileSync datei "utf8")))
        k (keyword user)]
    (some #(when (map? (get % k)) (get-in % [k :password]))
          (filter map? (tree-seq coll? seq creds)))))

(defn- anmelden
  [base user]
  (let [pw (or (passwort user) (throw (js/Error. (str "kein Passwort für " user))))]
    (plet [r (js/fetch (str base "/api/login")
                       #js {:method "POST" :signal (js/AbortSignal.timeout 30000)
                            :headers #js {"content-type" "application/transit+json"
                                          "accept" "application/transit+json"}
                            :body (js/JSON.stringify #js ["^ " "~:username" user "~:password" pw])})
           text (.text r)]
          (or (second (re-find #"\"~:token\",\"([^\"]+)\"" text))
              (throw (js/Error. (str "Login " user ": HTTP " (.-status r))))))))

(defn- api-fn [base tok]
  (fn [pfad opts]
    (js/fetch (str base pfad)
              (clj->js (merge {:signal (js/AbortSignal.timeout 30000)
                               :headers {"cookie" (str "sutra-session=" tok)
                                         "accept" "application/json"
                                         "content-type" "application/json"}}
                              opts)))))

(defn- sandbox!
  [ctx api user merke]
  (let [szenarien (atom {})
        szenario
        (fn [rid]
          (or (@szenarien rid)
              (let [pr (plet [x (api (str "/api/my-application/" rid "/scenarios")
                                     {:method "POST"
                                      :body (js/JSON.stringify
                                             #js {:name (str "walkthrough-" user "-"
                                                             (js/Date.now))})})
                              j (fang (.json x) (constantly #js {}))]
                             (or (.-scenario j)
                                 (throw (js/Error. (str "Szenario für " rid ": HTTP " (.-status x)
                                                        " " (js/JSON.stringify j))))))]
                (swap! szenarien assoc rid pr)
                pr)))
        weiter (fn [route & [opts]] (js-invoke route "continue" (clj->js opts)))
        sperre (fn [route text] (merke :gesperrt text) (.abort route "blockedbyclient"))]
    (plet [_ (.route ctx "**/api/**"
                     (fn [route]
                       (let [q (.request route) u (js/URL. (.url q)) pfad (.-pathname u)
                             m (re-find #"^/api/my-application/([^/]+)(/.*)?$" pfad)
                             lesend (contains? #{"GET" "HEAD"} (.method q))]
                         (cond
                           (and m (not (re-find #"/scenarios(/|$)" (or (nth m 2) ""))))
                           (.then (szenario (nth m 1))
                                  (fn [seg]
                                    (.set (.-searchParams u) "scenario" seg)
                                    (when-not lesend
                                      (merke :erlaubt (str (.method q) " " pfad " [szenario]")))
                                    (weiter route {:url (str u)}))
                                  (fn [e]
                                    (if lesend
                                      (do (merke :fehler (kappe (.-message e) 200)) (weiter route))
                                      (sperre route (str (.method q) " " pfad
                                                         " (" (.-message e) ")")))))
                           lesend (weiter route)
                           :else (sperre route (str (.method q) " " pfad " "
                                                    (kappe (.postData q) 300)))))))
           _ (.route ctx #"^(?!.*/api/).*"
                     (fn [route]
                       (let [q (.request route)]
                         (if (contains? #{"GET" "HEAD"} (.method q))
                           (weiter route)
                           (sperre route (str (.method q) " " (kappe (.url q) 120)))))))]
          (fn aufraeumen []
            (nacheinander
             @szenarien
             (fn [[rid pr]]
               (plet [s (fang pr (constantly nil))]
                     (when s
                       (fang (api (str "/api/my-application/" rid "/scenarios/"
                                  (js/encodeURIComponent s))
                                  {:method "DELETE"})
                             (constantly nil)))))
             (constantly false))))))

(defn- ort [p s]
  (let [sichtbar #(.first (.locator % "visible=true"))]
    (cond
      (:klick s) (sichtbar (.getByText p (:klick s) #js {:exact (boolean (:genau s))}))
      (:knopf s) (sichtbar (.getByRole p "button" #js {:name (:knopf s)}))
      (:zeile s) (.first (.locator p "tr" #js {:hasText (:zeile s)}))
      (:tippe s) (.first (.getByPlaceholder p (first (:tippe s))))
      (:option s) (.first (.locator p "li[role=option]" #js {:hasText (:option s)}))
      (:waehle s) (sichtbar (.locator p (first (:waehle s))))
      (:fuelle s) (sichtbar (.locator p (first (:fuelle s)))))))

(defn- handeln [p base s el]
  (let [m (.-mouse p)]
    (cond
      (:goto s) (.goto p (str base (:goto s)) #js {:waitUntil "domcontentloaded" :timeout 30000})
      (some s [:klick :knopf :zeile :option]) (.click el)
      (:tippe s) (.fill el (str (second (:tippe s))))
      (:waehle s) (.selectOption el (str (second (:waehle s))))
      (:fuelle s) (.fill el (str (second (:fuelle s))))
      (:taste s) (.press (.-keyboard p) (:taste s))
      (:ziehe s) (let [[a b c d] (:ziehe s)]
                   (plet [_ (.move m a b)
                          _ (.down m)
                          _ (.move m (/ (+ a c) 2) (/ (+ b d) 2) #js {:steps 5})
                          _ (.move m c d #js {:steps 5})]
                         (.up m)))
      (:maus s) (.click m (first (:maus s)) (second (:maus s)))
      (:rad s) (let [[x y dy] (:rad s)] (plet [_ (.move m x y)] (.wheel m 0 dy))))))

(defn- lies [p k & [arg]]
  (fang (.then (.evaluate p (probe k) arg) clj) (constantly nil)))

(defn- axe [p quelle methode]
  (when quelle
    (fang (plet [da (.evaluate p "!!window.axe")
                 _ (when-not da (.addScriptTag p #js {:content quelle}))
                 r (.evaluate p (probe "axe"))]
                (clj r))
          (fn [e] (swap! methode conj (str "axe: " (kappe (.-message e) 120))) nil))))

(defn- sichern! [{:keys [out protokoll]}]
  (fs/writeFileSync (str out "/protokoll.json") (js/JSON.stringify (clj->js @protokoll) nil 1)))

(defn- schritt [{:keys [p base log protokoll tastatur out axe-quelle] :as w} s]
  (reset! log {:gesperrt [] :fehler [] :erlaubt [] :antworten []})
  (let [nr (inc (count (:schritte @protokoll)))
        el (ort p s)
        abbruch (atom nil)
        methode (atom [])
        t0 (atom nil)
        bild (str out "/" (.padStart (str nr) 2 "0") "-" (or (:bild s) "schritt") ".png")]
    (plet [vor (lies p "inventar" (:sucht s))
           ziel (when el
                  (fang (plet [_ (.waitFor el #js {:state "visible"})
                               z (.evaluate el (probe "ziel"))]
                              (clj z))
                        (constantly {:fehlt true})))
           _ (when (:fehlt ziel) (reset! abbruch "Bedienelement nicht gefunden"))
           _ (reset! t0 (js/Date.now))
           lade (when-not @abbruch
                  (plet [_ (fang (handeln p base s el) #(reset! abbruch (erste-zeile %)))]
                        (fang (.evaluate p (probe "lade")) (constantly 0))))
           _ (fang (.waitForLoadState p "networkidle" #js {:timeout 15000}) (constantly nil))
           ms (max 0 (- (js/Date.now) @t0 500))
           _ (.waitForTimeout p (or (:warte s) 1200))
           nach (lies p "inventar" (:erwartet s))
           layout (lies p "layout")
           a11y (axe p axe-quelle methode)
           _ (fang (.screenshot p #js {:path bild}) (constantly nil))]
          (let [k (befund/klm s @tastatur)
                l @log
                rec {:nr nr :schritt s :ms ms :lade (or lade 0) :klm k :ziel ziel :abbruch @abbruch
                     :bild bild :nachher nach :layout layout :a11y a11y :methode @methode
                     :treffer (:treffer vor) :gesperrt (:gesperrt l) :erlaubt (:erlaubt l)
                     :antworten (:antworten l) :fehler (vec (take 15 (distinct (:fehler l))))}
                rec (assoc rec :fragen (befund/fragen s rec vor nach))
                rec (assoc rec :befunde (befund/befunde rec nach))]
            (reset! tastatur (:tastatur k))
            (swap! protokoll update :schritte conj rec)
            (sichern! w)
            rec))))

(defn- kompakt [r]
  (let [n (:nachher r)]
    {:nr (:nr r) :ms (:ms r) :abbruch (:abbruch r) :bild (:bild r)
     :url (:url n) :titel (:titel n) :knoepfe (:knoepfe n) :felder (:felder n)
     :modal (:modal n) :live (:live n) :text (kappe (:text n) 1500)
     :fragen (map #(str (:auto %) ": " (:beleg %)) (:fragen r))
     :befunde (map #(str "[S" (or (:schwere %) "–") "] " (:art %) ": " (:beleg %)) (:befunde r))
     :klm (:klm r) :gesperrt (:gesperrt r) :erlaubt (:erlaubt r) :antworten (:antworten r)
     :fehler (:fehler r)}))

(defn- urteilen! [{:keys [protokoll] :as w} {:keys [schritt notiz] :as u}]
  (let [i (dec schritt)]
    (when-not (get-in @protokoll [:schritte i]) (throw (js/Error. (str "kein Schritt " schritt))))
    (doseq [[k v] (dissoc u :schritt :notiz)
            :let [q (some->> (name k) (re-find #"^q([1-4])$") second js/parseInt)]]
      (when-not (and q (contains? #{"ja" "nein" "unklar"} v))
        (throw (js/Error. (str (name k) ": ja|nein|unklar"))))
      (swap! protokoll assoc-in [:schritte i :fragen (dec q) :urteil] v))
    (when notiz (swap! protokoll assoc-in [:schritte i :notiz] notiz))
    (sichern! w)
    (mapv #(or (:urteil %) (:auto %)) (get-in @protokoll [:schritte i :fragen]))))

(defn- abschluss! [{:keys [out protokoll aufraeumen browser] :as w}]
  (let [{:keys [md ampel]} (bericht @protokoll)]
    (fs/writeFileSync (str out "/bericht.md") md)
    (sichern! w)
    (plet [_ (aufraeumen) _ (.close browser)] ampel)))

(defn- steuern! [{:keys [out user log protokoll] :as w} port]
  (.listen
   (http/createServer
    (fn [req res]
      (let [body (atom "")
            antwort #(.end res (js/JSON.stringify (clj->js %) nil 1))]
        (.on req "data" #(swap! body str %))
        (.on req "end"
             (fn []
               (-> (js/Promise.resolve nil)
                   (.then
                    (fn [_]
                      (let [s (clj (js/JSON.parse (if (str/blank? @body) "{\"sieh\":true}" @body)))]
                        (cond
                          (:urteil s) {:urteil (urteilen! w (:urteil s))}
                          (:befund s) (do (swap! protokoll update :manuell conj
                                                 (assoc (:befund s)
                                                        :nr (get-in s [:befund :schritt])))
                                          (sichern! w)
                                          {:befunde (count (:manuell @protokoll))})
                          (:ende s) (plet [ampel (abschluss! w)]
                                          (.end res (js/JSON.stringify
                                                     #js {:ende true :ampel ampel
                                                          :bericht (str out "/bericht.md")}))
                                          (js/process.exit 0))
                          :else (.then (schritt w s) kompakt)))))
                   (.then #(when (map? %) (antwort %)))
                   (.catch #(antwort {:fehler (kappe (or (.-message %) %) 400)
                                      :gesperrt (:gesperrt @log)}))))))))
   port "127.0.0.1" #(println (str "walkthrough " user " bereit auf :" port))))

(defn- durchlaufen! [{:keys [out] :as w} schritte]
  (plet [_ (nacheinander
            schritte
            (fn [s]
              (plet [r (schritt w s)]
                    (println (str (.padStart (str (:nr r)) 2) " "
                                  (apply str (map #(first (:auto %)) (:fragen r))) " "
                                  (.toFixed (/ (:ms r) 1000) 1) "s "
                              (count (:befunde r)) " Befunde "
                                  (when (:abbruch r) (str "ABBRUCH " (:abbruch r) " ")) (:ziel s)))
                    r))
            (fn [r s] (and (:abbruch r) (not (:weiter s)))))
         ampel (abschluss! w)]
        (println (str out "/bericht.md — " ampel))
        (set! (.-exitCode js/process) (if (= ampel "ROT") 1 0))))

(defn- axe-quelle []
  (try (str (fs/readFileSync (.resolve (module/createRequire js/__filename) "axe-core/axe.min.js")
                             "utf8"))
       (catch :default _ nil)))

(defn main [& args]
  (let [[vor [_ port & nach]] (split-with #(not= "--port" %) args)
        [buch-pfad out-arg] (concat vor nach)
        _ (when-not buch-pfad
            (js/console.error "node out/walkthrough.js <drehbuch.json> [out-dir] [--port N]")
            (js/process.exit 2))
        buch (clj (js/JSON.parse (fs/readFileSync buch-pfad "utf8")))
        out (or out-arg (str "out/walk-" (-> buch-pfad (str/replace #"^.*/" "")
                                             (str/replace #"\.json$" ""))))
        base (or (.. js/process -env -WALK_BASE) "http://localhost:8030")
        user (or (:benutzer buch) (throw (js/Error. "Drehbuch ohne benutzer")))
        [breite hoehe] (or (:viewport buch) [1600 1000])
        quelle (axe-quelle)
        log (atom {})
        merke (fn [k x] (swap! log update k (fnil conj []) x))]
    (fs/mkdirSync out #js {:recursive true})
    (-> (plet [tok (anmelden base user)
               browser (.launch chromium #js {:executablePath "/usr/bin/chromium"
                                              :args #js ["--no-sandbox" "--disable-gpu"]})
               ctx (.newContext browser
                                #js {:viewport #js {:width breite :height hoehe} :locale "de-AT"
                                     :timezoneId "Europe/Vienna" :ignoreHTTPSErrors true
                                     :bypassCSP true})
               _ (.addCookies ctx #js [#js {:name "sutra-session" :value tok
                                            :domain (.-hostname (js/URL. base)) :path "/"}])
               aufraeumen (sandbox! ctx (api-fn base tok) user merke)
               p (.newPage ctx)]
              (.setDefaultTimeout p 15000)
              (.on p "pageerror" #(merke :fehler (str "JS " (kappe (.-message %) 200))))
              (.on p "console" #(when (= "error" (.type %)) (merke :fehler (kappe (.text %) 200))))
              (.on p "response"
                   (fn [x]
                     (when (re-find #"/submit" (.url x))
                       (.then (fang (.text x) (constantly "")) #(merke :antworten (kappe % 4000))))
                     (when (>= (.status x) 400)
                       (merke :fehler (str "HTTP " (.status x) " " (ende-von (.url x) 80))))))
              (let [w {:p p :base base :out out :user user :log log :browser browser
                       :aufraeumen aufraeumen :axe-quelle quelle :tastatur (atom false)
                       :protokoll (atom {:benutzer user :aufgabe (:aufgabe buch)
                                         :persona (:persona buch) :vorher (:vorher buch)
                                         :axe (boolean quelle) :start (.toISOString (js/Date.))
                                         :schritte [] :manuell []})}]
                (if port
                  (steuern! w (js/parseInt port))
                  (durchlaufen! w (:schritte buch)))))
        (.catch (fn [e]
                  (js/console.error (str "walkthrough: " (or (.-message e) e)))
                  (js/process.exit 2))))))
