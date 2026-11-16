(ns sutra-ui.shell.feedback
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra.types.feedback :as b]
            [sutra-ui.shell.feedback.recording :as aufzeichnung]
            [sutra-ui.state.db :as db]
            [reagent.dom.client :as rdomc]))

(def ^:private SICHTBAR 60)

(defn- leer [] {:entwurf {}})

(defonce ^:private z (r/atom (leer)))
(defonce ^:private zu? (r/atom false))
(defonce ^:private waehlen
  (r/atom nil))

(def ^:private REFERENZEN-MAX 9)

(defn- letzter-ort
  [ereignisse]
  (some->> (vals ereignisse) (remove b/meldung?) seq (apply max-key :t) :ort))

(defn- uhr [t]
  (let [s (quot t 1000)] (str (quot s 60) ":" (.padStart (str (mod s 60)) 2 "0"))))

(defn- melden! [bezug]
  (let [ereignisse @aufzeichnung/ereignisse
        ort (when bezug (get-in ereignisse [bezug :ort]))]
    (aufzeichnung/melde! (assoc (get-in @z [:entwurf bezug]) :bezug bezug :ort ort))
    (swap! z update :entwurf dissoc bezug)))

(defn- referenzieren!
  [bezug el]
  (swap! z update-in [:entwurf bezug]
         (fn [{:keys [referenzen text] :as e}]
           (let [nr (inc (reduce max 0 (map :nr referenzen)))]
             (assoc e
                    :referenzen (conj (vec referenzen) (aufzeichnung/referenz el nr))
                    :text (str (some-> text str/trimr not-empty (str " ")) "/" nr " "))))))

(defn- in-app? [el] (and el (not= el (.-body js/document)) (not (aufzeichnung/eigenes? el))))

(defn- schlucken! [ev]
  (when (in-app? (.-target ev))
    (.preventDefault ev)
    (.stopImmediatePropagation ev)
    true))

(defn- bei-zeigen [ev]
  (let [el (.-target ev)]
    (when-not (= el (:el @waehlen))
      (swap! waehlen assoc :el (when (in-app? el) el) :unten []))))

(defn- bei-druck [ev] (schlucken! ev))

(defn- bei-wahl [ev]
  (when (schlucken! ev)
    (let [{:keys [bezug el]} @waehlen]
      (reset! waehlen nil)
      (when el (referenzieren! bezug el)))))

(defn- bei-taste-waehlen
  [ev]
  (let [{:keys [el unten]} @waehlen
        oben (some-> el .-parentElement)
        k (.-key ev)]
    (when (#{"Escape" "ArrowUp" "ArrowDown"} k)
      (.preventDefault ev)
      (.stopImmediatePropagation ev)
      (case k
        "Escape" (reset! waehlen nil)
        "ArrowUp" (when (in-app? oben)
                    (swap! waehlen assoc :el oben :unten (conj unten el)))
        "ArrowDown" (when (seq unten)
                      (swap! waehlen assoc :el (peek unten) :unten (pop unten)))))))

(defn- bei-rollen [_] (swap! waehlen update :takt (fnil inc 0)))

(def ^:private hoerer
  [["mousemove" bei-zeigen] ["pointerdown" bei-druck] ["mousedown" bei-druck]
   ["pointerup" bei-druck] ["mouseup" bei-druck] ["click" bei-wahl]
   ["keydown" bei-taste-waehlen] ["scroll" bei-rollen]])

(defn- waehlen-folgen! [_ _ alt neu]
  (when (not= (some? alt) (some? neu))
    (let [f (if neu #(.addEventListener js/window %1 %2 true)
                #(.removeEventListener js/window %1 %2 true))]
      (doseq [[art h] hoerer] (f art h)))
    (.. js/document -documentElement -classList (toggle "feedback-waehlt" (some? neu)))))

(defn- referenzen [bezug rs]
  (when (seq rs)
    [:ul.feedback__referenzen
     (for [{:keys [nr ziel] :as r} rs]
       ^{:key nr}
       [:li.feedback__referenz {:title (b/referenz-wort r)}
        [:b (str "/" nr)] [:span (b/ziel-wort ziel)]
        [ui/button
         {:aria-label (i18n/t :ui/feedback-referenz-entfernen)
          :minimal    true
          :small      true
          :icon       :cross
          :on-click   #(swap! z update-in [:entwurf bezug :referenzen]
                              (fn [rs] (filterv (fn [x] (not= nr (:nr x))) rs)))}]])]))

(defn- eingabe [bezug]
  (let [{:keys [text] rs :referenzen} (get-in @z [:entwurf bezug])
        setzen #(swap! z assoc-in [:entwurf bezug %1] %2)
        bereit? (not (str/blank? text))
        waehlt? (and (some? @waehlen) (= bezug (:bezug @waehlen)))]
    [:div.feedback__eingabe
     [ui/text-area {:rows 2 :value (or text "") :fill true
                    :placeholder (i18n/t :ui/feedback-platzhalter)
                    :on-change #(setzen :text (.. % -target -value))
                    :on-key-down #(when (and bereit? (= "Enter" (.-key %))
                                             (or (.-ctrlKey %) (.-metaKey %)))
                                    (melden! bezug))}]
     [referenzen bezug rs]
     [:div.feedback__knoepfe
      [ui/button {:intent :primary :disabled (not bereit?) :on-click #(melden! bezug)}
       (i18n/t :ui/feedback-senden)]
      [ui/tooltip {:content (i18n/t :ui/feedback-referenzieren-hinweis)}
       [ui/button {:active       waehlt?
                   :outlined     waehlt?
                   :intent       (when waehlt? :primary)
                   :aria-pressed waehlt?
                   :disabled     (and (not waehlt?) (>= (count rs) REFERENZEN-MAX))
                   :on-click     #(reset! waehlen
                                          (when-not waehlt? {:bezug bezug :unten []}))}
        (i18n/t (if waehlt? :ui/feedback-referenzieren-abbrechen :ui/feedback-referenzieren))]]]
     (when waehlt? [:p.feedback__hinweis (i18n/t :ui/feedback-referenzieren-anleitung)])]))

(defn- markierung
  []
  (let [{:keys [el]} @waehlen]
    (when (and el (.-isConnected el))
      (let [r (.getBoundingClientRect el)
            breit (min 480 (- (.-innerWidth js/window) 16))
            links (- (min (.-left r) (- (.-innerWidth js/window) breit 8)) (.-left r))]
        [:div.feedback-markierung {:aria-hidden true
                                   :style {:left (.-left r) :top (.-top r)
                                           :width (.-width r) :height (.-height r)}}
         [:span {:class (when (< (.-top r) 20) "is-unten")
                 :style {:left (- links 2) :max-width breit}}
          (let [{:keys [knoten selektor]} (aufzeichnung/referenz el 0)]
            (if (:pfad knoten) (b/knoten-wort knoten) selektor))]]))))

(defn- karte [e]
  (when-let [[fx fy] (b/relativ e)]
    [:div.feedback__karte {:aria-hidden true}
     [:span.feedback__punkt {:style {:left (str (* 100 fx) "%")
                                     :top (str (* 100 fy) "%")}}]]))

(defn- schritt [e]
  (r/with-let [offen? (r/atom false)]
    [:li.feedback__schritt {:class (when (b/fehler? e) "is-fehler")}
     [:div.feedback__kopf
      [karte e]
      [:div.feedback__satz
       [:span.feedback__zeit (uhr (:t e))]
       (if (b/meldung? e) [:em (:satz e)] (:satz e))]
      (when-not (b/meldung? e)
        [ui/button {:small true :on-click #(swap! offen? not) :aria-expanded @offen?}
         (i18n/t :ui/feedback-melden)])]
     (for [[i z] (map-indexed vector (:herkunft e))]
       ^{:key i} [:div.feedback__stand z])
     (for [m (:meldungen e)]
       ^{:key (:id m)} [:div.feedback__meldung (:satz m)])
     (when-let [st (and (b/meldung? e) (:stand e))]
       [:div.feedback__stand st])
     (when @offen? [eingabe (:id e)])]))

(defn- laufend []
  (r/with-let [alle? (r/atom false)]
    (let [ereignisse @aufzeichnung/ereignisse
          schritte (rseq (b/verlauf (vals ereignisse)))
          gezeigt (if @alle? schritte (take SICHTBAR schritte))]
      [:<>
       [:section.feedback__frei
        [:h2 (i18n/t :ui/feedback-frei)]
        (when-let [st (b/stand (letzter-ort ereignisse))]
          [:p.feedback__stand (i18n/t :ui/feedback-gerade-offen {:stand st})])
        [eingabe nil]]
       (if (empty? schritte)
         [:p.feedback__leer (i18n/t :ui/feedback-schritte-leer)]
         [:ol.feedback__schritte
          (for [e gezeigt] ^{:key (:id e)} [schritt e])])
       (when (and (not @alle?) (> (count schritte) SICHTBAR))
         [ui/button {:minimal true :on-click #(reset! alle? true)}
          (i18n/t :ui/feedback-aeltere {:n (- (count schritte) SICHTBAR)})])])))

(defonce ^:private kopiert? (db/cursor [:shared.feedback :kopiert?] false))

(db/reg :shared.feedback/kopieren
        (fn [_ [_ text]]
          {:fx [[:call {:call [#(.writeText js/navigator.clipboard %) text]
                        :ok [:shared.feedback/kopiert true] :err [:sutra/ignored]}]]}))

(db/reg :shared.feedback/kopiert
        (fn [s [_ ja?]]
          {:db (assoc-in s [:shared.feedback :kopiert?] ja?)
           :fx [(when ja? [:after {:key ::kopiert :ms 2000
                                   :event [:shared.feedback/kopiert false]}])]}))

(defn- adresse
  [id]
  (str (.-origin js/location) "/feedback/" (js/encodeURIComponent id)))

(defn- kennung
  []
  (when-let [id (:id @aufzeichnung/aktiv)]
    [:div.feedback__kennung
     [:span (i18n/t :ui/feedback-kennung)]
     [ui/tooltip {:content (i18n/t :ui/feedback-kennung-hinweis)}
      [:code (adresse id)]]
     [ui/button
      {:small    true
       :icon     :duplicate
       :on-click #(db/send! [:shared.feedback/kopieren (adresse id)])}
      (i18n/t (if @kopiert? :ui/feedback-kopiert :ui/feedback-kopieren))]]))

(defn- spalte []
  (let [fehler @aufzeichnung/fehler]
    (if @zu?
      [ui/tooltip {:content (str (i18n/t :ui/feedback-ausklappen) " — "
               (i18n/t :ui/feedback-kennung) " " (:id @aufzeichnung/aktiv))}
       [:button.feedback-streifen
        {:on-click #(reset! zu? false)}
        [:span.feedback__punkt-laeuft] (i18n/t :ui/user-menu-feedback)]]
      [:aside.feedback {:aria-label (i18n/t :ui/feedback-titel)}
       [:header.feedback__titel
        [:h1 [:span.feedback__punkt-laeuft] (i18n/t :ui/feedback-titel)]
        [ui/tooltip {:content (i18n/t :ui/feedback-reset-hinweis)}
         [ui/button {:small true :minimal true :on-click aufzeichnung/neu!}
          (i18n/t :ui/feedback-reset)]]
        [ui/tooltip {:content (i18n/t :ui/feedback-einklappen)}
         [ui/button {:small true :minimal true :icon :double-chevron-right
                     :on-click #(reset! zu? true)
                     :aria-label (i18n/t :ui/feedback-einklappen)}]]
        [ui/tooltip {:content (i18n/t :ui/feedback-schliessen-hinweis)}
         [ui/button {:small true :minimal true :icon :cross
                     :on-click aufzeichnung/schliessen!
                     :aria-label (i18n/t :ui/feedback-schliessen)}]]]
       [kennung]
       (when fehler [:p.feedback__fehler {:role "alert"} fehler])
       [laufend]
       [markierung]])))

(defonce ^:private wurzel (atom nil))

(defn- layout!
  [aktiv]
  (let [cl (.. js/document -documentElement -classList)]
    (.toggle cl "mit-feedback" (some? aktiv))
    (.toggle cl "feedback-zu" (boolean @zu?))))

(defn- einhaengen! []
  (when-not @wurzel
    (let [el (doto (.createElement js/document "div")
               (.setAttribute "id" "feedback-spalte")
               (.setAttribute "data-feedback" "true"))
          root (rdomc/create-root el)]
      (.appendChild (.-body js/document) el)
      (rdomc/render root [spalte])
      (reset! wurzel [el root]))))

(defn- aushaengen! []
  (when-let [[el root] @wurzel]
    (rdomc/unmount root)
    (.remove el)
    (reset! wurzel nil)))

(defn- folgen! [_ _ alt neu]
  (when (not= (:id alt) (:id neu))
    (reset! z (leer))
    (reset! waehlen nil)
    (reset! zu? false))
  (if neu (einhaengen!) (aushaengen!))
  (layout! neu))

(defn install!
  []
  (add-watch aufzeichnung/aktiv ::spalte folgen!)
  (add-watch zu? ::layout (fn [& _] (layout! @aufzeichnung/aktiv)))
  (add-watch waehlen ::waehlen waehlen-folgen!)
  (folgen! nil nil nil @aufzeichnung/aktiv))

(defn feedback!
  []
  (if (aufzeichnung/aktiv?)
    (reset! zu? false)
    (aufzeichnung/starten!)))
