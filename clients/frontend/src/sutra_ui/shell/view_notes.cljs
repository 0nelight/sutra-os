(ns sutra-ui.shell.view-notes
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.feature :as feature]
            [sutra-ui.shell.view-notes.trail :as trail]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]))

(def ^:private doc-name "View-Notizen")

(def ^:private path [:shared.view-notes])

(defonce ^:private ui (db/cursor path {:open? false :text "" :state :idle :error nil}))

(defn- two [n] (if (< n 10) (str "0" n) (str n)))

(defn- jetzt []
  (let [d (js/Date.)]
    (str (.getFullYear d) "-" (two (inc (.getMonth d))) "-" (two (.getDate d))
         " " (two (.getHours d)) ":" (two (.getMinutes d)))))

(defn- ort
  []
  (let [ui  @shell/ui-state
        nav (feature/current-nav ui)]
    (or (feature/subject ui nav)
        (some->> nav name (str "Bereich "))
        "Sutra")))

(defn- pfad []
  (str (.. js/window -location -pathname) (.. js/window -location -search)))

(defn- geraet
  []
  (let [ua (or (.. js/navigator -userAgent) "")
        hat? (fn [s] (str/includes? ua s))]
    (cond
      (hat? "iPhone")  "iPhone"
      (hat? "iPad")    "iPad"
      (hat? "Android") "Android"
      (hat? "Macintosh") "Mac"
      (hat? "Windows") "Windows"
      (hat? "Linux")   "Linux"
      :else            "unbekannt")))

(defn- womit
  []
  (let [w   (.-innerWidth js/window)
        h   (.-innerHeight js/window)
        dpr (or (.-devicePixelRatio js/window) 1)
        tip (if (pos? (or (.-maxTouchPoints js/navigator) 0)) "Finger" "Maus")]
    (str (or (:username @auth/auth) (:user @auth/auth) "—")
         " · " (geraet) " · " w "×" h " px"
         (when (not= 1 dpr) (str " @" dpr "x"))
         " · " (if (< w h) "hochkant" "quer")
         " · " tip)))

(defn- eintrag [text]
  (str "\n\n## " (jetzt) " · " (ort) "\n\n`" (pfad) "`\n\n"
       (womit) "\n\n"
       (str/trim text) "\n"
       (when-let [w (trail/als-markdown)]
         (str "\n**Weg bis hierher**\n\n" w "\n"))
       "\n<!-- " (.. js/navigator -userAgent) " -->\n"))

(defn- call [req ok]
  (rq/ask {:ask req :then [ok] :error (conj path :error)
           :else [[:rf/set (conj path :state) :error]]}))

(db/reg :shared.view-notes/set (fn [s [_ m]] {:db (update-in s path merge m)}))

(db/reg :shared.view-notes/save
        (fn [s [_ eintrag]]
          {:db (update-in s path assoc :state :saving :error nil)
           :fx [(call [api/documents] [:shared.view-notes/found eintrag])]}))

(db/reg :shared.view-notes/found
        (fn [_ [_ eintrag r]]
          {:fx [(if-let [d (some #(when (= doc-name (:name %)) %) (:documents r))]
                  [:dispatch [:shared.view-notes/read eintrag d]]
                  (call [api/post! :documents {:name doc-name :format :markdown
                                              :body (str "# " doc-name "\n")}]
                        [:shared.view-notes/created eintrag]))]}))

(db/reg :shared.view-notes/created
        (fn [_ [_ eintrag r]] {:fx [[:dispatch [:shared.view-notes/read eintrag (:document r)]]]}))

(db/reg :shared.view-notes/read
        (fn [_ [_ eintrag d]]
          {:fx [(call [api/get! :document-of {:id (str (:id d))}]
                      [:shared.view-notes/append eintrag])]}))

(db/reg :shared.view-notes/append
        (fn [_ [_ eintrag d]]
          {:fx [(call [api/put! :document-of {:id (str (:id d))}
                       {:name           (:name d)
                        :format         (or (:format d) :markdown)
                        :body           (str (:body d) eintrag)
                        :expect-version (:version d)}]
                      [:rf/set path {:open? false :text "" :state :saved :error nil}])]}))

(defn- setzen! [m] (db/send! [:shared.view-notes/set m]))

(defn- speichern! []
  (let [text (:text @ui)]
    (when-not (str/blank? text) (db/send! [:shared.view-notes/save (eintrag text)]))))

(defn- sheet []
  (let [{:keys [text state error]} @ui
        busy? (= :saving state)]
    [:div {:class "viewnote__sheet"}
     [:div {:class "viewnote__head"}
      [:div {:class "viewnote__title"} (i18n/t :ui/viewnote-title)]
      [ui/tooltip {:content (i18n/t :ui/close)}
       [ui/button {:class      "viewnote__close"
                   :minimal    true
                   :small      true
                   :icon       :cross
                   :aria-label (i18n/t :ui/close)
                   :on-click   #(setzen! {:open? false :error nil})}]]]
     [:div {:class "viewnote__where"} (ort)]
     [:div {:class "viewnote__where"} (womit)]
     (let [n (count (trail/weg))]
       (when (pos? n)
         [:div {:class "viewnote__where"}
          (i18n/t (if (= 1 n) :ui/viewnote-trail-one :ui/viewnote-trail) {:n n})]))
     [ui/text-area {:large       true
                    :fill        true
                    :rows        5
                    :auto-focus  true
                    :placeholder (i18n/t :ui/viewnote-placeholder)
                    :value       text
                    :disabled    busy?
                    :on-change   #(setzen! {:text (.. % -target -value)})}]
     (when error [:div {:class "viewnote__error"} error])
     [:div {:class "viewnote__actions"}
      [ui/button {:class    "viewnote__save"
                  :intent   :primary
                  :loading  busy?
                  :disabled (str/blank? text)
                  :on-click speichern!}
       (i18n/t :ui/viewnote-save)]]]))

(defn fab
  []
  (when (= :authed (:status @auth/auth))
    (if (:open? @ui)
      [sheet]
      [ui/tooltip {:content (i18n/t :ui/viewnote-title)}
       [ui/button {:class    "viewnote-fab"
                   :on-click #(setzen! {:open? true :state :idle :error nil})}
        [:span.fab__icon "✎"] [:span.fab__label (i18n/t :ui/viewnote-open)]]])))
