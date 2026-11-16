(ns sutra-ui.platform
  (:require [sutra-ui.storage :as storage]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [reagent.core :as r]
            [sutra-ui.api :as api]))

(def sizes [:favicon :small :medium :large])

(defonce state (r/atom {:title nil :logo {} :uploaded []}))

(defn title [] (or (:title @state) i18n/default-platform))

(defn logo
  [size]
  (get-in @state [:logo size]))

(defn badge
  []
  (if-let [src (logo :small)]
    [:img {:class "shell-bar__logo-image" :src src :alt (title)}]
    [:div {:class "shell-bar__logo-mark" :style {:background "var(--c-primary)"}}
     (str/upper-case (subs (title) 0 1))]))

(defn wordmark []
  [:span {:class "shell-bar__logo-text"} (title)])

(defn portal-brand
  []
  [:div {:class "shell-bar__portal-brand"}
   (when-let [src (logo :medium)]
     [:img {:class "shell-bar__portal-logo" :src src :alt (title)}])
   [:span (title)]])

(defn gate-logo
  []
  (when-let [src (logo :large)]
    [:img {:class "login__logo" :src src :alt (title)}]))

(defn- set-favicon! [href]
  (when href
    (let [doc  js/document
          link (or (.querySelector doc "link[rel='icon']")
                   (let [l (.createElement doc "link")]
                     (set! (.-rel l) "icon")
                     (.appendChild (.-head doc) l)
                     l))]
      (set! (.-href link) href))))

(defn- apply! [pe]
  (reset! state pe)
  (i18n/set-platform-title! (:title pe))
  (set! (.-title js/document) (title))
  (storage/set-item! "sutra.platform-title" (title))
  (set-favicon! (get-in pe [:logo :favicon]))
  pe)

(defn refresh! []
  (-> (api/get! :platform-experience)
      (.then apply!)
      (.catch (fn [_] @state))))

(defn save! [m]
  (-> (api/post! :platform-experience-set m)
      (.then apply!)))
