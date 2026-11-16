(ns sutra-ui.ui.theme
  (:require [sutra-ui.storage :as storage]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]))

(def ^:private storage-key "sutra.theme")

(defn- stored []
  (some-> (storage/get-item storage-key) keyword))

(defonce choice (r/atom (or (#{:light :dark} (stored)) :system)))

(defonce ^:private query (delay (.matchMedia js/window "(prefers-color-scheme: dark)")))

(defn dark? []
  (case @choice :dark true :light false (.-matches @query)))

(defn- apply! [& _]
  (.. js/document -documentElement -classList (toggle "bp-dark" (dark?))))

(defn choose!
  [c]
  (reset! choice c)
  (storage/set-item! storage-key (when-not (= :system c) (name c)))
  (apply!))

(defn install! []
  (.addEventListener @query "change" apply!)
  (apply!))

(defn menu-item
  []
  (into [ui/menu-item {:text (i18n/t :ui/theme)}]
        (for [[c k] [[:system :ui/theme-system] [:light :ui/theme-light]
                     [:dark :ui/theme-dark]]]
          [ui/menu-item {:text     (i18n/t k)
                         :active   (= c @choice)
                         :on-click #(choose! c)}])))
