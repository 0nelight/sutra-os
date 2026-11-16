(ns sutra-ui.shell.desktop-nav
  (:require [reagent.core :as r]
            [sutra-ui.ui.hotkeys :as hotkeys]
            [sutra.i18n.core :as i18n]))

(defn desktop? []
  (some? (.-sutraDesktop js/window)))

(def can-go-back? (r/atom false))

(defn- sync! []
  (reset! can-go-back? (boolean (some-> (.-navigation js/window) .-canGoBack))))

(defn back! [] (.back js/history))

(defn- forward! [] (.forward js/history))

(def ^:private nav-keys
  [{:combo "alt+left" :label [i18n/t :ui/hotkeys-back] :prevent-default true
    :on-key-down (fn [_] (back!))}
   {:combo "alt+right" :label [i18n/t :ui/hotkeys-forward] :prevent-default true
    :on-key-down (fn [_] (forward!))}])

(defn- on-mouse [^js e]
  (case (.-button e)
    3 (do (.preventDefault e) (back!))
    4 (do (.preventDefault e) (forward!))
    nil))

(defonce ^:private installed? (atom false))

(defn install! []
  (when (and (desktop?) (not @installed?))
    (reset! installed? true)
    (sync!)
    (some-> (.-navigation js/window) (.addEventListener "currententrychange" sync!))
    (hotkeys/register! nav-keys)
    (.addEventListener js/window "mouseup" on-mouse)))
