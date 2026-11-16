(ns sutra-ui.apps.notebook.state
  (:require [reagent.core :as r]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]))

(def autosave-ms 1500)

(defonce ui (shell/slice :notebook))

(defonce notebook (machine/frame ui {:name :notebook}))

(defn send! [event] (machine/dispatch event {:frame notebook}))

(def defaults
  {:docs [] :templates [] :save-state :saved :layout-mode :view :left-tab :outline
   :v-filter :named :busy false :search "" :tpl-filter "" :proj-filter "" :reload 0
   :link-rows {} :pick-rows {} :diagram-rows {} :save-queue []})

(defn at
  [db k]
  (let [v (get db k)] (if (nil? v) (get defaults k) v)))

(machine/reg-sub :notebook/at (fn [db [_ k]] (at db k)))

(defn- sub [k] (machine/subscribe [:notebook/at k] {:frame notebook}))

(def docs (sub :docs))
(def templates (sub :templates))
(def open-doc (sub :open-doc))
(def save-state (sub :save-state))
(def layout-mode (sub :layout-mode))
(def rendered (sub :rendered))
(def left-tab (sub :left-tab))
(def versions (sub :versions))
(def v-filter (sub :v-filter))
(def v-open (sub :v-open))
(def dialog (sub :dialog))
(def wdialog (sub :wdialog))
(def busy (sub :busy))
(def err (sub :err))
(def search (sub :search))
(def tpl-filter (sub :tpl-filter))
(def proj-filter (sub :proj-filter))
(def new-in (sub :new-in))
(def reload (sub :reload))
(def print-view (sub :print-view))
(def session (sub :session))
(def seed-pending (sub :seed-pending))
(def link-rows (sub :link-rows))
(def pick-rows (sub :pick-rows))
(def diagram-rows (sub :diagram-rows))
(def graphs (sub :graphs))

(defonce frame-el (atom nil))
(defonce frame-outline (r/atom []))
(defonce editor (r/atom nil))
(defonce tick (r/atom 0))
(defonce sel-widget (r/atom nil))
(defonce image-input (atom nil))

(defn chain! [f] (when-let [^js ed @editor] (-> (.chain ed) (.focus) f (.run))))
