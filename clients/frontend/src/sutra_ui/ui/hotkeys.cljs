(ns sutra-ui.ui.hotkeys
  (:require [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]))

(def ^:private open-overlay (str "." (ui/cls "overlay") "." (ui/cls "overlay-open")))

(defn- overlay-owns? [h ^js e]
  (or (.-defaultPrevented e)
      (and (not (:in-overlay h)) (some-> (.-target e) (.closest open-overlay)))
      (and (= "Escape" (.-key e)) (.querySelector js/document open-overlay))))

(defn- page-hotkey [h]
  (cond-> (assoc h :global true)
    (:on-key-down h)
    (update :on-key-down (fn [f] (fn [e] (when-not (overlay-owns? h e) (f e)))))))

(defonce ^:private registry (r/atom {}))
(defonce ^:private ids (atom 0))

(defn register!
  [hotkeys]
  (let [id (swap! ids inc)]
    (swap! registry assoc id (mapv page-hotkey hotkeys))
    id))

(defn unregister! [id] (swap! registry dissoc id))

(def canvas-keys
  [[:cancel     ["esc"]                 :ui/hotkeys-cancel]
   [:confirm    ["enter"]               :ui/hotkeys-confirm]
   [:delete     ["delete" "backspace"]  :ui/hotkeys-delete]
   [:undo       ["mod+z"]               :ui/hotkeys-undo]
   [:redo       ["mod+shift+z" "mod+y"] :ui/hotkeys-redo]
   [:select-all ["mod+a"]               :ui/hotkeys-select-all]
   [:save       ["mod+s"]               :ui/hotkeys-save]
   [:zoom-in    ["plus" "="]            :ui/hotkeys-zoom-in]
   [:zoom-out   ["-"]                   :ui/hotkeys-zoom-out]
   [:fit        ["0"]                   :ui/hotkeys-fit]
   [:nudge      ["left" "right" "up" "down"
                 "shift+left" "shift+right" "shift+up" "shift+down"]
    :ui/hotkeys-nudge]])

(defn- elsewhere?
  [el ^js e]
  (when-let [o (some-> (.-target e) (.closest open-overlay))]
    (not (and el (.contains o el)))))

(defn canvas
  [el actions]
  (for [[a combos label] canvas-keys
        :let  [f (get actions a)]
        :when f
        combo combos]
    {:combo       combo
     :label       [i18n/t label]
     :group       [i18n/t :ui/hotkeys-group-canvas]
     :in-overlay  true
     :on-key-down (fn [e] (when (and (not (elsewhere? (el) e)) (f e)) (.preventDefault e)))}))

(def nudge-step
  {"ArrowLeft" [-1 0] "ArrowRight" [1 0] "ArrowUp" [0 -1] "ArrowDown" [0 1]})

(defn nudge-of [^js e]
  (when-let [[dx dy] (nudge-step (.-key e))]
    (let [f (if (.-shiftKey e) 10 1)] [(* f dx) (* f dy)])))

(defonce ^:private claim (atom nil))

(defn claim!
  [owner hotkeys]
  (let [{o :owner id :id} @claim]
    (when-not (identical? o owner)
      (some-> id unregister!)
      (reset! claim {:owner owner :id (register! hotkeys)}))))

(defn offer!
  [owner hotkeys]
  (when-not @claim (claim! owner hotkeys)))

(defn release! [owner]
  (let [{o :owner id :id} @claim]
    (when (identical? o owner)
      (unregister! id)
      (reset! claim nil))))

(defn global
  [_props]
  (let [id (atom nil)]
    (r/create-class
     {:display-name "sutra-hotkeys-global"
      :component-did-mount
      (fn [this] (reset! id (register! (:hotkeys (second (r/argv this))))))
      :component-did-update
      (fn [this _]
        (swap! registry assoc @id (mapv page-hotkey (:hotkeys (second (r/argv this))))))
      :component-will-unmount (fn [_] (unregister! @id))
      :reagent-render (fn [_] nil)})))

(defn root []
  [ui/hotkeys-target {:hotkeys   (into [] cat (vals @registry))
                      :tag-name  :span
                      :tab-index -1
                      :hidden    true}])
