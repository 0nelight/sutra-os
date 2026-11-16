(ns sutra-ui.shell.placement
  (:require [sutra-ui.ui.size :as size]))

(def tools
  {:notifications {:priority :primary}
   :feedback      {:priority :primary}
   :branch        {:priority :secondary}
   :account       {:priority :secondary}})

(def regions
  {[:builder :regular]   {:bar-end  [:branch :notifications :feedback :account]}
   [:builder :compact]   {:bar-end  [:notifications :feedback]
                          :overflow [:branch :account]}
   [:workspace :regular] {:bar-end  [:notifications :feedback :account]}
   [:workspace :compact] {:bar-home [:notifications :feedback]
                          :overflow [:account]}
   [:bare :compact]      {:back-row [:notifications :feedback]}
   [:portal :regular]    {:bar-end  [:notifications :feedback :account]}
   [:portal :compact]    {:bar-end  [:notifications :feedback :account]}})

(defn size-class []
  (if (size/kompakt?) :compact :regular))

(defn in
  [variant region]
  (let [s (size-class)
        v (if (and (= variant :bare) (= s :regular)) :workspace variant)]
    (get-in regions [[v s] region])))

(defn hidden-primaries
  []
  (for [[k rs] regions
        :let [seen (set (mapcat val (dissoc rs :overflow)))]
        [t {:keys [priority]}] tools
        :when (and (= priority :primary) (not (seen t)))]
    [k t]))
