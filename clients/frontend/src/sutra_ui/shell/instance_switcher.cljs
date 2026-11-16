(ns sutra-ui.shell.instance-switcher
  (:require [sutra-ui.state.shell :as shell]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.ui.core :as ui]))

(def ^:private last-key "sutra.last-instance")

(defn- last-instance []
  (try (.getItem js/localStorage last-key) (catch :default _ nil)))

(defn- remember! [nm]
  (try (.setItem js/localStorage last-key nm) (catch :default _ nil)))

(defonce ^:private listed (r/atom nil))

(defn- load! []
  (when (nil? @listed)
    (reset! listed [])
    (-> (api/get! :federation-instances)
        (.then (fn [{:keys [instances]}] (reset! listed (vec instances))))
        (.catch (fn [_] (reset! listed []))))))

(defn- switch! [nm]
  (-> (api/post! :federation-assert {:to nm})
      (.then (fn [{:keys [assertion url]}]
               (remember! nm)
               (set! (.-location js/window)
                     (str url "/#federated=" (js/encodeURIComponent assertion)))))
      (.catch (fn [e]
                (shell/fail! (i18n/t :ui/user-menu-instance-failed
                                     {:name nm
                                      :error (or (:error (:body (ex-data e))) (ex-message e))}))))))

(defn items
  []
  (load!)
  (let [is   @listed
        last (last-instance)]
    (when (seq is)
      (into [:<> [ui/menu-divider {:title (i18n/t :ui/user-menu-instances)}]]
            (for [{nm :name url :url} (sort-by #(if (= (:name %) last) "" (:name %)) is)]
              ^{:key nm}
              [ui/menu-item {:text     nm
                             :icon     :globe-network
                             :label    (if (= nm last)
                                         (i18n/t :ui/user-menu-instance-last)
                                         url)
                             :on-click #(switch! nm)}])))))
