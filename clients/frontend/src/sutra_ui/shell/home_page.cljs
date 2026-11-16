(ns sutra-ui.shell.home-page
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.storage :as storage]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.shell.unavailable :refer [unavailable]]
            [clojure.string :as str]
            [sutra.kit.remote :as rd]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.auth :as auth]))

(def ^:private last-key "sutra.last-workspace")

(defn- remembered [] (storage/get-item last-key))

(defn- remember! [slug] (storage/set-item! last-key slug))

(defn- path [] (str/replace (str (.-pathname js/location)) #"/+$" ""))

(defn- root? [] (contains? #{"" "/index.html"} (path)))

(defn- bare-workspace? [] (= "/workspace" (path)))

(defonce choosing? (r/atom false))

(defn- go!
  [url]
  (let [q (.-search js/location)]
    (.replaceState js/history nil "" (if (str/includes? url "?") url (str url q)))))

(defn- to-workspace!
  [home]
  (if-let [p (get-in home [:default-workspace :path])]
    (go! p)
    (-> (pstate/ensure-workspaces!)
        (.then (fn [_]
                 (if-let [ws (routing/workspace-of-slug (remembered) (pstate/workspace-list))]
                   (go! (routing/workspace-path ws nil))
                   (reset! choosing? true)))))))

(defn land!
  []
  (if-not (or (root?) (bare-workspace?))
    (js/Promise.resolve nil)
    (-> (api/get! :my-home)
        (.then (fn [{:keys [url] :as home}]
                 (when (and (root?) (seq url) (not= "/" url)) (go! url))
                 (when (bare-workspace?) (to-workspace! home))))
        (.catch (fn [_] nil)))))

(defn landing-first [f] (fn [] (.then (land!) (fn [_] (f)))))

(defn install!
  []
  (add-watch auth/current-workspace ::last-visited
             (fn [_ _ _ ws]
               (when ws
                 (reset! choosing? false)
                 (remember! (routing/workspace-slug ws))))))

(defn chooser
  []
  (pstate/ensure-workspaces!)
  (fn []
    (let [wss      @pstate/workspaces
          promoted (filter :promoted (pstate/workspace-list))]
      [:div {:class "ws-home"}
       [:div {:class "ws-home__head"}
        [:h1 {:class "ws-home__name"} (i18n/t :ui/ws-choose)]
        [:div {:class "ws-home__subtitle"} (i18n/t :ui/ws-choose-hint)]]
       (cond
         (rd/loading? wss) [busy/block {:class "loading"} (i18n/t :ui/loading)]
         (rd/failed? wss)  [unavailable (i18n/t :ui/ws-load-failed {:reason (rd/reason wss)})]
         (empty? promoted)
         [ui/card {:class "ws-panel"}
          [:div {:class "ws-panel__desc"} (i18n/t :ui/ws-choose-none)]
          [:div {:class "ws-row" :on-click #(do (reset! choosing? false) (go! "/"))}
           (i18n/t :ui/ws-choose-skip)]]
         :else
         [ui/card {:class "ws-panel"}
          [:div {:class "ws-panel__cards"}
           (for [w promoted]
             ^{:key (str (:id w))}
             [:div {:class "ws-card" :on-click #(pstate/enter-workspace! w)}
              [:span {:class "ws-item__icon"} (or (:icon w) "🧭")]
              [:span {:class "ws-item__label"} (or (:display-name w) (some-> (:name w) name))]
              (when-let [d (or (:subtitle w) (:description w))]
                [:span {:class "ws-item__desc"} d])])]])])))
