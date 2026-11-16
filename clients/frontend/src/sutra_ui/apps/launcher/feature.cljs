(ns sutra-ui.apps.launcher.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.apphost.view-state :as view-state]
            [sutra-ui.apps.launcher.core :as core]))

(def ^:private glyph
  (feature/icon
   {:width 1.5 :join "round"}
   [:rect {:x 3 :y 3 :width 6 :height 6 :rx 1}]
   [:rect {:x 11 :y 3 :width 6 :height 6 :rx 1}]
   [:rect {:x 3 :y 11 :width 6 :height 6 :rx 1}]
   [:rect {:x 11 :y 11 :width 6 :height 6 :rx 1}]))

(feature/register-app!
 {:id :applications :order 1 :label-key :ui/app-applications :glyph glyph
  :mobile-tab :ui/mobile-tab-apps
  :branching? true :on-branch-change pstate/reload-consumer-apps!})

(feature/register-screen!
 {:id :applications :app :applications :segment routing/portal-segment
  :aliases [routing/portal-alias] :glyph glyph
  :view core/applications-launcher
  :tail (fn [_] (if-let [slug (routing/open-app-slug @pstate/current-consumer-app)]
                  (routing/app-tail slug @pstate/consumer-app-version)
                  []))
  :params #{:open-app-slug :app-version}
  :enter (fn [slice _] slice)
  :parse-tail routing/parse-app-tail
  :on-enter (fn [params] (core/send! [:launcher/enter params]))})

(defn- input-href
  [{nm :name} {:keys [external-id set?]} {:keys [id query]}]
  (let [v (if set? (vec (:ids query)) (str id))]
    (str "/" routing/portal-segment "/" (routing/enc (name nm))
         "?" (routing/enc (name external-id)) "=" (routing/enc (view-state/encode-value v)))))

(defn- takes?
  [{:keys [object-type set?]} value]
  (and (= (some-> object-type name) (some-> (:object-type value) name))
       (if set?
         (and (= #{:ids} (set (keys (:query value)))) (seq (get-in value [:query :ids])))
         (= :object (:kind value)))))

(defn discoverable-targets
  [workspaces current]
  (let [mods (if current
               (:discoverable current)
               (mapcat :discoverable (filter :promoted workspaces)))]
    (vec (for [app  (vals (into {} (map (juxt :id identity)) mods))
               in   (:inputs app)]
           {:id       [:app (:id app) (:external-id in)]
            :accepts  (if (:set? in) :object-set :object)
            :order    100
            :label    (or (:display-name app) (some-> (:name app) name))
            :accepts? #(takes? in %)
            :href     #(input-href app in %)}))))

(feature/contribute!
 :open-in-source :discoverable-apps
 {:targets (fn []
             (pstate/ensure-workspaces!)
             (discoverable-targets (pstate/workspace-list) @auth/current-workspace))})

