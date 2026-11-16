(ns sutra-ui.apps.admin-settings.peering
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.table :as table]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.apps.admin-settings.model :as model]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.style :as style]))

(defn- send! [& ev] (model/send! (vec ev)))

(defn- decke [{:keys [classification markings]}]
  (str/join ", " (cond-> (vec markings)
                   classification (conj (str (:name classification) " ("
                                             (:category classification) " ≤ "
                                             (:rank classification) ")")))))

(defn- datenarten [dts]
  (str/join " · " (for [[dt dir] dts :when (not= "none" (name dir))]
                    (str (name dt) " " (name dir)))))

(def ^:private zustand-intent {"healthy" :success "failing" :danger "idle" :none})

(defn- jobs [{:keys [connection-id]}]
  (let [js (get-in @model/db [:peering-jobs connection-id])]
    (when (seq (:jobs js))
      [table/data-table
       {:rows    (take 20 (:jobs js))
        :row-key :job-id
        :class   "u-mt-8"
        :cols
        [{:label (i18n/t :ui/peer-job-at) :render #(str (:at %))}
         {:label  (i18n/t :ui/peer-job-kind)
          :render #(str (name (:data-type %)) " " (if (= "in" (name (:way %))) "←" "→"))}
         {:label (i18n/t :ui/peer-job-type) :render #(str (:local-type %))}
         {:label  (i18n/t :ui/peer-job-items)
          :render (fn [{:keys [items held superseded]}]
                    (str items (when (pos? held) (str " · " held " " (i18n/t :ui/peer-held)))
                         (when (pos? superseded)
                           (str " · " superseded " " (i18n/t :ui/peer-superseded)))))}
         {:label  (i18n/t :ui/peer-job-latency)
          :render #(if-let [ms (:latency-ms %)] (str ms " ms") "—")}
         {:label  (i18n/t :ui/peer-job-state)
          :render #(vector :span {:title (:error %)} (name (:state %)))}]}])))

(defn- latenz
  [{:keys [connection-id]}]
  (when-let [ms (some #(when (and (= "out" (name (:way %))) (:latency-ms %)) (:latency-ms %))
                      (:jobs (get-in @model/db [:peering-jobs connection-id])))]
    [:div {:class style/small-muted} (i18n/t :ui/peer-latency {:ms ms})]))

(defn- verbindung [{:keys [connection-id name trust space remote-space status flowing role
                           data-types ceiling pending-ceiling disabled-local disabled-remote
                           health mappings]
                    :as c}]
  [ui/callout {:class "u-mb-12" :intent (zustand-intent (some-> health clojure.core/name))}
   [:div {:class "u-flex-items-gap-8"}
    [:div {:class "u-flex-1"}
     [:div {:class "u-bold"} name " · " trust " · " (clojure.core/name status)
      (when-not flowing (str " · " (i18n/t :ui/peer-not-flowing)))]
     [:div {:class style/small-muted}
      (i18n/t :ui/peer-spaces {:hier (or space "—") :dort (or remote-space "—")})
      " · " (datenarten data-types) " · " (i18n/t :ui/peer-mappings {:n mappings})]
     [:div {:class style/small-muted} (i18n/t :ui/peer-ceiling) ": " (decke ceiling)]
     (when pending-ceiling
       [:div {:class style/small-muted}
        (i18n/t (if (= "remote" (clojure.core/name (:proposed-by pending-ceiling)))
                  :ui/peer-ceiling-theirs :ui/peer-ceiling-mine))
        ": " (decke (:ceiling pending-ceiling))])
     (when (or disabled-local disabled-remote)
       [:div {:class style/small-muted}
        (i18n/t (if disabled-local :ui/peer-disabled-here :ui/peer-disabled-there))])
     [latenz c]
     [jobs c]]
    [:div {:class "u-flex-col-gap-6"}
     (when (and (= "originator" (clojure.core/name role)) (= "accepted" (clojure.core/name status)))
       [ui/button {:intent :primary :on-click #(send! :admin.peering/act connection-id
                                                       :peering-activate {})}
        (i18n/t :ui/peer-activate)])
     (when (and pending-ceiling (= "remote" (clojure.core/name (:proposed-by pending-ceiling))))
       [ui/button {:intent :primary :on-click #(send! :admin.peering/act connection-id
                                                       :peering-ceiling {:approve true})}
        (i18n/t :ui/peer-ceiling-approve)])
     [ui/button {:on-click #(send! :admin.peering/act connection-id :peering-status
                                   {:disabled (not disabled-local)})}
      (i18n/t (if disabled-local :ui/peer-enable :ui/peer-disable))]
     [ui/button {:minimal true :on-click #(send! :admin.peering/jobs connection-id)}
      (i18n/t :ui/peer-show-jobs)]]]])

(def view
  (machine/entering
   model/admin [:admin.peering/load]
   (fn []
     (let [{rdata :data error :error} (:peering @model/db)
           data  (rd/value-or rdata nil)
           error (or error (rd/reason rdata))]
       [:div
        [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/peer-intro)]
        (when error [ui/problem error nil "u-mb-12"])
        (if (seq (:connections data))
          (for [c (:connections data)] ^{:key (:connection-id c)} [verbindung c])
          [:div {:class style/small-muted} (i18n/t :ui/peer-none)])]))))
