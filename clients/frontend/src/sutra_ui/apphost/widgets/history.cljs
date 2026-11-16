(ns sutra-ui.apphost.widgets.history
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.display :as display :refer [timeline]]
            [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [empty-state]]
            [sutra.capps.edits :as edits]
            [sutra.capps.leaf-props :refer [as-rows]]
            [sutra.capps.variables :refer [deref*]]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apphost.client :as client :refer [page-generation]]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(defn- object-of
  [{:keys [descriptor]} {:keys [object rows type]}]
  (let [row (first (as-rows (deref* rows)))]
    [(some-> (or (:id row) (deref* object)) str str/trim not-empty)
     (or (->kw (deref* type))
         (->kw (or (:object-type-ref row) (:object-type row)))
         (->kw (:object-type descriptor)))]))

(defn- reader [{:keys [descriptor]}] (or (:edit-history-fn descriptor) client/edit-history!))

(defn- no-object []
  [empty-state {:icon :history :title (i18n/t :ui/sv-history-no-object)
                :hint (i18n/t :ui/sv-history-no-object-hint)}])

(defn- nothing []
  [empty-state {:icon :history :title (i18n/t :ui/sv-history-empty)
                :hint (i18n/t :ui/sv-history-empty-hint)}])

(defn- failed [e] [empty-state {:icon :warning-sign :title (i18n/t :ui/sv-history-failed) :hint e}])

(defn- shown
  [res pipeline?]
  (vec (or (seq (:history res)) (when pipeline? (:provenance res)))))

(defn- holding!
  [f held slot]
  (when (not= slot @held)
    (some->> @held (vector :capp/drop-read) (frame/send! f))
    (reset! held slot)))

(defn- at-key [db slot key f]
  (when (= key (get-in db [:reads slot :key])) {:db (update-in db [:reads slot] f)}))

(machine/reg-event ::more
                   (fn [{:keys [db]} [_ slot key ask pipeline?]]
                     (rq/request db {:ask  ask
                                     :then [[::more-ok slot key pipeline?]]
                                     :else [[::more-err slot key]]})))

(machine/reg-event ::more-ok
                   (fn [{:keys [db]} [_ slot key pipeline? res]]
                     (at-key db slot key
                             (fn [s]
                               (update s :rd
                                       #(assoc % :history (into (shown % pipeline?)
                                                                (:history res))
                                               :next-page-token (:next-page-token res)
                                               :total (:total res)))))))

(machine/reg-event ::more-err
                   (fn [{:keys [db]} [_ slot key e]] (at-key db slot key #(assoc % :error e))))

(defn- edit-history-widget
  [env cfg]
  (r/with-let [f    (frame/frame-for env)
               held (atom nil)]
    (let [{:keys [rev sort props kinds editors limit include-pipeline?]} cfg
          [pk t] (object-of env cfg)
          names  (fn [xs] (when (seq xs) (mapv name xs)))
          spec   (cond-> {:type (some-> t name) :pk pk :limit limit}
                   sort              (assoc :sort (name (->kw sort)))
                   (seq props)       (assoc :props (names props))
                   (seq kinds)       (assoc :kinds (names kinds))
                   (seq editors)     (assoc :editors (names editors))
                   include-pipeline? (assoc :provenance true))
          slot   [::edits spec]
          key    [(deref* rev) (page-generation (:descriptor env))]
          ask    (fn [token]
                   [(reader env) (:descriptor env) (cond-> spec token (assoc :token token))])]
      (when (and t pk) (holding! f held slot))
      (if-not (and t pk)
        [no-object]
        (let [{:keys [data error]} (frame/read! env slot key (ask nil))
              found (when data (shown data include-pipeline?))
              more  (:next-page-token data)]
          (cond
            error          [failed error]
            (nil? found)   [busy/block {:class "capp-objectview__loading"}
                            (i18n/t :ui/sv-history-loading)]
            (empty? found) [nothing]
            :else
            [:div
             [timeline (edits/->timeline found {:props props})]
             (when more
               [:div {:style {:display "flex" :align-items "center" :gap "8px"
                              :margin-top "8px"}}
                [ui/button {:intent   :primary
                            :on-click #(frame/send! f [::more slot key (ask more)
                                                       include-pipeline?])}
                 (i18n/t :ui/sv-history-more)]
                [:span {:class "u-muted"}
                 (i18n/t :ui/sv-history-count {:shown (count found)
                                               :total (:total data)})]])]))))
    (finally (some->> @held (vector :capp/drop-read) (frame/send! f)))))

(defn- timeline-track
  [{:keys [label marks bars]}]
  [:div {:style {:display "flex" :align-items "center" :gap "8px" :margin-bottom "6px"}}
   [:div {:title label
          :style {:width "140px" :flex "none" :overflow "hidden"
                  :text-overflow "ellipsis" :white-space "nowrap"}}
    label]
   (into [:div {:style {:position "relative" :flex 1 :height "18px"
                        :border-bottom "1px solid var(--c-border)"}}]
         (concat
          (for [{:keys [left width title]} bars]
            [:div {:title title
                   :style {:position "absolute" :left (str left "%") :width (str width "%")
                           :top "4px" :height "8px" :border-radius "2px"
                           :background "var(--c-accent)" :opacity 0.5}}])
          (for [{:keys [left title tone]} marks]
            [:div {:title title
                   :style {:position "absolute" :left (str "calc(" left "% - 5px)")
                           :top "3px" :width "10px" :height "10px"
                           :transform "rotate(45deg)"
                           :background (case tone
                                         :danger "var(--c-danger)"
                                         :ok     "var(--c-success)"
                                         "var(--c-accent)")}}])))])

(defn- object-timeline-widget
  [env cfg]
  (r/with-let [f    (frame/frame-for env)
               held (atom nil)]
    (let [[pk t] (object-of env cfg)
          slot   [::timeline pk t]
          {:keys [data error]}
          (when (and t pk)
            (holding! f held slot)
            (frame/read! env slot
                         [pk t (deref* (:rev cfg)) (page-generation (:descriptor env))]
                         [(reader env) (:descriptor env)
                          {:type (name t) :pk pk :limit 1 :timeline true}]))
          found (when data (or (:timeline data) {}))
          view  (some-> found edits/object-timeline)
          {:keys [tokens runtime-ms runs coverage]} (:summary view)]
      (cond
        (not (and t pk)) [no-object]
        error            [failed error]
        (nil? found)     [busy/block {:class "capp-objectview__loading"}
                          (i18n/t :ui/sv-history-loading)]
        (nil? view)      [nothing]
        :else
        [:div
         [:div {:style {:color "var(--c-muted)" :margin-bottom "8px"}}
          (i18n/t :ui/sv-timeline-summary
                  {:edits (:edits (:summary view)) :runs runs :tokens tokens
                   :minutes (if runtime-ms (js/Math.round (/ runtime-ms 60000)) 0)
                   :coverage (or coverage 0)})]
         (into [:div] (map (fn [tr] [timeline-track tr])) (:tracks view))]))
    (finally (some->> @held (vector :capp/drop-read) (frame/send! f)))))

(def widgets
  {:edit-history    (fn [cfg env] [edit-history-widget env cfg])
   :object-timeline (fn [cfg env] [object-timeline-widget env cfg])})
