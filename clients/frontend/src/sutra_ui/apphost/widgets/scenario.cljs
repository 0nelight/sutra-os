(ns sutra-ui.apphost.widgets.scenario
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.sel :as sel]
            [sutra-ui.ui.overlay :as overlay]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui :refer [checkbox html-select radio]]
            [reagent.core :as r]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.scenario-view :as sv]
            [sutra.capps.variables :refer [deref*]]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(defn- wr-if! [h v] (when (satisfies? IReset h) (sel/wr! h v)))

(def ^:private known [::scenarios])

(defn- known-scenarios
  [{:keys [descriptor] :as env}]
  (or (:data (frame/read! env known descriptor [client/scenarios-list descriptor])) []))

(machine/reg-fx ::prompt
                (fn [{f :frame} {:keys [ask ok]}]
                  (let [reply (machine/answering f)]
                    (.then (overlay/prompt! ask) #(when-not (str/blank? %) (reply (conj ok %)))))))

(machine/reg-event ::act
                   (fn [{:keys [db]} [_ id {:keys [ask prompt] :as m} nm]]
                     (cond
                       prompt
                       {:fx [[::prompt {:ask prompt :ok [::act id (dissoc m :prompt)]}]]}

                       (:confirm m)
                       {:fx [[:confirm {:ask (:confirm m) :ok [::act id (dissoc m :confirm)]}]]}

                       :else
                       (rq/request (update db ::failed dissoc id)
                                   {:ask   (cond-> (if (fn? ask) (ask nm) ask)
                                             (:opts m) (conj (:opts m)))
                                    :busy  [::busy id]
                                    :error [::failed id]
                                    :then  [[::acted m]]}))))

(machine/reg-event ::acted
                   (fn [_ [_ {:keys [then reread page?]} answer]]
                     {:fx (-> [(when then [:invoke [then answer]])]
                              (into (map (fn [s] [:dispatch [:capp/reread s]])) reread)
                              (conj (when page? [:invoke [client/refresh-page!]])))}))

(defn- at [f k id] @(machine/subscribe [:capp/at [k id]] {:frame f}))

(defn- failure [f id]
  (when-let [m (at f ::failed id)] [ui/problem m nil]))

(defn- named [title confirm-label]
  {:title title :label (i18n/t :ui/capp-scenario-name)
   :confirm-label (or confirm-label (i18n/t :ui/capp-scenario-save))})

(defn- drop-question [nm]
  {:title   (i18n/t :ui/capp-scenario-drop-title {:name nm})
   :body    (i18n/t :ui/capp-scenario-drop-body)
   :danger? true})

(defn- rename!
  [descriptor seg nm]
  (.then (client/scenario-duplicate! descriptor seg nm)
         (fn [{:keys [scenario]}]
           (.then (client/scenario-drop! descriptor seg) (constantly scenario)))))

(defn- row-actions
  [{:keys [scenario name removable?]} act! busy?]
  [:span.capp-scenarios__actions
   [ui/button {:intent :primary :disabled busy? :on-click #(act! :copy scenario name)}
    (i18n/t :ui/capp-scenario-duplicate)]
   (when removable?
     [ui/button {:intent :primary :disabled busy? :on-click #(act! :rename scenario name)}
      (i18n/t :ui/capp-scenario-rename)])
   (when removable?
     [ui/button
      {:class "capp-l-scenario__drop" :intent :danger
       :disabled busy? :on-click #(act! :drop scenario name)}
      (i18n/t :ui/capp-scenario-drop)])])

(defn scenario-manager
  [cfg env]
  (r/with-let [f  (frame/frame-for env)
               id (random-uuid)]
    (let [{:keys [static dynamic active title new-label class]} cfg
          {:keys [descriptor]} env
          dyn?  (contains? cfg :dynamic)
          rows  (sv/manager-list {:static   (deref* static)
                                  :dynamic  (deref* dynamic)
                                  :dynamic? dyn?}
                                 (known-scenarios env))
          cur   (deref* active)
          busy? (at f ::busy id)
          dyn!  (fn [g & args] (when dyn? (wr-if! dynamic (apply g (deref* dynamic) args))))
          act!  (fn [m] (frame/send! f [::act id (assoc m :reread [known])]))
          row!  (fn [what seg nm]
                  (act!
                   (case what
                     :copy   {:prompt (named (i18n/t :ui/capp-scenario-duplicate) nil)
                              :ask    (fn [n] [client/scenario-duplicate! descriptor seg n])
                              :then   #(dyn! sv/dynamic-after-create (:scenario %))}
                     :rename {:prompt (named (i18n/t :ui/capp-scenario-rename) nil)
                              :ask    (fn [n] [rename! descriptor seg n])
                              :then   (fn [scenario]
                                        (dyn! sv/dynamic-after-rename seg scenario)
                                        (when (= seg cur) (wr-if! active scenario)))}
                     :drop   {:confirm (drop-question nm)
                              :ask     [client/scenario-drop! descriptor seg]
                              :then    (fn [_]
                                         (dyn! sv/dynamic-after-drop seg)
                                         (wr-if! active (sv/active-after-drop cur seg)))})))]
      [:div.capp-scenarios (when class {:class class})
       (when title [:div.capp-table__title title])
       [failure f id]
       (into [:div.capp-scenarios__list]
             (map (fn [{:keys [scenario name] :as row}]
                    ^{:key scenario}
                    [:div.capp-scenarios__row
                     {:class (when (= scenario (str cur)) "is-active")}
                     [radio {:class     "capp-scenarios__name"
                             :checked   (= scenario (str cur))
                             :label     name
                             :on-change #(wr-if! active scenario)}]
                     [row-actions row row! busy?]]))
             rows)
       [ui/button
        {:class    "capp-l-scenario__new"
         :intent   :primary
         :disabled busy?
         :on-click #(act! {:prompt (named (i18n/t :ui/capp-scenario-new) nil)
                           :ask    (fn [n] [client/scenario-create! descriptor n])
                           :then   (fn [{:keys [scenario]}]
                                     (dyn! sv/dynamic-after-create scenario)
                                     (wr-if! active scenario))})}
        (or new-label (i18n/t :ui/capp-scenario-new))]])))

(defn- option-list
  [rows picked on-pick]
  (into [:div.capp-l-scenario__multi]
        (map (fn [{:keys [scenario name]}]
               ^{:key scenario}
               [checkbox {:class     "capp-l-scenario__opt"
                          :checked   (boolean (some #{scenario} picked))
                          :label     name
                          :on-change #(on-pick scenario)}]))
        rows))

(defn scenario-selector
  [{:keys [candidates sel multi? dropdown? title class]} env]
  (let [rows   (sv/candidates (when (some? candidates) (deref* candidates)) (known-scenarios env))
        raw    (deref* sel)
        picked (if (sequential? raw) (mapv str raw) (if raw [(str raw)] []))]
    [:div.capp-l-scenario (when class {:class class})
     (when title [:span.capp-l-scenario__label title])
     (cond
       (and multi? dropdown?)
       [ui/section
        {:title (i18n/t :ui/capp-scenario-picked {:n (count picked)}) :collapsible true
         :compact true :collapse-props {:default-is-open false}
         :class "capp-scenarios__dropdown"}
        [ui/section-card
         [option-list rows picked #(wr-if! sel (sv/toggled picked %))]]]

       multi?
       [option-list rows picked #(wr-if! sel (sv/toggled picked %))]

       :else
       (into [html-select
              {:class     "capp-l-scenario__select"
               :value     (or (first picked) "")
               :on-change (fn [e]
                            (let [v (.. e -target -value)]
                              (wr-if! sel (when-not (str/blank? v) v))))}
              [:option {:value ""} (i18n/t :ui/capp-scenario-base)]]
             (map (fn [{:keys [scenario name]}]
                    ^{:key scenario} [:option {:value scenario} name]))
             rows))]))

(defn scenario-summary
  [cfg env]
  (r/with-let [f    (frame/frame-for env)
               id   (random-uuid)
               undo (r/atom [])
               held (atom nil)]
    (let [{:keys [scenario title class]} cfg
          {:keys [descriptor]} env
          seg   (str (deref* scenario))
          slot  [::actions seg]
          _     (when (not= slot @held)
                  (some->> @held (vector :capp/drop-read) (frame/send! f))
                  (reset! held slot)
                  (reset! undo []))
          {:keys [data error]} (when-not (str/blank? seg)
                                 (frame/read! env slot seg
                                              [client/scenario-actions descriptor seg]))
          rows  (some-> data sv/summary-rows)
          busy? (at f ::busy id)
          act!  (fn [ask opts]
                  (frame/send! f [::act id {:ask ask :opts opts :reread [slot] :page? true}]))]
      [:div.capp-scenarios (when class {:class class})
       (when title [:div.capp-table__title title])
       [failure f id]
       (when error [ui/problem error nil])
       (cond
         (str/blank? seg) [ui/empty-state {:title (i18n/t :ui/capp-scenario-none)}]
         (nil? rows)      [busy/block {:class "capp-objectview__loading"} (i18n/t :ui/capp-loading)]
         (empty? rows)    [ui/empty-state {:title (i18n/t :ui/capp-scenario-no-actions)}]
         :else
         (into [:ul.capp-scenarios__actions-list]
               (map (fn [{n :seq :keys [action removable?] :as row}]
                      ^{:key n}
                      [:li.capp-scenarios__action
                       [:span (str action)]
                       (when removable?
                         [ui/tooltip
                          {:content (i18n/t :ui/capp-scenario-remove-action)}
                          [ui/button {:icon :cross :minimal true :small true :disabled busy?
                                      :on-click (fn []
                                                  (swap! undo sv/removed row)
                                                  (act! [client/scenario-rebuild!
                                                         descriptor seg {:omit [n]}] nil))}]])]))
               rows))
       (when (seq @undo)
         [ui/button
          {:intent   :primary
           :disabled busy?
           :on-click (fn []
                       (let [[{:keys [action args]} more] (sv/undone @undo)]
                         (reset! undo more)
                         (act! [client/submit-strict! descriptor (or args {})]
                               {:action action :scenario seg})))}
          (i18n/t :ui/capp-scenario-undo)])])
    (finally (some->> @held (vector :capp/drop-read) (frame/send! f)))))

(defn- scenario-picker
  [{:keys [sel title class new-label allow-drop? allow-apply? apply-label multi?]} env]
  (r/with-let [f  (frame/frame-for env)
               id (random-uuid)]
    (let [{:keys [descriptor]} env
          items  (known-scenarios env)
          raw    (when (satisfies? IDeref sel) @sel)
          picked (if multi? (vec (lp/seg-list raw)) raw)
          cur    (if multi? (last picked) raw)
          busy?  (at f ::busy id)
          pick!  #(when (satisfies? IReset sel) (reset! sel %))
          act!   (fn [m] (frame/send! f [::act id (assoc m :reread [known])]))
          seg?   (not (str/blank? (str cur)))]
      [:div.capp-l-scenario (when class {:class class})
       (when title [:span.capp-l-scenario__label title])
       (if multi?
         (into [:div.capp-l-scenario__multi]
               (map (fn [{:keys [scenario name]}]
                      (let [on? (boolean (some #{(str scenario)} picked))]
                        [checkbox
                         {:class     "capp-l-scenario__opt"
                          :checked   on?
                          :label     (or name (str scenario))
                          :on-change #(pick! (if on?
                                               (vec (remove #{(str scenario)} picked))
                                               (conj picked (str scenario))))}]))
                    items))
         (into [html-select
                {:class     "capp-l-scenario__select"
                 :value     (if seg? (str cur) "")
                 :on-change (fn [e] (let [v (.. e -target -value)]
                                      (pick! (when-not (str/blank? v) v))))}
                [:option {:value ""} (i18n/t :ui/capp-scenario-base)]]
               (map (fn [{:keys [scenario name]}]
                      [:option {:value (str scenario)} (or name (str scenario))])
                    items)))
       [ui/button
        {:class    "capp-l-scenario__new"
         :intent   :primary
         :icon     :add
         :loading  busy?
         :on-click #(act! {:prompt (named (i18n/t :ui/capp-scenario-new)
                                          (i18n/t :ui/capp-scenario-create))
                           :ask    (fn [n] [client/scenario-create! descriptor n])
                           :then   (fn [{:keys [scenario]}]
                                     (pick! (if multi? (conj picked (str scenario)) scenario)))})}
        (or new-label (i18n/t :ui/capp-scenario-new))]
       (when (and allow-apply? seg?)
         [ui/button
          {:class    "capp-l-scenario__apply"
           :intent   :primary
           :icon     :tick
           :loading  busy?
           :on-click #(act! {:confirm {:title (i18n/t :ui/capp-scenario-apply-title
                                                      {:name (lp/scenario-name items cur)})
                                       :body  (i18n/t :ui/capp-scenario-apply-body)
                                       :confirm-label
                                       (or apply-label (i18n/t :ui/capp-scenario-apply))}
                             :ask     [client/scenario-apply! descriptor cur]})}
          (or apply-label (i18n/t :ui/capp-scenario-apply))])
       (when (and allow-drop? seg?)
         [ui/button
          {:class    "capp-l-scenario__drop"
           :intent   :danger
           :icon     :trash
           :disabled busy?
           :on-click #(act! {:confirm (drop-question (lp/scenario-name items cur))
                             :ask     [client/scenario-drop! descriptor cur]
                             :then    (fn [_]
                                        (pick! (when multi?
                                                 (vec (remove #{(str cur)} picked)))))})}
          (i18n/t :ui/delete)])
       [failure f id]])))

(def widgets
  {:scenario-picker   (fn [cfg env] [scenario-picker cfg env])
   :scenario-manager  (fn [cfg env] [scenario-manager cfg env])
   :scenario-selector (fn [cfg env] [scenario-selector cfg env])
   :scenario-summary  (fn [cfg env] [scenario-summary cfg env])})
