(ns sutra-ui.apphost.widgets.pairing
  (:require [sutra-ui.ui.overlay :as overlay]
            [cljs.reader :as reader]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [checkbox]]
            [sutra.capps.pairing :as pairing]
            [sutra.capps.variables :refer [deref*]]
            [sutra-ui.apphost.toast :refer [toast!]]))

(defn- url-scope []
  (.get (js/URLSearchParams. (.. js/window -location -search)) "appPairingScopeId"))

(defn- shared-values
  [shared enrich?]
  (into {} (map (fn [{:keys [key var]}] [key (pairing/wire-value (deref* var) enrich?)])) shared))

(defn- keyed
  [shared]
  (mapv (fn [{:keys [name var] :as s}] (assoc s :key (or name (:var-name s)) :var var)) shared))

(defn app-pairing
  [_cfg]
  (let [st      (r/atom nil)
        chan    (atom nil)
        menu    (r/atom false)
        cfg*    (atom nil)
        sent    (atom nil)
        send!   (fn [msgs]
                  (when-let [^js c @chan]
                    (doseq [m msgs] (.postMessage c (pr-str m)))))
        publish! (fn []
                   (let [{:keys [shared enrich?]} @cfg*
                         vals (shared-values shared enrich?)]
                     (reset! sent vals)
                     (when-let [m (pairing/outgoing @st shared vals)]
                       (send! [m]))))
        apply!  (fn [vars]
                  (doseq [[k v] vars
                          :let  [h (some #(when (= k (:key %)) (:var %)) (:shared @cfg*))]
                          :when (satisfies? IReset h)]
                    (reset! h v))
                  (reset! sent (shared-values (:shared @cfg*) (:enrich? @cfg*))))
        step!   (fn [{:keys [state send apply toast changed? publish?]}]
                  (reset! st state)
                  (when send (send! send))
                  (when apply (apply! apply))
                  (when (and toast (:toasts? @cfg*))
                    (toast! (str "Gekoppelt mit " toast)))
                  (when (and changed? (pairing/opens-menu? (:auto-open @cfg*) :change))
                    (reset! menu true))
                  (when publish? (publish!)))]
    (r/create-class
     {:display-name "app-pairing"
      :component-did-mount
      (fn [this]
        (let [{:keys [scope display-name auto-open] :as cfg} (second (r/argv this))
              c (js/BroadcastChannel.
                 (pairing/scope-channel (or (deref* scope) (url-scope))))
              s (pairing/initial (str (random-uuid)) (or display-name "App"))]
          (reset! cfg* (update cfg :shared keyed))
          (reset! st s)
          (reset! chan c)
          (set! (.-onmessage c)
                (fn [^js e] (step! (pairing/receive @st (reader/read-string (.-data e))))))
          (when (pairing/opens-menu? auto-open :load) (reset! menu true))
          (send! [(pairing/hello s)])))
      :component-will-unmount
      (fn [_]
        (when-let [^js c @chan]
          (.postMessage c (pr-str (pairing/bye @st)))
          (.close c)))
      :component-did-update
      (fn [this]
        (reset! cfg* (update (second (r/argv this)) :shared keyed))
        (let [{:keys [shared enrich?]} @cfg*]
          (when (not= @sent (shared-values shared enrich?))
            (publish!))))
      :reagent-render
      (fn [{:keys [shared enrich? pause-toggle? display-name]}]
        (shared-values (keyed shared) enrich?)
        (let [{:keys [peers paired paused?]} @st
              others (dissoc peers paired)]
          [:div.capp-pairing
           [ui/button {:intent :primary :on-click #(swap! menu not)}
            (if paired
              (str "⇄ " (get peers paired paired))
              (str "⇄ " (or display-name "App-Kopplung")))]
           (when @menu
             [overlay/popover {:place :end :on-close #(reset! menu false)}
              [:div.capp-pairing__menu
               (when paired
                 [:div.capp-pairing__group
                  [:div.capp-pairing__head "Gekoppelt"]
                  [:div.capp-pairing__row
                   [:span (get peers paired paired)]
                   [ui/button
                    {:on-click #(step! (pairing/unpair @st))}
                    "Entkoppeln"]]])
               [:div.capp-pairing__group
                [:div.capp-pairing__head "Entdeckt"]
                (if (seq others)
                  (for [[id nm] others]
                    ^{:key id}
                    [:div.capp-pairing__row
                     [:span nm]
                     [ui/button
                      {:intent :primary :on-click #(step! (pairing/pair-with @st id))}
                      "Koppeln"]])
                  [:div.capp-pairing__none "Keine andere App geöffnet."])]
               (when pause-toggle?
                 [checkbox {:class     "capp-pairing__pause"
                            :checked   (boolean paused?)
                            :label     "Aktualisierungen pausieren"
                            :on-change #(step! (pairing/pause @st
                                                              (.. % -target -checked)))}])]])]))})))

(def widgets
  {:app-pairing (fn [cfg _] [app-pairing cfg])})
