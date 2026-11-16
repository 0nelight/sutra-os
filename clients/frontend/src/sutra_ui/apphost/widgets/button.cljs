(ns sutra-ui.apphost.widgets.button
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.download :as dl]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.button-group :as bg]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.object-display :as od]
            [sutra.capps.variables :refer [deref*]]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.types.submission :as csub]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.widgets.display :as dw]
            [sutra-ui.apphost.events :refer [blocked-app]]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.interactions :as interactions]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.apphost.toast :refer [toast!]]
            [sutra-ui.api :as api]))

(defn- deref-vals [m] (into {} (map (fn [[k v]] [k (deref* v)])) (or m {})))

(defn- fire-event! [env ev] (when-let [f ((:event-fn env) ev)] (f nil)))

(defn- gate-of
  [{:keys [descriptor]} {:keys [action params]}]
  (csub/submit-gate (get (:action-params descriptor) (->kw action)) (deref-vals params)))

(defn- ask! [env ask opts then]
  (frame/request! (frame/frame-for env)
                  {:ask (cond-> ask opts (conj opts)) :then [[:rf/invoke then]] :report true}))

(defn- submit-action!
  [{:keys [descriptor refresh] :as env} {:keys [action params scenario on-start on-done output]}]
  (fire-event! env on-start)
  (ask! env [client/submit-strict! descriptor (deref-vals params)]
        {:action action :scenario (deref* scenario)}
        (fn [data]
          (when (and refresh data) (refresh data))
          (when (satisfies? IReset output) (reset! output (vec (:created (meta data)))))
          (fire-event! env on-done))))

(defn- open-url!
  [{:keys [url open-in]}]
  (if (= :same-tab (->kw open-in))
    (set! (.-href js/location) url)
    (js/window.open url "_blank" "noopener,noreferrer")))

(defn- export-rows!
  [{:keys [rows props filename custom-names?] :as spec}]
  (let [rows  (lp/as-rows (deref* rows))
        sheet (od/export-sheet rows (bg/export-cols props rows) custom-names?)]
    (case (bg/export-format spec)
      :clipboard (-> (js/navigator.clipboard.writeText (bg/tsv-text sheet))
                     (.then (fn [_] (toast! "Kopiert"))))
      :csv       (dw/export! :csv sheet (or filename "export"))
      :xlsx      (dw/export! :xlsx sheet (or filename "export")))))

(defn- base64->bytes
  [s]
  (let [bin (js/atob s)
        arr (js/Uint8Array. (.-length bin))]
    (dotimes [i (.-length bin)] (aset arr i (.charCodeAt bin i)))
    arr))

(defn- function-output
  [{:keys [column]} rows]
  (let [row (or (:values (first rows)) (first rows))]
    (if column (get row (->kw column)) (first (vals row)))))

(defn- export-function!
  [{:keys [descriptor] :as env} {:keys [function args] :as spec}]
  (ask! env [client/function! descriptor {:function function :args (deref-vals args)}] nil
        (fn [{:keys [rows]}]
          (let [{:keys [error text binary? mime filename]}
                (bg/function-file spec (function-output spec rows))]
            (if error
              (toast! error {:error? true})
              (dl/blob! (js/Blob. #js [(if binary? (base64->bytes text) text)]
                                      #js {:type mime})
                            filename))))))

(defn- blob-of [url]
  (.then (api/fetch-url url #js {:credentials "same-origin"}) #(.blob ^js %)))

(defn- download-each!
  [{:keys [media-src] :as env} files]
  (when-let [{:keys [src name]} (first files)]
    (frame/request! (frame/frame-for env)
                    {:ask    [blob-of (media-src src)]
                     :then   [[:rf/invoke (fn [blob]
                                            (dl/blob! blob name)
                                            (download-each! env (rest files)))]]
                     :report "Download fehlgeschlagen: "})))

(defn- export-media!
  [env {:keys [items] :as spec}]
  (let [items (mapv #(update % :rows (comp lp/as-rows deref*)) items)]
    (download-each! env (bg/media-downloads (assoc spec :items items)
                                            (.toISOString (js/Date.))))))

(defn- fire!
  [env b open-form]
  (when-let [[what spec] (bg/plan b)]
    (case what
      :action          (if (:form? spec)
                         (open-form {:spec     {:action (:action spec) :label (:label spec)}
                                     :defaults (deref-vals (:params spec))})
                         (submit-action! env spec))
      :url             (open-url! spec)
      :export          (export-rows! (:export spec))
      :function-export (export-function! env (:function-export spec))
      :media-export    (export-media! env (:media-export spec))
      :events          (fire-event! env (:on-click spec)))))

(defn- blocked-reason
  [env {:keys [action on-click form?] :as b}]
  (cond
    (and action (not form?)) (:reason (gate-of env b))
    :else                    (when-let [app (blocked-app on-click)]
                               (str "Keine Berechtigung für " app))))

(defn- button-tip [env b] (or (:description b) (blocked-reason env b)))

(defn- button-attrs
  [env b open-form extra]
  (let [reason (blocked-reason env b)]
    (merge
     (bg/button-props b)
     {:style    (bg/button-style b)
      :disabled (or (= :disabled (bg/state b)) (some? reason))
      :on-click #(fire! env b open-form)}
     extra)))

(defn- button-body
  [{:keys [left-icon label right-icon]}]
  [:<>
   (when left-icon [:span.capp-btn__icon left-icon])
   label
   (when right-icon [:span.capp-btn__icon right-icon])])

(defn- menu-items
  [env buttons open-form _close]
  (into [ui/menu]
        (for [b buttons]
          [ui/menu-item {:text [button-body b]
                         :disabled (or (= :disabled (bg/state b)) (some? (blocked-reason env b)))
                         :html-title (button-tip env b)
                         :on-click #(fire! env b open-form)}])))

(defn button-group
  [_cfg _env]
  (let [open   (r/atom false)
        form   (r/atom nil)
        toggle (atom nil)]
    (fn [{:keys [button-type buttons label left-icon right-icon] :as cfg} env]
      (let [style    (select-keys cfg [:minimal? :tag? :large? :fill? :intent :color])
            shown    (mapv #(merge style %) (bg/shown buttons))
            kind     (or (->kw button-type) :inline)
            set-form #(reset! form %)
            close    #(reset! open false)]
        [:div {:class (cond-> "capp-btn-group" (:fill? cfg) (str " capp-btn-group--fill"))}
         (case kind
           :menu
           [:div.capp-btn-menu-wrap
            [ui/button (merge
                        (bg/button-props style)
                        {:style         (bg/button-style style)
                         :aria-haspopup "menu"
                         :active        @open
                         :right-icon    (when-not right-icon :caret-down)
                         :on-click      #(swap! open not)})
             [button-body {:left-icon left-icon :right-icon right-icon :label label}]]
            (when @open
              (if (groesse/kompakt?)
                [overlay/bottom-drawer {:on-close close :title label}
                 (menu-items env shown set-form close)]
                [overlay/popover {:on-close close} (menu-items env shown set-form close)]))]

           :two-part
           (let [[primary & others] shown]
             [ui/button-group {:class "capp-btn-menu-wrap"}
              (when primary
                [ui/tooltip {:content (button-tip env primary)}
                 [ui/button (button-attrs env primary set-form {}) [button-body primary]]])
              (when (seq others)
                [ui/button (merge
                            (bg/button-props style)
                            {:style         (bg/button-style style)
                             :aria-haspopup "menu"
                             :aria-label    "weitere"
                             :active        @open
                             :icon          :caret-down
                             :ref           #(reset! toggle %)
                             :on-click      #(swap! open not)})])
              (when (and @open (seq others))
                (if (groesse/kompakt?)
                  [overlay/bottom-drawer {:on-close close}
                   (menu-items env (vec others) set-form close)]
                  [overlay/popover {:on-close close :anchor #(deref toggle)}
                   (menu-items env (vec others) set-form close)]))])

           (into [:<>]
                 (map-indexed
                  (fn [i b]
                    ^{:key i}
                    [ui/tooltip {:content (button-tip env b)}
                     [ui/button (button-attrs env b set-form {}) [button-body b]]]))
                 shown))
         (when-let [f @form]
           [interactions/form-modal env f #(reset! form nil)])]))))

(def widgets
  {:button-group (fn [cfg env] [button-group cfg env])})
