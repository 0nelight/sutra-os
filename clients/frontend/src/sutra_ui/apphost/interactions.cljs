(ns sutra-ui.apphost.interactions
  (:require [sutra-ui.ui.overlay :as overlay]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.interaction :as interaction]
            [sutra.kit.coerce :refer [->kw]]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.events :refer [run-effects!]]
            [sutra-ui.apphost.action-form :as af]
            [sutra-ui.apphost.page-ctx :refer [submitted]]
            [sutra-ui.apphost.toast :refer [toast!]]))

(defn- submit!
  [{:keys [descriptor ev-ctx] :as env} action params]
  (frame/request! (or (:frame env) (:frame ev-ctx))
                  {:ask    [client/submit-strict! descriptor params {:action action}]
                   :then   [[:rf/invoke (submitted env)]]
                   :report true}))

(defn invoke!
  [{:keys [ev-ctx] :as env} {:keys [action] :as spec} row open-form]
  (when-let [[kind x] (interaction/plan spec row)]
    (case kind
      :events (run-effects! x row ev-ctx)
      :submit (submit! env action x)
      :form   (open-form {:spec spec :defaults x}))))

(defn form-modal
  [{:keys [descriptor] :as env} {:keys [spec defaults]} close]
  (let [nm (or (->kw (:action spec)) (:default-action descriptor))]
    [overlay/modal {:title (or (:label spec) (str nm)) :on-close close}
     [af/action-form descriptor
      {:action      nm
       :specs       (get (:action-params descriptor) nm)
       :sections    (get (:action-forms descriptor) nm)
       :defaults    defaults
       :object      (:object descriptor)
       :object-type (:object-of-type descriptor)
       :label       (:label spec)
       :on-cancel   close
       :on-ok       (submitted env (fn [_] (close)))
       :on-error    (fn [m] (toast! (str m)))}]]))

(defn bar
  [_opts]
  (let [open (r/atom nil)]
    (fn [{:keys [env items row kind class default-icon]}]
      [:span {:class (cond-> "capp-actions-bar" class (str " " class))}
       (doall
        (for [[i {:keys [icon label] :as spec}] (map-indexed vector items)
              :when (or (nil? kind) (interaction/usable? spec kind))]
          ^{:key i}
          [ui/tooltip {:content label}
           [ui/button
            {:variant  :minimal
             :size     :small
             :class    "capp-actions-bar__btn"
             :on-click (fn [e]
                         (.stopPropagation e)
                         (invoke! env spec row #(reset! open %)))}
            (or icon label default-icon "✎")]]))
       (when @open [form-modal env @open #(reset! open nil)])])))
