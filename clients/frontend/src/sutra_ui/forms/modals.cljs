(ns sutra-ui.forms.modals
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.forms.events]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.state.shell :as shell]
            [sutra.i18n.core :as i18n]
            [sutra-ui.forms.base :refer [connect-modal-form new-link-form
                                                new-object-form]]
            [sutra-ui.forms.link :refer [link-form submit-link!]]
            [sutra-ui.forms.object :refer [default-backing-mode object-form
                                                  save-object-form!]]))

(def ^:private create-dialog-body-style
  {:width "640px"})

(defn add-object-form []
  (when (= :add-object (:panel @shell/ui-state))
    (let [close! (fn []
                   (fs/reset-form! :new-object)
                   (shell/show-panel! :none))]
      (when (nil? (:backing-mode @new-object-form))
        (fs/edit! new-object-form assoc :backing-mode (default-backing-mode)))
      [overlay/modal {:on-close close!
                      :style create-dialog-body-style
                      :title (i18n/t :ui/object-type-new)}
       [object-form
        {:form-atom new-object-form
         :create? true
         :on-save #(db/send! [:shared.forms/stage-object
                              (fn [] (save-object-form! new-object-form nil)) close!])
         :on-cancel close!}]])))

(defn add-link-form []
  (when (= :add-link (:panel @shell/ui-state))
    (let [close! (fn []
                   (fs/reset-form! :new-link)
                   (shell/show-panel! :none))]
      [overlay/modal {:on-close close!
                      :style create-dialog-body-style
                      :title (i18n/t :ui/link-type-new)}
       [link-form
        {:form-atom new-link-form
         :on-create #(submit-link! new-link-form true close!)
         :on-cancel close!}]])))

(defn connect-modal []
  (let [close! (fn []
                 (fs/reset-form! :connect-modal)
                 (shell/close-connect-modal!))]
    [overlay/modal {:on-close close! :title (i18n/t :ui/link-type-new)}
     [link-form
      {:form-atom connect-modal-form
       :from-locked? false
       :to-locked? false
       :on-create #(submit-link! connect-modal-form true close!)
       :on-cancel close!}]]))
