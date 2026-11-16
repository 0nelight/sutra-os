(ns sutra-ui.ui.form
  (:require [sutra-ui.ui.core :as ui]))

(defn field
  [{:keys [label help error required? inline? class]} & controls]
  (into [ui/form-group {:label       label
                        :label-info  (when required? "*")
                        :helper-text (or error help)
                        :intent      (when error :danger)
                        :inline      (boolean inline?)
                        :class       class}]
        controls))

(defn row
  [& controls]
  (into [:div {:class "ui-form__row"}] controls))
