(ns sutra-ui.apphost.filters
  (:require
            [sutra-ui.ui.filter :as ui-filter :refer [filter-pills]]
            [sutra-ui.ui.inputs :refer [text-input number-input date-input
                                                           select-input checkbox]]))

(defn- control
  [{:keys [kind sel items placeholder all-label min-sel max-sel step class] :as field}]
  (case kind
    :search  [text-input sel {:placeholder (or placeholder "Suchen…") :class class}]
    :select  [select-input sel {:items items :placeholder placeholder :class class}]
    :toggle  [checkbox sel {:label (:toggle-label field) :class class}]
    :number  [number-input sel {:placeholder placeholder :class class :step step}]
    :date    [date-input sel {:class class}]
    :range   [:div.capp-sidebar__range
              [number-input min-sel {:placeholder (or (:min-label field) "von") :step step}]
              [number-input max-sel {:placeholder (or (:max-label field) "bis") :step step}]]
    :pills   [filter-pills sel items {:all-label all-label}]
    [filter-pills sel items {:all-label all-label}]))

(defn filter-sidebar
  [{:keys [title fields class]}]
  (into [:div {:class (cond-> "capp-sidebar" class (str " " class))}
         (when title [:div.capp-sidebar__head title])]
        (map (fn [{:keys [label] :as field}]
               [:div.capp-sidebar__field
                (when label [:label.capp-sidebar__label label])
                (control field)]))
        fields))
