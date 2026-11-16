(ns blueprint-cljs.alert
  "Alert: a modal confirmation with an intent-coloured confirm button."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.dialog :as dialog]
            [blueprint-cljs.icon :as icon]))

(def ^:private own-keys
  [:cancel-button-text :can-escape-key-cancel :can-outside-click-cancel :class
   :confirm-button-text :icon :intent :is-open :loading :on-cancel :on-close :on-confirm])

(defn alert
  "Props: `:is-open`, `:intent`, `:icon`, `:confirm-button-text` (\"OK\"),
   `:cancel-button-text`, `:on-confirm (fn [e])`, `:on-cancel (fn [e])`,
   `:on-close (fn [confirmed? e])`, `:loading`, `:can-escape-key-cancel`,
   `:can-outside-click-cancel`, `:class`, plus overlay lifecycle props."
  [& args]
  (let [[props children] (if (map? (first args)) [(first args) (rest args)] [{} args])
        {:keys [cancel-button-text can-escape-key-cancel can-outside-click-cancel class
                confirm-button-text icon intent is-open loading on-cancel on-close
                on-confirm]
         :or   {confirm-button-text "OK"}} props
        done   (fn [confirmed? e]
                 (when-let [f (if confirmed? on-confirm on-cancel)] (f e))
                 (when on-close (on-close confirmed? e)))]
    [dialog/dialog
     (merge (apply dissoc props own-keys)
            {:role                    "alertdialog"
             :class                   (bp/cx (bp/cls "alert") class)
             :can-escape-key-close    (boolean can-escape-key-cancel)
             :can-outside-click-close (boolean can-outside-click-cancel)
             :is-open                 is-open
             :on-close                #(done false %)})
     [:div {:class (bp/cls "alert-body")}
      (icon/render-icon icon {:size 40 :intent intent})
      (into [:div {:class (bp/cls "alert-contents")}] children)]
     [:div {:class (bp/cls "alert-footer")}
      [button/button {:loading loading :intent intent :text confirm-button-text
                      :on-click #(done true %)}]
      (when cancel-button-text
        [button/button {:text cancel-button-text :disabled loading
                        :on-click #(done false %)}])]]))
