(ns sutra-ui.ui.sheet
  (:require [sutra.capps.spreadsheet :as ss]))

(defn grid
  [{:keys [rows cols sel cell class] :as opts}]
  (let [{:keys [start-row start-col end-row end-col] :as rg}
        (when (:a sel) (ss/selection-range sel))]
    [:div (merge {:class (cond-> "ui-sheet" class (str " " class)) :tab-index 0}
                 (select-keys opts [:ref :on-key-down :on-copy :on-paste :on-mouse-up]))
     [:table {:class "ui-sheet__grid"}
      [:thead
       (into [:tr [:th {:class "ui-sheet__corner"}]]
             (for [c (range cols)]
               ^{:key c} [:th {:class (when (and rg (<= start-col c end-col)) "is-sel")}
                          (ss/column-name c)]))]
      (into [:tbody]
            (for [r (range rows)]
              ^{:key r}
              (into [:tr [:th {:class (when (and rg (<= start-row r end-row)) "is-sel")} (inc r)]]
                    (for [c (range cols)]
                      (with-meta (cell r c (boolean (and rg (ss/in-range? rg [r c]))))
                        {:key c})))))]]))
