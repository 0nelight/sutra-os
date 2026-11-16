(ns sutra-ui.apps.enta-editor.fields
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.inputs :as inputs]
            [sutra.kit.coerce :refer [->kw]]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.prop-source :as ps]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.object.card :as oc]))

(defn kw-or-nil [s] (when-not (str/blank? s) (keyword s)))

(defn prop-str [v] (cond (keyword? v) (subs (str v) 1) (nil? v) "" :else (str v)))

(defn at
  [shown write!]
  (reify
    IDeref (-deref [_] shown)
    IReset (-reset! [_ v] (write! v) v)))

(defn value-str [v] (if (keyword? v) (name v) (str (if (nil? v) "" v))))

(defn- option-item
  [o]
  (if (map? o)
    o
    (let [[v l] (if (vector? o) o [o o])] {:value (str v) :label (str l) :title (str v)})))

(defn select
  ([v options on-change placeholder] (select v options on-change placeholder nil))
  ([v options on-change placeholder {:keys [class fill? clearable?]}]
   [inputs/select-input nil {:items       (mapv option-item options)
                             :value       (value-str v)
                             :placeholder placeholder
                             :strict?     true
                             :clearable?  clearable?
                             :class       (ui/cx class (when fill? "bp-fill"))
                             :on-change   (fn [s _] (on-change s))}]))

(defn dashed-button [label on-click]
  [ui/button {:small true :outlined true :intent :primary :on-click on-click} label])

(defn row-button
  ([label title on-click] (row-button label title on-click false))
  ([label title on-click disabled?]
   (let [icon? (keyword? label)]
     [ui/tooltip {:content title}
      [ui/button {:minimal true :small true :on-click on-click :disabled disabled?
                  :icon (when icon? label) :aria-label (when icon? title)
                  :intent (when (= label :cross) :danger)}
       (when-not icon? label)]])))

(defn type-label
  ([t] (type-label t false))
  ([t plural?]
   (let [m (oc/type-meta (if (string? t) (keyword t) t))]
     (str (when (seq (:symbol m)) (str (:symbol m) " ")) (oc/type-label m plural?)))))

(defn object-type-options
  []
  (vec (sort-by (comp str/lower-case #(oc/type-label (keyword %)) first)
                (for [t (keys (:objects @catalog/schema))]
                  [(name t) (type-label t)]))))

(defn var-label
  [nm]
  (or (tbl/humanize (name nm)) ""))

(defn var-options [names] (vec (for [n names] [(name n) (var-label n)])))

(defn pick-or-free
  [_v _options _on-change _placeholder]
  (let [free? (r/atom false)]
    (fn [v options on-change placeholder]
      (let [known (set (map first options))
            cur   (prop-str v)]
        (if (or @free? (empty? options))
          [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center" :flex 1}}
           [inputs/text-input (at cur #(on-change (kw-or-nil %)))
            {:placeholder placeholder :small true :class "u-flex-1 ab-field--min"}]
           (when (seq options)
             [row-button :list (i18n/t :ui/app-builder-pick-from-list) #(reset! free? false)])]
          [select cur
           (concat (when (and (seq cur) (not (contains? known cur)))
                     [[cur (str cur " " (i18n/t :ui/app-builder-unknown))]])
                   options
                   [["::frei" (i18n/t :ui/app-builder-other-value)]])
           #(if (= % "::frei") (reset! free? true) (on-change (kw-or-nil %)))
           placeholder {:class "u-flex-1 ab-field--min"}])))))

(defn prop-field
  [_v _otype _opts _on-change]
  (let [free? (r/atom false)]
    (fn [v otype opts on-change]
      (let [groups (ps/prop-options (:objects @catalog/schema) (es/derived-module) otype)
            known  (set (mapcat :props groups))
            disp   (when otype (es/prop-display-of (keyword otype)))
            label-of (fn [p] (or (not-empty (str (get-in disp [(->kw p) :label])))
                                 (prop-str p)))]
        (if (or @free? (empty? known))
          [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
           [:div {:class "u-flex-1"}
            [inputs/text-input (at (prop-str v) #(on-change (kw-or-nil %)))
             (merge {:placeholder "Eigenschaft"} (or opts {:fill true}))]]
           (when (seq known)
             [row-button :list (i18n/t :ui/app-builder-pick-from-list) #(reset! free? false)])]
                    [inputs/select-input nil
           (merge {:value       (prop-str v)
                   :title       (prop-str v)
                   :strict?     true
                   :placeholder "Eigenschaft…"
                   :items       (concat
                                 (when (and (some? v) (not (contains? known (->kw v))))
                                   [{:value (prop-str v)
                                     :label (str (prop-str v) " "
                                                 (i18n/t :ui/app-builder-unknown))}])
                                 (for [{:keys [label props]} groups
                                       pr props]
                                   {:value (prop-str pr) :label (label-of pr) :group label
                                    :title (prop-str pr)})
                                 [{:value "::frei" :label (i18n/t :ui/app-builder-other-value)}])
                   :on-change   (fn [t _]
                                  (if (= t "::frei")
                                    (reset! free? true)
                                    (on-change (kw-or-nil t))))}
                  (or opts {:fill true}))])))))
