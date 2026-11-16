(ns sutra-ui.apps.assistant.providers
  (:require [sutra-ui.ui.busy :as busy]
            [sutra.kit.remote :as rd]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.assistant.providers-events :as pe]
            [sutra.types.llm-registration :as llm-reg]
            [sutra-ui.state.machine :as machine]))

(defonce ^:private ui (r/atom pe/initial))

(defonce ^:private prov (machine/frame ui {:name :assistant/providers}))

(defn- send! [event] (machine/dispatch event {:frame prov}))

(defn load! [] (send! [:providers/load]))

(defn- busy? [] (boolean (:busy @ui)))

(defn- key-badge [{:keys [has-key? local?]}]
  (cond
    has-key? [ui/tag {:minimal true :intent :success} (i18n/t :ui/prov-key-set)]
    local?   [ui/tag {:minimal true} (i18n/t :ui/prov-key-local)]
    :else    [ui/tag {:minimal true :intent :warning} (i18n/t :ui/prov-key-missing)]))

(defn- key-form [src on-change]
  (let [v (:key-draft @ui)]
    [:div {:class "prov__inline"}
     [ui/input-group {:type "password" :autoComplete "off" :value v
                      :placeholder (i18n/t :ui/prov-key-placeholder)
                      :on-change #(send! [:providers.key/draft (.. % -target -value)])}]
     [ui/button
      {:intent :primary :disabled (or (str/blank? v) (busy?))
       :on-click #(send! [:providers.key/submit src on-change])} (i18n/t :ui/prov-save)]
     [ui/button {:outlined true :on-click #(send! [:providers.key/close])}
      (i18n/t :ui/cancel)]
     [:div {:class "prov__hint"} (i18n/t :ui/prov-key-sealed)]]))

(defn- model-form [src on-change]
  (let [{:keys [id in out currency default tool-calling window]} (:model-draft @ui)
        listed (get-in @ui [:listed (:name src)])
        known  (:models (pe/preset-for-source src))
        field  (fn [k] #(send! [:providers.model/field k (.. % -target -value)]))
        ok?    (empty? (llm-reg/problems {:id id :in in :out out}))]
    [:div {:class "prov__form"}
     [:div {:class "prov__row"}
      [:label (i18n/t :ui/prov-model)]
      [ui/input-group
       {:list (str "prov-models-" (:name src)) :value id
        :placeholder "model-id"
        :on-change #(send! [:providers.model/pick src (.. % -target -value)])}]
      [:datalist {:id (str "prov-models-" (:name src))}
       (for [m (distinct (concat (map :id known) listed))] ^{:key m} [:option {:value m}])]
      [ui/button {:outlined true :on-click #(send! [:providers.model/list (:name src)])}
       (i18n/t :ui/prov-fetch-models)]]
     [:div {:class "prov__row"}
      [:label (i18n/t :ui/prov-price)]
      [ui/input-group {:class "prov__input--num" :value in :on-change (field :in)}]
      [:span "/"]
      [ui/input-group {:class "prov__input--num" :value out :on-change (field :out)}]
      [ui/input-group {:class "prov__input--ccy" :value currency
                       :on-change (field :currency)}]
      [:span {:class "prov__hint"} (i18n/t :ui/prov-price-unit)]]
     [:div {:class "prov__row"}
      [:label (i18n/t :ui/prov-window)]
      [ui/input-group {:class "prov__input--num" :value window :placeholder "128000"
                       :on-change (field :window)}]
      [:span {:class "prov__hint"}
       (i18n/t (if (:local? src) :ui/prov-window-local :ui/prov-window-hint))]]
     [:div {:class "prov__row"}
      [ui/checkbox {:class "u-m-0" :checked (boolean tool-calling)
                    :label (i18n/t :ui/prov-tool-calling)
                    :on-change #(send! [:providers.model/toggle :tool-calling])}]
      [ui/checkbox {:class "u-m-0" :checked (boolean default)
                    :label (i18n/t :ui/prov-default)
                    :on-change #(send! [:providers.model/toggle :default])}]]
     [:div {:class "prov__row"}
      [ui/button
       {:intent   :primary :disabled (or (not ok?) (busy?))
        :on-click #(send! [:providers.model/register
                           {:id (str/trim id) :in in :out out :currency currency
                            :source (:name src) :provider (:wire src)
                            :default (boolean default) :context-window window
                            :capabilities (when tool-calling [:tool-calling])}
                           on-change])}
       (i18n/t :ui/prov-register)]
      [ui/button {:outlined true :on-click #(send! [:providers.model/close])}
       (i18n/t :ui/cancel)]]]))

(defn- test-result [r]
  (when r
    (cond
      (:running? r) [busy/inline {:size 12 :class "prov__hint"} nil]
      (:ok r)       [ui/tag {:minimal true :intent :success}
                     (str "✓ " (:ms r) " ms · " (:answer r))]
      :else         [ui/tag {:minimal true :intent :warning} (str "✗ " (:error r))])))

(defn- model-row [src m on-change]
  [:div {:class "prov__model"}
   [:span {:class "prov__model-id"} (:id m)]
   (when (:default m) [ui/tag {:minimal true :intent :success} (i18n/t :ui/prov-is-default)])
   (when (:deprecated m) [ui/tag {:minimal true} (i18n/t :ui/prov-deprecated)])
   [:span {:class "prov__hint"} (str (:in m) " / " (:out m) " " (or (:currency m) "USD"))]
   (when-let [w (:context-window m)]
     [:span {:class "prov__hint"} (i18n/t :ui/prov-window-short {:n w})])
   [:span {:class "prov__spacer"}]
   [test-result (get-in @ui [:tests [(:name src) (:id m)]])]
   [ui/button {:outlined true :on-click #(send! [:providers/test (:name src) (:id m)])}
    (i18n/t :ui/prov-test)]
   (when-not (:default m)
     [ui/button
      {:outlined true :disabled (busy?)
       :on-click #(send! [:providers.model/register (assoc m :default true) on-change])}
      (i18n/t :ui/prov-make-default)])])

(defn- eur [x] (if (number? x) (str (.toFixed x 2) " €") "—"))

(defn- cap-row [src]
  (let [{:keys [budget cap-draft busy]} @ui
        s      (:name src)
        cap    (some (fn [[k v]] (when (= s (name k)) v)) (:source-budgets budget))
        today  (some #(when (= s (:source %)) %) (:sources budget))
        used   (+ (or (:spent today) 0) (or (:reserved today) 0))
        draft  (get cap-draft s)
        n      (when draft
                 (let [x (js/parseFloat (str/replace draft "," "."))] (when-not (js/isNaN x) x)))
        pct    (if (and (number? cap) (pos? cap)) (min 100 (* 100 (/ used cap))) 0)]
    (when budget
      [:div {:class "prov__row"}
       [:label (i18n/t :ui/prov-cap)]
       [ui/input-group
        {:class "prov__input--num" :inputMode "decimal"
         :value (or draft (if (number? cap) (str cap) ""))
         :placeholder (i18n/t :ui/llm-budget-no-quota)
         :on-change #(send! [:providers.cap/draft s (.. % -target -value)])}]
       [:span "€"]
       [:div
        {:style {:height "6px" :width "120px" :background "var(--c-border-soft)"
                 :border-radius "var(--radius)" :overflow "hidden"}}
        [:div {:style {:width (str pct "%") :height "100%"
                       :background (if (>= pct 100) "var(--c-danger)" "var(--c-primary)")}}]]
       [:span {:class "prov__hint"}
        (i18n/t :ui/prov-cap-today
                {:used (eur used)
                 :cap (if (number? cap) (eur cap) (i18n/t :ui/llm-budget-unlimited))})]
       (when (some? draft)
         [:<>
          [ui/button
           {:intent :primary :disabled (or busy (and (not (str/blank? draft)) (nil? n)))
            :on-click #(send! [:providers.cap/save s n])} (i18n/t :ui/prov-save)]
          [ui/button
           {:outlined true :on-click #(send! [:providers.cap/cancel s])}
           (i18n/t :ui/cancel)]])
       (when (and (nil? draft) (number? cap))
         [ui/button
          {:outlined true :disabled busy :on-click #(send! [:providers.cap/save s nil])}
          (i18n/t :ui/remove)])])))

(defn- source-card [src on-change]
  (let [{:keys [key-for model-for]} @ui]
    [ui/card {:class "prov__card"}
     [:div {:class "prov__card-head"}
      [:strong (:name src)]
      [:span {:class "prov__hint"} (:base-url src)]
      [ui/tag {:minimal true} (:wire src)]
      [key-badge src]
      [:span {:class "prov__spacer"}]
      (when-not (:local? src)
        [ui/button
         {:outlined true :on-click #(send! [:providers.key/open (:name src)])}
         (i18n/t (if (:has-key? src) :ui/prov-key-replace :ui/prov-key-set-btn))])]
     (when-let [d (:description src)] [:div {:class "prov__hint"} d])
     (when (= key-for (:name src)) [key-form src on-change])
     [cap-row src]
     (for [m (:models src)] ^{:key (:id m)} [model-row src m on-change])
     (if (= model-for (:name src))
       [model-form src on-change]
       [ui/button {:minimal true :small true :icon :plus
                   :on-click #(send! [:providers.model/open src])}
        (i18n/t :ui/prov-add-model)])]))

(defn- unassigned [data on-change]
  (let [srcs (:sources data)
        free (remove :source (:models data))]
    (when (seq free)
      [ui/card {:class "prov__card"}
       [:div {:class "prov__card-head"} [:strong (i18n/t :ui/prov-unassigned)]]
       [:div {:class "prov__hint"} (i18n/t :ui/prov-unassigned-hint)]
       (for [m free]
         ^{:key (:id m)}
         [:div {:class "prov__model"}
          [:span {:class "prov__model-id"} (:id m)]
          [:span {:class "prov__hint"} (str (:in m) " / " (:out m) " " (or (:currency m) "USD"))]
          [:span {:class "prov__spacer"}]
          [ui/html-select
           {:value ""
            :on-change (fn [e]
                         (when-let [s (some #(when (= (.. e -target -value) (:name %)) %) srcs)]
                           (send! [:providers.model/register (assoc m :source (keyword (:name s))
                                                          :provider (keyword (:wire s)))
                                   on-change])))}
           [:option {:value ""} (i18n/t :ui/prov-assign)]
           (for [s srcs] ^{:key (:name s)} [:option {:value (:name s)} (:name s)])]])])))

(defn- add-form [data on-change]
  (let [{:keys [preset name base-url key project wire] :as v} (:add-draft @ui)
        p      (pe/preset-of preset)
        taken? (some #(= name (:name %)) (:sources data))
        local? (pe/local-url? base-url)
        ok?    (and (re-matches #"[a-z][a-z0-9-]*" (str name)) (not taken?)
                    (re-find #"^https?://" (str base-url))
                    (or local? (seq key)))
        field  (fn [k] #(send! [:providers.add/field k (.. % -target -value)]))]
    [ui/card {:class "prov__card prov__card--add"}
     [:div {:class "prov__card-head"} [:strong (i18n/t :ui/prov-add)]]
     [:div {:class "prov__presets"}
      (for [x pe/presets]
        ^{:key (:id x)}
        [ui/button {:small    true :active (= preset (:id x))
                    :on-click #(send! [:providers.add/preset x])}
         (if (= :custom (:id x)) (i18n/t :ui/prov-custom) (:label x))])]
     [:div {:class "prov__row"}
      [:label (i18n/t :ui/prov-name)]
      [ui/input-group {:value name :on-change (field :name)}]
      (when taken?
        [ui/tag {:minimal true :intent :warning} (i18n/t :ui/prov-name-taken)])]
     [:div {:class "prov__row"}
      [:label (i18n/t :ui/prov-url)]
      [ui/input-group {:class "prov__input--wide" :value base-url
                       :on-change (field :base-url)}]]
     [:div {:class "prov__row"}
      [:label (i18n/t :ui/prov-wire)]
      [ui/html-select {:value wire :on-change (field :wire)}
       (for [w (:wires data)] ^{:key w} [:option {:value w} w])]]
     (when-not local?
       [:div {:class "prov__row"}
        [:label (i18n/t :ui/prov-key)]
        [ui/input-group
         {:type "password" :autoComplete "off" :class "prov__input--wide" :value key
          :placeholder (i18n/t :ui/prov-key-placeholder)
          :on-change (field :key)}]])
     (when (seq (:projects data))
       [:div {:class "prov__row"}
        [:label (i18n/t :ui/prov-project)]
        [ui/html-select {:value project :on-change (field :project)}
         (for [pr (:projects data)]
           ^{:key (:name pr)} [:option {:value (:name pr)} (:name pr)])]])
     [:div {:class "prov__hint"} (i18n/t (if local? :ui/prov-local-hint :ui/prov-key-sealed))]
     [:div {:class "prov__row"}
      [ui/button
       {:intent   :primary :disabled (or (not ok?) (busy?))
        :on-click #(send! [:providers.add/submit
                           (assoc v :description
                                  (str (:label p) " — " (i18n/t :ui/prov-added-here)))
                           on-change])}
       (i18n/t :ui/prov-create)]
      [ui/button {:outlined true :on-click #(send! [:providers.add/close])}
       (i18n/t :ui/cancel)]]]))

(def view
  (machine/entering
   prov [:providers/load]
   (fn [{:keys [on-change]}]
     (let [{rdata :data :keys [error adding busy]} @ui
           data  (rd/value-or rdata nil)
           error (or error (rd/reason rdata))]
       [:div {:class "prov"}
        [:div {:class "prov__intro"} (i18n/t :ui/prov-intro)]
        (when error [ui/callout {:intent :danger :class "assistant__error"} error
                     [ui/button
                      {:minimal true :small true :icon :cross :aria-label (i18n/t :ui/close)
                       :on-click #(send! [:providers/clear-error])}]])
        (when busy [:div {:class "prov__hint"} (i18n/t :ui/prov-saving)])
        (cond
          (rd/loading? rdata)
          [busy/block {:class "assistant__muted"} (i18n/t :ui/loading)]
          data
          [:<>
           (for [s (:sources data)] ^{:key (:name s)} [source-card s on-change])
           [unassigned data on-change]
           (if adding
             [add-form data on-change]
             [ui/button
              {:intent :primary :class "prov__add-btn" :on-click #(send! [:providers.add/open])}
              (str "+ " (i18n/t :ui/prov-add))])])]))))
