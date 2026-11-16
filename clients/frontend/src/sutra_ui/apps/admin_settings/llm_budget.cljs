(ns sutra-ui.apps.admin-settings.llm-budget
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.table :as table]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.apps.admin-settings.model :as model]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.style :as style]))

(defn- budget [] (:budget @model/db))

(defn- draft-at [path shown] (model/draft-at [:admin.budget/draft path] shown))

(defn- eur [x] (if (number? x) (str (.toFixed x 2) " €") "—"))

(defn- bar [used cap]
  (let [frac (if (and (number? cap) (pos? cap)) (min 1 (/ used cap)) 0)]
    [ui/progress-bar {:class   "admin-bar"
                      :value   frac
                      :intent  (if (>= frac 1) :danger :primary)
                      :animate false
                      :stripes false}]))

(defn- number-field [k value placeholder]
  [inputs/text-input (draft-at [k] (get-in (budget) [:draft k]
                                           (if (number? value) (str value) "")))
   {:inputMode "decimal" :class "admin-num" :placeholder placeholder}])

(defn- limits-card [{:keys [budget-eur interactive-reserve-pct batch-cap-eur spent-today]}]
  (let [{:keys [draft busy]} (budget)
        dirty? (some #(contains? draft %) [:eur :reserve-pct])]
    [ui/callout {:class "u-mb-12"}
     [:div {:class style/section-h3} (i18n/t :ui/llm-budget-limits)]
     [form/field {:label (i18n/t :ui/llm-budget-daily) :inline? true
                  :help  (i18n/t :ui/llm-budget-daily-hint)}
      [form/row [number-field :eur budget-eur "1.00"] [:span "€"]]]
     [form/field {:label (i18n/t :ui/llm-budget-reserve) :inline? true
                  :help  (i18n/t :ui/llm-budget-reserve-hint {:cap (eur batch-cap-eur)})}
      [form/row [number-field :reserve-pct interactive-reserve-pct "20"] [:span "%"]]]
     [:div {:class "u-flex-items-gap-8 u-mb-8"}
      [:span {:class "admin-label"} (i18n/t :ui/llm-budget-today)]
      [bar spent-today budget-eur]
      [:span
       (str (eur spent-today) " / "
            (if (pos? budget-eur) (eur budget-eur) (i18n/t :ui/llm-budget-unlimited)))]]
     [:div {:class "u-flex-items-gap-8 u-mt-8"}
      [ui/button {:intent :primary :disabled (or busy (not dirty?))
                  :on-click #(model/send! [:admin.budget/save-limits])}
       (i18n/t (if busy :ui/cp-saving :ui/save))]
      (when dirty?
        [ui/button
         {:on-click #(model/send! [:admin.budget/discard-limits])}
         (i18n/t :ui/cp-discard)])]]))

(defn- pool-of [pools p] (some #(when (= (str "batch:" p) (:pool %)) %) pools))

(defn- used-of [pool] (+ (or (:spent pool) 0) (or (:reserved pool) 0)))

(defn- limit-cols
  [name-label cap-label kind drafts]
  (let [shown (fn [{:keys [k cap]}]
                (or (get-in (budget) [:draft drafts k]) (if (number? cap) (str cap) "")))]
    [{:label name-label :render :k}
     {:label  cap-label
      :render (fn [{:keys [k] :as r}]
                [inputs/text-input (draft-at [drafts k] (shown r))
                 {:inputMode "decimal" :small true :class "admin-num admin-num--s"
                  :placeholder (i18n/t :ui/llm-budget-no-quota)}])}
     {:label (i18n/t :ui/llm-budget-used) :render (fn [{:keys [cap used]}] [bar used cap])}
     {:label (i18n/t :ui/llm-budget-spent) :cell-class "admin-nowrap" :render #(eur (:spent %))}
     {:label  ""
      :render (fn [{:keys [k cap]}]
                (let [busy  (:busy (budget))
                      draft (get-in (budget) [:draft drafts k])]
                  [:<>
                   (when (some? draft)
                     [ui/button {:disabled busy
                                 :on-click #(model/send! [:admin.budget/save-quota kind k])}
                      (i18n/t :ui/save)])
                   (when (and (nil? draft) (number? cap))
                     [ui/button
                      {:outlined true :intent :danger :disabled busy
                       :on-click #(model/send! [:admin.budget/save {kind k :quota nil}])}
                      (i18n/t :ui/remove)])]))}]))

(defn- quotas-card [{:keys [projects project-quotas pools]}]
  [ui/callout {:class "u-mb-12"}
   [:div {:class style/section-h3} (i18n/t :ui/llm-budget-quotas)]
   [:div {:class [style/small-muted "u-mb-8"]} (i18n/t :ui/llm-budget-quotas-hint)]
   [table/data-table
    {:row-key :k
     :cols    (limit-cols (i18n/t :ui/llm-budget-project) (i18n/t :ui/llm-budget-quota)
                          :project :quotas)
     :rows    (for [p (distinct (concat projects (keys project-quotas)))
                    :let [p    (name p)
                          pool (pool-of pools p)]]
                {:k     p
                 :cap   (get project-quotas (keyword p) (get project-quotas p))
                 :used  (used-of pool)
                 :spent (:spent pool)})}]])

(defn- sources-card [{:keys [llm-sources source-budgets sources]}]
  (let [today (into {} (map (juxt :source identity)) sources)
        caps  (into {} (map (fn [[k v]] [(name k) v])) source-budgets)]
    [ui/callout {:class "u-mb-12"}
     [:div {:class style/section-h3} (i18n/t :ui/llm-budget-sources)]
     [:div {:class [style/small-muted "u-mb-8"]} (i18n/t :ui/llm-budget-sources-hint)]
     [table/data-table
      {:row-key :k
       :cols    (limit-cols (i18n/t :ui/llm-budget-source) (i18n/t :ui/llm-budget-source-cap)
                            :source :sources)
       :rows    (for [src (distinct (concat llm-sources (keys caps) (keys today)))]
                  {:k     src
                   :cap   (get caps src)
                   :used  (used-of (get today src))
                   :spent (:spent (get today src))})}]]))

(def ^:private pool-cols
  [{:label  (i18n/t :ui/llm-budget-project)
    :render (fn [{:keys [kind project]}]
              (if (= "interactive" kind)
                (i18n/t :ui/llm-budget-interactive)
                (i18n/t :ui/llm-budget-batch
                        {:project (or project (i18n/t :ui/llm-budget-no-project))})))}
   {:label (i18n/t :ui/llm-budget-spent) :cell-class "admin-nowrap" :render #(eur (:spent %))}
   {:label      ""
    :cell-class style/small-muted
    :render     (fn [{:keys [reserved]}]
                  (when (pos? reserved) (i18n/t :ui/llm-budget-in-flight {:eur (eur reserved)})))}])

(defn- pools-card [{:keys [pools day zone]}]
  [ui/callout {:class "u-mb-12"}
   [:div {:class style/section-h3} (i18n/t :ui/llm-budget-pools {:day day :zone zone})]
   [table/data-table {:rows    pools
                      :row-key :pool
                      :cols    pool-cols
                      :empty   [:div {:class style/small-muted}
                                (i18n/t :ui/llm-budget-no-calls)]}]])

(def view
  (machine/entering
   model/admin [:admin.budget/load]
   (fn []
     (let [{rdata :data error :error} (budget)
           data (rd/value-or rdata nil)
           error (or error (rd/reason rdata))]
       [:div
        [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/llm-budget-intro)]
        (when error [ui/problem error nil "u-mb-12"])
        (if (or (nil? rdata) (rd/loading? rdata))
          [busy/block {:class style/small-muted} (i18n/t :ui/loading)]
          (when data
            [:<>
             [sources-card data]
             [limits-card data]
             [quotas-card data]
             [pools-card data]]))]))))
