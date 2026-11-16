(ns sutra-ui.apps.assistant.evals
  (:require [sutra.kit.remote :as rd]
            [sutra-ui.ui.core :as ui :refer [html-table]]
            [clojure.string :as str]
            [sutra-ui.ui.busy :as busy]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.assistant.evals-events :as ee]
            [sutra-ui.state.machine :as machine]))

(defonce ^:private ui (r/atom ee/initial))

(defonce ^:private tests (machine/frame ui {:name :assistant/evals}))

(defn- send! [event] (machine/dispatch event {:frame tests}))

(defn load! [] (send! [:evals/load]))

(defn- metric [run nm] (some #(when (= nm (name (:metric %))) %) (:metrics run)))

(defn- judge-of [run i]
  (some #(when (= i (:case %)) %) (get-in (metric run "richter") [:detail :detail :scores])))

(defn- contains-ok? [run i kase]
  (when (seq (get-in kase [:expected :answer]))
    (not (some #{i} (get-in (metric run "enthaelt") [:detail :detail :misses])))))

(defn- pct [v] (when (number? v) (str (js/Math.round (* 100 v)) " %")))

(defn- latest-per-model
  [runs]
  (->> runs
       (reduce (fn [acc r]
                 (let [m (get-in r [:summary :model])]
                   (if (some #(= m (get-in % [:summary :model])) acc) acc (conj acc r)))) [])
       reverse vec))

(defn- cell [run i kase]
  (let [j   (judge-of run i)
        c   (get-in run [:summary :cases i])
        ok? (contains-ok? run i kase)
        good? (and (or (nil? j) (>= (:score j) 0.7)) (not (false? ok?)) (not (:error c)))]
    [:td {:class (str "evals__cell " (if good? "is-good" "is-bad"))}
     [:div {:class "evals__score"}
      (if j (str (if good? "✓ " "✗ ") (pct (:score j))) (if good? "✓" "✗"))
      (when (some? ok?)
        [:span {:class "evals__hint"}
         (if ok? (i18n/t :ui/evals-has-figures) (i18n/t :ui/evals-misses-figures))])]
     (when-let [why (:reason j)] [:div {:class "evals__reason"} why])
     (when-let [e (:error c)] [:div {:class "evals__reason evals__reason--err"} e])
     (when-let [a (not-empty (:answer c))]
       [ui/section
        {:title (i18n/t :ui/evals-answer) :collapsible true :compact true
         :collapse-props {:default-is-open false} :class "evals__answer"}
        [ui/section-card
         [:div a]]])]))

(defn- model-picks [suite models agent-model]
  (let [picked (get-in @ui [:picked suite] #{agent-model})]
    [:div {:class "evals__models"}
     [:span {:class "evals__hint"} (i18n/t :ui/evals-models)]
     (for [{:keys [id source]} models]
       ^{:key id}
       [ui/checkbox
        {:class "evals__model u-m-0" :checked (contains? picked id) :label id
         :on-change #(send! [:evals/pick suite id agent-model])}
        (when source [:span {:class "evals__hint"} (str " (" source ")")])])]))

(defn- suite-view [{:keys [name agent cases runs running failed]} models agents]
  (let [agent-model (some #(when (= agent (:name %)) (:model %)) agents)
        picked      (get-in @ui [:picked name] #{agent-model})
        latest      (latest-per-model runs)]
    [ui/card {:class "evals__suite"}
     [:div {:class "evals__suite-head"}
      [:strong (i18n/t :ui/evals-suite {:agent agent :n (count cases)})]
      [:span {:class "evals__spacer"}]
      (if (seq running)
        [:span {:class "evals__running"}
         (i18n/t :ui/evals-running {:models (str/join ", " running)})]
        [ui/button
         {:intent :primary :disabled (or (empty? cases) (empty? picked))
          :on-click #(send! [:evals/run name picked (get-in @ui [:version name])])}
         (i18n/t :ui/evals-run {:n (count picked)})])]
     [model-picks name models agent-model]
     [:label {:class "evals__models"}
      [:span {:class "evals__hint"} (i18n/t :ui/evals-against)]
      [ui/html-select
       {:value (or (get-in @ui [:version name]) "")
        :on-change #(send! [:evals/version name (not-empty (.. % -target -value))])}
       [:option {:value ""} (i18n/t :ui/evals-against-published)]
       [:option {:value "draft"} (i18n/t :ui/evals-against-draft)]]]
     (for [[m e] failed] ^{:key m} [ui/problem (str m ": " e) nil])
     (if (empty? cases)
       [:div {:class "assistant__muted"} (i18n/t :ui/evals-no-cases)]
       [:div {:class "evals__table-wrap"}
        [html-table {:compact true :bordered true :class "evals__table"}
         [:thead
          [:tr [:th (i18n/t :ui/evals-question)] [:th (i18n/t :ui/evals-expected)]
           (for [r latest] ^{:key (:id r)}
                [:th (get-in r [:summary :model])
                 [:div {:class "evals__hint"}
                  (str (pct (get-in r [:summary :by-metric :richter])) " · "
                       (.toLocaleString (js/Date. (:run-at r))))]])
           [:th]]]
         [:tbody
          (for [[_i kase] (map-indexed vector cases)]
            ^{:key (:id kase)}
            [:tr
             [:td {:class "evals__q"}
              (let [in (:input kase)]
                (if (map? in)
                  [:<> (:question in)
                   [:div {:class "evals__hint"}
                    (i18n/t :ui/evals-followup {:n (count (:history in))})]]
                  in))]
             [:td {:class "evals__exp"}
              (when-let [ref (get-in kase [:expected :reference])] [:div ref])
              (when-let [must (seq (get-in kase [:expected :answer]))]
                [:div {:class "evals__hint"}
                 (i18n/t :ui/evals-must {:figures (str/join ", " must)})])]
             (for [r latest]
               (let [ri (some (fn [[k c]] (when (= (:id kase) (:id c)) k))
                              (map-indexed vector (get-in r [:summary :cases])))]
                 (if ri
                   ^{:key (:id r)} [cell r ri kase]
                   ^{:key (:id r)} [:td {:class "evals__cell"} "—"])))
             [:td [ui/tooltip {:content (i18n/t :ui/evals-delete)}
                   [ui/button
                    {:minimal true :small true :icon :cross :aria-label (i18n/t :ui/evals-delete)
                     :on-click #(send! [:evals/delete-case name (:id kase)])}]]]])]]])]))

(def view
  (machine/entering
   tests [:evals/load] [:evals/leave]
   (fn [{:keys [models agents]}]
     (let [{rdata :data error :error} @ui
           data  (rd/value-or rdata nil)
           error (or error (rd/reason rdata))]
       [:div {:class "prov"}
        [:div {:class "prov__intro"} (i18n/t :ui/evals-intro)]
        (when error [ui/callout {:intent :danger :class "assistant__error"} error
                     [ui/button
                      {:minimal true :small true :icon :cross :aria-label (i18n/t :ui/close)
                       :on-click #(send! [:evals/clear-error])}]])
        (cond
          (rd/loading? rdata)    [busy/block {:class "assistant__muted"} (i18n/t :ui/loading)]
          (nil? data)             nil
          (empty? (:suites data)) [ui/empty-state {:icon :clipboard
                                                   :title (i18n/t :ui/evals-empty)
                                                   :hint (i18n/t :ui/evals-empty-hint)}]
          :else (for [s (:suites data)] ^{:key (:name s)} [suite-view s models agents]))]))))

(defn save-case
  [_]
  (let [f     (machine/frame (r/atom ee/case-initial) {:name :assistant/save-case :lifetime :view})
        send! #(machine/dispatch % {:frame f})]
    (fn [{:keys [answer history] :as ctx}]
      [:<>
       [machine/leaving f]
       (let [{:keys [open reference must saved? error]} @(:db f)]
         (cond
           saved? [:div {:class "evals__saved"} (i18n/t :ui/evals-saved)]
           (nil? open)
           [:div {:class "evals__thumbs"}
            [ui/tooltip {:content (i18n/t :ui/evals-good)}
             [ui/button
              {:minimal true :small true :icon :thumbs-up
               :on-click #(send! [:evals.case/good answer]) :aria-label (i18n/t :ui/evals-good)}]]
            [ui/tooltip {:content (i18n/t :ui/evals-bad)}
             [ui/button
              {:minimal true :small true :icon :thumbs-down
               :on-click #(send! [:evals.case/bad]) :aria-label (i18n/t :ui/evals-bad)}]]]
           :else
           [:div {:class "prov__form evals__form"}
            [:div {:class "evals__hint"}
             (i18n/t (if (= :good open) :ui/evals-form-good :ui/evals-form-bad))]
            (when (seq history)
              [:div {:class "evals__hint"} (i18n/t :ui/evals-with-history {:n (count history)})])
            [ui/text-area
             {:class "assistant__input" :rows 4 :value reference
              :placeholder (i18n/t :ui/evals-reference-ph)
              :on-change #(send! [:evals.case/field :reference (.. % -target -value)])}]
            [ui/input-group
             {:class "prov__input--wide" :value must
              :placeholder (i18n/t :ui/evals-must-ph)
              :on-change #(send! [:evals.case/field :must (.. % -target -value)])}]
            (when error [:div {:class "assistant__step-error"} error])
            [:div {:class "prov__row"}
             [ui/button
              {:intent :primary :disabled (and (str/blank? reference) (str/blank? must))
               :on-click #(send! [:evals.case/save ctx load!])}
              (i18n/t :ui/evals-save)]
             [ui/button
              {:outlined true :on-click #(send! [:evals.case/cancel])}
              (i18n/t :ui/cancel)]]]))])))
