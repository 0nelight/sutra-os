(ns sutra-ui.apps.evals.results
  (:require [sutra-ui.ui.chart :as chart]
            [sutra-ui.ui.core :as ui :refer [link]]
            [cljs.pprint]
            [sutra-ui.ui.busy :as busy]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.evals.events :refer [max-compare]]
            [sutra-ui.apps.evals.model :as m]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra.types.eval-runs :as er]))

(defn- send! [& event] (m/send! (vec event)))

(defn- ok-class [pass?] (case pass? true "aev-ok" false "aev-bad" nil))

(defn- status-chip [status]
  (let [s (some-> status name)]
    [ui/tag {:minimal true
             :intent  (case s "passed" :success "failed" :danger nil)
             :icon    (case s "passed" :tick "failed" :cross nil)}
     (case s
       "passed"   (i18n/t :ui/ops-status-passed)
       "failed"   (i18n/t :ui/ops-status-failed)
       "no-cases" (i18n/t :ui/aev-no-cases)
       (i18n/t :ui/aev-ungraded))]))

(defn- verdict [passed]
  (cond (true? passed)  [ui/icon {:icon :tick :intent :success}]
        (false? passed) [ui/icon {:icon :cross :intent :danger}]
        :else           [ui/icon {:icon :minus :class "aev-muted"}]))

(defn- pass-of [run]
  (let [{:keys [passed total]} (:summary run)]
    (when total (str passed "/" total))))

(defn- muted [& xs] (into [:span {:class "aev-muted"}] xs))

(defn- column-chooser
  [hidden-key columns]
  [ui/popover
   {:placement :bottom-end
    :content
    (into [:div {:class "aev-columns-menu"}]
          (for [{:keys [id label]} columns]
            ^{:key id}
            [ui/checkbox
             {:label label
              :checked (not (contains? (m/at hidden-key) id))
              :on-change #(send! :eval-suites/toggle-in hidden-key id)}]))}
   [ui/tooltip {:content (i18n/t :ui/aev-columns)}
    [ui/button {:icon :menu :small true}]]])

(def ^:private group-keys
  [[nil :ui/aev-group-none] [:model :ui/aev-group-model] [:version :ui/aev-group-version]
   [:experiment :ui/aev-group-experiment] [:ref :ui/aev-group-ref]])

(defn- metric-cell [x]
  [:<> (m/fmt-value (:value x))
   (when (and (number? (:variance x)) (pos? (:variance x)))
     [muted (str " ±" (m/fmt-value (:variance x)))])])

(defn- metric-class [x] (case (some-> (:status x) name) "failed" "aev-bad" "passed" "aev-ok" nil))

(defn- total-tokens [run]
  (let [{:keys [prompt-tokens completion-tokens]} (m/usage run)]
    (when prompt-tokens (+ prompt-tokens completion-tokens))))

(defn- run-columns
  [runs metrics]
  (let [cost (fn [r] (:cost (m/usage r)))
        cur  (some #(:currency (m/usage %)) runs)
        dash (fn [f] (fn [run] (or (f run) "—")))]
    (vec
     (concat
      [{:id "passed" :label (i18n/t :ui/aev-passed)
        :class #(if (m/run-failed? %) "aev-bad" "aev-ok")
        :cell (fn [run]
                [:<> (or (pass-of run) "—")
                 (when-let [r (get-in run [:summary :pass-rate])]
                   [muted (str " · " (m/pct r))])])
        :agg (fn [rs] (m/pct (er/mean (map #(get-in % [:summary :pass-rate]) rs))))}]
      (for [nm metrics]
        {:id (str "metric:" nm) :label nm
         :class #(metric-class (m/metric-of % nm))
         :cell #(metric-cell (m/metric-of % nm))
         :agg (fn [rs] (m/fmt-value (er/mean (map #(:value (m/metric-of % nm)) rs))))})
      (for [nm (m/param-names runs)]
        {:id (str "param:" nm) :label nm :class (constantly "aev-param")
         :cell #(str (er/param-of % nm))})
      [{:id "cost" :label (i18n/t :ui/aev-cost)
        :cell (dash #(m/fmt-cost (cost %) (:currency (m/usage %))))
        :agg (fn [rs] (m/fmt-cost (er/mean (map cost rs)) cur))}
       {:id "tokens" :label (i18n/t :ui/aev-tokens) :cell (dash total-tokens)
        :agg (fn [rs] (some-> (er/mean (map total-tokens rs)) js/Math.round))}
       {:id "ms" :label (i18n/t :ui/aev-mean-ms) :cell (dash m/mean-ms)
        :agg (fn [rs] (some-> (er/mean (map m/mean-ms rs)) js/Math.round))}
       {:id "model" :label (i18n/t :ui/model) :cell (dash er/run-model)}
       {:id "version" :label (i18n/t :ui/version) :cell (dash er/run-version)}
       {:id "experiment" :label (i18n/t :ui/aev-experiment)
        :cell (dash #(get-in % [:summary :experiment :name]))}
       {:id "ref" :label (i18n/t :ui/aev-ref) :mono? true :cell :ref}
       {:id "iterations" :label (i18n/t :ui/aev-iterations) :cell :iterations}]))))

(defn- finished-note
  []
  (when-let [{:keys [name runs]} (m/at :finished)]
    [ui/callout {:compact true :intent :success}
     [page/toolbar
      (i18n/t :ui/aev-experiment-done {:name name :n runs})
      [ui/button
       {:minimal true :small true :icon :cross :on-click #(send! :eval-suites/assoc :finished nil)
        :aria-label (i18n/t :ui/close)}]]]))

(defn- group-row? [row] (contains? row ::group))

(defn- table-cols
  [shown picked columns]
  (let [pick (fn [run]
               (let [on? (boolean (some #{(:id run)} picked))]
                 [ui/checkbox
                  {:checked on? :title (i18n/t :ui/aev-pick-compare {:n max-compare})
                   :disabled (and (not on?) (>= (count picked) max-compare))
                   :on-change #(send! :eval-suites/compare (:id run))}]))]
    (concat
     [{:label "" :render #(when-not (group-row? %) (pick %))}
      {:label  (i18n/t :ui/aev-run-at)
       :render (fn [row]
                 (if (group-row? row)
                   [:<> [:b (or (::group row) "—")]
                    [muted (str " · " (count (:runs row)) " " (i18n/t :ui/aev-runs))]]
                   [ui/tooltip {:content (er/params-text row)}
                    [link {:on-click #(m/open-run! (:id row))} (:run-at row)]]))}]
     (for [{:keys [label cell agg class mono?]} shown]
       {:label      label
        :mono?      mono?
        :cell-class #(when (and class (not (group-row? %))) (class %))
        :render     #(if (group-row? %) (when agg [:b (agg (:runs %))]) (cell %))})
     [{:label [column-chooser :hidden-runs columns] :align :right :render (constantly nil)}])))

(defn runs-table [suite]
  (let [all     (m/runs-of (:id suite))
        runs    (m/shown-runs all)
        metrics (m/metric-names runs)
        k       (m/at :group-by)
        picked  (m/at :compare)
        exps    (m/experiments-of all)
        columns (run-columns runs metrics)
        shown   (remove #(contains? (m/at :hidden-runs) (:id %)) columns)
        rows    (if k
                  (mapcat (fn [[g rs]] (cons {::group g :runs rs :id (str "g" g)} rs))
                          (er/groups k runs))
                  runs)]
    [:<>
     [finished-note]
     [page/toolbar
      (when (seq exps)
        [:label (muted (i18n/t :ui/aev-experiment) " ")
         [ui/html-select
          {:value (or (m/at :experiment) "")
           :on-change #(send! :eval-suites/assoc :experiment (not-empty (.. % -target -value)))}
          [:option {:value ""} (i18n/t :ui/aev-all-runs)]
          (for [{:keys [id name runs]} exps]
            ^{:key id} [:option {:value id} (str name " (" runs ")")])]])
      [:label (muted (i18n/t :ui/aev-group-by) " ")
       [ui/html-select
        {:value (if k (name k) "")
         :on-change #(send! :eval-suites/assoc :group-by
                            (some-> (.. % -target -value) not-empty keyword))}
        (for [[gk label] group-keys]
          ^{:key (str gk)} [:option {:value (if gk (name gk) "")} (i18n/t label)])
        (for [nm (m/param-names runs)
              :when (not (#{"model" "version"} nm))]
          ^{:key nm} [:option {:value nm} nm])]]
      [muted (i18n/t :ui/aev-picked {:n (count picked) :max max-compare})]
      [ui/button
       {:intent :primary :disabled (< (count picked) 2)
        :on-click #(send! :eval-suites/assoc :tab :compare)}
       (i18n/t :ui/aev-view-test-cases)]]
     [table/data-table {:cols (table-cols shown picked columns) :rows rows :row-key :id
                        :scroll? true}]]))

(defn- block [title v]
  [ui/section {:title title :compact true}
   [ui/section-card
    (if (or (nil? v) (= "" v))
      [muted "—"]
      [:pre {:class "aev-pre"} (if (string? v) v (with-out-str (cljs.pprint/pprint v)))])]])

(defn- evaluator-tab [c run]
  (into [:<>]
        (for [mt (:metrics run)
              :let [nm    (name (:metric mt))
                    iters (m/case-metric c nm)]]
          ^{:key nm}
          [ui/section {:title nm :subtitle (some-> (get-in mt [:detail :kind]) name) :compact true}
           [ui/section-card
            (if (empty? (remove nil? iters))
              [muted (i18n/t :ui/aev-not-graded)]
              (into [:<>]
                    (map-indexed
                     (fn [i it]
                       ^{:key i}
                       [:div
                        (when (> (count iters) 1)
                          [muted (i18n/t :ui/aev-iteration {:n (inc i)}) " "])
                        [verdict (:pass? it)] " "
                        [:span {:class "aev-mono"} (m/fmt-value (:value it))]
                        (when-some [d (:debug it)]
                          [block (i18n/t :ui/aev-debug-output) (m/show d)])])
                     iters)))]])))

(defn- log-block
  [c]
  (when (seq (:trace c))
    [ui/section {:title (i18n/t :ui/aev-logs) :compact true}
     (into [ui/section-card]
           (for [[i {:keys [ms spans log]}] (map-indexed vector (:trace c))]
             ^{:key i}
             [:div
              [muted (str (i18n/t :ui/aev-iteration {:n (inc i)}) (when ms (str " · " ms " ms")))]
              (when (seq spans)
                [:div {:class "aev-mono"}
                 (str/join ", " (for [{:keys [name ms open]} spans]
                                  (str name " " (if open "…" (str ms " ms")))))])
              (when (seq log)
                [:pre {:class "aev-pre"} (str/join "\n" log)])]))]))

(defn- function-tab [c run]
  [:<>
   (when-let [e (:error c)]
     [ui/problem e nil])
   [block (i18n/t :ui/aev-input) (:input c)]
   [block (i18n/t :ui/aev-actual) (:actual c)]
   [block (i18n/t :ui/aev-expected) (:expected c)]
   [log-block c]
   [ui/button {:on-click #(m/draft-case! {:id       (str (:case-id c) "-"
                                                         (subs (str (:id run)) 0 8))
                                          :input    (:input c)
                                          :expected (if (seq (:expected c))
                                                      (:expected c)
                                                      (:actual c))})}
    (i18n/t :ui/aev-add-as-case)]])

(defn- debug-pairs
  [suite]
  (let [[run-id case-id] (m/at :debug)
        ids  (or (m/at :debug-runs) (when run-id [run-id]))
        runs (keep #(m/run-by-id (:id suite) %) ids)]
    (doseq [id ids] (m/ensure-cases! id))
    (for [r runs :let [c (m/case-by-id (m/cases-of (:id r)) case-id)] :when c] [r c])))

(defn debug-view
  [suite]
  (let [[run-id case-id tab] (m/at :debug)
        pair (debug-pairs suite)
        tab  (or tab :function)
        wide (> (count pair) 1)]
    [:<>
     [ui/tabs {:id :aev-debug :selected-tab-id tab
               :on-change #(send! :eval-suites/assoc :debug [run-id case-id %])}
      (for [[k label] [[:function :ui/aev-tab-function] [:evaluators :ui/aev-tab-evaluators]]]
        [ui/tab {:id k :title (i18n/t label)}])]
     (into [:div {:class (when wide "aev-cols")}]
           (for [[r c] pair]
             ^{:key (:id r)}
             [:div
              (when wide
                [ui/tooltip {:content (m/run-label r)}
                 [:b [verdict (:passed c)] " "
                  (or (not-empty (er/params-text r)) (m/run-label r))]])
              (if (= tab :evaluators)
                [evaluator-tab c r]
                [function-tab c r])]))]))

(defn debug-drawer [suite]
  (let [[_ case-id] (m/at :debug)
        pair (debug-pairs suite)
        wide (> (count pair) 1)]
    (when (seq pair)
      [overlay/drawer
       {:side     :right
        :size     (if wide :l :m)
        :backdrop? false
        :title    [:span (when-not wide [verdict (:passed (second (first pair)))]) " " case-id
                   (when-not wide [muted " · " (m/run-label (ffirst pair))])]
        :on-close #(send! :eval-suites/debug nil)}
       [debug-view suite]])))

(defn- open-debug! [run-id cid runs]
  (send! :eval-suites/debug run-id cid nil (when (m/at :group-cases?) (mapv :id runs))))

(defn- metric-card [{:keys [metric value variance status detail]}]
  [ui/card {:compact true}
   [ui/entity-title {:title (name metric) :tags [status-chip status]}]
   [:div {:class "aev-big"} (m/fmt-value value)
    (when (and (number? variance) (pos? variance))
      [muted (str " ±" (m/fmt-value variance))])]
   (when-let [t (:objective detail)]
     [:div [muted (i18n/t :ui/aev-objective) " " (m/show t)]])
   (when (number? (:pass-rate detail))
     [:div [muted (i18n/t :ui/aev-case-pass-rate {:rate (m/pct (:pass-rate detail))})]])])

(defn- filter-bar [cases]
  (let [n (fn [pred] (count (filter pred cases)))]
    [ui/segmented-control
     {:small true :value (m/at :filter)
      :on-value-change #(send! :eval-suites/assoc :filter %)
      :options (for [[k label pred] [[:all :ui/aev-filter-all (constantly true)]
                                     [:failed :ui/aev-filter-failed #(false? (:passed %))]
                                     [:passed :ui/aev-filter-passed #(true? (:passed %))]
                                     [:error :ui/aev-filter-error #(some? (:error %))]]]
                 {:value k :label (str (i18n/t label) " (" (n pred) ")")})}]))

(defn- keep-case? [c]
  (case (m/at :filter)
    :failed (false? (:passed c))
    :passed (true? (:passed c))
    :error  (some? (:error c))
    true))

(defn- clipped [v n] [:span {:title (m/show v)} (m/clip v n)])

(defn- last-metric [c nm] (last (remove nil? (m/case-metric c nm))))

(defn- case-table [run cases]
  (let [metrics (map (comp name :metric) (:metrics run))]
    [table/data-table
     {:scroll? true
      :row-key :case-id
      :rows    (filter keep-case? cases)
      :cols    (concat
                [{:label "" :render #(verdict (:passed %))}
                 {:label (i18n/t :ui/aev-case) :mono? true :render :case-id}
                 {:label (i18n/t :ui/aev-input) :render #(clipped (:input %) 120)}
                 {:label  (i18n/t :ui/aev-actual)
                  :render #(if-let [e (:error %)]
                             [:span {:class "aev-bad"} e]
                             (clipped (:actual %) 160))}
                 {:label (i18n/t :ui/aev-expected) :render #(clipped (:expected %) 120)}]
                (for [nm metrics]
                  {:label      nm
                   :cell-class #(ok-class (:pass? (last-metric % nm)))
                   :render     #(some-> (last-metric % nm) :value m/fmt-value)})
                [{:label  ""
                  :render (fn [c]
                            [ui/button {:small true :minimal true
                                        :on-click #(open-debug! (:id run) (:case-id c) nil)}
                             (i18n/t :ui/aev-open)])}])}]))

(defn run-view [suite]
  (let [run-id (m/at :run)
        run    (m/run-by-id (:id suite) run-id)
        _      (m/ensure-cases! run-id)
        cases  (m/cases-of run-id)
        s      (:summary run)
        others (remove #(= run-id (:id %)) (m/runs-of (:id suite)))]
    (when run
      [:<>
       [page/toolbar
        [ui/button {:minimal true :icon :arrow-left
                    :on-click #(send! :eval-suites/assoc :run nil)}
         (i18n/t :ui/aev-all-runs)]
        (when (seq others)
          [ui/html-select
           {:value ""
            :on-change (fn [e]
                         (let [v (.. e -target -value)]
                           (when (seq v)
                             (send! :eval-suites/assoc :compare [run-id v] :tab :compare))))}
           [:option {:value ""} (i18n/t :ui/aev-compare-another)]
           (for [o others] ^{:key (:id o)} [:option {:value (:id o)} (m/run-label o)])])]
       [page/header
        {:title    [:span {:class (if (m/run-failed? run) "aev-bad" "aev-ok")}
                    (or (m/pct (:pass-rate s)) "—") " · "
                    (i18n/t :ui/ops-metrics-passed {:passed (:passed s) :total (:total s)})
                    (when (pos? (or (:ungraded s) 0))
                      [muted (str " · " (i18n/t :ui/aev-ungraded-n {:n (:ungraded s)}))])]
         :subtitle (str/join
                    " · "
                    (remove nil?
                            (concat
                             [(:run-at run) (er/run-model run)
                              (some->> (er/run-version run) (str "v"))
                              (er/run-experiment run) (:ref run)
                              (i18n/t :ui/aev-iterations-n {:n (:iterations run)})]
                             (for [[k v] (:meta s)] (str (name k) "=" v))
                             (when-let [{:keys [cost currency calls]} (m/usage run)]
                               [(m/fmt-cost cost currency)
                                (i18n/t :ui/aev-calls {:n calls})
                                (some-> (m/mean-ms run) (str " ms"))]))))}]
       (into [:div {:class "aev-cards"}]
             (for [mt (:metrics run)] ^{:key (str (:metric mt))} [metric-card mt]))
       (cond
         (nil? cases) [busy/block nil (i18n/t :ui/loading)]
         (empty? cases) [ui/empty-state {:title (i18n/t :ui/aev-no-case-results)}]
         :else [:<> [filter-bar cases] [case-table run cases]])])))

(defn- case-columns
  [metrics]
  (into [{:id "input" :label (i18n/t :ui/aev-input)}
         {:id "output" :label (i18n/t :ui/aev-actual)}
         {:id "expected" :label (i18n/t :ui/aev-expected)}]
        (for [nm metrics] {:id (str "metric:" nm) :label nm})))

(defn- shown? [id] (not (contains? (m/at :hidden-cases) id)))

(defn- iteration-selector
  [run cid n]
  (let [path [(:id run) cid]
        i    (get-in (m/at :iteration) [path] (dec n))]
    [ui/html-select {:value i
                     :on-change #(send! :eval-suites/assoc-in [:iteration path]
                                        (js/parseInt (.. % -target -value)))}
     (for [x (range n)]
       ^{:key x} [:option {:value x} (i18n/t :ui/aev-iteration {:n (inc x)})])]))

(defn- iteration-body
  [c i metrics base]
  (let [out (m/show (m/iteration-output c i))]
    [:div {:class (when (and base (not= out base)) "aev-changed")}
     [verdict (m/iteration-verdict c i)]
     " "
     (when (shown? "output")
       [:span {:title out}
        (if-let [e (:error c)] [:span {:class "aev-bad"} e] (m/clip out 200))])
     (doall
      (for [nm metrics
            :when (shown? (str "metric:" nm))
            :let [it (nth (m/case-metric c nm) i nil)]]
        ^{:key nm}
        [:div {:class (or (ok-class (:pass? it)) "aev-muted")}
         (str nm " " (if it (m/fmt-value (:value it)) "—"))]))]))

(defn- case-instance
  [run c cid metrics base runs]
  (let [n (max 1 (:iterations run))]
    [:<>
     [ui/button {:small true :minimal true :on-click #(open-debug! (:id run) cid runs)}
      (i18n/t :ui/aev-open)]
     (if (or (m/at :group-iterations?) (= 1 n))
       [:<>
        (when (> n 1) [iteration-selector run cid n])
        [iteration-body c (get-in (m/at :iteration) [[(:id run) cid]] (dec n)) metrics base]]
       (for [i (range n)]
         ^{:key i}
         [:div
          [muted (i18n/t :ui/aev-iteration {:n (inc i)})]
          [iteration-body c i metrics base]]))]))

(defn- run-tag
  [r]
  [ui/tooltip {:content (m/run-label r)}
   [ui/tag {:minimal true} (or (not-empty (er/params-text r)) (:run-at r))]])

(defn- grouped-table
  [runs csets case-ids metrics]
  (let [rows (for [cid case-ids
                   :let [cs      (map #(m/case-by-id % cid) csets)
                         first-c (first (remove nil? cs))]
                   :when (keep-case? (or (some #(when (false? (:passed %)) %) cs) first-c))]
               {:cid cid :cs cs :first-c first-c
                :base (when (m/at :diff?) (m/show (:actual first-c)))})]
    [table/data-table
     {:scroll? true :row-key :cid :rows rows
      :cols (cons {:label  (i18n/t :ui/aev-case)
                   :render (fn [{:keys [cid first-c]}]
                             [:<> [:span {:class "aev-mono"} cid]
                              (when (shown? "input") [:div [clipped (:input first-c) 80]])
                              (when (shown? "expected")
                                [:div [clipped (:expected first-c) 80]])])}
                  (map-indexed
                   (fn [i r]
                     {:label  [run-tag r]
                      :render (fn [{:keys [cid cs base]}]
                                (if-let [c (nth cs i)]
                                  [case-instance r c cid metrics base runs]
                                  [muted "—"]))})
                   runs))}]))

(defn- instance-table
  [runs csets case-ids metrics]
  [table/data-table
   {:scroll? true
    :row-key #(str (:cid %) (:id (:run %)))
    :rows (for [cid case-ids
                [r cs] (map vector runs csets)
                :let [c (m/case-by-id cs cid)]
                :when (and c (keep-case? c))]
            {:cid cid :run r :c c})
    :cols (concat
           [{:label (i18n/t :ui/aev-case) :mono? true :render :cid}
            {:label (i18n/t :ui/aev-run-at) :render #(run-tag (:run %))}]
           (when (shown? "input")
             [{:label (i18n/t :ui/aev-input) :render #(clipped (:input (:c %)) 120)}])
           (when (shown? "expected")
             [{:label (i18n/t :ui/aev-expected) :render #(clipped (:expected (:c %)) 120)}])
           [{:label  (i18n/t :ui/aev-actual)
             :render (fn [{:keys [cid run c]}] [case-instance run c cid metrics nil runs])}])}])

(defn compare-view
  [suite]
  (let [ids      (m/at :compare)
        runs     (keep #(m/run-by-id (:id suite) %) ids)
        _        (doseq [id ids] (m/ensure-cases! id))
        csets    (map #(m/cases-of (:id %)) runs)
        case-ids (distinct (for [cs csets c cs] (:case-id c)))
        metrics  (m/metric-names runs)
        toggle   (fn [k label]
                   [ui/checkbox {:inline true :checked (boolean (m/at k)) :label (i18n/t label)
                                 :on-change #(send! :eval-suites/flip k)}])]
    [:<>
     [page/toolbar
      [ui/button {:minimal true :icon :arrow-left
                  :on-click #(send! :eval-suites/assoc :tab :results)}
       (i18n/t :ui/aev-all-runs)]
      (toggle :group-cases? :ui/aev-group-cases)
      (toggle :group-iterations? :ui/aev-group-iterations)
      (toggle :diff? :ui/aev-view-diff)
      [column-chooser :hidden-cases (case-columns metrics)]]
     [table/data-table
      {:row-key identity
       :rows    metrics
       :cols    (cons {:label (i18n/t :ui/aev-metric) :render #(vector :b %)}
                      (for [r runs]
                        {:label      [:span [run-tag r] " " [muted (:run-at r)] " "
                                      [:span {:class (if (m/run-failed? r) "aev-bad" "aev-ok")}
                                       (pass-of r)]]
                         :cell-class #(metric-class (m/metric-of r %))
                         :render     #(m/fmt-value (:value (m/metric-of r %)))}))}]
     (cond
       (some nil? csets)    [busy/block nil (i18n/t :ui/loading)]
       (m/at :group-cases?) [grouped-table runs csets case-ids metrics]
       :else                [instance-table runs csets case-ids metrics])]))

(defn- line-chart
  [title points]
  [ui/card {:compact true}
   [:b title]
   [chart/chart {:rows       (filterv (comp number? :v) points)
                 :x :label :y :v :chart-kind :line+markers :height 90
                 :on-point   #(m/open-run! (:id %))}]])

(defn dashboard [suite]
  (let [runs    (reverse (take 30 (m/runs-of (:id suite))))
        metrics (m/metric-names runs)
        latest  (last runs)
        _       (some-> latest :id m/ensure-cases!)
        cases   (some-> latest :id m/cases-of)]
    (if (empty? runs)
      [ui/empty-state {:title (i18n/t :ui/aev-no-runs)}]
      [:<>
       (into [:div {:class "aev-cards"}
              [line-chart (i18n/t :ui/aev-pass-rate)
               (for [r runs] {:v (get-in r [:summary :pass-rate]) :id (:id r)
                              :label (m/run-label r)})]]
             (for [nm metrics]
               ^{:key nm}
               [line-chart nm
                (for [r runs :let [x (m/metric-of r nm)]]
                  {:v (:value x) :id (:id r) :label (m/run-label r)})]))
       (when (seq cases)
         [ui/section {:title (i18n/t :ui/aev-case-matrix {:at (:run-at latest)}) :compact true}
          [table/data-table
           {:scroll?      true
            :row-key      :case-id
            :rows         cases
            :on-row-click #(send! :eval-suites/debug (:id latest) (:case-id %) nil nil)
            :cols         (cons {:label  (i18n/t :ui/aev-case)
                                 :render #(vector :span [verdict (:passed %)] " "
                                                  [:span {:class "aev-mono"} (:case-id %)])}
                                (for [nm metrics]
                                  {:label      nm
                                   :cell-class #(ok-class (:pass? (last-metric % nm)))
                                   :render     (fn [c]
                                                 (when-let [it (last-metric c nm)]
                                                   [:span {:title (m/show (:debug it))}
                                                    (m/fmt-value (:value it))]))}))}]])])))
