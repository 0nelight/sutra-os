(ns sutra-ui.apps.evals.core
  (:require [sutra-ui.ui.core :as ui]
            [cljs.pprint]
            [sutra-ui.apphost.platform :as platform]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra-ui.ui.busy :as busy]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.apps.evals.model :as m]
            [sutra-ui.apps.evals.results :as results]))

(defn- target-of [suite]
  (let [spec (:spec suite)]
    (some (fn [k] (when-some [t (get spec k)] [k (name t)]))
          [:function :action :agent])))

(defn- target-link [suite]
  (when-let [[kind t] (target-of suite)]
    [:span {:class "aev-muted"} (str (name kind) " · " t)]))

(defn- send! [& event] (m/send! (vec event)))

(defn- suite-item [{:keys [name latest declared] :as suite}]
  (let [s   (:summary latest)
        on? (= name (m/at :sel))]
    [ui/button {:minimal true :fill true :align-text :start :active on?
                :on-click #(m/select-suite! name)}
     [ui/entity-title
      {:title    name
       :tags     (when declared [ui/tag {:minimal true :intent :primary}
                                 (i18n/t :ui/wfl-evals-declared)])
       :subtitle [:<>
                  [:div (some->> (target-of suite) (map clojure.core/name) (str/join " · "))]
                  (if latest
                    [:div {:class (if (m/run-failed? latest) "aev-bad" "aev-ok")}
                     [ui/icon {:icon (if (m/run-failed? latest) :cross :tick) :size 12}] " "
                     (i18n/t :ui/ops-metrics-passed {:passed (:passed s) :total (:total s)})]
                    [:div (i18n/t :ui/wfl-evals-not-run)])]}]]))

(defn- register-form []
  (let [txt (r/atom "")]
    (fn []
      (let [{:keys [busy? err]} (m/at :register)
            fail! #(send! :eval-suites/register-failed %)]
        [ui/section-card
         [:div {:class "aev-muted"} (i18n/t :ui/ops-suite-register-hint)]
         [ui/text-area {:fill true :class "bp-monospace-text" :rows 8 :value @txt
                        :on-change #(reset! txt (.. % -target -value))}]
         (when err [ui/problem err nil])
         [ui/button
          {:intent   :primary
           :disabled (or busy? (str/blank? @txt))
           :on-click (fn [_]
                       (let [ok (try (reader/read-string @txt)
                                     (catch :default e (fail! (m/err-text e)) nil))]
                         (cond
                           (not (map? ok)) (fail! (i18n/t :ui/ops-suite-needs-map))
                           (not (:name ok)) (fail! (i18n/t :ui/ops-suite-needs-name))
                           :else (send! :eval-suites/register ok))))}
          (i18n/t (if busy? :ui/ops-saving :ui/ops-suite-register))]]))))

(defn- sidebar []
  (let [open? (m/at :register-open?)
        s     (m/at :suites)]
    [:<>
     [page/header {:title (i18n/t :ui/aev-suites)}
      [ui/button {:small true :icon (if open? :cross :plus)
                  :aria-label (i18n/t :ui/ops-suite-register)
                  :on-click #(send! :eval-suites/assoc :register-open? (not open?)
                                    :register nil)}]]
     (when open? [register-form])
     (cond
       (= :loading s) [busy/block nil (i18n/t :ui/loading)]
       (:error s)     [ui/problem (:error s) nil]
       (empty? s)     [ui/empty-state {:title (i18n/t :ui/ops-suites-none)}]
       :else (for [x s] ^{:key (:id x)} [suite-item x]))]))

(defn- field [label & body]
  (into [ui/form-group {:label label}] body))

(defn- edn-text [v] (if (some? v) (with-out-str (cljs.pprint/pprint v)) ""))

(defn- case-form
  [suite draft]
  (r/with-let [f (r/atom {:id       (str (:id draft))
                          :input    (edn-text (:input draft))
                          :expected (edn-text (:expected draft))})]
    (let [{:keys [busy? err done]} (m/at :case-form)
          put  (fn [k] #(swap! f assoc k (.. % -target -value)))
          read (fn [t] (when-not (str/blank? t) (reader/read-string t)))]
      [ui/section-card
       [field (i18n/t :ui/aev-case)
        [ui/input-group {:small true :value (:id @f) :on-change (put :id)}]]
       [field (i18n/t :ui/aev-input)
        [ui/text-area {:fill true :class "bp-monospace-text" :rows 4 :value (:input @f)
                       :placeholder "{:dokument \"…\"}" :on-change (put :input)}]]
       [field (i18n/t :ui/aev-expected)
        [ui/text-area {:fill true :class "bp-monospace-text" :rows 4 :value (:expected @f)
                       :on-change (put :expected)}]]
       (when err [ui/problem err nil])
       (when done [:div {:class "aev-muted"} done])
       [page/toolbar
        [ui/button
         {:intent   :primary
          :disabled busy?
          :on-click (fn [_]
                      (when-let [c (try {:id       (str/trim (:id @f))
                                         :input    (read (:input @f))
                                         :expected (read (:expected @f))}
                                        (catch :default e
                                          (send! :eval-suites/case-invalid (m/err-text e))
                                          nil))]
                        (send! :eval-suites/add-case suite c)))}
         (i18n/t :ui/aev-add-case)]
        [ui/button {:on-click #(send! :eval-suites/drop-draft)}
         (i18n/t :ui/cancel)]]])))

(defn- mono [v] [:span {:class "aev-mono"} (m/show v)])

(defn- clipped [v n] [:span {:title (m/show v)} (m/clip v n)])

(defn- definition [suite]
  (let [{:keys [cases evaluators cases-from iterations]} (:spec suite)]
    [:<>
     [ui/section {:compact true
                  :title   (str (i18n/t :ui/aev-evaluators) " (" (count evaluators) ")")}
      [table/data-table
       {:row-key #(str (:name %))
        :rows    evaluators
        :cols    [{:label (i18n/t :ui/name) :render #(vector :b (m/show (:name %)))}
                  {:label (i18n/t :ui/aev-kind) :render #(m/show (:kind %))}
                  {:label (i18n/t :ui/aev-field) :render #(mono (:field %))}
                  {:label (i18n/t :ui/aev-objective) :render #(mono (:objective %))}]}]]
     (when (seq cases-from)
       [ui/section {:compact true :title (i18n/t :ui/aev-object-set-cases)}
        (into [ui/section-card]
              (for [[i c] (map-indexed vector cases-from)]
                ^{:key i} [:pre {:class "aev-pre"} (m/show c)]))])
     [ui/section {:compact true
                  :title   (str (i18n/t :ui/aev-test-cases) " (" (count cases) ")"
                                (when iterations
                                  (str " · " (i18n/t :ui/aev-iterations-n {:n iterations}))))}
      [table/data-table
       {:scroll? true
        :rows    (map-indexed vector cases)
        :row-key first
        :empty   [ui/empty-state {:title (i18n/t :ui/evals-no-cases)}]
        :cols    [{:label  (i18n/t :ui/aev-case)
                   :render (fn [[i c]] [:span {:class "aev-mono"}
                                        (or (some-> (:id c) m/show) (str i))])}
                  {:label (i18n/t :ui/aev-input) :render (fn [[_ c]] (clipped (:input c) 200))}
                  {:label  (i18n/t :ui/aev-expected)
                   :render (fn [[_ c]] (clipped (:expected c) 200))}]}]]
     (if-let [d (m/at :draft-case)]
       ^{:key (str (:id d))} [case-form suite d]
       [ui/button {:icon :plus :on-click #(m/draft-case! {})} (i18n/t :ui/aev-add-case)])
     (when (:declared suite)
       [:div {:class "aev-muted"} (i18n/t :ui/aev-declared-hint)])]))

(defn- parse-meta [s]
  (into {} (for [line (str/split-lines (or s ""))
                 :let [[k v] (map str/trim (str/split line #"=" 2))]
                 :when (and (seq k) (seq v))]
             [(keyword k) v])))

(defn- value-lines [text]
  (vec (remove str/blank? (map str/trim (str/split-lines (or text ""))))))

(defn- experiment-params
  [rows]
  (into {} (for [{:keys [param values]} rows
                 :let [vs (value-lines values)]
                 :when (and (seq param) (seq vs))]
             [(keyword param) (mapv m/parse-value vs)])))

(defn- run-body [suite {:keys [iterations model version meta exp-on? exp-name exp-rows]}]
  (let [params (when exp-on? (experiment-params exp-rows))]
    (cond-> {:suite (:name suite)}
      (pos? (js/parseInt iterations)) (assoc :iterations (js/parseInt iterations))
      (seq model) (assoc :model model)
      (seq version) (assoc :version version)
      (seq (parse-meta meta)) (assoc :meta (parse-meta meta))
      (seq params) (assoc :experiment {:name (if (str/blank? exp-name) "experiment" exp-name)
                                       :params params}))))

(defn- param-row
  [suite row on-change on-remove]
  (let [known  (:parameters suite)
        spec   (some #(when (= (:param row) (:name %)) %) known)
        extend #(on-change (update row :values
                                   (fn [t] (str (when-not (str/blank? t) (str t "\n")) %))))]
    [page/toolbar
     (if (seq known)
       [ui/html-select {:value (or (:param row) "")
                        :on-change #(on-change (assoc row :param (.. % -target -value)))}
        [:option {:value ""} (i18n/t :ui/aev-pick-param)]
        (for [{:keys [name kind required]} known]
          ^{:key name}
          [:option {:value name}
           (str name (case (keyword kind)
                       (:model :language-model) (str " · " (i18n/t :ui/model))
                       :version                 (str " · " (i18n/t :ui/version))
                       (when required " *")))])]
       [ui/input-group {:small true :value (or (:param row) "")
                        :placeholder (i18n/t :ui/aev-pick-param)
                        :on-change #(on-change (assoc row :param (.. % -target -value)))}])
     (when (seq (:options spec))
       [ui/html-select
        {:value ""
         :on-change #(let [v (.. % -target -value)] (when (seq v) (extend v)))}
        [:option {:value ""} (i18n/t :ui/aev-add-value)]
        (for [o (:options spec)] ^{:key o} [:option {:value o} o])])
     [ui/text-area
      {:class "bp-monospace-text" :rows 3 :value (or (:values row) "")
       :placeholder (i18n/t :ui/aev-values-ph)
       :on-change #(on-change (assoc row :values (.. % -target -value)))}]
     [ui/tooltip {:content (i18n/t :ui/close)}
      [ui/button
       {:minimal true :small true :icon :cross :on-click on-remove
        :aria-label (i18n/t :ui/close)}]]]))

(defn- combination-table [{:keys [combinations runs answers]}]
  (let [ks (sort-by str (distinct (mapcat keys combinations)))]
    [ui/section {:compact true :title (i18n/t :ui/aev-preview-of {:runs runs :answers answers})}
     [table/data-table
      {:scroll? true
       :rows    (map-indexed vector combinations)
       :row-key first
       :cols    (cons {:label "#" :render #(inc (first %))}
                      (for [k ks] {:label (name k) :render #(str (get (second %) k))}))}]]))

(defn- experiment-section
  [suite f]
  (let [preview (get-in (m/at :run-form) [(:name suite) :preview])
        {:keys [exp-on? exp-name exp-rows]} @f
        rows    (vec exp-rows)
        params  (experiment-params rows)
        runs    (reduce * (map count (vals params)))]
    [:<>
     [ui/checkbox {:checked (boolean exp-on?)
                   :label-element [:b (i18n/t :ui/aev-experiment)]
                   :on-change #(swap! f update :exp-on? not)}]
     (when exp-on?
       [:<>
        [:div {:class "aev-muted"} (i18n/t :ui/aev-experiment-hint)]
        [field (i18n/t :ui/name)
         [ui/input-group {:small true :value (or exp-name "")
                          :on-change #(swap! f assoc :exp-name (.. % -target -value))}]]
        [field (i18n/t :ui/aev-experiment-params)
         (for [[i row] (map-indexed vector rows)]
           ^{:key i}
           [param-row suite row
            #(swap! f assoc-in [:exp-rows i] %)
            #(swap! f update :exp-rows
                    (fn [rs] (into (subvec (vec rs) 0 i) (subvec (vec rs) (inc i)))))])
         [ui/button {:small true :icon :plus
                     :on-click #(swap! f update :exp-rows (fnil conj []) {})}
          (i18n/t :ui/aev-add-param)]]
        (when (seq params)
          [page/toolbar
           [:b (i18n/t :ui/aev-runs-n {:n runs})]
           [ui/button
            {:small true
             :on-click #(send! :eval-suites/preview (:name suite) (run-body suite @f))}
            (i18n/t :ui/aev-preview)]])
        (when (and preview (seq params))
          [combination-table preview])])]))

(defn- run-config [_suite]
  (let [f (r/atom {})]
    (fn [suite]
      (let [running (m/at :running)
            mine?   (= (:name suite) (get-in running [:suite :name]))
            busy    (and mine? (:busy? running))
            err     (or (when mine? (:err running))
                        (get-in (m/at :run-form) [(:name suite) :err]))
            [kind] (target-of suite)
            put (fn [k] #(swap! f assoc k (.. % -target -value)))
            exp? (:experiment (run-body suite @f))
            has-model? (some #(= "model" (:name %)) (:parameters suite))]
        [:<>
         [:div {:class "aev-muted"} (i18n/t :ui/aev-run-hint)]
         [field (i18n/t :ui/aev-iterations)
          [ui/numeric-input
           {:small true :min 1 :minor-step-size nil :value (:iterations @f)
            :placeholder (i18n/t :ui/aev-iterations-ph)
            :on-value-change (fn [_ s _] (swap! f assoc :iterations s))}]]
         (when has-model?
           [field (i18n/t :ui/model)
            [ui/input-group
             {:small true :value (:model @f)
              :placeholder (i18n/t :ui/aev-model-ph) :on-change (put :model)}]])
         (if (= kind :agent)
           [field (i18n/t :ui/version)
            [ui/html-select {:value (or (:version @f) "")
                             :on-change (put :version)}
             [:option {:value ""} (i18n/t :ui/evals-against-published)]
             [:option {:value "draft"} (i18n/t :ui/evals-against-draft)]]]
           (when (seq (:versions suite))
             [field (i18n/t :ui/version)
              (into [ui/html-select {:value (or (:version @f) "")
                                     :on-change (put :version)}
                     [:option {:value ""} (i18n/t :ui/evals-against-saved)]]
                    (for [v (rseq (vec (:versions suite)))]
                      [:option {:key v :value v} v]))]))
         [field (i18n/t :ui/aev-meta)
          [ui/text-area {:fill true :class "bp-monospace-text" :rows 2 :value (:meta @f)
                         :placeholder "prompt=v2" :on-change (put :meta)}]]
         [experiment-section suite f]
         (when err [ui/problem err nil])
         [ui/button
          {:intent :primary :disabled busy
           :on-click #(send! :eval-suites/start (select-keys suite [:id :name])
                             (run-body suite @f))}
          (cond
            busy  (str (i18n/t :ui/ops-scoring)
                       (when-let [{:keys [done total]} (:progress running)]
                         (when total (str " " (or done 0) "/" total))))
            exp?  (i18n/t :ui/aev-run-experiment)
            :else (i18n/t :ui/ops-run-eval))]
         (when-let [{:keys [done total]} (when busy (:progress running))]
           (when (and total (pos? total))
             [ui/progress-bar {:value (/ (or done 0) total) :intent :primary}]))]))))

(def ^:private tabs
  [[:results :ui/aev-tab-results] [:dashboard :ui/aev-tab-dashboard]
   [:cases :ui/aev-tab-cases] [:run :ui/aev-tab-run]])

(defn- suite-view [suite]
  (let [tab  (m/at :tab)
        runs (get (m/at :runs) (:id suite))]
    [:<>
     [page/header
      {:title    (:name suite)
       :subtitle [:<> [target-link suite]
                  (str " · " (i18n/t :ui/wfl-evals-shape
                                     {:cases      (count (get-in suite [:spec :cases]))
                                      :evaluators (count (get-in suite [:spec :evaluators]))}))]}
      [ui/button {:icon :refresh :small true :aria-label (i18n/t :ui/refresh)
                  :on-click #(send! :eval-suites/runs (:id suite))}]]
     [page/toolbar
      [ui/tabs {:id :aev-suite
                :selected-tab-id (if (= tab :compare) :results tab)
                :on-change #(send! :eval-suites/assoc :tab %)}
       (for [[k label] tabs]
         [ui/tab {:id k :title (i18n/t label)}])]]
     (case tab
       :cases [definition suite]
       :run   [run-config suite]
       (cond
         (= :loading runs)  [busy/block nil (i18n/t :ui/loading)]
         (:error runs)      [ui/problem (:error runs) nil]
         (empty? runs)      [ui/empty-state {:title (i18n/t :ui/aev-no-runs)}]
         (= tab :dashboard) [results/dashboard suite]
         (= tab :compare)   [results/compare-view suite]
         (m/at :run)        [results/run-view suite]
         :else              [results/runs-table suite]))
     [results/debug-drawer suite]]))

(def ^:private suite-page
  (machine/entering m/evals #(vector :eval-suites/ensure-runs (:id %)) suite-view))

(defn- detail []
  (if-let [s (m/selected)]
    ^{:key (:name s)} [suite-page s]
    [ui/empty-state {:icon :lab-test :title (i18n/t :ui/ops-evals-hint)}]))

(def ^:private open
  (reify
    IDeref (-deref [_] (cond (m/selected) [(m/at :sel)]
                             (not (groesse/kompakt?)) [:hint]))
    IReset (-reset! [_ _] (m/select-suite! nil) nil)))

(page/view! :evals/suites sidebar)
(page/view! :evals/suite detail)

(def ^:private tree
  (page/liste-detail {:list   {:kind :rows :scroll? true :padding :compact
                               :items [(page/view :evals/suites)]}
                      :detail {:kind :flow :padding :regular
                               :items [(page/view :evals/suite)]}
                      :open   open
                      :list-w 300
                      :resize :evals/suites}))

(def app
  (machine/entering
   m/evals [:eval-suites/load]
   (fn [] [platform/module :evals tree])))
