(ns sutra-ui.apps.doc-intel.core
  (:require [sutra-ui.ui.core :as ui :refer [tag]]
            [clojure.string :as str]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.filter :as ui-filter]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.doc-intel.events :as ev]
            [sutra-ui.apps.doc-intel.model :as m]
            [sutra-ui.apphost.widgets.markdown :as md]
            [sutra-ui.apphost.widgets.pdf :as pdf]
            [sutra-ui.apphost.platform :as platform]
            [sutra-ui.storage :as storage]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]))

(defonce ^:private ui (shell/slice :document-intelligence))

(defonce ^:private doc-intel (machine/frame ui {:name :doc-intel}))

(defn- send! [event] (machine/dispatch event {:frame doc-intel}))

(machine/reg-sub ::st (fn [db _] (or db (m/initial nil))))

(defn- st [] @(machine/subscribe [::st] {:frame doc-intel}))

(def ^:private item-cap 300)

(defn- t [k & [params]] (i18n/t (keyword "ui" (str "dint-" k)) params))

(defn- kind-label [kind] (t (str "kind-" kind)))

(defn- kind-badge [kind] [:span {:class (str "dint-kind dint-kind--" kind)} (kind-label kind)])

(defn- strategy-detail [{:keys [strategy model preprocess mode]}]
  (if (= "vlm" strategy)
    (str/join " · " (remove nil? [model (when (and preprocess (not= "none" preprocess))
                                          (str "+ " (t (str "pre-" preprocess))))]))
    (when (and mode (not= "raw-text" strategy) (not= "AUTO" mode)) mode)))

(defn- seconds [ms] (when (number? ms) (str (.toFixed (/ ms 1000) 1) " s")))

(defn- dollars [c] (when (number? c) (str "$" (.toFixed c (if (< c 0.01) 4 3)))))

(defn- tokens [{:keys [tokens_in tokens_out]}]
  (when (or tokens_in tokens_out) (str (or tokens_in "–") " / " (or tokens_out "–"))))

(defn- score-text [x] (if (number? x) (.toFixed x 1) "–"))

(defn- score-class [x]
  (str "dint-pill " (cond (not (number? x)) "dint-pill--none" (>= x 4.5) "dint-pill--good"
                          (>= x 3.5) "dint-pill--ok" :else "dint-pill--bad")))

(defn- on-value [f] #(f (.. % -target -value)))

(defn- file-kind [it] (cond (m/pdf? it) "pdf" (m/image? it) "img" :else "other"))

(defn- source-picker [st]
  (let [ss (:sources st)]
    (cond
      (= :loading ss) [busy/block {:class "dint-muted"} (i18n/t :ui/loading)]
      (:error ss)     [ui/problem (:error ss) nil]
      (empty? ss)     [ui/empty-state {:icon :database :title (t "no-sources")}]
      :else
      [ui/html-select
       {:fill true :value (some-> (:source st) m/source-key)
        :on-change (on-value (fn [k] (send! [:doc-intel/source
                                                (some #(when (= k (m/source-key %)) %) ss)])))}
       (for [s ss]
         ^{:key (m/source-key s)}
         [:option {:value (m/source-key s)}
          (str (:name s) " — " (t (str "source-" (:kind s))))])])))

(defn- item-row [st it]
  (let [k (file-kind it)]
    [ui/tooltip {:content (str (:label it) "\n" (:address it))}
     [ui/button {:minimal true :fill true :align-text :start
                 :active (= (:address it) (:address (:item st)))
                 :on-click #(send! [:doc-intel/open-item it])}
      [:span {:class (str "dint-file dint-file--" k)} (t (str "file-" k))]
      [:span.dint-item-name (if (= (:label it) (:address it))
                              (str (t "document") " " (subs (:address it) 0 8))
                              (:label it))]]]))

(defn- item-list [st]
  (let [raw (get (:items st) (some-> (:source st) m/source-key))
        hit (m/matching (m/items-of st) (:q st))]
    (cond
      (nil? (:source st)) nil
      (= :loading raw)    [busy/block {:class "dint-muted"} (i18n/t :ui/loading)]
      (:error raw)        [ui/problem (:error raw) nil]
      :else
      [:<>
       [:div.dint-muted (t "items-count" {:n (count hit)})]
       (into [:div]
             (for [it (take item-cap hit)] ^{:key (:address it)} [item-row st it]))
       (when (> (count hit) item-cap)
         [:div.dint-muted (t "items-more" {:n (- (count hit) item-cap)})])])))

(defn- sidebar []
  (let [st (st)]
    [:div.dint-media
     [:div.dint-eyebrow (t "media")]
     [source-picker st]
     [ui-filter/bar {:value (:q st) :placeholder (t "search")
                  :on-search #(send! [:doc-intel/query %])}]
     [item-list st]]))

(defn- welcome [st]
  [:div.dint-welcome
   [:h1.dint-h1 (i18n/t :ui/app-document-intelligence)]
   [:p.dint-lead (t "lead")]
   (into [:ol.dint-steps]
         (for [n (range 1 6)]
           [:li [:span.dint-step-n n]
            [:div [:b (t (str "step-" n))] [:span (t (str "step-" n "-text"))]]]))
   [:div.dint-eyebrow (t "strategies")]
   (into [:div.dint-cards]
         (for [k m/strategy-kinds]
           [:div {:class (str "dint-card dint-card--" k)}
            [kind-badge k]
            [:p (t (str "kind-" k "-text"))]
            [:div.dint-muted (t (str "kind-" k "-cost"))]]))
   (when (seq (:strategies st))
     [:div.dint-muted (t "configured" {:n (count (:strategies st))})])])

(defn- preview [st]
  (let [{:keys [item source]} st
        focus (some->> (:focus st) (m/strategy-of st))
        marks (some-> (when focus (:row (m/result st focus))) m/block-marks)
        src   (m/blob-path source (:address item))]
    [:div.dint-preview
     [:div.dint-preview-head
      [:b (t "original")]
      (when (seq marks)
        [:span.dint-muted (t "blocks-of" {:n (count marks)
                                          :strategy (kind-label (:strategy focus))})])]
     [:div.consumer__app.dint-preview-body
      (cond
        (m/pdf? item)   [pdf/pdf {:src src :annotations (or marks []) :class "dint-pdf"}]
        (m/image? item) [:img.dint-img {:src src :alt (:label item)}]
        :else           [ui/empty-state {:icon :document :title (t "not-a-document")}])]]))

(defn- pager [st]
  (let [n (m/page-count st) p (:page st)]
    [:span.dint-pager
     [ui/button {:small true :icon :chevron-left :disabled (zero? p)
                 :on-click #(send! [:doc-intel/page (dec p)])}]
     [:span (t "page" {:page (inc p) :of (or n "?")})]
     [ui/button {:small true :icon :chevron-right :disabled (and n (>= (inc p) n))
                 :on-click #(send! [:doc-intel/page (inc p)])}]]))

(defn- toolbar [st]
  (let [it (:item st) k (file-kind it)]
    [page/toolbar
     [:span {:class (str "dint-file dint-file--" k)} (t (str "file-" k))]
     [ui/tooltip {:content (:address it)} [:b.dint-title (:label it)]]
     [pager st]
     [ui/html-select {:value ""
                      :on-change (on-value #(send! [:doc-intel/add-strategy %]))}
      [:option {:value ""} (t "add-strategy")]
      (for [k m/strategy-kinds] ^{:key k} [:option {:value k} (kind-label k)])]
     [ui/button {:intent :primary :icon :play :on-click #(send! [:doc-intel/run-all])}
      (t "run-all")]
     [ui/divider]
     [:label.dint-muted (t "judge")]
     [ui/html-select {:value (or (:judge st) "")
                      :on-change (on-value #(send! [:doc-intel/judge %]))}
      (for [x (:models st)] ^{:key x} [:option {:value x} x])]
     [ui/button {:icon :star :disabled (nil? (:judge st))
                 :on-click #(send! [:doc-intel/evaluate-all])}
      (t "evaluate-all")]]))

(defn- comparison [st]
  (let [runs (vec (for [s (:strategies st)]
                    (let [row (:row (m/result st s)) ev (:row (m/evaluation st s))]
                      {:id (:id s) :s s :row row :ev ev :mean (m/mean-score (:scores ev))})))
        best (m/bests (map (juxt :id :row :ev) runs))
        on   (fn [extra] (fn [{:keys [id]}]
                           (str (when (= id (:focus st)) "dint-compare--on ")
                                (when (and extra (= id (get best extra))) "dint-lead-cell"))))
        num  (fn [label k render]
               {:label label :align :right :cell-class (on k) :render render})]
    (when (some :row runs)
      [:div.dint-compare
       [table/data-table
        {:row-key      :id
         :rows         runs
         :on-row-click #(send! [:doc-intel/focus (:id %)])
         :cols
         (into [{:label (t "strategy") :cell-class (on nil)
                 :render (fn [{:keys [s]}]
                           [:<> [kind-badge (:strategy s)] " "
                            [:span.dint-muted (strategy-detail s)]])}
                (num (t "time") :ms #(seconds (:ms (:row %))))
                (num (t "tokens") nil #(tokens (:row %)))
                (num (t "cost") :cost #(or (dollars (:cost (:row %))) (when (:row %) (t "free"))))
                (num (t "chars") :chars #(some-> (:markdown (:row %)) count))
                (num (t "score") :score
                     #(vector :span {:class (score-class (:mean %))} (score-text (:mean %))))]
               (for [c m/criteria]
                 {:label (t (str "crit-" (name c))) :align :center :cell-class (on nil)
                  :render #(let [x (get (:scores (:ev %)) c)]
                             [:span {:class (score-class x)} (score-text x)])}))}]])))

(defn- field [label control] [:label.dint-field [:span.dint-muted label] control])

(defn- deploy-panel [st id]
  (let [{:keys [name repository result]} (m/deployment st id)
        {:keys [status row error]} result
        put (fn [k] (on-value #(send! [:doc-intel/deployment id k %])))]
    [:div.dint-config
     [field (t "deploy-name")
      [ui/input-group {:small true :value name :placeholder "beleg-seiten"
                       :on-change (put :name)}]]
     [field (t "deploy-repository")
      [ui/input-group {:small true :value repository :on-change (put :repository)}]]
     [ui/button
      {:small true
       :disabled (or (= :running status) (str/blank? name) (str/blank? repository)
                     (nil? (:source st)))
       :on-click #(send! [:doc-intel/deploy id])}
      (t "deploy")]
     (when error [ui/problem error nil])
     (when row
       [:<>
        [field (:path row) [ui/text-area
                            {:fill true :class "bp-monospace-text" :rows 12 :read-only true
                             :value (get (:files row) (:path row))}]]
        [:div.dint-muted (t "deploy-next" {:repository (:repository row)
                                           :branch (:branch row)})]])]))

(defn- config [st {:keys [id strategy] :as s}]
  (let [put (fn [k] (on-value #(send! [:doc-intel/edit-strategy id k %])))]
    [:div.dint-config
     [field (t "strategy")
      [ui/html-select {:value strategy :on-change (put :strategy)}
       (for [k m/strategy-kinds] ^{:key k} [:option {:value k} (kind-label k)])]]
     (when-not (#{"raw-text" "ocr"} strategy)
       [field (t "mode")
        [ui/html-select {:value (:mode s) :on-change (put :mode)}
         (for [x m/modes] ^{:key x} [:option {:value x} x])]])
     (when-not (= "raw-text" strategy)
       [field (t "languages")
        [ui/input-group {:small true :value (:languages s) :placeholder "deu, eng"
                         :on-change (put :languages)}]])
     (when (= "vlm" strategy)
       [:<>
        [field (t "model")
         [ui/html-select {:value (or (:model s) "") :on-change (put :model)}
          (for [x (:models st)] ^{:key x} [:option {:value x} x])]]
        [field (t "preprocess")
         [ui/html-select {:value (:preprocess s) :on-change (put :preprocess)}
          (for [x m/preprocessings]
            ^{:key x} [:option {:value x} (t (str "pre-" x))])]]
        [field (t "prompt")
         [ui/text-area {:fill true :class "bp-monospace-text" :rows 5 :value (:prompt s)
                        :placeholder (t "prompt-default")
                        :on-change (put :prompt)}]]])
     [deploy-panel st id]]))

(defn- scores [ev]
  (case (:status ev)
    nil      nil
    :running [busy/block {:class "dint-band dint-muted"} (t "evaluating")]
    :error   [ui/problem (:error ev) nil "dint-band"]
    (let [{:keys [scores notes]} (:row ev)]
      [:div.dint-band.dint-scores
       (for [c m/criteria :let [x (get scores c)]]
         ^{:key c}
         [:div.dint-score
          [:span.dint-muted (t (str "crit-" (name c)))]
          [:span.dint-score-bar [:span {:class (score-class x)
                                        :style {:width (str (* 20 (or x 0)) "%")}}]]
          [:span.dint-score-n (score-text x)]])
       (when-not (str/blank? notes) [:div.dint-notes notes])])))

(defn- output [st s res]
  (case (:status res)
    nil      [ui/empty-state {:icon :play :title (t "not-run")}]
    :running [busy/block {:class "dint-idle"} (t "running")]
    :error   [ui/problem (:error res) nil]
    (let [text (:markdown (:row res))]
      (cond
        (str/blank? text)        [ui/empty-state {:icon :document :title (t "empty")}]
        (m/show-raw? st s)       [:pre.dint-raw text]
        :else                    [:div.consumer__app
                                  [md/markdown {:text text :class "dint-md"}]]))))

(defn- metrics [st {:keys [id] :as s} row ev]
  [:div.dint-metrics
   [tag {:minimal true :icon :time} (seconds (:ms row))]
   (when-let [x (tokens row)] [tag {:minimal true} (t "tokens-io" {:io x})])
   [tag {:minimal true} (or (dollars (:cost row)) (t "free"))]
   (when-let [x (m/mean-score (:scores (:row ev)))]
     [:span {:class (score-class x)} [ui/icon {:icon :star :size 12}] " " (score-text x)])
   [ui/button
    {:small true :on-click #(send! [:doc-intel/toggle-raw id])}
    (t (if (m/show-raw? st s) "rendered" "markdown"))]
   [ui/button
    {:small true :disabled (or (nil? (:judge st)) (= :running (:status ev)))
     :on-click #(send! [:doc-intel/evaluate id])} (t "evaluate")]])

(defn- column [st {:keys [id strategy] :as s}]
  (let [res (m/result st s)
        row (:row res)
        ev  (m/evaluation st s)]
    [:div {:class (str "dint-col dint-col--" strategy (when (= id (:focus st)) " dint-col--on"))
           :on-click #(send! [:doc-intel/focus id])}
     [:div.dint-col-head
      [kind-badge strategy]
      [:span.dint-col-detail (strategy-detail s)]
      [ui/tooltip {:content (t "configure")}
       [ui/button
        {:small true :icon :cog :active (= id (:editing st))
         :on-click #(send! [:doc-intel/toggle-editing id])
         :aria-label (t "configure")}]]
      [ui/tooltip {:content (t "run")}
       [ui/button
        {:small true :icon :play :disabled (= :running (:status res))
         :on-click #(send! [:doc-intel/extract id]) :aria-label (t "run")}]]
      [ui/tooltip {:content (t "remove")}
       [ui/button
        {:small true :icon :cross :on-click #(send! [:doc-intel/remove-strategy id])
         :aria-label (t "remove")}]]]
     (when (= id (:editing st)) [config st s])
     (when row [metrics st s row ev])
     [scores ev]
     [:div.dint-out [output st s res]]]))

(defn- main []
  (let [st (st)]
    (if-not (:item st)
      [welcome st]
      [:div.dint-main
       [toolbar st]
       [comparison st]
       (into [:div.dint-cols [preview st]]
             (for [s (:strategies st)] ^{:key (:id s)} [column st s]))])))

(page/view! :doc-intel/media sidebar)
(page/view! :doc-intel/main main)

(def ^:private tree
  (page/werkzeug {:sidebar       :doc-intel/media
                  :sidebar-label :ui/dint-media
                  :sidebar-w     280
                  :main          :doc-intel/main}))

(defn- stored [k] (storage/get-json (ev/stored-keys k)))

(def app
  (machine/entering
   doc-intel #(vector :doc-intel/enter (stored :strategies) (stored :source))
   (fn [] [platform/module :document-intelligence tree])))
