(ns sutra-ui.apps.notebook.use-case
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.api :as api]
            [sutra-ui.apps.notebook.state :as st :refer [send!]]
            [sutra-ui.apps.notebook.ui :refer [btn]]
            [sutra-ui.editor.code :as ce]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]))

(defn use-case-doc? [d] (= :use-case (some-> (:format d) keyword)))

(def starter
  (str/join
   "\n"
   [";; Use-Case-Entwurf — §3 Typen, §4 Kanten, §6 Lebenszyklus, §8 Schreibweg."
    ";; Klassen: :core (+ :domain), :core-internal, :derived, :use-case, :retired,"
    ";;          :report (→ App), :property-of (→ Eigenschaft, mit :of)."
    "{:use-case \"Name des Use Cases\""
    " :project  :mein-projekt"
    " :group    :mein-use-case"
    ""
    " :object-model"
    " {:title \"Skizze des Objektmodells (Soll)\""
    "  :types [{:id :vorgang :label \"Vorgang\" :pk \"vorgang-id\" :kind :use-case"
    "           :plural \"Vorgänge\" :note \"im Ablauf bearbeitet\"}"
    "          {:id :kunde :label \"Kunde\" :pk \"kunde-id\" :kind :core :domain \"ERP\"}]"
    "  :links [{:from :vorgang :to :kunde :card \"N:1\"}]}"
    ""
    " :statecharts"
    " [{:id :vorgang-status :type :vorgang :property :status"
    "   :title \"Lebenszyklus eines Vorgangs\""
    "   :states [{:id :start :kind :initial}"
    "            {:id :neu :label \"neu\" :tone :warn}"
    "            {:id :geschlossen :label \"geschlossen\" :tone :ok}]"
    "   :transitions [{:from :start :to :neu :action :vorgang-anlegen :role \"Erfasser\"}"
    "                 {:from :neu :to :geschlossen :action :vorgang-schliessen"
    "                  :role \"Prüfer\"}]}]"
    ""
    " :sources    {}"
    " :writebacks []}"
    ""]))

(defn- branch [] (:current-branch @shell/ui-state))

(defn- trunk? [] (contains? #{nil "" "main"} (some-> (branch) str)))

(defn- id-of [db] (some-> (:id (:open-doc db)) str))

(defn- working
  [db doing ask & {:as more}]
  (rq/request db (merge {:ask ask :busy [[:use-case :busy] doing] :error [:use-case :error]}
                        more)))

(machine/reg-event :notebook.use-case/plan
                   (fn [{:keys [db]} [_ preview?]]
                     (when-let [id (id-of db)]
                       (working db (if preview? :preview :plan)
                                [api/post! :use-case-plan {:id id} {:preview? (boolean preview?)}]
                                :path [:use-case] :pick #(hash-map :plan %)))))

(machine/reg-event :notebook.use-case/apply
                   (fn [{:keys [db]} _]
                     (when-let [id (id-of db)]
                       (update (working db :apply [api/post! :use-case-apply {:id id} {}]
                                        :path [:use-case]
                                        :pick #(hash-map :plan % :applied (:staged %)))
                               :fx #(into [[:dispatch [:notebook/save nil []]]] %)))))

(defn- delete-one [[kind nm]]
  (let [n (name nm)]
    (case (keyword kind)
      :object-type (api/delete! :object-type-by {:name n :cascade true})
      :link-type   (api/delete! :conn-type-by {:name n})
      :action-type (api/delete! :action-type-by {:name n}))))

(defn- delete-all [rs]
  (reduce (fn [p r] (.then p (fn [_] (delete-one r)))) (js/Promise.resolve nil) rs))

(machine/reg-event :notebook.use-case/retire
                   (fn [{:keys [db]} _]
                     (let [rs (get-in db [:use-case :plan :retire])]
                       (when (seq rs)
                         {:fx [[:confirm
                                {:ask {:title   "Zurückbauen"
                                       :body    (str (count rs) " Konstrukt(e) auf dem Zweig "
                                                     (branch) " löschen: "
                                                     (str/join ", " (map #(name (second %)) rs)))
                                       :danger? true}
                                 :ok  [:notebook.use-case/retire-now rs]}]]}))))

(machine/reg-event :notebook.use-case/retire-now
                   (fn [{:keys [db]} [_ rs]]
                     (working db :retire [delete-all rs] :then [[:notebook.use-case/plan false]])))

(machine/reg-event :notebook.use-case/draw
                   (fn [{:keys [db]} [_ views]]
                     (when-let [id (id-of db)]
                       {:db (-> db
                                (cond-> (not= id (first (get-in db [:use-case :drawn-for])))
                                  (assoc :use-case {}))
                                (assoc-in [:use-case :drawn-for]
                                          [id (:version (:open-doc db))]))
                        :fx (vec (for [[k attrs] views]
                                   (rq/ask
                                    {:ask  [api/post! :document-widget
                                            {:attrs (merge {:kind "use-case-draft" :draft id}
                                                           attrs)}]
                                     :path [:use-case :figures k] :pick :html
                                     :else [(fn [s]
                                              [:rf/set [:use-case :figures k]
                                               (str "<span class=\"np-missing\">" s
                                                    "</span>")])]})))})))

(def ^:private state (machine/subscribe [:notebook/at :use-case] {:frame st/notebook}))

(defn- views-of
  [body]
  (let [charts (for [[_ id] (re-seq #"\{\s*:id\s+:([\w-]+)[^{}]*?:states" (str body))] id)]
    (cond-> []
      (str/includes? (str body) ":object-model") (conj ["objektmodell" {:view "object-model"}])
      true (into (for [c charts] [c {:view "statechart" :chart c}])))))

(defn- draw-if-new!
  [d]
  (when (and (:version d) (not= (:drawn-for @state) [(str (:id d)) (:version d)]))
    (send! [:notebook.use-case/draw (views-of (:body d))])))

(def ^:private waiting "<span class=\"np-muted\">wird gezeichnet…</span>")

(defn- figures [_]
  (r/create-class
   {:component-did-mount  (fn [this] (draw-if-new! (second (r/argv this))))
    :component-did-update (fn [this] (draw-if-new! (second (r/argv this))))
    :reagent-render
    (fn [d]
      (let [{:keys [figures]} @state]
        [:div {:class "np-uc__figures"}
         (for [[k _] (views-of (:body d))]
           ^{:key k}
           [:div {:class "np-uc__figure"
                  :dangerouslySetInnerHTML {:__html (or (get figures k) waiting)}}])]))}))

(defn surface
  [d]
  [:div {:class "np-uc"}
   [:div {:class "np-uc__source"}
    [ce/code-editor {:value (:body d) :min-height "70vh"
                     :on-change #(send! [:notebook/edit {:body %}])}]]
   [figures d]])

(def ^:private op-text
  {:create "anlegen" :change "ändern" :retire "zurückbauen" :keep "vorhanden" :check "prüfen"})

(def ^:private kind-text
  {:object-type "Objekttyp" :link-type "Link-Typ" :action-type "Action" :source "Quelle"
   :writeback "Rückschreiben" :object-type-group "Gruppe"})

(defn- step-row [{:keys [op kind name auto? why warn]}]
  [:li {:class (str "np-uc__step np-uc__step--" (some-> op clojure.core/name))}
   [:div [:span {:class "np-uc__op"} (op-text (keyword op))] " "
    [:span {:class "np-muted"} (kind-text (keyword kind))] " "
    [:code (some-> name clojure.core/name)]
    (when auto? [:span {:class "np-uc__auto" :title "legt der Assistent selbst an"} " ✓"])]
   [:div {:class "np-muted"} why]
   (for [w warn] ^{:key w} [:div {:class "np-missing"} (str "⚠ " w)])])

(defn panel
  [d]
  (let [{:keys [plan busy error applied]} @state
        {:keys [steps hints summary preview retire]} plan
        open   (remove #(= "keep" (some-> (:op %) name)) steps)
        auto-n (count (filter :auto? steps))]
    [:div {:class "np-uc__panel"}
     [:div {:class "np-uc__buttons"}
      [btn {:label (if (= :plan busy) "prüft…" "Plan prüfen") :disabled? (boolean busy)
            :on-click #(send! [:notebook.use-case/plan false])}]
      [btn {:label (if (= :preview busy) "plant…" "Vorschau") :tone :ghost
            :disabled? (boolean busy)
            :title "das Anlegen in einer zurückgerollten Transaktion planen"
            :on-click #(send! [:notebook.use-case/plan true])}]]
     (when error [:div {:class "np-missing"} error])
     (when plan
       [:<>
        [:div {:class "np-uc__summary"}
         (str/join " · " (for [[k n] (dissoc summary :auto) :when (pos? n)]
                           (str n " " (op-text (keyword k)))))]
        (if (trunk?)
          [:div {:class "np-muted"}
           (str auto-n " Schritte kann der Assistent selbst anlegen — dafür erst einen Zweig "
                "wählen (oben im Zweigmenü), dann hier anlegen, prüfen, vorschlagen.")]
          [btn {:label (if (= :apply busy) "legt an…"
                           (str auto-n " Schritte auf „" (branch) "“ anlegen"))
                :disabled? (or (boolean busy) (zero? auto-n))
                :on-click #(send! [:notebook.use-case/apply])}])
        (when (and (seq retire) (not (trunk?)))
          [btn {:label (str (count retire) " zurückbauen") :tone :danger
                :disabled? (boolean busy)
                :on-click #(send! [:notebook.use-case/retire])}])
        (when applied
          [:div {:class "np-uc__applied"}
           "Gestaged. Weiter: Zweig committen und vorschlagen (Zweigmenü)."])
        (when preview
          [:div {:class "np-muted"}
           (str "Vorschau: " (pr-str (select-keys preview [:added :updated :deleted :refused?])))])
        [:ul {:class "np-uc__steps"}
         (for [s open] ^{:key (str (:kind s) "/" (:name s))} [step-row s])]
        (when (seq hints)
          [:<> [:div {:class "np-section-title"} "Offen"]
           [:ul {:class "np-uc__hints"} (for [h hints] ^{:key h} [:li h])]])
        [:details [:summary (str (- (count steps) (count open)) " vorhanden")]
         [:ul {:class "np-uc__steps"}
          (for [s (filter #(= "keep" (some-> (:op %) name)) steps)]
            ^{:key (str (:kind s) "/" (:name s))} [step-row s])]]
        [:div {:class "np-section-title"} "In ein Dokument einfügen"]
        [:pre {:class "np-uc__snippet"}
         (str/join "\n"
                   (for [[_ attrs] (views-of (:body d))]
                     (str "<sutra-widget data-kind=\"use-case-draft\" data-draft=\"" (:id d) "\""
                          (str/join (for [[k v] attrs] (str " data-" (name k) "=\"" v "\"")))
                          "/>")))
         (str "\n<sutra-widget data-kind=\"use-case-draft\" data-draft=\"" (:id d)
              "\" data-view=\"plan\"/>")]])]))

