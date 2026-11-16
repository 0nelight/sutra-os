(ns sutra-ui.apps.developer-apps.core
  (:require [sutra-ui.ui.chart :as chart]
            [sutra-ui.storage :as storage]
            [sutra-ui.ui.core :as ui :refer [tag link]]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra-ui.apphost.platform :as platform]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.download :as download]
            [sutra-ui.resource.link :as rl]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.apps.developer-apps.events :as ev]
            [sutra-ui.apps.developer-apps.model :as model]
            [sutra-ui.ui.style :as style]
            [sutra.types.redirect-uri :as redirect-uri]))

(def ^:private sections
  [{:id :overview    :label :ui/devcon-sec-overview}
   {:id :oauth       :label :ui/devcon-sec-oauth}
   {:id :sdk         :label :ui/devcon-sec-sdk}
   {:id :permissions :label :ui/devcon-sec-permissions}
   {:id :hosting     :label :ui/devcon-sec-hosting}
   {:id :metrics     :label :ui/devcon-sec-metrics}
   {:id :declaration :label :ui/devcon-sec-declaration}])

(def ^:private grant-types
  [{:id :authorization-code :title :ui/devcon-grant-user :hint :ui/devcon-grant-user-hint}
   {:id :client-credentials :title :ui/devcon-grant-app :hint :ui/devcon-grant-app-hint}])

(def ^:private operations
  [{:id :objects      :label :ui/devcon-op-objects}
   {:id :object-types :label :ui/devcon-op-object-types}
   {:id :actions      :label :ui/devcon-op-actions}
   {:id :function     :label :ui/devcon-op-function}
   {:id :datasets     :label :ui/devcon-op-datasets}
   {:id :admin        :label :ui/devcon-op-admin}])

(def ^:private drafts-key "sutra.devcon.drafts")

(defn- declared
  []
  (or (:custom-applications @catalog/canonical-schema) {}))

(defn- app-keys [] (ev/app-keys @model/db (declared)))

(defn- app-of [k] (ev/app-of @model/db (declared) k))

(defn- dirty? [k]
  (let [drafts (:drafts @model/db)]
    (and (contains? drafts k)
         (not= (get drafts k) (get (declared) k)))))

(defn- edit! [k path v] (model/send! [:dev-apps/edit (declared) k path v]))

(defn- toggle-in! [k path v] (model/send! [:dev-apps/toggle (declared) k path v]))

(defn- show-section! [id] (model/send! [:dev-apps/section id]))

(defn- names-of
  [m]
  (into #{} (keep #(some-> % name)) (keys m)))

(defn- live-object-types [] (names-of (:object-types @catalog/schema)))
(defn- live-actions []      (names-of (catalog/declared-actions)))
(defn- live-projects []     (names-of (:projects @catalog/canonical-schema)))
(defn- live-repositories [] (names-of (:repositories @catalog/canonical-schema)))

(defn- kw-name [x] (cond (keyword? x) (name x) (nil? x) nil :else (str x)))

(defn- app-title [k app]
  (or (not-empty (:display-name app)) (kw-name k)))

(defn- host-url [app]
  (let [sub (get-in app [:hosting :subdomain])]
    (when-not (str/blank? sub)
      (str sub "." (or (some-> js/window .-location .-host) "")))))

(defn- callback-url [app]
  (when-let [h (host-url app)] (str "https://" h "/auth/callback")))

(defn- checks
  [app]
  (let [ots  (live-object-types)
        acts (live-actions)
        prjs (live-projects)
        reps (live-repositories)
        cb   (callback-url app)]
    (concat
     (for [o (get-in app [:scope :object-types])]
       {:kind :object-type :ref (kw-name o) :field :ui/devcon-field-scope :section :oauth
        :ok? (contains? ots (kw-name o)) :bad :ui/devcon-bad-object-type})
     (for [a (get-in app [:scope :actions])]
       {:kind :action :ref (kw-name a) :field :ui/devcon-field-scope :section :oauth
        :ok? (contains? acts (kw-name a)) :bad :ui/devcon-bad-action})
     (when-let [p (kw-name (:project app))]
       [{:kind :project :ref p :field :ui/devcon-project :section :overview
         :ok? (contains? prjs p) :bad :ui/devcon-bad-project}])
     (when-let [rp (kw-name (:repository app))]
       [{:kind :repository :ref rp :field :ui/devcon-repository :section :overview
         :ok? (contains? reps rp) :bad :ui/devcon-bad-repository}])
     (for [u (get-in app [:client :redirect-uris])]
       {:kind :uri :ref u :field :ui/devcon-redirect-uris :section :oauth
        :ok? (redirect-uri/valid? u) :bad :ui/devcon-bad-uri})
     (when cb
       [{:kind :uri :ref cb :field :ui/devcon-field-callback :section :hosting
         :ok? (boolean (some #{cb} (get-in app [:client :redirect-uris])))
         :bad :ui/devcon-bad-callback}]))))

(defn- problems [app] (remove :ok? (checks app)))

(defn- kw-vec [xs]
  (str "[" (str/join " " (map #(str ":" (kw-name %)) xs)) "]"))

(defn- str-vec [xs]
  (str "[" (str/join " " (map pr-str xs)) "]"))

(defn- decl->edn
  [k app]
  (let [pad (fn [s] (apply str s (repeat (max 0 (- 14 (count s))) " ")))]
    (str/join
     "\n"
     [":custom-applications"
      (str "{:" (kw-name k))
      (str " {" (pad ":display-name") (pr-str (:display-name app)))
      (str "  " (pad ":status") ":" (kw-name (:status app)))
      (str "  " (pad ":project") ":" (kw-name (:project app)))
      (str "  " (pad ":repository") ":" (kw-name (:repository app)))
      (str "  " (pad ":client") "{:grant-types " (kw-vec (get-in app [:client :grant-types])))
      (str "                 :redirect-uris " (str-vec (get-in app [:client :redirect-uris])))
      (str "                 :token-ttl " (get-in app [:client :token-ttl]) "}")
      (str "  " (pad ":scope") "{:object-types " (kw-vec (get-in app [:scope :object-types])))
      (str "                 :actions      " (kw-vec (get-in app [:scope :actions])))
      (str "                 :operations   " (kw-vec (get-in app [:scope :operations])) "}")
      (str "  " (pad ":sdk") "{:namespace " (pr-str (get-in app [:sdk :namespace])) "}")
      (str "  " (pad ":permissions") "{:roles " (kw-vec (get-in app [:permissions :roles])) "}")
      (str "  " (pad ":hosting") "{:subdomain " (pr-str (get-in app [:hosting :subdomain]))
           " :root " (pr-str (get-in app [:hosting :root])))
      (str "                 :csp {:script-src " (str-vec (get-in app [:hosting :csp :script-src]))
           "}}")
      (str "  " (pad ":metrics") (boolean (:metrics app)) "}}")])))

(defn- pill [kind label]
  [tag {:minimal true :intent (case kind :ok :success :bad :danger :warn :warning nil)}
   label])

(defn- status-label [status]
  (case (kw-name status)
    "draft"  (i18n/t :ui/devcon-status-draft)
    "active" (i18n/t :ui/devcon-status-active)
    nil      (i18n/t :ui/devcon-status-none)
    (kw-name status)))

(defn- field-row [label & body]
  [:div {:class "devcon-row"}
   [:div {:class "devcon-row__key"} label]
   (into [:div {:class "devcon-row__val"}] body)])

(defn- text-field [{:keys [value on-change placeholder mono? size]}]
  [ui/input-group {:class ["devcon-input" (when size (str "devcon-input--" (name size)))]
                   :input-class-name (when mono? "devcon-mono")
                   :value (or value "")
                   :placeholder placeholder
                   :on-change #(on-change (.. % -target -value))}])

(defn- name-picker
  [{:keys [value live none on-change]}]
  (let [v     (kw-name value)
        known (set live)]
    [ui/html-select
     {:class "devcon-input" :value (or v "")
      :on-change #(let [x (.. % -target -value)] (on-change (when (seq x) (keyword x))))}
     [:option {:value ""} none]
     (when (and v (not (contains? known v)))
       [:option {:value v} (i18n/t :ui/devcon-not-in-schema-option {:name v})])
     (for [x (sort live)] ^{:key x} [:option {:value x} x])]))

(defn- chip-row
  [{:keys [label on? hint on-click]}]
  [ui/tooltip {:content hint}
   [tag {:large true :minimal (not on?) :intent (when on? :primary)
         :icon (if on? :tick :plus) :on-click on-click}
    label]])

(defn- empty-note [& body]
  (into [:div {:class "devcon-empty"}] body))

(defn- resource-picker
  [{:keys [label kind chosen live none on-pick]}]
  (let [chosen   (vec chosen)
        known    (set live)
        dangling (remove #(contains? known (kw-name %)) chosen)
        label-of #(rl/label kind %)]
    [:<>
     [:div {:class style/label} label]
     (if (and (empty? chosen) (empty? live))
       [empty-note none]
       [:div {:class "devcon-chips"}
        (concat
         (for [o dangling]
           ^{:key (str "x" (kw-name o))}
           [chip-row {:label (kw-name o) :on? true
                      :hint (i18n/t :ui/devcon-not-in-schema)
                      :on-click #(on-pick (keyword (kw-name o)))}])
         (for [o (sort-by label-of live)]
           ^{:key o}
           [chip-row {:label (label-of o) :hint o
                      :on? (boolean (some #(= o (kw-name %)) chosen))
                      :on-click #(on-pick (keyword o))}]))])
     (when-let [ok (seq (filter #(contains? known (kw-name %)) chosen))]
       [:div {:class "u-flex-wrap-gap-6 u-mb-8"}
        [:span {:class style/small-muted} (i18n/t :ui/devcon-open-resource)]
        (for [o ok] ^{:key (kw-name o)} [rl/resource-link kind o])])]))

(defn- reference-cell [{:keys [kind ref ok?]}]
  (if (and ok? (#{:object-type :action} kind))
    [rl/resource-link kind ref]
    [:span {:class "devcon-mono"} ref]))

(defn- references-table [cs]
  [table/data-table
   {:rows cs
    :cols [{:label (i18n/t :ui/devcon-reference) :render (fn [c] [reference-cell c])}
           {:label  (i18n/t :ui/devcon-field)
            :render (fn [c]
                      [link {:href     "#"
                             :on-click (fn [e] (.preventDefault e) (show-section! (:section c)))}
                       (i18n/t (:field c))])}
           {:label  (i18n/t :ui/status)
            :render (fn [c]
                      (if (:ok? c)
                        [pill :ok (i18n/t :ui/devcon-resolves)]
                        [pill :bad (i18n/t (:bad c))]))}]}])

(defn- overview-pane [k app]
  (let [cs      (checks app)
        bad     (remove :ok? cs)
        scope-n (+ (count (get-in app [:scope :object-types]))
                   (count (get-in app [:scope :actions])))]
    [:div {:class "devcon-stack"}
     [:div {:class "devcon-tiles"}
      [:div {:class "devcon-tile"}
       [:div {:class "devcon-tile__label"} (i18n/t :ui/status)]
       [:div {:class "devcon-tile__value"} (status-label (:status app))]
       [:div {:class "devcon-tile__meta"}
        (i18n/t (if (dirty? k) :ui/devcon-draft-uncommitted :ui/devcon-as-declared))]]
      [:div {:class "devcon-tile"}
       [:div {:class "devcon-tile__label"} (i18n/t :ui/devcon-sec-hosting)]
       [:div {:class "devcon-tile__value"}
        (let [sub (get-in app [:hosting :subdomain])]
          (if (str/blank? sub) (i18n/t :ui/none) sub))]
       [:div {:class "devcon-tile__meta devcon-mono"}
        (or (host-url app) (i18n/t :ui/devcon-no-subdomain))]]
      [:div {:class "devcon-tile"}
       [:div {:class "devcon-tile__label"} "SDK"]
       [:div {:class "devcon-tile__value"}
        (let [ns (get-in app [:sdk :namespace])] (if (str/blank? ns) "—" ns))]
       [:div {:class "devcon-tile__meta"}
        (if-let [v (first (:sdk-versions app))]
          (i18n/t :ui/devcon-sdk-version {:version (:version v)})
          (i18n/t :ui/devcon-sdk-not-generated))]]
      [:div {:class "devcon-tile"}
       [:div {:class "devcon-tile__label"} (i18n/t :ui/devcon-field-scope)]
       [:div {:class "devcon-tile__value"} scope-n]
       [:div {:class "devcon-tile__meta"}
        (i18n/t :ui/devcon-operations-count {:n (count (get-in app [:scope :operations]))})]]]

     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-application)]
      [field-row (i18n/t :ui/devcon-display-name)
       [text-field {:value (:display-name app) :size :l
                    :on-change #(edit! k [:display-name] %)}]]
      [field-row (i18n/t :ui/devcon-project)
       [name-picker {:value (:project app) :live (live-projects)
                     :none (i18n/t :ui/devcon-pick-project)
                     :on-change #(edit! k [:project] %)}]]
      [field-row (i18n/t :ui/devcon-repository)
       [name-picker {:value (:repository app) :live (live-repositories)
                     :none (i18n/t :ui/devcon-pick-repository)
                     :on-change #(edit! k [:repository] %)}]]
      [ui/section
       {:title (i18n/t :ui/devcon-technical) :collapsible true :compact true
        :collapse-props {:default-is-open false}}
       [ui/section-card
        [field-row (i18n/t :ui/devcon-key) [:span {:class "devcon-mono"} (kw-name k)]]
        [field-row (i18n/t :ui/devcon-client-id)
         (if-let [cid (:client-id app)]
           [:span {:class "devcon-mono"} cid]
           [:span {:class "devcon-muted"} (i18n/t :ui/devcon-issued-on-install)])]]]]

     [:section
      [:div {:class style/section-h3}
       (i18n/t :ui/devcon-references)
       [:span {:class "devcon-count"}
        (i18n/t :ui/devcon-unresolved-of {:bad (count bad) :n (count cs)})]]
      (if (empty? cs)
        [empty-note (i18n/t :ui/devcon-nothing-referenced)]
        [references-table cs])]]))

(defn- oauth-pane [k app]
  (let [uris (vec (get-in app [:client :redirect-uris]))
        cb   (callback-url app)]
    [:div {:class "devcon-stack"}
     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-auth-client)]
      [:div {:class "devcon-grants"}
       (for [{:keys [id title hint]} grant-types]
         (let [on? (boolean (some #{id} (get-in app [:client :grant-types])))]
           ^{:key id}
           [ui/checkbox-card
            {:compact true :checked on?
             :on-change #(toggle-in! k [:client :grant-types] id)}
            [:span {:class "devcon-grant__title"} (i18n/t title)]
            [:div {:class style/small-muted} (i18n/t hint)]]))]
      [field-row (i18n/t :ui/devcon-client-secret)
       (cond
         (not (some #{:client-credentials} (get-in app [:client :grant-types])))
         [:span {:class "devcon-muted"} (i18n/t :ui/devcon-no-secret)]

         (:client-secret app)
         [:<>
          [:span {:class "devcon-mono"}
           (if (:secret-shown? @model/db) (:client-secret app) "••••••••••••••••")]
          [ui/button {:small true :on-click #(model/send! [:dev-apps/secret])}
           (i18n/t (if (:secret-shown? @model/db) :ui/devcon-hide :ui/devcon-show))]]

         :else [:span {:class "devcon-muted"} (i18n/t :ui/devcon-issued-on-install)])]
      [field-row (i18n/t :ui/devcon-token-ttl)
       [ui/numeric-input
        {:min 300 :max 86400 :step-size 300 :major-step-size 3600 :minor-step-size nil
         :class "devcon-input devcon-input--num"
         :value (get-in app [:client :token-ttl])
         :on-value-change (fn [_ s _] (edit! k [:client :token-ttl] (js/parseInt s 10)))}]
       [:span {:class style/small-muted} (i18n/t :ui/devcon-seconds)]]]

     [:section
      [:div {:class style/section-h3}
       (i18n/t :ui/devcon-redirect-uris)
       (when (and cb (not (some #{cb} uris)))
         [ui/button {:small true :on-click #(edit! k [:client :redirect-uris] (conj uris cb))}
          (i18n/t :ui/devcon-add-hosted-callback)])]
      (if (empty? uris)
        [empty-note (i18n/t :ui/devcon-no-redirect)]
        (for [[i u] (map-indexed vector uris)]
          ^{:key i}
          [:div {:class "devcon-listrow"}
           [text-field {:value u :mono? true :size :xl
                        :on-change #(edit! k [:client :redirect-uris] (assoc uris i %))}]
           (if (redirect-uri/valid? u)
             [pill :ok (i18n/t :ui/devcon-valid)]
             [pill :bad (i18n/t :ui/devcon-invalid)])
           [ui/button {:small true :on-click #(edit! k [:client :redirect-uris]
                                   (vec (concat (subvec uris 0 i) (subvec uris (inc i)))))}
            (i18n/t :ui/remove)]]))
      [:div {:class "devcon-listrow"}
       [ui/button {:on-click #(edit! k [:client :redirect-uris]
                               (conj uris "http://localhost:8080/auth/callback"))}
        (i18n/t :ui/devcon-add-localhost)]]]

     [:section
      [:div {:class style/section-h3}
       (i18n/t :ui/devcon-ontology-resources)
       [:span {:class "devcon-count"} (i18n/t :ui/devcon-scope-sdk-contents)]]
      [resource-picker {:label (i18n/t :ui/devcon-object-types) :kind :object-type
                        :chosen (get-in app [:scope :object-types])
                        :live (live-object-types) :none (i18n/t :ui/devcon-no-object-types)
                        :on-pick #(toggle-in! k [:scope :object-types] %)}]
      [resource-picker {:label (i18n/t :ui/devcon-action-types) :kind :action
                        :chosen (get-in app [:scope :actions])
                        :live (live-actions) :none (i18n/t :ui/devcon-no-action-types)
                        :on-pick #(toggle-in! k [:scope :actions] %)}]]

     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-operations)]
      (for [{:keys [id label]} operations]
        ^{:key id}
        [ui/checkbox {:checked   (boolean (some #{id} (get-in app [:scope :operations])))
                      :label     (i18n/t label)
                      :on-change #(toggle-in! k [:scope :operations] id)}])]]))

(defn- sdk-pane [k app]
  (let [ns   (get-in app [:sdk :namespace])
        cmd  (str "sutra sdk generate " (if (str/blank? ns) "<namespace>" ns)
                  " --ontology main --out sdk.clj")
        ots  (get-in app [:scope :object-types])
        acts (get-in app [:scope :actions])]
    [:div {:class "devcon-stack"}
     [:section
      [:div {:class style/section-h3} "SDK"]
      [field-row (i18n/t :ui/devcon-namespace)
       [text-field {:value ns :mono? true :size :m :placeholder "fleet"
                    :on-change #(edit! k [:sdk :namespace] (str/replace % #"[^A-Za-z0-9.-]" ""))}]]
      [field-row (i18n/t :ui/devcon-contents)
       [:span (i18n/t :ui/devcon-contents-count {:ots (count ots) :acts (count acts)})]]
      [:div {:class "devcon-code"} [:span {:class "devcon-code__prompt"} "$ "] cmd]
      (if-let [vs (seq (:sdk-versions app))]
        [table/data-table
         {:rows    vs
          :row-key :version
          :cols    [{:label (i18n/t :ui/devcon-version) :render :version :mono? true}
                    {:label (i18n/t :ui/devcon-fingerprint) :render :fingerprint :mono? true}
                    {:label (i18n/t :ui/devcon-generated) :render #(str (:generated-at %))}
                    {:label  (i18n/t :ui/status)
                     :render #(if (:current? %)
                                [pill :ok (i18n/t :ui/devcon-current)]
                                [pill :mute (i18n/t :ui/devcon-superseded)])}]}]
        [empty-note (i18n/t :ui/devcon-no-sdk-1)
         [:span {:class "devcon-mono"} "sdk verify"]
         (i18n/t :ui/devcon-no-sdk-2)])]

     [:section
      [:div {:class style/section-h3}
       (i18n/t :ui/devcon-documentation)
       [:span {:class "devcon-count"} (i18n/t :ui/devcon-docs-derived)]]
      (if (and (str/blank? ns) (empty? ots) (empty? acts))
        [empty-note (i18n/t :ui/devcon-docs-empty)]
        [:div {:class "devcon-code devcon-code--block"}
         (str "(require '[" (if (str/blank? ns) "<ns>" ns) ".objects :as obj]\n"
              "         '[" (if (str/blank? ns) "<ns>" ns) ".actions :as act])\n\n"
              (when-let [o (first ots)]
                (str "(obj/" (kw-name o) "-seq {:limit 50})\n"))
              (when-let [a (first acts)]
                (str "(act/" (kw-name a) " {…})")))])]]))

(defn- permissions-pane [k app]
  (let [roles  (vec (get-in app [:permissions :roles]))
        known  (sort (names-of (:roles @catalog/canonical-schema)))
        grants (get-in app [:client :grant-types])
        user?  (some #{:authorization-code} grants)
        app?   (some #{:client-credentials} grants)]
    [:div {:class "devcon-stack"}
     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-roles)]
      (if (empty? roles)
        [empty-note (i18n/t :ui/devcon-no-roles)]
        [:div {:class "devcon-chips"}
         (for [r roles]
           ^{:key (kw-name r)}
           [chip-row {:label (kw-name r) :on? true
                      :on-click #(toggle-in! k [:permissions :roles] (keyword (kw-name r)))}])])
      (when (seq known)
        [:<>
         [:div {:class style/label} (i18n/t :ui/devcon-known-roles)]
         [:div {:class "devcon-chips"}
          (for [r known]
            ^{:key r}
            [chip-row {:label r
                       :on? (boolean (some #(= r (kw-name %)) roles))
                       :on-click #(toggle-in! k [:permissions :roles] (keyword r))}])]])
      [:div {:class "devcon-listrow"}
       [text-field {:value (:new-role @model/db "") :mono? true :size :m
                    :placeholder (i18n/t :ui/devcon-role-placeholder)
                    :on-change #(model/send! [:dev-apps/role-typed %])}]
       [ui/button {:on-click #(model/send! [:dev-apps/role-add (declared) k])}
        (i18n/t :ui/devcon-add-role)]]]

     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-token-carries)]
      [field-row (i18n/t :ui/devcon-permission-type)
       (i18n/t (cond (and user? app?) :ui/devcon-perm-both
                     app?             :ui/devcon-grant-app
                     user?            :ui/devcon-grant-user
                     :else            :ui/devcon-perm-none))]
      [field-row (i18n/t :ui/devcon-follows-from)
       (if (seq grants)
         (str/join " · " (for [g grants
                               :let [t (some #(when (= g (:id %)) (:title %)) grant-types)]]
                           (if t (i18n/t t) (kw-name g))))
         "—")]
      [field-row (i18n/t :ui/devcon-service-user)
       (if app?
         [:span {:class "devcon-muted"} (i18n/t :ui/devcon-service-user-per-client)]
         [:span {:class "devcon-muted"} (i18n/t :ui/none)])]]]))

(defn- hosting-pane [k app]
  (let [h    (:hosting app)
        cb   (callback-url app)
        srcs (vec (get-in h [:csp :script-src]))]
    [:div {:class "devcon-stack"}
     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-domain)]
      [field-row (i18n/t :ui/devcon-subdomain)
       [text-field {:value (:subdomain h) :mono? true :size :m :placeholder "fleet"
                    :on-change #(edit! k [:hosting :subdomain]
                                       (str/replace % #"[^A-Za-z0-9-]" ""))}]]
      [field-row (i18n/t :ui/devcon-address)
       (if-let [u (host-url app)]
         [:span {:class "devcon-mono"} (str "https://" u)]
         [:span {:class "devcon-muted"} (i18n/t :ui/devcon-pick-subdomain)])]
      [field-row (i18n/t :ui/devcon-asset-root)
       [text-field {:value (:root h) :mono? true :size :s :placeholder "dist"
                    :on-change #(edit! k [:hosting :root] %)}]
       [:span {:class style/small-muted} (i18n/t :ui/devcon-static-only)]]
      [field-row (i18n/t :ui/devcon-field-callback)
       (if cb
         [:<> [:span {:class "devcon-mono"} cb]
          (if (some #{cb} (get-in app [:client :redirect-uris]))
            [pill :ok (i18n/t :ui/devcon-is-redirect)]
            [pill :bad (i18n/t :ui/devcon-bad-callback)])]
         [:span {:class "devcon-muted"} "—"])]]

     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-deployments)]
      (if-let [as (seq (:assets app))]
        [table/data-table
         {:rows    as
          :row-key :id
          :cols    [{:label (i18n/t :ui/devcon-asset) :render :id :mono? true}
                    {:label (i18n/t :ui/devcon-files) :render :files :mono? true}
                    {:label (i18n/t :ui/devcon-uploaded) :render #(str (:uploaded-at %))}
                    {:label  (i18n/t :ui/status)
                     :render #(case (:stage %)
                                "production" [pill :ok (i18n/t :ui/devcon-production)]
                                [pill :warn (str (:stage %))])}]}]
        [empty-note (i18n/t :ui/devcon-nothing-deployed {:root (or (:root h) "dist")})])]

     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-csp)]
      [field-row "script-src"
       [:div {:class "devcon-chips"}
        (for [s srcs]
          ^{:key s}
          [chip-row {:label s :on? true
                     :on-click #(edit! k [:hosting :csp :script-src] (vec (remove #{s} srcs)))}])
        (for [s ["'self'" "https://cdnjs.cloudflare.com" "'unsafe-inline'"]
              :when (not (some #{s} srcs))]
          ^{:key (str "add" s)}
          [chip-row {:label s :on? false
                     :on-click #(edit! k [:hosting :csp :script-src] (conj srcs s))}])]]
      [:div {:class "devcon-code"}
       (str "Content-Security-Policy: script-src " (str/join " " srcs) "; default-src 'self'")]]]))

(defn- bars [series]
  [chart/chart {:rows (vec series) :x :label :y :value :chart-kind :bar
                :orientation :vertical :height 150}])

(defn- metrics-pane [k app]
  (let [m (:metrics-series app)]
    [:div {:class "devcon-stack"}
     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-collection)]
      [field-row (i18n/t :ui/devcon-sec-metrics)
       [ui/switch {:class "u-m-0" :checked (boolean (:metrics app))
                   :label (i18n/t (if (:metrics app) :ui/devcon-collected :ui/devcon-off))
                   :on-change #(edit! k [:metrics] (not (:metrics app)))}]]
      [field-row (i18n/t :ui/devcon-counted-calls)
       [:span (i18n/t :ui/devcon-counted-calls-hint)]]]

     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-requests)]
      (if-let [rs (seq (:requests m))]
        [bars rs]
        [empty-note (i18n/t :ui/devcon-no-series)])]

     [:section
      [:div {:class style/section-h3} (i18n/t :ui/devcon-latency)]
      (if-let [ls (seq (:latency m))]
        [table/data-table
         {:rows    ls
          :row-key :endpoint
          :cols    (into [{:label (i18n/t :ui/endpoint) :render :endpoint :mono? true}]
                         (for [q [:p50 :p95 :p99]]
                           {:label (name q) :mono? true :render #(str (get % q) " ms")}))}]
        [empty-note (i18n/t :ui/devcon-no-latency)])]]))

(defn- declaration-pane [k app]
  (let [edn (decl->edn k app)]
    [:div {:class "devcon-stack"}
     [:section
      [:div {:class style/section-h3}
       "ontology.ent"
       [:span {:class "devcon-count"}
        (i18n/t (if (dirty? k) :ui/devcon-differs :ui/devcon-matches))]]
      [:div {:class [style/small-muted "u-mb-8"]} (i18n/t :ui/devcon-declaration-hint)]
      [:pre {:class "devcon-edn"} edn]
      [:div {:class "devcon-listrow"}
       [ui/button {:intent :primary :on-click #(-> (.writeText js/navigator.clipboard edn)
                            (.catch (fn [_] nil)))}
        (i18n/t :ui/devcon-copy)]
       [ui/button {:on-click #(download/text! edn "text/plain" (str (kw-name k) ".ent"))}
        (i18n/t :ui/devcon-download)]
       [ui/button {:outlined true :on-click #(model/send! [:dev-apps/discard k])}
        (i18n/t :ui/devcon-discard-draft)]]]]))

(defn- new-application! [] (model/send! [:dev-apps/new (declared)]))

(defn- app-list []
  (let [cur (:selected @model/db)
        ks  (app-keys)]
    [:<>
     [ui/menu
      [ui/menu-item {:text     (i18n/t :ui/devcon-home) :icon :home :active (nil? cur)
                     :on-click #(model/send! [:dev-apps/select nil])}]
      [ui/menu-divider {:title (i18n/t :ui/devcon-applications)}]
      (for [k ks]
        (let [app (app-of k)
              n   (count (problems app))]
          ^{:key k}
          [ui/menu-item {:text          (app-title k app)
                         :active        (= k cur)
                         :label-element [:<>
                                         (when (pos? n)
                                           [pill :bad (i18n/t :ui/devcon-problems {:n n})])
                                         (when (dirty? k)
                                           [pill :warn (i18n/t :ui/devcon-draft)])]
                         :on-click      #(model/send! [:dev-apps/select k])}]))]
     (when (empty? ks) [empty-note (i18n/t :ui/devcon-none-declared)])
     [ui/button {:intent :primary :icon :plus :minimal true :on-click new-application!}
      (i18n/t :ui/devcon-new-app)]]))

(def ^:private home-cols
  [{:label  (i18n/t :ui/devcon-application)
    :render (fn [k] [:<> [:span.u-bold (app-title k (app-of k))]
                     (when (dirty? k) [:span " " [pill :warn (i18n/t :ui/devcon-draft)]])])}
   {:label (i18n/t :ui/status) :render #(status-label (:status (app-of %)))}
   {:label (i18n/t :ui/devcon-sec-hosting) :mono? true :render #(or (host-url (app-of %)) "—")}
   {:label  (i18n/t :ui/devcon-open-problems)
    :render #(let [bad (problems (app-of %))]
               (if (seq bad)
                 [pill :bad (i18n/t :ui/devcon-problems {:n (count bad)})]
                 [pill :ok (i18n/t :ui/devcon-no-problems)]))}])

(defn- home-pane
  []
  (let [ks (app-keys)]
    [:div {:class "devcon-stack"}
     [:div {:class "devcon-tiles"}
      [:div {:class "devcon-tile"}
       [:div {:class "devcon-tile__label"} (i18n/t :ui/devcon-applications)]
       [:div {:class "devcon-tile__value"} (count ks)]]
      [:div {:class "devcon-tile"}
       [:div {:class "devcon-tile__label"} (i18n/t :ui/devcon-open-problems)]
       [:div {:class "devcon-tile__value"} (reduce + 0 (map #(count (problems (app-of %))) ks))]]
      [:div {:class "devcon-tile"}
       [:div {:class "devcon-tile__label"} (i18n/t :ui/devcon-drafts)]
       [:div {:class "devcon-tile__value"} (count (filter dirty? ks))]
       [:div {:class "devcon-tile__meta"} (i18n/t :ui/devcon-drafts-local)]]]
     (if (empty? ks)
       [empty-note (i18n/t :ui/devcon-none-intro)]
       [table/data-table {:rows ks :cols home-cols
                          :on-row-click #(model/send! [:dev-apps/open %])}])]))

(defn- section-nav []
  [ui/tabs {:id :devcon-sections
            :selected-tab-id (:section @model/db :overview)
            :on-change show-section!}
   (for [{:keys [id label]} sections]
     [ui/tab {:id id :title (i18n/t label)}])])

(defn- head []
  (let [k   (:selected @model/db)
        app (when k (app-of k))]
    (if-not app
      [page/header {:title (i18n/t :ui/devcon-home) :subtitle (i18n/t :ui/devcon-intro)}
       [ui/button {:intent :primary :on-click new-application!} (i18n/t :ui/devcon-new-app)]]
      [page/header
       {:title    (app-title k app)
        :subtitle [:span.u-flex-wrap-gap-6
                   [pill (if (= :active (:status app)) :ok :mute) (status-label (:status app))]
                   (when-let [u (host-url app)] [:span {:class "devcon-mono"} u])
                   (when (dirty? k) [pill :warn (i18n/t :ui/devcon-draft-uncommitted)])]}
       [section-nav]])))

(defn- body []
  (let [k   (:selected @model/db)
        app (when k (app-of k))]
    (if-not app
      [home-pane]
      (case (:section @model/db :overview)
        :overview    [overview-pane k app]
        :oauth       [oauth-pane k app]
        :sdk         [sdk-pane k app]
        :permissions [permissions-pane k app]
        :hosting     [hosting-pane k app]
        :metrics     [metrics-pane k app]
        :declaration [declaration-pane k app]
        [overview-pane k app]))))

(page/view! :dev-apps/list app-list)
(page/view! :dev-apps/head head)
(page/view! :dev-apps/body body)

(def ^:private tree
  (page/werkzeug
   {:sidebar       :dev-apps/list
    :sidebar-label :ui/devcon-applications
    :main          {:kind  :rows
                    :items [(page/view :dev-apps/head)
                            {:kind :flow :padding :compact :box {:grow? true}
                             :items [(page/view :dev-apps/body)]}]}}))

(defn- console [] [platform/module :developer-apps tree])

(def developer-apps
  (machine/entering model/console
                    #(vector :dev-apps/restore (storage/get-edn drafts-key) (declared))
                    console))
