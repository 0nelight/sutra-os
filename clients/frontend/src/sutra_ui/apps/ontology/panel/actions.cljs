(ns sutra-ui.apps.ontology.panel.actions
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.apps.ontology.fields :as fields]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.style :as style]
            [sutra-ui.object.card :as oc]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.ontology.panel.common :refer [confirm-delete! section-shell]]))

(def action-editor-form (fs/form :action-editor))

(def action-wizard (fs/form :action-wizard))

(def ^:private empty-action-form
  {:editing-name nil :name "" :display-name "" :description "" :params [] :rules []
   :submission-criteria []})

(def empty-wizard {:display-name "" :kind "update" :object-type "" :props #{}})

(def ^:private scalar-param-types ["string" "number" "boolean" "date" "timestamp"])

(defn- ot-name-opts []  (ont-model/authorable-object-types))

(defn- lt-name-opts []  (sort (keys (get @catalog/name->rid :link-types))))

(defn- xform-name-opts [] (sort (keys (:transforms @catalog/canonical-schema))))

(defn- source-name-opts [] (sort (keys (:sources @catalog/canonical-schema))))

(defn- query-fn-name-opts
  []
  (sort (keys (into {} (filter (comp :function-name val))
                    (:transforms @catalog/canonical-schema)))))

(defn- edit-fn-name-opts
  []
  (sort (keep :function-name (vals (:functions @catalog/canonical-schema)))))

(defn- declared-params [form] (->> (:params form) (map :pname) (filter seq)))

(defn- drop-at [v i] (vec (concat (subvec v 0 i) (subvec v (inc i)))))

(def ^:private new-param "__new__")

(defn- check
  [checked on-change label]
  [ui/checkbox {:class [style/label "u-m-0"] :inline true
                :checked (boolean checked) :on-change on-change :label label}])

(def ^:private via fields/via)

(defn- pick
  ([value items on-change] (pick value items on-change nil))
  ([value items on-change opts]
   [fields/pick value items on-change (merge {:class "u-flex-1"} opts)]))

(defn- labelled [pairs] (for [[v k] pairs] {:value v :label (i18n/t k)}))

(def ^:private named fields/named)

(defn- param-text
  [form p]
  (let [lbl (some #(when (= p (:pname %)) (:label %)) (:params form))]
    (if (seq lbl) (str lbl " · " p) p)))

(defn- param-pick
  ([form opts val on-change] (param-pick form opts val on-change nil))
  ([form opts val on-change on-new]
   [pick val
    (cond-> (mapv (fn [p] {:value p :label (param-text form p)}) opts)
      (and (seq val) (not (some #{val} opts))) (conj {:value val :label val})
      on-new (conj {:value new-param :label (i18n/t :ui/new-param-option)}))
    #(if (and on-new (= % new-param)) (on-new) (on-change %))
    {:placeholder (i18n/t :ui/pick-a-param)}]))

(defn- prop-pick [ot val on-change]
  [pick val (for [[p t] (ont-model/object-type-props ot)] {:value p :label (str p " · " t)})
   on-change {:placeholder (i18n/t :ui/pick-a-property)}])

(def ^:private prefill-opts
  [["uuid" :ui/prefill-uuid] ["now" :ui/prefill-now]
   ["current-user" :ui/prefill-current-user] ["current-object" :ui/prefill-current-object]])

(def ^:private value-srcs
  [["param" :ui/value-src-param] ["prop" :ui/default-prop] ["static" :ui/default-static]
   ["now" :ui/prefill-now] ["user" :ui/prefill-current-user]])

(def ^:private webhook-srcs
  [["param" :ui/value-src-param] ["prop" :ui/default-prop] ["static" :ui/default-static]])

(defn- scalar? [t] (some #{t} scalar-param-types))

(defn- action-param-row [form-atom i]
  (let [form @form-atom
        {:keys [pname type label required hidden prefill default-src default-from
                default-prop extra]}
        (get-in form [:params i])
        setp (fn [k v] (fs/edit! form-atom assoc-in [:params i k] v))]
    [:div {:class "ontology-action__rule-card"}
     [:div {:class "ontology-action__rule-head"}
      [:span {:class "ontology-action__param-title"}
       (if (seq label) label (or (not-empty pname) "…"))]
      [:span {:class "ontology-action__rule-tag"} (or type "")]
      (when-let [n (some-> (:overrides extra) seq count)]
        [ui/tooltip {:content (i18n/t :ui/param-conditional-hint)}
         [:span {:class "ontology-action__rule-tag"}
          (i18n/t :ui/param-conditional {:count n})]])
      [:span {:class "u-flex-1"}]
      [ui/tooltip {:content (i18n/t :ui/remove)}
       [ui/button
        {:minimal true :small true :icon :cross
         :on-click #(fs/edit! form-atom update :params drop-at i)
         :aria-label (i18n/t :ui/remove)}]]]
     [form/row
      [inputs/text-input (fs/at form-atom [:params i :label])
       {:placeholder (i18n/t :ui/param-display-name) :class "u-flex-1" :small true}]
      [ui/tooltip {:content (i18n/t :ui/param-api-name) :class "u-flex-1"
                   :target-tag-name :div}
       [inputs/text-input (via pname #(fs/edit! form-atom ont-model/rename-param i %))
        {:placeholder (i18n/t :ui/param-name-example) :class "ontology-action__mono"
         :small true :fill true}]]
      [pick type (concat (map #(assoc % :group (i18n/t :ui/object-type)) (named (ot-name-opts)))
                         (map #(assoc % :group (i18n/t :ui/scalar)) (named scalar-param-types)))
       #(setp :type %)
       {:placeholder (i18n/t :ui/pick-a-kind)}]]
     [form/row
      [check required #(setp :required (.. % -target -checked)) (i18n/t :ui/param-required)]
      [check hidden #(setp :hidden (.. % -target -checked)) (i18n/t :ui/param-hidden)]
      [form/field {:label (i18n/t :ui/param-prefill) :inline? true :class "u-flex-1"}
       [pick prefill (labelled prefill-opts) #(setp :prefill %)
        {:placeholder (i18n/t :ui/prefill-none)}]]]
     [form/field {:label (i18n/t :ui/param-default) :inline? true}
      [form/row
       [pick default-src (labelled [["static" :ui/default-static] ["prop" :ui/default-prop]])
        #(setp :default-src %) {:placeholder (i18n/t :ui/default-none)}]
       (case default-src
         "static" [inputs/text-input (fs/at form-atom [:params i :default])
                   {:class "u-flex-1" :small true}]
         "prop"   [:<>
                   [param-pick form (ont-model/object-param-names form) default-from
                    #(setp :default-from %)]
                   [prop-pick (ont-model/form-param-type form default-from) default-prop
                    #(setp :default-prop %)]]
         nil)]]
     (when (scalar? type)
       [form/field {:label (i18n/t :ui/param-allowed-values) :inline? true}
        [inputs/text-input (fs/at form-atom [:params i :one-of])
         {:placeholder (i18n/t :ui/param-allowed-values-hint) :small true :fill true}]])]))

(defn- value-row
  [form-atom path srcs left on-new]
  (let [form (deref form-atom)
        {:keys [src param oprop raw]} (get-in form path)
        setv (fn [k v] (fs/edit! form-atom assoc-in (conj path k) v))]
    [:div {:class "ontology-form__field-row"}
     left
     [:span "←"]
     (if (= "expr" src)
       [ui/tooltip {:content (i18n/t :ui/value-src-expr)}
        [:code {:class "u-flex-1"} (pr-str raw)]]
       [pick (or src "param") (labelled srcs) #(setv :src %) {:clearable? false}])
     (case src
       "prop"   [:<>
                 [param-pick form (ont-model/object-param-names form) param #(setv :param %)]
                 [prop-pick (ont-model/form-param-type form param) oprop #(setv :oprop %)]]
       "static" [inputs/text-input (fs/at form-atom (conj path :value))
                 {:class "u-flex-1" :small true}]
       ("now" "user" "expr") nil
       [param-pick form (declared-params form) param #(setv :param %) on-new])]))

(defn- rule-value-row [form-atom i j]
  (let [form (deref form-atom)
        rule (get-in form [:rules i])
        ot   (ont-model/rule-object-type form rule)
        prop (get-in rule [:vals j :prop])]
    [value-row form-atom [:rules i :vals j] value-srcs
     [:<>
      [prop-pick ot prop #(fs/edit! form-atom ont-model/set-value-prop i j %)]]
     #(fs/edit! form-atom ont-model/new-param-for-value i j)]))

(defn- webhook-inputs
  [form-atom i]
  (let [form   @form-atom
        rule   (get-in form [:rules i])
        inputs (ont-model/webhook-inputs (get-in rule [:pick :source])
                                         (get-in rule [:free :webhook]))
        have   (set (map :prop (:wvals rule)))
        rows   (:wvals rule)]
    [:div
     [:div {:class "ontology-action__section-hint"} (i18n/t :ui/webhook-inputs-hint)]
     (when (and (empty? inputs) (empty? rows))
       [:div {:class "ontology-action__section-hint"} (i18n/t :ui/webhook-pick-first)])
     (doall
      (for [j (range (count rows))
            :let [p (get-in rows [j :prop])]]
        ^{:key (str "w" j)}
        [value-row form-atom [:rules i :wvals j] webhook-srcs
         [:span {:class "ontology-action__input-name"} p]
         #(fs/edit! form-atom ont-model/new-param-for-webhook-input i j)]))
     (doall
      (for [p inputs :when (not (contains? have p))]
        ^{:key (str "m" p)}
        [:div {:class "ontology-form__field-row"}
         [:span {:class "ontology-action__input-name"} p]
         [:span "←"]
         [ui/button
          {:small true :on-click #(fs/edit! form-atom update-in [:rules i :wvals] (fnil conj [])
                       {:prop p :src "param" :param ""})}
          (i18n/t :ui/webhook-map-input)]]))]))

(defn- rule-field-label [f] (i18n/t (keyword "ui" (str "rule-field-" (name f)))))

(defn- rule-op-items []
  (for [cat (distinct (map :cat ont-model/action-rule-ops))
        :let [g (i18n/t (keyword "ui" (str "rule-cat-"
                                           (str/replace (str/lower-case cat) " " "-"))))]
        {:keys [op]} (filter #(= cat (:cat %)) ont-model/action-rule-ops)]
    {:value (name op) :label (i18n/t (keyword "ui" (str "rule-op-" (name op)))) :group g}))

(defn- implicit-param? [form p]
  (contains? (set (map name (keys (:implicit form)))) (str p)))

(defn- action-rule-row [form-atom i]
  (let [form @form-atom
        rule (get-in form [:rules i])
        op   (:op rule)
        m    (ont-model/action-op-meta op)
        tref (:type-ref m)
        setk (fn [path v] (fs/edit! form-atom assoc-in (into [:rules i] path) v))]
    [:div {:class "ontology-action__rule-card"}
     [:div {:class "ontology-action__rule-head"}
      [:span {:class "ontology-action__rule-tag"} (i18n/t :ui/rule-n {:n (inc i)})]
      (if (and (seq op) (nil? m))
        [:span {:class "u-flex-1 ontology-action__section-hint"}
         (i18n/t :ui/unknown-rule-op {:op op})]
        [:div {:class "u-flex-1"}
         [pick op (rule-op-items)
          #(fs/edit! form-atom assoc-in [:rules i]
                     {:op % :type "" :bind {} :pick {} :free {}
                      :text {} :bool {} :vals [] :wvals []})
          {:placeholder (i18n/t :ui/pick-a-rule) :fill true}]])
      [ui/tooltip {:content (i18n/t :ui/remove-rule)}
       [ui/button
        {:minimal true :small true :icon :cross
         :on-click #(fs/edit! form-atom update :rules drop-at i)
         :aria-label (i18n/t :ui/remove-rule)}]]]
     (when m
       [:div {:class "ontology-action__grid"}
        (when tref
          [form/field {:label (i18n/t (cond (:type-is-provenance? m)    :ui/rule-field-provenance
                                            (= :link-types (:kind tref)) :ui/link-type
                                            :else                        :ui/object-type))}
           [pick (:type rule)
            (named (if (= :link-types (:kind tref)) (lt-name-opts) (ot-name-opts)))
            #(fs/edit! form-atom ont-model/set-rule-type i %) {:class nil}]])
        (doall
         (for [[f _] (:binds m)
               :when (not (and (:value-map? m) (= :values f)
                               (empty? (get-in rule [:bind :values]))))
               :let [cur (get-in rule [:bind f])
                     target? (and (= :target f) (#{:update :delete-object} (:op m)))
                     opts (if target?
                            (ont-model/object-param-names form (not-empty (:type rule)))
                            (declared-params form))]]
           ^{:key (str "b" (name f))}
           [form/field {:label (rule-field-label f)}
            (if (implicit-param? form cur)
              [:div {:class "ontology-action__section-hint"} (i18n/t :ui/rule-id-auto)]
              [param-pick form opts cur #(setk [:bind f] %)
               (when (and target? (seq (:type rule)))
                 #(fs/edit! form-atom
                            (fn [fm]
                              (let [[fm p] (ont-model/ensure-param fm (:type rule) (:type rule)
                                                                   {:required true
                                                                    :prefill "current-object"})]
                                (assoc-in fm [:rules i :bind :target] p)))))])]))
        (doall
         (for [[f kind _] (:picks m)]
           ^{:key (str "p" (name f))}
           [form/field {:label (rule-field-label f)}
            [pick (get-in rule [:pick f])
             (named (case kind
                      :sources         (source-name-opts)
                      :query-functions (query-fn-name-opts)
                      :edit-functions  (edit-fn-name-opts)
                      (xform-name-opts)))
             #(setk [:pick f] %) {:class nil}]]))
        (doall
         (for [[f _] (:free m)
               :let [whs (when (and (:webhook-params? m) (= :webhook f))
                           (some-> (get-in @catalog/canonical-schema
                                           [:sources (keyword (get-in rule [:pick :source]))
                                            :webhooks])
                                   keys sort))]]
           ^{:key (str "f" (name f))}
           [form/field {:label (rule-field-label f)}
            (if (seq whs)
              [pick (get-in rule [:free f]) (named whs) #(setk [:free f] %) {:class nil}]
              [inputs/text-input (fs/at form-atom [:rules i :free f]) {:fill true :small true}])]))
        (doall
         (for [[f _] (:texts m)]
           ^{:key (str "t" (name f))}
           [form/field {:label (rule-field-label f)}
            [inputs/text-input (fs/at form-atom [:rules i :text f]) {:fill true :small true}]]))
        (doall
         (for [[f _] (:bools m)]
           ^{:key (str "x" (name f))}
           [check (get-in rule [:bool f]) #(setk [:bool f] (.. % -target -checked))
            (rule-field-label f)]))])
     (when (:value-map? m)
       [:div
        [:div {:class "ontology-action__section-hint"} (i18n/t :ui/rule-values-hint)]
        (doall (for [j (range (count (:vals rule)))]
                 ^{:key j}
                 [:div {:class "ontology-action__value-line"}
                  [rule-value-row form-atom i j]
                  [ui/tooltip {:content (i18n/t :ui/remove)}
                   [ui/button
                    {:minimal true :small true :icon :cross
                     :on-click #(fs/edit! form-atom update-in [:rules i :vals] drop-at j)
                     :aria-label (i18n/t :ui/remove)}]]]))
        [ui/button
         {:small true :on-click #(fs/edit! form-atom update-in [:rules i :vals] (fnil conj [])
                      {:prop "" :src "param" :param ""})} (i18n/t :ui/add-property)]])
     (when (:webhook-params? m)
       [webhook-inputs form-atom i])]))

(def ^:private crit-op-keys
  {"=" :ui/crit-op-eq "not=" :ui/crit-op-ne "!=" :ui/crit-op-ne "<" :ui/crit-op-lt
   ">" :ui/crit-op-gt
   "<=" :ui/crit-op-le ">=" :ui/crit-op-ge "matches" :ui/crit-op-matches "in" :ui/crit-op-in})

(defn- value-kind
  [form {:keys [left-src param prop]}]
  (let [t (case left-src
            "user" "string"
            "prop" (some (fn [[p b]] (when (= p prop) b))
                         (ont-model/object-type-props (ont-model/form-param-type form param)))
            (ont-model/form-param-type form param))]
    (cond (nil? t)                                             :unknown
          (#{"number" "integer" "long" "short" "double" "float" "decimal" "date"
             "timestamp"} t) :ordered
          (#{"string"} t)                                      :text
          :else                                                :other)))

(defn- crit-ops-for [form c]
  (let [k   (value-kind form c)
        ops (for [{:keys [op for]} ont-model/crit-operators
                  :when (or (= :unknown k) (contains? for :any) (contains? for k))]
              op)]
    (cond-> (vec ops) (and (seq (:op c)) (not (some #{(:op c)} ops))) (conj (:op c)))))

(defn- crit-condition [form-atom path rm-btn]
  (let [form (deref form-atom)
        c    (get-in form path)
        setc (fn [k v] (fs/edit! form-atom assoc-in (conj path k) v))
        {:keys [left-src param prop op right-src rparam rprop]} c
        side (fn [src p pr set-p set-pr]
               (case src
                 "prop" [:<> [param-pick form (ont-model/object-param-names form) p set-p]
                         [prop-pick (ont-model/form-param-type form p) pr set-pr]]
                 ("user" "static" "none") nil
                 [param-pick form (declared-params form) p set-p]))]
    [:div
     [:div {:class "u-flex-row-center-6"}
      [pick (or left-src "param")
       (labelled [["param" :ui/value-src-param] ["prop" :ui/default-prop]
                  ["user" :ui/prefill-current-user]])
       #(setc :left-src %) {:clearable? false}]
      (side left-src param prop #(setc :param %) #(setc :prop %))
      rm-btn]
     [:div {:class "u-flex-row-center-6"}
      [pick (or op "=")
       (for [o (crit-ops-for form c)]
         {:value o :label (if-let [k (crit-op-keys o)] (i18n/t k) o)})
       #(setc :op %) {:clearable? false :class "ontology-action__select"}]
      (if (= "in" op)
        [inputs/text-input (fs/at form-atom (conj path :value))
         {:placeholder (i18n/t :ui/crit-values-placeholder) :class "u-flex-1" :small true}]
        [:<>
         [pick (or right-src "static")
          (labelled [["static" :ui/default-static] ["param" :ui/value-src-param]
                     ["prop" :ui/default-prop] ["none" :ui/crit-right-none]])
          #(setc :right-src %) {:clearable? false}]
         (if (contains? #{nil "static"} right-src)
           [inputs/text-input (fs/at form-atom (conj path :value))
            {:placeholder (i18n/t :ui/crit-value-placeholder) :class "u-flex-1" :small true}]
           (side right-src rparam rprop #(setc :rparam %) #(setc :rprop %)))])]]))

(def ^:private new-condition {:left-src "param" :param "" :op "=" :right-src "static" :value ""})

(defn- crit-node [form-atom path rm!]
  (let [node   (get-in @form-atom path)
        rm-btn [ui/tooltip {:content (i18n/t :ui/remove)}
                [ui/button
                 {:minimal true :small true :icon :cross :on-click rm!
                  :aria-label (i18n/t :ui/remove)}]]]
    (if (:group node)
      [:div {:class "ontology-action__crit-group"}
       [:div {:class "u-flex-row-center-6"}
        [pick (:group node)
         (labelled [["all" :ui/crit-logic-all] ["any" :ui/crit-logic-any]
                    ["none" :ui/crit-logic-none]])
         #(fs/edit! form-atom assoc-in (conj path :group) %) {:clearable? false}]
        rm-btn]
       (doall
        (for [j (range (count (:children node)))]
          ^{:key j}
          [crit-node form-atom (into path [:children j])
           #(fs/edit! form-atom update-in (conj path :children) drop-at j)]))
       [:div {:class "ontology-form__field-row"}
        [ui/button
         {:small true
          :on-click #(fs/edit! form-atom update-in (conj path :children)
                               (fnil conj []) new-condition)}
         (i18n/t :ui/add-condition)]
        [ui/button
         {:small true :on-click #(fs/edit! form-atom update-in (conj path :children) (fnil conj [])
                      {:group "all" :children [new-condition]})} (i18n/t :ui/add-group)]]]
      [crit-condition form-atom path rm-btn])))

(defn- action-criterion-row [form-atom i]
  (let [form (deref form-atom)
        c    (get-in form [:submission-criteria i])
        rm!  #(fs/edit! form-atom update :submission-criteria drop-at i)]
    (if (:raw c)
      [:div {:class "u-flex-row-center-6"}
       [:span (i18n/t :ui/crit-raw)] [:code {:class "u-flex-1"} (pr-str (:raw c))]
       [ui/tooltip {:content (i18n/t :ui/remove)}
        [ui/button
         {:minimal true :small true :icon :cross :on-click rm! :aria-label (i18n/t :ui/remove)}]]]
      [:div {:class "ontology-action__rule-card"}
       [crit-node form-atom [:submission-criteria i] rm!]
       [inputs/text-input (fs/at form-atom [:submission-criteria i :message])
        {:placeholder (i18n/t :ui/crit-message) :fill true :small true}]])))

(defn- move-at [v i d]
  (let [j (+ i d)]
    (if (< -1 j (count v)) (assoc v i (v j) j (v i)) v)))

(defn- order-btns [on-move on-remove]
  [:<>
   [ui/tooltip {:content (i18n/t :ui/move-up)}
    [ui/button
     {:minimal true :small true :icon :arrow-up :on-click #(on-move -1)
      :aria-label (i18n/t :ui/move-up)}]]
   [ui/tooltip {:content (i18n/t :ui/move-down)}
    [ui/button
     {:minimal true :small true :icon :arrow-down :on-click #(on-move 1)
      :aria-label (i18n/t :ui/move-down)}]]
   [ui/tooltip {:content (i18n/t :ui/remove)}
    [ui/button
     {:minimal true :small true :icon :cross :on-click on-remove
      :aria-label (i18n/t :ui/remove)}]]])

(def ^:private hide-ops
  [["set" :ui/form-section-hide-set]
   ["empty" :ui/form-section-hide-empty] ["=" :ui/form-section-hide-eq]
   ["!=" :ui/form-section-hide-ne]])

(defn- section-hide-row [form-atom i]
  (let [form @form-atom
        {:keys [hide-raw hide-param hide-op]} (get-in form [:sections i])
        sets (fn [k v] (fs/edit! form-atom assoc-in [:sections i k] v))]
    [form/field {:label (i18n/t :ui/form-section-hide-when) :inline? true}
     (if hide-raw
       [form/row
        [:code {:class "u-flex-1"} (pr-str hide-raw)]
        [ui/tooltip {:content (i18n/t :ui/remove)}
         [ui/button
          {:minimal true :small true :icon :cross
           :on-click #(fs/edit! form-atom update-in [:sections i] dissoc :hide-raw)
           :aria-label (i18n/t :ui/remove)}]]]
       [form/row
        [pick hide-op (labelled hide-ops) #(sets :hide-op %)
         {:class nil :placeholder (i18n/t :ui/form-section-hide-never)}]
        (when (seq hide-op)
          [param-pick form (declared-params form) hide-param #(sets :hide-param %)])
        (when (#{"=" "!="} hide-op)
          [inputs/text-input (fs/at form-atom [:sections i :hide-value])
           {:class "u-flex-1" :small true}])])]))

(defn- form-section-card [form-atom i unplaced]
  (let [form @form-atom
        {:keys [columns collapsible collapsed params]} (get-in form [:sections i])
        sets (fn [k v] (fs/edit! form-atom assoc-in [:sections i k] v))]
    [:div {:class "ontology-action__rule-card"}
     [:div {:class "ontology-action__rule-head"}
      [inputs/text-input (fs/at form-atom [:sections i :title])
       {:placeholder (i18n/t :ui/form-section-title) :class "u-flex-1" :small true}]
      [order-btns #(fs/edit! form-atom update :sections move-at i %)
       #(fs/edit! form-atom update :sections drop-at i)]]
     [:div {:class "ontology-form__field-row"}
      [inputs/text-input (fs/at form-atom [:sections i :description])
       {:placeholder (i18n/t :ui/form-section-description) :class "u-flex-1" :small true}]]
     [:div {:class "ontology-form__field-row"}
      [pick (or columns 1)
       (labelled [[1 :ui/form-section-cols-1] [2 :ui/form-section-cols-2]])
       #(sets :columns %) {:class nil :clearable? false}]
      [check (or collapsible collapsed)
       #(let [on? (.. % -target -checked)]
          (sets :collapsible on?)
          (when-not on? (sets :collapsed false)))
       (i18n/t :ui/form-section-collapsible)]
      [check collapsed
       #(let [on? (.. % -target -checked)]
          (sets :collapsed on?)
          (when on? (sets :collapsible true)))
       (i18n/t :ui/form-section-collapsed)]]
     [section-hide-row form-atom i]
     (doall
      (for [[j p] (map-indexed vector params)]
        ^{:key (str p j)}
        [:div {:class "ontology-form__field-row ontology-action__form-param"}
         [:span {:class "u-flex-1"} (param-text form p)]
         [order-btns #(fs/edit! form-atom update-in [:sections i :params] move-at j %)
          #(fs/edit! form-atom update-in [:sections i :params] drop-at j)]]))
     (when (seq unplaced)
       [:div {:class "ontology-form__field-row"}
        [param-pick form unplaced nil
         #(when (seq %) (fs/edit! form-atom update-in [:sections i :params] (fnil conj []) %))]])]))

(defn- form-layout [form-atom]
  (let [form     @form-atom
        placed   (into #{} (mapcat :params) (:sections form))
        unplaced (vec (remove placed (declared-params form)))]
    [:<>
     [:div {:class "ontology-action__subsection-label"} (i18n/t :ui/form-layout)]
     [:div {:class "ontology-action__section-hint"} (i18n/t :ui/form-layout-hint)]
     (doall (for [i (range (count (:sections form)))]
              ^{:key i} [form-section-card form-atom i unplaced]))
     (when (and (seq (:sections form)) (seq unplaced))
       [:div {:class "ontology-action__section-hint"}
        (i18n/t :ui/form-layout-unplaced
                {:params (str/join ", " (map #(param-text form %) unplaced))})])
     [ui/button {:small true :on-click #(fs/edit! form-atom update :sections (fnil conj [])
                             {:title "" :description "" :columns 1 :params []})}
      (i18n/t :ui/form-section-add)]]))

(defn- action-section-label [t]
  [:div {:class "ontology-action__section-label"} t])

(defn- action-problems [problems]
  (when (seq problems)
    [:div {:class "ontology-action__problems"}
     [:div {:class "ontology-action__section-label"} (i18n/t :ui/problems-title)]
     (for [[n {:keys [level key args]}] (map-indexed vector problems)]
       ^{:key n}
       [:div
        {:class (if (= :error level) "ontology-action__problem" "ontology-action__problem--warn")}
        (i18n/t key args)])]))

(defn- on-branch-options
  [form-atom]
  (let [on   (get-in @form-atom [:extra :on-branch])
        tick (fn [k label]
               [ui/checkbox {:checked (true? (get on k))
                             :on-change #(fs/edit! form-atom assoc-in [:extra :on-branch k]
                                                   (.. % -target -checked))
                             :label (i18n/t label)}])]
    [:div
     [action-section-label (i18n/t :ui/action-on-branch)]
     [:div {:class style/small-muted} (i18n/t :ui/action-on-branch-hint)]
     (tick :webhooks :ui/action-on-branch-webhooks)
     (tick :external-calls :ui/action-on-branch-external)
     (tick :notifications :ui/action-on-branch-notifications)
     (when (true? (:notifications on))
       [form/field {:label (i18n/t :ui/action-on-branch-recipients) :inline? true}
        [pick (keyword (name (or (:notification-recipients on) :default)))
         (labelled [[:default :ui/action-on-branch-recipients-default]
                    [:branch-owner :ui/action-on-branch-recipients-owner]])
         #(fs/edit! form-atom assoc-in [:extra :on-branch :notification-recipients] %)
         {:class nil :clearable? false}]])]))

(defn- action-editor [form-atom {:keys [on-save on-cancel save-label on-delete]}]
  (let [form     @form-atom
        new?     (nil? (:editing-name form))
        add!     (fn [k v] (fs/edit! form-atom update k (fnil conj []) v))
        problems (ont-model/action-form-problems form)
        blocked? (some #(= :error (:level %)) problems)
        hidden-n (count (:implicit form))]
    [:div
     [action-section-label (i18n/t :ui/action-overview)]
     [form/field {:label (i18n/t :ui/action-display-name)}
      [inputs/text-input
       (via (:display-name form)
            (fn [v] (fs/edit! form-atom (fn [f] (cond-> (assoc f :display-name v)
                                                  (and new? (not (:api-touched? f)))
                                                  (assoc :name (ont-model/api-name-of v)))))))
       {:placeholder (i18n/t :ui/action-display-name-example) :fill true}]]
     [form/field {:label (i18n/t :ui/action-api-name) :help (i18n/t :ui/action-api-name-hint)}
      [inputs/text-input
       (via (:name form) #(fs/edit! form-atom assoc :name % :api-touched? true))
       {:placeholder (i18n/t :ui/action-name-example) :disabled (not new?)
        :class "ontology-action__mono" :fill true}]]
     [form/field {:label (i18n/t :ui/action-description)}
      [ui/text-area {:value (or (:description form) "") :rows 2
                     :on-change #(fs/edit! form-atom assoc :description (.. % -target -value))
                     :fill true}]]

     [action-section-label (i18n/t :ui/rules)]
     [:div {:class "ontology-action__section-hint"} (i18n/t :ui/rules-hint)]
     (doall (for [i (range (count (:rules form)))] ^{:key i} [action-rule-row form-atom i]))
     (when (seq (:log-rules form))
       [:div {:class "ontology-action__section-hint"} (i18n/t :ui/rule-log-note)])
     [ui/button
      {:small true :on-click #(add! :rules
                        {:op "" :type "" :bind {} :pick {} :free {} :text {} :bool {} :vals []
                         :wvals []})} (i18n/t :ui/add-rule)]

     [action-section-label (i18n/t :ui/action-form-section)]
     [:div {:class "ontology-action__section-hint"} (i18n/t :ui/parameters-hint)]
     (doall (for [i (range (count (:params form)))] ^{:key i} [action-param-row form-atom i]))
     (when (pos? hidden-n)
       [:div {:class "ontology-action__section-hint"} (i18n/t :ui/params-implicit {:n hidden-n})])
     [ui/button
      {:small true :on-click #(add! :params {:pname "" :type "" :label "" :prefill ""})}
      (i18n/t :ui/add-parameter)]
     [form-layout form-atom]

     [action-section-label (i18n/t :ui/submission-criteria)]
     [:div {:class "ontology-action__section-hint"} (i18n/t :ui/submission-criteria-hint)]
     (doall (for [i (range (count (:submission-criteria form)))]
              ^{:key i} [action-criterion-row form-atom i]))
     [:div {:class "ontology-form__field-row"}
      [ui/button {:small true :on-click #(add! :submission-criteria new-condition)}
       (i18n/t :ui/add-criterion)]
      [ui/button
       {:small true
        :on-click #(add! :submission-criteria {:group "all" :children [new-condition] :message ""})}
       (i18n/t :ui/add-logic)]]

     [on-branch-options form-atom]
     [action-problems problems]
     [:div {:class "ontology-action__editor-actions"}
      [overlay/form-actions {:on-cancel    on-cancel
                             :ok-label     (or save-label (i18n/t :ui/create))
                             :on-ok        on-save
                             :ok-disabled? blocked?}]
      (when on-delete
        [ui/button {:intent :danger :outlined true :on-click on-delete}
         (i18n/t :ui/delete)])]]))

(defn- action-wizard-view
  [on-continue on-cancel]
  (let [w     @action-wizard
        setw  (fn [k v] (fs/edit! action-wizard assoc k v))
        ot    (:object-type w)
        props? (#{"create" "update"} (:kind w))]
    [:div
     [form/field {:label (i18n/t :ui/action-display-name)}
      [inputs/text-input (fs/at action-wizard :display-name)
       {:placeholder (i18n/t :ui/action-display-name-example) :fill true}]]
     [form/field {:label (i18n/t :ui/wizard-kind)}
      [pick (:kind w)
       (labelled [["create" :ui/wizard-kind-create] ["update" :ui/wizard-kind-update]
                  ["delete" :ui/wizard-kind-delete] ["other" :ui/wizard-kind-other]])
       #(setw :kind %) {:class nil :clearable? false}]]
     (when (not= "other" (:kind w))
       [form/field {:label (i18n/t :ui/object-type)}
        [pick ot (for [t (ot-name-opts)] {:value (name t) :label (oc/type-label t)})
         #(do (setw :object-type %) (setw :props #{})) {:class nil}]])
     (when (and props? (seq ot))
       [form/field {:label (i18n/t :ui/wizard-props) :help (i18n/t :ui/wizard-props-hint)}
        [:div {:class "ontology-action__prop-grid"}
         (for [[p t] (ont-model/object-type-props ot)]
           ^{:key p}
           [check (contains? (:props w) p)
            #(fs/edit! action-wizard update :props
                       (fn [s]
                         (if (.. % -target -checked) (conj (or s #{}) p)
                             (disj s p))))
            [:<> p [:span {:class "ontology-action__rule-tag"} (str " " t)]]])]])
     [:div {:class "ontology-action__editor-actions"}
      [overlay/form-actions {:on-cancel    on-cancel
                             :ok-label     (i18n/t :ui/wizard-continue)
                             :on-ok        on-continue
                             :ok-disabled? (or (str/blank? (:display-name w))
                                               (and (not= "other" (:kind w)) (str/blank? ot)))}]]]))

(defn action-types-pane []
  (let [declared  (sort-by (fn [[k spec]] (str/lower-case (or (:display-name spec) (name k))))
                           (catalog/declared-actions))
        form      @action-editor-form
        open?     (:editor-open? @ont-model/ui)
        close!    (fn []
                    (ont-model/edit! [] assoc :editor-open? false :detail nil)
                    (fs/put! action-editor-form empty-action-form))
        open-new! (fn [] (fs/put! action-wizard empty-wizard)
                    (ont-model/edit! [] assoc :editor-open? :wizard))
        continue! (fn [] (let [w @action-wizard]
                           (fs/put! action-editor-form
                                    (ont-model/wizard->form (update w :props #(sort (vec %)))))
                           (ont-model/edit! [] assoc :editor-open? true)))
        open-edit! (fn [k spec] (fs/put! action-editor-form (ont-model/action->form k spec))
                     (ont-model/edit! [] assoc :editor-open? true))
        save!     (fn []
                    (when (not-any? #(= :error (:level %)) (ont-model/action-form-problems form))
                      (if (:editing-name form)
                        (ont-model/update-action! form)
                        (ont-model/add-action! form))
                      (close!)))]
    [section-shell
     (cond
       (= :wizard open?)
       [:div
        [:div {:class "ontology-action__editor-title"} (i18n/t :ui/action-type-new)]
        [action-wizard-view continue! close!]]

       open?
       [:div
        [:div {:class "ontology-action__editor-title"}
         (if (:editing-name form)
           (i18n/t :ui/action-edit {:name (or (not-empty (:display-name form)) (:name form))})
           (i18n/t :ui/action-type-new))]
        [action-editor action-editor-form
         {:save-label (i18n/t (if (:editing-name form) :ui/save :ui/create))
          :on-save save! :on-cancel close!
          :on-delete (when (:editing-name form)
                       (fn []
                         (confirm-delete! (i18n/t :ui/action-delete-confirm
                                                  {:name (or (not-empty (:display-name form))
                                                             (name (:editing-name form)))})
                                          [[:dispatch [:ontology/mutate :delete-action
                                                       {:name (:editing-name form)}]]
                                           [:invoke [close!]]])))}]]

       :else
       [:div
        [:div {:class "ontology-list__header-row"}
         [:div {:class "u-section-label"}
          (str (i18n/t :ui/action-types) " (" (count declared) ")")]
         [ui/button {:intent :primary :on-click open-new!}
          (i18n/t :ui/action-type-new-button)]]
        (if (empty? declared)
          [:div {:class "u-text-empty"}
           (i18n/t :ui/action-types-none)]
          (doall
           (for [[k spec] declared]
             ^{:key k}
             [:div {:on-click #(open-edit! k spec)
                    :class "ontology-action__list-row"}
              [:span {:class "ontology-action__list-icon"} "⚡"]
              [:div {:class "u-flex-1"}
               [:div {:class "ontology-action__list-title"}
                (or (not-empty (:display-name spec)) (clojure.core/name k))]
               [:div {:class "ontology-action__list-sub"}
                [:span {:class "ontology-action__mono"} (clojure.core/name k)]
                (str " · " (i18n/t :ui/rules-count {:n (count (:rules spec))})
                     (let [n (count (remove #(ont-model/hidden-action-param? spec %)
                                            (keys (:params spec))))]
                       (when (pos? n) (i18n/t :ui/params-count-suffix {:n n}))))]]])))])]))
