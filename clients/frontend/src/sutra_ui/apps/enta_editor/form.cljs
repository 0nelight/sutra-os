(ns sutra-ui.apps.enta-editor.form
  (:require [sutra-ui.ui.core :as ui :refer [tag]]
            [sutra.kit.coerce :refer [->kw]]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.bind :as bind]
            [sutra.capps.catalog :as cat]
            [sutra.capps.prop-source :as ps]
            [sutra.capps.variables :as vars]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.enta-editor.state :as es]
            [sutra-ui.apps.enta-editor.event-fields :as ev]
            [sutra-ui.apps.enta-editor.fields :as f]
            [sutra-ui.apps.enta-editor.names :as names]
            [sutra-ui.apps.enta-editor.ui :as eui]
            [sutra-ui.ui.form :as uf]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.object.card :as oc]
            [sutra-ui.graph-editor.shared :as gs]))

(defn action-options
  []
  (vec (sort-by (comp str/lower-case second)
                (for [[k a] (get @catalog/canonical-schema :actions {})]
                  [(name k) (or (not-empty (str (:display-name a))) (name k))]))))

(defn- resource-options
  []
  (let [named (fn [section k] (for [[_ e] (get @catalog/canonical-schema section)
                                    :let [n (get e k)] :when n]
                                (name n)))]
    (vec (sort (distinct (concat (named :datasets :dataset-name)
                                 (named :transforms :transform-name)))))))

(defn- open? [k] (contains? (es/ui-get :form-open) k))
(defn- toggle! [k] (es/ui-flip! :form-open k))

(def ^:private phrase
  {:on-focus (fn [_] (es/mode! :type-start))
   :on-blur  (fn [_] (es/mode! :commit))})

(defn text-field [v on-change placeholder]
  [:div phrase
   [inputs/text-input (f/at (if (keyword? v) (name v) (str (or v "")))
                            #(on-change (when-not (str/blank? %) %)))
    {:placeholder placeholder :fill true}]])

(defn number-field [_v _on-change]
  (let [txt (r/atom nil)]
    (fn [v on-change]
      [ui/numeric-input
       (merge phrase
              {:value (or @txt (if (number? v) (str v) ""))
               :on-value-change (fn [n s _]
                                  (reset! txt s)
                                  (on-change (when-not (or (str/blank? s) (js/isNaN n)) n)))
               :on-blur (fn [e] (reset! txt nil) ((:on-blur phrase) e))
               :fill true})])))

(defn bool-field [v on-change]
  [ui/checkbox {:checked (boolean v)
                :on-change #(on-change (.. % -target -checked))
                :label (if v "ja" "nein")}])

(defn enum-field [v options default on-change]
  [f/select (if (some? v) v (some-> default name))
   (map f/value-str options)
   #(on-change (case % nil nil "true" true "false" false (keyword %)))
   "—" {:fill? true}])

(defn select-field
  [v options on-change placeholder]
  [f/select v options #(on-change (f/kw-or-nil %)) placeholder {:fill? true}])

(defn- new-set-var!
  [path k t]
  (es/put-param-with-var! path k :val
                          (es/fresh-var-name (oc/type-label (keyword t) true))
                          {:source (keyword t)}))

(defn- ref-field
  [_v _var-names _on-change _new-set]
  (let [picking? (r/atom false)]
    (fn [v var-names on-change new-set]
      (let [mode (case (bind/form-id v)
                   (:source :from) :source
                   :val            :val
                   :var            :var
                   (if (nil? v) :none :literal))]
        [:div
         [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
          [f/select (when-not (= :none mode) mode)
           [["source" (i18n/t :ui/app-builder-ref-type)]
            ["val" (i18n/t :ui/app-builder-ref-val)]
            ["var" (i18n/t :ui/app-builder-ref-var)]
            ["literal" (i18n/t :ui/app-builder-ref-literal)]]
           #(on-change (case % nil nil "source" {:source nil} "val" {:val nil}
                             "var" {:var nil} ""))
           "—"]
          [:div {:class "u-flex-1"}
           (case mode
             :none     nil
             :source   [select-field (bind/value-of v) (f/object-type-options)
                        #(on-change {:source %}) "Objekttyp…"]
             :val      [select-field (:val v) (f/var-options var-names) #(on-change {:val %})
                        "Variable…"]
             :var      [select-field (:var v) (f/var-options var-names) #(on-change {:var %})
                        "Variable…"]
             [text-field v on-change "Wert"])]]
         (when new-set
           (if @picking?
             [:div {:style {:margin-top "var(--sp-1)"}}
              [select-field nil (f/object-type-options)
               #(do (reset! picking? false) (when % (new-set %)))
               (i18n/t :ui/app-builder-new-set-var-type)]]
             [:div {:style {:margin-top "var(--sp-1)"}}
              [f/dashed-button (i18n/t :ui/app-builder-new-set-var) #(reset! picking? true)]]))]))))

(defn- handle-field
  [v var-names path k]
  (let [node  (cat/node-at (es/tree) path)
        words (str (:label (cat/node-spec node)) " "
                   (i18n/t (case k
                             :active   :ui/app-builder-out-active
                             :selected :ui/app-builder-out-selected
                             :ui/app-builder-out-value)))]
    [f/select (:var v)
     (conj (f/var-options var-names)
           ["::neu" (i18n/t :ui/app-builder-new-var-named {:name words})])
     #(cond
        (= % "::neu") (es/put-param-with-var! path k :var (es/fresh-var-name words)
                                              {:default nil})
        (str/blank? %) (es/put-param! path k nil)
        :else (es/put-param! path k {:var (keyword %)}))
     "Variable…" {:fill? true}]))

(defn edn-field [_v _on-change]
  (let [txt (r/atom nil)
        err (r/atom nil)]
    (fn [v on-change]
      (let [shown (or @txt (if (nil? v) "" (pr-str v)))]
        [:div
         [ui/text-area {:value shown
                        :on-change (fn [e]
                                     (let [s (.. e -target -value)]
                                       (reset! txt s)
                                       (if (str/blank? s)
                                         (do (reset! err nil) (on-change nil))
                                         (let [parsed (es/read-edn s)]
                                           (reset! err (:error parsed))
                                           (when-not (:error parsed)
                                             (on-change (:value parsed)))))))
                        :on-blur #(reset! txt nil)
                        :rows 3
                        :fill true :class "u-mono-12"}]
         (when @err [:div {:style {:color "var(--c-danger-strong)"
                                   :font-size "var(--fs-xs)"}} @err])]))))

(defn missing-columns
  [otype rows]
  (when otype
    (let [shown (set (keep (comp ->kw :prop) rows))
          disp  (es/prop-display-of (keyword otype))]
      (vec (for [{:keys [name]} (get-in @catalog/schema [:object-types (keyword otype) :fields])
                 :when (not (contains? shown name))
                 :when (not= :hidden
                             (some-> (get-in disp [name :visibility]) clojure.core/name keyword))]
             name)))))

(defn- cols-field
  [v otype on-change]
  (let [rows (vec (or v []))
        disp (when otype (es/prop-display-of otype))]
    [:div {:style {:display "flex" :flex-direction "column" :gap "var(--sp-1)"}}
     (for [[i c] (map-indexed vector rows)
           :let [put!    (fn [k val] (on-change (if (nil? val)
                                                  (assoc rows i (dissoc c k))
                                                  (assoc-in rows [i k] val))))
                 inherit (get disp (:prop c))]]
       ^{:key i}
       [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
        [:div {:class "ab-field--key"}
         [f/prop-field (:prop c) otype {:small true :class "ab-field--key"}
          #(put! :prop %)]]
        [:div {:class "u-flex-1"
               :title (when (:label inherit)
                        (str "Erbt „" (:label inherit) "\" von der Eigenschaft — leer lassen."))}
         [inputs/text-input (f/at (str (or (:label c) ""))
                                  #(put! :label (when-not (str/blank? %) %)))
          {:placeholder (or (:label inherit) "Titel") :small true :fill true}]]
        [f/select (:fmt c) (map name cat/format-options) #(put! :fmt (f/kw-or-nil %))
         (if (:fmt inherit) "fmt: geerbt" "fmt…")]
        [f/row-button :cross "entfernen" #(on-change (vec (concat (subvec rows 0 i)
                                                               (subvec rows (inc i)))))]])
     [:div {:style {:display "flex" :gap "var(--sp-1)"}}
      (f/dashed-button "+ Spalte" #(on-change (conj rows {:prop nil})))
      (when-let [more (seq (missing-columns otype rows))]
        (f/dashed-button (i18n/t :ui/app-builder-add-all-props)
                         #(on-change (into (vec (remove (comp nil? :prop) rows))
                                           (map (fn [p] {:prop p}))
                                           more))))]]))

(declare param-field)

(defn- items-field [{item-spec :item :keys [add-label]} v var-names otype on-change]
  (let [rows (vec (or v []))]
    [:div {:style {:display "flex" :flex-direction "column" :gap "var(--sp-2)"}}
     (for [[i it] (map-indexed vector rows)]
       ^{:key i}
       [:div {:style {:border "1px solid var(--c-border-soft)" :border-radius "var(--radius)"
                      :padding "var(--sp-1) var(--sp-3)"}}
        [:div {:style {:display "flex" :justify-content "space-between"}}
         [:span {:style {:color gs/grey :font-size "var(--fs-sm)"}} (str "#" (inc i))]
         [:span
          [f/row-button :arrow-up "nach oben" #(on-change (cat/item-moved rows i -1))]
          [f/row-button :arrow-down "nach unten" #(on-change (cat/item-moved rows i 1))]
          [f/row-button :duplicate "duplizieren" #(on-change (cat/item-duplicated rows i))]
          [f/row-button :cross "entfernen" #(on-change (vec (concat (subvec rows 0 i)
                                                                 (subvec rows (inc i)))))]]]
        (for [{:keys [key] :as p} item-spec]
          ^{:key key}
          [uf/field {:label (cat/param-label key) :inline? true :class "ab-row-field"}
           [param-field p (get it key) var-names otype
            (fn [val] (on-change (if (nil? val)
                                   (assoc rows i (dissoc it key))
                                   (assoc-in rows [i key] val))))]])])
     (f/dashed-button (str "+ " (or add-label "Eintrag")) #(on-change (conj rows {})))]))

(defn action-params
  [action]
  (let [declared (get-in @catalog/canonical-schema [:actions (some-> action name keyword) :params])]
    (vec (sort-by (comp str/lower-case second)
                  (for [[k spec] declared
                        :when (not= :hidden (some-> (:exposure spec) name keyword))]
                    [k (or (not-empty (str (:name spec))) (name k)) spec])))))

(defn hidden-without-default
  [action defaults]
  (vec (for [[k label spec] (action-params action)
             :when (and (= :current-object (some-> (:prefill spec) name keyword))
                        (nil? (get defaults k)))]
         label)))

(defn- params-field
  [v var-names on-change]
  (let [rows     (vec (or (seq v) []))
        action   (:action (es/node-at-cursor))
        declared (action-params action)
        known    (set (map first declared))
        taken    (set (map first rows))]
    [:div {:style {:display "flex" :flex-direction "column" :gap "var(--sp-1)"}}
     (for [[i [k val]] (map-indexed vector rows)]
       ^{:key i}
       [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
        (let [put-key! #(on-change (into {} (assoc rows i [(f/kw-or-nil %) val])))]
          (if (seq declared)
            [f/select k
             (concat (when (and k (not (contains? known k)))
                       [[(name k) (str (name k) " " (i18n/t :ui/app-builder-unknown))]])
                     (for [[pk label] declared
                           :when (or (= pk k) (not (contains? taken pk)))]
                       [(name pk) label]))
             put-key! (i18n/t :ui/app-builder-param-pick) {:class "ab-field--key"}]
            [inputs/text-input (f/at (f/value-str k) put-key!)
             {:placeholder "Parameter" :small true :class "ab-field--key"}]))
        [:div {:class "u-flex-1"}
         [ref-field val var-names #(on-change (into {} (assoc rows i [k %]))) nil]]
        [f/row-button :cross "entfernen"
         #(on-change (into {} (concat (subvec rows 0 i) (subvec rows (inc i)))))]])
     (f/dashed-button "+ Parameter" #(on-change (into {} (conj rows [nil nil]))))]))

(defn param-field
  ([p v var-names on-change] (param-field p v var-names nil on-change))
  ([{:keys [key kind options default path] :as p} v var-names otype on-change]
   (case kind
     :text       [text-field v on-change (name (or key :text))]
     :long-text  [ui/text-area {:value (str (or v "")) :rows 4
                                :on-change #(let [s (.. % -target -value)]
                                              (on-change (when-not (str/blank? s) s)))
                                :fill true}]
     :number     [number-field v on-change]
     :bool       [bool-field v on-change]
     :enum       [enum-field v options default on-change]
     :fmt        [select-field v (map name cat/format-options) on-change "Format…"]
     :prop       [f/prop-field v otype nil on-change]
     :action     [select-field v (action-options) on-change "Action…"]
     :object-type [select-field v (f/object-type-options) on-change "Objekttyp…"]
     :resource   [select-field v (resource-options) on-change "Ressource…"]
     :var-name   [select-field v (f/var-options var-names) on-change "Variable…"]
     :handle     (if path
                   [handle-field v var-names path key]
                   [select-field (:var v) (f/var-options var-names) #(on-change (when % {:var %}))
                    "Variable…"])
     :object-set [ref-field v var-names on-change (when path #(new-set-var! path key %))]
     :value      [ref-field v var-names on-change nil]
     :event      [ev/event-field v on-change var-names otype]
     :cols       [cols-field v otype on-change]
     :items      [items-field p v var-names otype on-change]
     :params     [params-field v var-names on-change]
     [edn-field v on-change])))

(defn- set-type
  [x]
  (when x
    (let [idx (ps/vars-index (es/tree))
          k   (keyword x)]
      (if (contains? idx k) (ps/ref-type idx {:val k}) k))))

(defn- spec-otype
  [spec]
  (set-type (or (:of spec) (:from spec) (:source spec))))

(defn- values-field
  [v var-names on-change]
  (if (map? v)
    [ref-field v var-names on-change nil]
    [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
     [:div {:class "u-flex-1"} [edn-field v on-change]]
     [f/row-button :variable "stattdessen eine Variable" #(on-change {:val nil})]]))

(defn- where-editor [clauses var-names otype on-change]
  (let [rows (vec (or clauses []))]
    [:div {:style {:display "flex" :flex-direction "column" :gap "var(--sp-1)"}}
     (for [[i [prop op target]] (map-indexed vector rows)]
       ^{:key i}
       [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
        [:div {:class "ab-field--key"}
         [f/prop-field (when (keyword? prop) prop) otype {:small true :class "ab-field--key"}
          #(on-change (assoc rows i [% op target]))]]
        [f/select (or op :=) (for [{:keys [op label]} cat/pred-ops] [(name op) label])
         #(let [o (keyword %)]
            (on-change (assoc rows i [prop o (when-not (= :none (cat/pred-operand o))
                                               target)])))
         nil {:clearable? false}]
        [:div {:class "u-flex-1"}
         (case (cat/pred-operand op)
           :none   [:span {:style {:color gs/grey :font-size "var(--fs-xs)"}} "kein Wert nötig"]
           :values [values-field target var-names #(on-change (assoc rows i [prop op %]))]
           [ref-field target var-names #(on-change (assoc rows i [prop op %])) nil])]
        [f/row-button :cross "entfernen"
         #(on-change (vec (concat (subvec rows 0 i) (subvec rows (inc i)))))]])
     (f/dashed-button "+ Bedingung" #(on-change (conj rows [nil := nil])))]))

(defn- agg-field
  [v spec on-change]
  (let [[fun prop] (or v [:sum nil])]
    [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
     [f/select (or fun :sum)
      (for [[id label] (cat/var-field-options :agg-fns)] [(name id) label])
      #(on-change [(keyword %) prop]) nil {:clearable? false}]
     [:div {:class "u-flex-1"}
      [f/prop-field prop (spec-otype spec) {:small true} #(on-change [fun %])]]]))

(defn- var-field
  [{:keys [key kind label hint options free needs clears]} spec var-names put!]
  (when (or (nil? needs) (contains? spec needs))
    (let [opts (when options (cat/var-field-options options))
          url? (= :url-behaviors options)
          setzen! (fn [v]
                    (put! (cond
                            (nil? v) (apply dissoc spec key clears)
                            (and url? (nil? (:as spec)))
                            (assoc spec key v :as (vars/coercion-of spec))
                            :else (assoc spec key v))))]
      [uf/field {:label label :inline? true :class "ab-row-field"
                 :help  (if url? (:hint (vars/url-behavior (vars/in-url spec))) hint)}
       (case kind
         :bool        [bool-field (get spec key) setzen!]
         :enum        (if free
                        [f/pick-or-free (get spec key) opts setzen! "—"]
                        [select-field (get spec key) opts setzen! "—"])
         :var-name    [inputs/text-input (f/at (f/value-str (get spec key))
                                               #(setzen! (f/kw-or-nil %)))
                       {:fill true :placeholder "external id — leer = privat"}]
         :type        [select-field (get spec key) (f/object-type-options) setzen! "Objekttyp…"]
         :set-or-type [select-field (get spec key)
                       (concat (f/var-options var-names) (f/object-type-options))
                       setzen! "Variable/Typ…"]
         :prop        [f/prop-field (get spec key) (spec-otype spec) nil setzen!]
         :agg         [agg-field (get spec key) spec setzen!]
         :where       [where-editor (get spec key) var-names (spec-otype spec)
                       #(setzen! (vec %))]
         [edn-field (get spec key) setzen!])])))

(defn- var-body
  [_spec _var-names _put!]
  (let [mehr? (r/atom false)]
    (fn [spec var-names put!]
      (let [fields (cat/var-fields spec)
            offen  (remove :advanced fields)
            tief   (filter :advanced fields)
            zeigen (if @mehr? tief (filter #(contains? spec (:key %)) tief))]
        [:div
         (for [fld offen] ^{:key (:key fld)} [var-field fld spec var-names put!])
         (for [fld zeigen] ^{:key (:key fld)} [var-field fld spec var-names put!])
         (when (and (seq tief) (not @mehr?) (not= (count zeigen) (count tief)))
           [:div {:style {:margin-top "var(--sp-1)"}}
            [f/dashed-button "+ weitere Angaben" #(reset! mehr? true)]])]))))

(defn- add-var! [path]
  (es/send! [:enta.var/add path
             {:title (i18n/t :ui/app-builder-new-var-title)
              :label (i18n/t :ui/app-builder-new-var-label)
              :placeholder (i18n/t :ui/app-builder-new-var-placeholder)}
             (es/env)]))

(defn- var-row [path k spec var-names]
  (let [put!  (fn [s] (es/put-var! path k s))
        open? (= (es/ui-get :open-var) [path k])
        kind  (:label (cat/var-spec spec))]
    [:div {:class (str "ab-var" (when open? " ab-var--open"))}
     [ui/button {:minimal true :fill true :align-text :start :class "ab-var__head"
                 :end-icon (if open? :chevron-down :chevron-right)
                 :on-click #(es/ui-set! {:open-var (when-not open? [path k])})}
      [ui/tooltip {:content (name k)}
       [:span {:class "ab-var__name"} (f/var-label k)]]
      [:span {:class "ab-var__kind"} kind]]
     [ui/collapse {:is-open open?}
      [:div {:class "ab-var__body"}
       [:div {:style {:display "flex" :gap "var(--sp-1)" :align-items "center"}}
        [inputs/text-input (f/at (name k) #(when-let [nk (f/kw-or-nil %)]
                                             (es/rename-var! path k nk)
                                             (es/ui-set! {:open-var [path nk]})))
         {:placeholder "Name" :small true :class "ab-field--key ab-field--strong"}]
        [:div {:class "u-flex-1" :title (:hint (cat/var-spec spec))}
         [f/select (cat/var-kind spec) (for [{:keys [id label]} cat/var-kinds] [(name id) label])
          #(put! (cat/blank-var (keyword %))) nil {:fill? true :clearable? false}]]
        (let [grund (es/refusal :drop-var {:name k :path path})]
          [f/row-button (if grund :disable :cross) (or grund "entfernen")
           #(when-not grund (es/drop-var! path k))])]
       ^{:key (name (cat/var-kind spec))}
       [var-body spec var-names put!]]]]))

(defn- vars-editor [path specs var-names]
  [:div {:class "ab-vars"}
   (for [[k spec] (sort-by (comp f/var-label key) specs)]
     ^{:key (name k)} [var-row path k spec var-names])
   [f/dashed-button "+ Variable" #(add-var! path)]])

(defn- set-value? [v]
  (not (or (nil? v) (= "" v) (and (coll? v) (empty? v)))))

(defn- param-row [path node vidx var-names {:keys [key] :as p}]
  [uf/field {:label (cat/param-label key)}
   [param-field (assoc p :path path) (get node key) var-names
    (ps/node-object-type vidx node key)
    (fn [val] (es/put-param! path key val))]
   (when (and (= key :defaults) (= :action-form (->kw (:widget node))))
     (when-let [ps (seq (hidden-without-default (:action node) (:defaults node)))]
       [:div {:class "ab-warn"}
        (i18n/t :ui/app-builder-hidden-param-needs-default {:params (str/join ", " ps)})]))])

(defn- retyping! [on?] (es/ui-set! {:retyping? on?}))

(defn- retype-select [path node]
  [inputs/select-input nil
   {:class      "ab-select" :auto-focus true :strict? true :clearable? false
    :value      (name (->kw (:widget node)))
    :on-blur    #(retyping! false)
    :items      (for [{group :label items :items} (cat/palette)
                      {:keys [id label type]} items
                      :when (not= :container type)]
                  {:value (name id) :label label :group group})
    :on-change  (fn [v _]
                  (let [w (f/kw-or-nil v)]
                    (retyping! false)
                    (when (and w (not= w (->kw (:widget node))))
                      (es/retype! path w))))}])

(defn- inspector-head [path node spec]
  (let [crumbs (names/crumb-entries (es/tree) path)]
    [:div {:class "ab-insp__head"}
     (when (seq crumbs)
       [ui/breadcrumbs {:class "ab-crumbs"
                        :items (for [{p :path label :label} crumbs]
                                 {:text label :on-click #(es/select! p)})
                        :current-breadcrumb-renderer (fn [props] [ui/breadcrumb props])}])
     [:div {:class "ab-insp__title-row"}
      [:div {:class "ab-insp__title"} (names/node-label (es/tree) node)]
      (when (seq path)
        [:<>
         [ui/tooltip {:content "verdoppeln (Strg+D)"}
          [ui/button {:minimal true :small true :icon :duplicate :aria-label "verdoppeln"
                      :on-click #(es/duplicate! path)}]]
         [ui/tooltip {:content "entfernen (Entf)"}
          [ui/button {:minimal true :small true :icon :trash :aria-label "entfernen"
                      :on-click #(es/remove! path)}]]])]
     (when spec
       (if (and (es/ui-get :retyping?) (= :widget (cat/node-type node)))
         [retype-select path node]
         [ui/tooltip {:content (when (= :widget (cat/node-type node))
               "in einen anderen Baustein umwandeln — passende Angaben bleiben")}
          (let [w? (= :widget (cat/node-type node))]
            [tag {:minimal true :right-icon (when w? :swap-horizontal)
                  :on-click (when w? #(retyping! true))}
             (:label spec)])]))]))

(defn inspector []
  (let [path      (es/cursor)
        node      (es/node-at-cursor)
        spec      (cat/node-spec node)
        declared  (cat/declared-vars (es/tree))
        var-names (map name declared)
        vidx      (ps/vars-index (es/tree))
        problem   (cat/explain-node node (set declared))
        params    (cat/node-params node)
        own       (remove :advanced params)
        shown?    #(or (:required %) (cat/core-param-keys (:key %))
                       (set-value? (get node (:key %))))
        main      (filter shown? own)
        more      (remove shown? own)
        advanced  (filter :advanced params)
        adv-set?  (some #(set-value? (get node (:key %))) advanced)
        row       (fn [p] ^{:key (:key p)} [param-row path node vidx var-names p])]
    [:div {:class "ab-insp"}
     [inspector-head path node spec]
     (when problem
       [:div {:class "ab-problem"} (:error problem)])
     (if (nil? spec)
       [:div {:class "ab-muted"} (i18n/t :ui/app-builder-unknown-widget)]
       [:div {:class "ab-insp__main"}
        (doall (map row main))
        (when (and (empty? main) (seq more))
          [:div {:class "ab-muted"} "Keine Pflichtangaben — die Einstellungen stehen unten."])])

     (when (seq more)
       [eui/section {:title (i18n/t :ui/app-builder-more-settings) :count (count more)
                     :open? (open? :more) :on-toggle #(toggle! :more)}
        (doall (map row more))])

     [eui/section {:title "Variablen" :count (count (:vars node))
                   :open? (open? :vars) :on-toggle #(toggle! :vars)}
      [vars-editor path (:vars node) var-names]]

     [eui/section {:title "Sichtbarkeit & Erweitert" :badge? adv-set?
                   :open? (open? :raw) :on-toggle #(toggle! :raw)}
      (doall (map row advanced))
      [uf/field {:label (i18n/t :ui/app-builder-edit-as-edn)}
       [edn-field (dissoc node cat/path-key) #(es/replace-node! path %)]]]]))
