(ns sutra-ui.apphost.action-form
  (:require [sutra-ui.ui.table :as table :refer [object-table]]
            [sutra-ui.ui.overlay :as overlay :refer [portal-class]]
            [sutra-ui.apphost.uploader :refer [file->b64]]
            [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            [sutra.kit.coerce :refer [->kw]]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [multi-select section file-input input-group text-area]]
            [clojure.string :as str]
            [sutra.capps.format :as fmt :refer []]
            [sutra-ui.render-ctx :as rctx]
            [sutra.capps.leaf-props :as lp]
            [sutra.capps.table :as tbl]
            [sutra.capps.intent :as ci]
            [sutra.capps.variables :as vars :refer [row-prop]]
            [sutra.ref.term :as term]
            [sutra.ref.tokens :as rt]
            [sutra.types.submission :as csub]
            [sutra.types.value-types :as vt]
            [sutra.types.media :as mt]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apphost.form-machine :as fm]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.apphost.client :refer [submit! evaluate-params!]]
            [sutra-ui.apphost.frame :as fr]
            [sutra-ui.ui.inputs :refer
             [text-input option-list time-input number-input date-input select-input
              choice-input combo-input checkbox]]))

(deftype StagedCell [value put!]
  IDeref
  (-deref [_] value)
  IReset
  (-reset! [_ v] (put! v) v))

(defn- param-label [p spec] (or (not-empty (str (:name spec))) (name p)))

(defn- current-object-of?
  [spec object object-type]
  (and object (= :current-object (->kw (:prefill spec)))
       (or (nil? object-type) (nil? (:type spec)) (= (->kw object-type) (->kw (:type spec))))))

(defn- declared-defaults
  [specs object object-type]
  (into {}
        (keep (fn [[p spec]]
                (let [d (:default spec)]
                  (cond
                    (current-object-of? spec object object-type)
                    [p (vars/item-field object :id)]

                    (map? d)
                    (let [from (some-> (:from d) ->kw)
                          ok?  (if from
                                 (current-object-of? (get specs from) object object-type)
                                 true)]
                      (when (and ok? (:property d) object)
                        [p (vars/item-field object (->kw (:property d)))]))
                    (:override-default spec) nil
                    (some? d) [p d]
                    :else     nil))))
        specs))

(defn- object-label
  [row]
  (not-empty (str (or (row-prop row :title)
                      (row-prop row :name)
                      (row-prop row :bezeichnung)
                      ""))))

(defn- object-caption
  [title-key key-props]
  (fn [row]
    (or (when title-key
          (let [x (row-prop row (->kw title-key))]
            (when-not (or (nil? x) (str/blank? (str x))) (str x))))
        (object-label row)
        (not-empty (str/join " · " (keep (fn [k] (let [x (row-prop row (->kw k))]
                                                   (when-not (or (nil? x) (str/blank? (str x)))
                                                     (str x))))
                                         key-props))))))

(defn- multi-values
  [v]
  (cond (nil? v) [] (sequential? v) (vec v) :else [v]))

(defn- option-matches?
  [query {:keys [label search]}]
  (let [doc (rt/tokens (str label " " search))]
    (every? (fn [t] (some #(str/starts-with? % t) doc)) (rt/tokens query))))

(defn multi-pick
  [_]
  (let [query (r/atom "")]
    (fn [{:keys [options chosen on-toggle on-create on-query placeholder disabled? class limit
                 label-of]}]
      (let [chosen   (vec chosen)
            on?      (into #{} (map str) chosen)
            by-value (into {} (map (fn [o] [(str (:value o)) o])) options)
            tag-of   (fn [v] (or (get by-value (str v))
                                 {:value v :label (if label-of (label-of v) (str v))}))]
        [multi-select
         {:class                      (cond-> "capp-multi" class (str " " class))
          :items                      (vec options)
          :items-equal                (fn [a b] (= (str (:value a)) (str (:value b))))
          :item-predicate             (fn [q o _ exact?]
                                        (if exact?
                                          (= (str/lower-case (str (:label o)))
                                             (str/lower-case (str/trim q)))
                                          (option-matches? q o)))
          :item-list-predicate        (fn [q items]
                                        (take (or limit csub/option-visible-max)
                                              (filter #(option-matches? q %) items)))
          :item-renderer
          (fn [o {:keys [modifiers handle-click handle-focus ref id query]}]
            (when (:matches-predicate modifiers)
              ^{:key (str (:value o))}
              [ui/menu-item {:role-structure         :listoption
                             :should-dismiss-popover false
                             :active                 (:active modifiers)
                             :disabled               (:disabled modifiers)
                             :selected               (contains? on? (str (:value o)))
                             :id                     id
                             :ref                    ref
                             :on-click               handle-click
                             :on-focus               handle-focus
                             :label                  (some-> (:count o) str)
                             :text                   (if (:created? o)
                                                       [:em (:label o)]
                                                       (ui/highlight-text (str (:label o))
                                                                          query))}]))
          :no-results                 [ui/menu-item {:disabled       true
                                                     :role-structure :listoption
                                                     :text           "Keine Treffer"}]
          :selected-items             (mapv tag-of chosen)
          :tag-renderer               (fn [o] (if (:created? o) [:em (:label o)] (:label o)))
          :on-item-select             (fn [o _]
                                        (if (::new o)
                                          (on-create (:label o))
                                          (on-toggle (:value o)))
                                        (reset! query ""))
          :on-remove                  (fn [o _] (on-toggle (:value o)))
          :query                      @query
          :on-query-change            (fn [q _] (reset! query q) (when on-query (on-query q)))
          :create-new-item-from-query (when on-create
                                        (fn [q] (when-not (str/blank? q)
                                                  {::new true :value q :label q})))
          :create-new-item-renderer   (fn [q active? handle-click]
                                        [ui/menu-item {:icon           :add
                                                       :text           (str "„" q "“ anlegen")
                                                       :role-structure :listoption
                                                       :active         active?
                                                       :on-click       handle-click
                                                       :should-dismiss-popover false}])
          :reset-on-select            true
          :placeholder                (or placeholder "suchen …")
          :disabled                   (boolean disabled?)
          :fill                       true
          :tag-input-props            {:tag-props {:minimal true}}
          :popover-props              {:minimal           true
                                       :portal-class-name portal-class}}]))))

(defn- multi-control
  [_h _opts]
  (let [seen (atom {})]
    (fn [h {:keys [items disabled? placeholder on-search] :as opts}]
      (let [cur (multi-values (rd h))]
        (if (seq items)
          (let [opts* (option-list items opts)
                _     (swap! seen into (map (fn [{:keys [value label]}] [(str value) label]))
                             opts*)]
            [multi-pick {:options     opts*
                         :chosen      cur
                         :label-of    #(get @seen (str %) (str %))
                         :disabled?   disabled?
                         :on-query    on-search
                         :placeholder (if (seq cur) "weitere suchen …" "suchen …")
                         :on-toggle   (fn [v]
                                        (wr! h (if (some #(= (str %) (str v)) cur)
                                                 (filterv #(not= (str %) (str v)) cur)
                                                 (conj cur v))))}])
          [:span.capp-actionform__list
           (into [:span.capp-actionform__listrows]
                 (map-indexed
                  (fn [i v]
                    [:span.capp-actionform__listrow {:key i}
                     [text-input (StagedCell. v #(wr! h (assoc cur i %)))
                      {:placeholder placeholder}]
                     [ui/button
                      {:icon       :minus
                       :aria-label "entfernen"
                       :disabled   (boolean disabled?)
                       :on-click   #(wr! h (into (subvec cur 0 i) (subvec cur (inc i))))}]]))
                 cur)
           [ui/button
            {:icon       :plus
             :aria-label "hinzufügen"
             :disabled   (boolean disabled?)
             :on-click   #(wr! h (conj cur ""))}]])))))

(def ^:private picker-search-from 50)

(defn- picker
  [h rows opts truncated?]
  (if (and (not truncated?) (<= 1 (count rows) picker-search-from))
    [select-input h (dissoc (merge opts {:placeholder "—"}) :on-search)]
    [combo-input h (merge opts {:strict? true :placeholder "suchen …"})]))

(defn- media-param-input
  []
  (let [err (r/atom nil)]
    (fn [spec h disabled?]
      (let [v (rd h)]
        [:span.capp-mediaparam
         [file-input
          {:text            (if (mt/upload? v) (:filename v) (i18n/t :ui/capp-file-choose))
           :button-text     (i18n/t :ui/capp-file-browse)
           :has-selection   (mt/upload? v)
           :input-props     {:accept (:accept spec)}
           :disabled        (boolean disabled?)
           :on-input-change (fn [e]
                              (reset! err nil)
                              (when-let [f (some-> e .-target .-files (aget 0))]
                                (if (> (.-size f) mt/max-upload-bytes)
                                  (reset! err (str "Datei zu groß (max "
                                                   (quot mt/max-upload-bytes (* 1024 1024)) " MB)"))
                                  (file->b64 f
                                             (fn [b64] (wr! h {:bytes-b64 b64
                                                               :filename  (.-name f)
                                                               :mime      (.-type f)}))
                                             #(reset! err %)))))}]
         (cond
           @err            [:span.capp-actionform__note.capp-actionform__note--neg @err]
           (mt/upload? v)  [:span.capp-actionform__hint
                            (str (:filename v) " — wird beim Absenden gespeichert")]
           (mt/address? v) [:span.capp-actionform__hint
                            (str "gespeichert · " (subs (str v) 0 12))])]))))

(defn- row-marks [spec]
  (filter #{:prominent :compact} (map ->kw (:render-hints spec))))

(defn- one-of-items
  [spec one]
  (if-let [dn (not-empty (get-in spec [:constraints :display-names]))]
    (mapv (fn [v] {:value v :label (get dn v)}) one)
    (vec one)))

(defn- form-control
  [p spec h {:keys [options disabled? controls descriptor search! truncated]}]
  (let [t     (->kw (:type spec))
        one   (get-in spec [:constraints :one-of])
        hints (into #{} (map ->kw) (:render-hints spec))
        opts  {}
        rows* (get options p)
        prop  (->kw (get-in spec [:options-from :property]))
        rows  (if prop (filterv #(some? (get (:values %) prop)) rows*) rows*)
        olbl  (if prop
                (if (get-in spec [:options-from :label]) :name prop)
                (object-caption (lp/type-entry descriptor :object-titles t)
                                (let [ks (lp/type-entry descriptor :object-keys t)]
                                  (if (sequential? ks) ks (some-> ks vector)))))
        ph    (if-not prop
                h
                (StagedCell. (rd h) #(wr! h (csub/coerce-param-value spec %))))]
    (cond
      disabled?              [input-group
                              {:type "text" :read-only true :disabled true
                               :value (let [v (rd h) f (:format spec)
                                            ctx (rctx/current)
                                            {:keys [prefix suffix]} (fmt/affixes f)]
                                        (cond (nil? v)    ""
                                              (and f (fmt/calc v))
                                              (str prefix (when prefix fmt/sep)
                                                   (fmt/input-text v f ctx)
                                                   (when suffix fmt/sep) suffix)
                                              (number? v) (str (fmt/auto v ctx))
                                              :else       (str v)))}]
      (:multiple spec)       [multi-control ph
                              {:items (or rows (vec one))
                               :item-value (when rows (or prop :id))
                               :item-label (when rows olbl)
                               :item-search (when (and rows (not prop))
                                              (fn [row]
                                                (str/join " "
                                                          (keep #(when (string? %) %)
                                                                (vals (or (:values row) row))))))
                               :on-search (when search! #(search! p %))
                               :placeholder (:description spec)}]
      (vt/media? t)          [media-param-input spec h disabled?]
      (= :boolean t)         [checkbox h {}]
      (= :date t)            [date-input h opts]
      (:hh-mm hints)         [time-input h opts]
      (and (seq one) (or (:radio hints) (:segmented hints)))
      [choice-input h (merge opts {:items   (one-of-items spec one)
                                   :display (if (:segmented hints) :segmented :radio)
                                   :layout  :horizontal
                                   :selection-required? (boolean (:required spec))})]
      (seq one)              [select-input h (merge opts {:items (one-of-items spec one)
                                                          :placeholder "—"})]

      (and (seq rows) prop)
      [picker ph rows (merge opts {:items rows :item-value prop :item-label olbl
                                   :on-search (when search! #(search! p %))})
       (contains? truncated p)]
      (contains? vt/numeric-types t)
      [number-input h (merge opts (select-keys (:constraints spec) [:min :max])
                             {:format (:format spec)})]
      (and (:rtf hints) (:rtf controls))
      [(:rtf controls) h {:rows 6 :placeholder (:description spec)}]

      (or (:long-text hints) (:rtf hints))
      [text-area {:rows      6
                  :value     (let [v (rd h)] (if (nil? v) "" (str v)))
                  :on-change #(wr! h (.. % -target -value))}]

      (contains? options p)
      [picker ph rows
       (merge opts {:items rows :item-value (or prop :id) :item-label olbl
                    :item-search (fn [row]
                                   (str/join " " (keep #(when (string? %) %)
                                                       (vals (or (:values row) row)))))
                    :on-search (when search! #(search! p %))})
       (contains? truncated p)]
      :else                  [text-input h opts])))

(defn- no-value?
  [v]
  (or (nil? v) (and (coll? v) (empty? v))))

(defn- object-row-for
  [rows key-props v]
  (when (and (some? v) (seq rows))
    (let [s (str v)]
      (or (some #(when (= s (str (:id %))) %) rows)
          (some (fn [row]
                  (when (some #(= s (str (row-prop row (->kw %)))) key-props) row))
                rows)))))

(defn- same-param-value?
  [t {:keys [object? rows key-props]} a b]
  (cond
    (and (no-value? a) (no-value? b)) true
    (or (no-value? a) (no-value? b))  (and (str/blank? (str a)) (str/blank? (str b)))
    (and (sequential? a) (sequential? b))
    (= (frequencies (map str a)) (frequencies (map str b)))
    object?
    (let [ra (object-row-for rows key-props a)
          rb (object-row-for rows key-props b)]
      (if (and ra rb) (= (str (:id ra)) (str (:id rb))) (= (str a) (str b))))
    (contains? vt/numeric-types t)
    (let [x (fmt/calc a) y (fmt/calc b)]
      (if (and (number? x) (number? y)) (== x y) (= (str a) (str b))))
    (= :date t)    (let [ctx (rctx/current)]
                     (= (or (fmt/iso-date a ctx) (str a)) (or (fmt/iso-date b ctx) (str b))))
    :else          (= (str a) (str b))))

(defn- param-value-text
  [t {:keys [object? rows key-props caption] :as opts} v]
  (cond
    (nil? v)        ""
    (sequential? v) (str/join ", " (map #(param-value-text t opts %) v))
    object?         (or (some-> (object-row-for rows key-props v) caption) (str v))
    (boolean? v)    (if v "ja" "nein")
    (and (contains? vt/numeric-types t) (:format opts) (fmt/calc v))
    (let [{:keys [prefix suffix]} (fmt/affixes (:format opts))]
      (str prefix (when prefix fmt/sep) (fmt/input-text v (:format opts) (rctx/current))
           (when suffix fmt/sep) suffix))
    (contains? vt/numeric-types t) (str (or (fmt/auto (fmt/calc v) (rctx/current)) v))
    (= :date t)     (or (fmt/iso-date v (rctx/current)) (str v))
    :else           (str v)))

(defn prefill-edit
  [spec original current opts]
  (let [t (->kw (:type spec))]
    (when-not (or (no-value? original) (same-param-value? t opts original current))
      {:original (param-value-text t opts original)})))

(defn- form-term
  [spec]
  (term/of spec vector))

(defn- default-of
  [spec v]
  (let [t (form-term spec)
        v (if (and (sequential? v) (not (#{:list :array :object-set :vector} (first t))))
            (when (seq v) (if (= :ref (first t)) (first v) v))
            v)
        {:keys [value problems]} (term/render t v)]
    {:value (if (seq problems) v value) :objects (term/references t v)}))

(defonce ^:private applied
  (r/atom 0))

(machine/reg-event :action-form/seed (fn [{:keys [db]} [_ opts]] (fm/seeded db opts)))

(machine/reg-event :action-form/applied (fn [{:keys [db]} [_ n]] (fm/applied db n)))

(machine/reg-event :action-form/put (fn [{:keys [db]} [_ p v specs]] (fm/put db p v specs)))

(machine/reg-event :action-form/search
                   (fn [{:keys [db]} [_ p term]] (fm/searched db p term)))

(machine/reg-event :action-form/fire (fn [{:keys [db]} _] (fm/fired db)))

(machine/reg-event :action-form/evaluated
                   (fn [{:keys [db]} [_ req resp]] (fm/evaluated db req resp)))

(machine/reg-event :action-form/baselined
                   (fn [{:keys [db]} [_ seed values]] (fm/baselined db seed values)))

(machine/reg-event :action-form/submit (fn [{:keys [db]} _] (fm/submit db)))

(machine/reg-event :action-form/submitted
                   (fn [{:keys [db]} [_ id defaults]] (fm/submitted db id defaults)))

(machine/reg-event :action-form/failed (fn [{:keys [db]} [_ id err]] (fm/settled db id err)))

(machine/reg-event :action-form/stale (fn [{:keys [db]} [_ id]] (fm/settled db id nil)))

(defn- form-ctx [f] (:ctx @(:rt f)))

(machine/reg-fx :action-form/schedule
                (fn [{f :frame :as ctx} ms]
                  (when (:evaluate (form-ctx f))
                    (if (pos? ms)
                      (machine/fx! ctx [:after {:key ::fire :ms ms :event [:action-form/fire]}])
                      (do (machine/fx! ctx [:cancel ::fire])
                          (machine/dispatch [:action-form/fire] {:frame f}))))))

(machine/reg-fx :action-form/cancel-timer (fn [ctx _] (machine/fx! ctx [:cancel ::fire])))

(defn- evaluating [{f :frame} ask then]
  (machine/dispatch [:rf/request {:ask  ask
                                  :then [then]
                                  :else [(fn [_] (conj then nil))]}]
                    {:frame f}))

(machine/reg-fx :action-form/evaluate
                (fn [{f :frame :as ctx} req]
                  (let [{:keys [evaluate action]} (form-ctx f)]
                    (evaluating ctx [evaluate action (:vals req) (:search req)]
                                [:action-form/evaluated req]))))

(machine/reg-fx :action-form/baseline
                (fn [{f :frame :as ctx} seed]
                  (let [{:keys [evaluate action]} (form-ctx f)]
                    (evaluating ctx [evaluate action seed nil]
                                [:action-form/baseline-answered seed]))))

(machine/reg-event :action-form/baseline-answered
                   (fn [_ [_ seed answer]]
                     {:fx [[:dispatch [:action-form/baselined seed (:values answer)]]]}))

(machine/reg-fx :action-form/send
                (fn [{f :frame} {:keys [id vals]}]
                  (let [{:keys [submit descriptor action scenario]} (form-ctx f)]
                    (machine/dispatch
                     [:rf/request {:ask  [submit descriptor vals
                                          {:action action :scenario scenario}]
                                   :then [[:action-form/sent id]]
                                   :else [[:action-form/send-failed id]]}]
                     {:frame f}))))

(machine/reg-event :action-form/sent
                   (fn [_ [_ id answer]] {:fx [[:action-form/answer [id answer]]]}))

(machine/reg-event :action-form/send-failed
                   (fn [_ [_ id sentence]]
                     {:fx [[:dispatch [:action-form/failed id sentence]]
                           [:action-form/answer [id nil sentence]]]}))

(machine/reg-fx :action-form/answer
                (fn [{f :frame} [id {:keys [data stale]} sentence]]
                  (let [{:keys [on-ok on-error on-stale defaults]} (form-ctx f)
                        send #(machine/dispatch % {:frame f})]
                    (cond
                      sentence        (when on-error (on-error sentence))
                      (and stale on-stale) (do (send [:action-form/stale id]) (on-stale stale))
                      stale           (do (send [:action-form/failed id (:message stale)])
                                          (when on-error (on-error (:message stale))))
                      :else           (do (send [:action-form/submitted id defaults])
                                          (swap! applied inc)
                                          (when on-ok (on-ok data)))))))

(defn- form-seed
  [specs defaults object object-type]
  (reduce-kv (fn [m p v]
               (assoc m p (csub/coerce-param-value
                           (get specs p) (:value (default-of (get specs p) v)))))
             {} (into (declared-defaults specs object object-type)
                      (remove (comp no-value? val))
                      defaults)))

(defn- form-evaluator
  [descriptor {:keys [action specs scenario evaluate]}]
  (or evaluate
      (:evaluate-fn descriptor)
      (when (or (some :overrides (vals specs))
                (some #{(->kw (or action (:default-action descriptor)))}
                      (map ->kw (concat (:action-evaluated descriptor)
                                        (:action-optioned descriptor)))))
        (fn [act values search]
          (evaluate-params! descriptor act values {:scenario scenario :search search})))))

(defn- form-sync!
  [m descriptor
   {:keys [action specs defaults object object-type scenario submit on-ok on-error on-stale]
    :as opts}]
  (let [seed      (form-seed specs defaults object object-type)
        evaluate  (form-evaluator descriptor opts)
        object-id (some-> object (vars/item-field :id) str)]
    (swap! (:rt m) assoc :ctx {:evaluate evaluate :action action :submit (or submit submit!)
                               :descriptor descriptor :scenario scenario :defaults defaults
                               :on-ok on-ok :on-error on-error :on-stale on-stale})
    (when (not= (:seed @(:db m)) seed)
      (machine/dispatch [:action-form/seed {:seed seed :object-id object-id :applied @applied
                                            :evaluate? (boolean evaluate)}]
                        {:frame m}))
    (machine/dispatch [:action-form/applied @applied] {:frame m})))

(defn- default-objects
  [specs defaults]
  (into {} (keep (fn [[p v]]
                   (when-let [os (seq (:objects (default-of (get specs p) v)))]
                     [p (vec os)])))
        defaults))

(defn- form-field
  [m descriptor {:keys [specs options truncated reveal]} controls title p]
  (let [form-state                 (:db m)
        {:keys [vals filled base]} @form-state
        spec (get specs p)
        dis? (or (= :disabled (->kw (:exposure spec))) (false? (:editable spec)))
        t    (->kw (:type spec))
        prop (->kw (get-in spec [:options-from :property]))
        ks   (let [ks (lp/type-entry descriptor :object-keys t)]
               (if (sequential? ks) ks (some-> ks vector)))
        edit (when (and (contains? base p) (contains? vals p))
               (prefill-edit
                spec (get base p) (get vals p)
                {:format    (:format spec)
                 :object?   (and (not prop) (contains? options p))
                 :rows      (get options p)
                 :key-props ks
                 :caption   (object-caption (lp/type-entry descriptor :object-titles t) ks)}))
        echo? (and (not edit) (not (:required spec))
                   (= (str/lower-case (str/trim (str title)))
                      (str/lower-case (param-label p spec))))
        marks (row-marks spec)]
    [:label.capp-actionform__row
     (cond-> {:key (str p) :data-param (name p)}
       (seq marks)
       (assoc :class (str/join " " (map #(str "capp-actionform__row--" (name %)) marks)))
       (= p reveal)
       (assoc :ref (fn [el]
                     (when el
                       (.scrollIntoView el #js {:block "nearest" :behavior "smooth"})
                       (swap! form-state dissoc :revealed)))))
     [:span.capp-actionform__label
      (when echo? {:class "capp-actionform__label--echo"})
      (param-label p spec)
      (when (:required spec) [:span.capp-actionform__req "*"])
      (when edit
        [:span.capp-actionform__edited
         {:title (i18n/t :ui/capp-prefill-was
                         {:value (let [o (:original edit)] (if (str/blank? o) "—" o))})}
         (i18n/t :ui/capp-prefill-edited)])]
     [:span.capp-actionform__control
      [form-control p spec
       (StagedCell. (get vals p (get filled p))
                    #(machine/dispatch [:action-form/put p % specs] {:frame m}))
       {:options options :disabled? dis? :controls controls
        :descriptor descriptor
        :search! (fn [p term] (machine/dispatch [:action-form/search p term] {:frame m}))
        :truncated truncated}]]
     (when-let [d (:description spec)]
       [:span.capp-actionform__hint d])]))

(defn- form-section
  [form-state field {:keys [reveal specs]} i
   {:keys [title description columns collapsible collapsed params layout]}]
  (let [fold? (or collapsible collapsed)
        open? (boolean (or (not fold?)
                           (some #{reveal} params)
                           (get-in @form-state [:open i] (not collapsed))))
        card? (or title description (= 2 columns) fold?)
        cls   (cond-> "capp-actionform__section"
                card?       (str " capp-actionform__section--card")
                (not open?) (str " capp-actionform__section--shut"))
        body  [:<>
               (when description [:div.capp-actionform__section-note description])
               (into [:div.capp-actionform__fields
                      {:class (cond
                                (= "totals" (some-> layout name)) "capp-actionform__fields--totals"
                                (= 2 columns) "capp-actionform__fields--2")}]
                     (map (partial field (when (= 1 (count params)) title)))
                     params)]]
    (if fold?
      ^{:key i}
      [section
       {:class          cls
        :compact        true
        :collapsible    true
        :title          (or title (str/join ", " (map #(param-label % (get specs %)) params)))
        :title-renderer :div
        :collapse-props {:is-open   open?
                         :on-toggle #(swap! form-state assoc-in [:open i] (not open?))}}
       body]
      [:div {:key i :class cls}
       (when title [:div.capp-actionform__section-title title])
       body])))

(defn- action-form*
  [m descriptor
   {:keys [action specs sections controls defaults options label
           busy-label class cancel-label on-cancel aux intent hidden extra issue] :as opts}]
  @applied
  (let [form-state                    (:db m)
        {:keys [vals busy err] :as s} @form-state
        vm    (fm/view-model s {:specs           specs
                                :sections        sections
                                :own-options     options
                                :default-objects (default-objects specs defaults)
                                :evaluate?       (boolean (form-evaluator descriptor opts))})
        gate  (:gate vm)
        field (partial form-field m descriptor vm controls)]
    [:div.capp-actionform
     {:class class
      :data-aktion (some-> (or action (:default-action descriptor)) name)}
     [machine/leaving m]
     (into [:div.capp-actionform__body]
           (map-indexed (partial form-section form-state field vm))
           (if (seq hidden)
             (keep (fn [sec]
                     (let [ps (into [] (remove hidden) (:params sec))]
                       (when (seq ps) (assoc sec :params ps))))
                   (:sections vm))
             (:sections vm)))
     (when extra [:div.capp-actionform__extra extra])
     [:div.capp-actionform__foot
      (when-let [note (or err issue (when (:touched s) (:reason gate)))]
        (let [issue? (or err issue (:blocked? gate))]
          [:span.capp-actionform__note
           {:class (when issue? "capp-actionform__note--neg")}
           (when issue? [ui/icon {:icon "error" :size 14 :class "capp-actionform__issue"}])
           [:span note]]))
      [:span.capp-actionform__spacer]
      (for [{:keys [label on-click]} aux]
        ^{:key label}
        [ui/button {:disabled busy :on-click (fn [_] (on-click vals))} label])
      (when cancel-label
        [ui/button {:on-click (fn [_] (when on-cancel (on-cancel)))} cancel-label])
      [ui/tooltip
       {:content (:reason gate)}
       [ui/button
        (merge (ci/btn-props (if (contains? #{nil :primary} (->kw intent)) :success intent))
               {:loading  (boolean busy)
                :disabled (:blocked? gate)
                :on-click (fn [_] (machine/dispatch [:action-form/submit] {:frame m}))})
        (if busy (or busy-label "…") (or label (i18n/t :ui/capp-submit)))]]]]))

(def ^:private action-form-body
  (r/create-class
   {:display-name         "capp-actionform"
    :component-did-update (fn [this _]
                            (let [[_ m descriptor opts] (r/argv this)]
                              (form-sync! m descriptor opts)))
    :reagent-render       action-form*}))

(defn action-form
  [descriptor opts]
  (let [m (machine/frame (r/atom nil) {:name :action-form :lifetime :view :rt (atom {})})]
    (form-sync! m descriptor opts)
    (fn [descriptor opts] [action-form-body m descriptor opts])))

(defn- edit-opts [spec] (if (map? spec) spec {}))

(defn- cell-input
  [spec h]
  (let [o (edit-opts spec)]
    (case (->kw (if (map? spec) (:kind spec) spec))
      :number [number-input h (select-keys o [:placeholder :class :step :min :max])]
      :date   [date-input h (select-keys o [:class :min :max])]
      :select [select-input h (merge (select-keys o [:item-value :item-label :sort :strict?])
                                     {:items (:options o) :placeholder (:placeholder o)})]
      :check  [checkbox h {:label (:label o)}]
      [text-input h (select-keys o [:placeholder :class :type])])))

(defn- apply-staged!
  [f descriptor {:keys [action scenario]} pairs {:keys [on-done on-fail on-step]}]
  (letfn [(step [i data]
            (if (>= i (count pairs))
              (on-done data)
              (let [[_ params] (nth pairs i)]
                (on-step i)
                (fr/request!
                 f (fr/submit-request
                    descriptor params
                    {:action   action
                     :scenario scenario
                     :on-ok    (fn [d] (step (inc i) (or d data)))
                     :on-error (fn [m] (on-fail i m))
                     :on-stale (fn [_] (on-fail i (str "Zeile " (inc i)
                                                       " wurde zwischenzeitlich geändert"
                                                       " — bitte neu laden")))})))))]
    (step 0 nil)))

(defn action-table
  [_descriptor _opts]
  (let [edits (r/atom {})
        sent  (r/atom #{})
        busy  (r/atom nil)
        err   (r/atom nil)]
    (fn [descriptor {:keys [cols rows row-key action key-param scenario params
                            prefill? submit-label busy-label class on-ok on-error intent
                            before-fire]
                     :as cfg}]
      (let [keyf   (or (tbl/field-fn row-key) :id)
            staged @edits
            b?     (some? @busy)
            param  (into {} (keep (fn [{:keys [prop param edit]}]
                                    (when (and edit prop) [prop (or param prop)])))
                         cols)
            cols*  (mapv (fn [{:keys [edit prop] :as c}]
                           (if-not (and edit prop)
                             c
                             (let [valf (or (tbl/field-fn prop) (constantly nil))]
                               (-> (dissoc c :edit :param)
                                   (assoc :render
                                          (fn [row]
                                            (let [k   (keyf row)
                                                  staged-value  (get-in staged [k prop] ::rein)
                                                  cur (if (= ::rein staged-value) (valf row)
                                                          staged-value)]
                                              [:div {:class (cond-> "capp-cell--edit"
                                                              (not= ::rein staged-value)
                                                              (str " capp-cell--dirty"))}
                                               [cell-input edit
                                                (StagedCell. cur
                                                             #(swap! edits assoc-in [k prop]
                                                                     %))]])))))))
                         cols)
            seeded (if prefill?
                     (into {}
                           (map (fn [row]
                                  [(keyf row)
                                   (merge
                                    (into {}
                                          (keep (fn [{:keys [prop edit]}]
                                                  (when-let [f (and edit prop (tbl/field-fn prop))]
                                                    (let [v (f row)
                                                          v (if (and (string? v)
                                                                     (= :number
                                                                        (->kw (if (map? edit)
                                                                                (:kind edit)
                                                                                edit))))
                                                              (let [x (js/parseFloat v)]
                                                                (when-not (js/isNaN x) x))
                                                              v)]
                                                      (when (some? v) [prop v])))))
                                          cols)
                                    (get staged (keyf row)))]))
                           (remove (comp @sent keyf) rows))
                     staged)
            n      (reduce + 0 (map count (vals staged)))
            pairs  (mapv (fn [[k props]]
                           [k (into (merge (or params {}) {(or (->kw key-param) :id) k})
                                    (map (fn [[prop val]] [(get param prop prop) val]))
                                    props)])
                         seeded)
            fire!  (fn []
                     (reset! err nil)
                     (reset! busy {:done 0 :total (count pairs)})
                     (apply-staged!
                      (:frame cfg)
                      descriptor {:action action :scenario scenario} pairs
                      {:on-step (fn [i] (swap! busy assoc :done i))
                       :on-done (fn [data]
                                  (when prefill? (swap! sent into (map first pairs)))
                                  (reset! edits {}) (reset! busy nil)
                                  (when on-ok (on-ok data)))
                       :on-fail (fn [i m]
                                  (swap! edits #(apply dissoc % (map first (take i pairs))))
                                  (when prefill? (swap! sent into (map first (take i pairs))))
                                  (reset! busy nil) (reset! err m)
                                  (when on-error (on-error m)))}))]
        [:div {:class (cond-> "capp-action-table" class (str " " class))}
         [object-table (assoc cfg :cols cols* :rows rows)]
         (when (or b? (pos? n) (and prefill? (seq pairs)))
           [:div.capp-action-table__bar
            [:span.capp-action-table__count
             (cond
               b?       (str "sendet " (inc (:done @busy)) "/" (:total @busy) " …")
               prefill? (str (count pairs) (if (= 1 (count pairs)) " Zeile" " Zeilen")
                             (when (pos? n) (str ", " n (if (= 1 n) " Änderung" " Änderungen"))))
               :else    (str n (if (= 1 n) " Änderung" " Änderungen")
                             " in " (count pairs)
                             (if (= 1 (count pairs)) " Zeile" " Zeilen")))]
            [ui/button
             {:disabled b?
              :on-click #(do (reset! edits {}) (reset! err nil))}
             "Verwerfen"]
            [ui/button
             (merge (ci/btn-props (or intent :success))
                    {:loading  b?
                     :on-click (if before-fire #(before-fire fire!) fire!)})
             (if b? (or busy-label "…") (or submit-label (i18n/t :ui/capp-save-changes)))]])
         (when-let [e @err] [:div.capp-action-table__error e])]))))

(defn- move-item
  [v i j]
  (if (or (= i j) (neg? j) (>= j (count v)))
    v
    (let [x (nth v i)
          r (into (subvec v 0 i) (subvec v (inc i)))]
      (into (conj (subvec r 0 j) x) (subvec r j)))))

(defn order-list
  [_descriptor _opts]
  (let [state (r/atom nil)
        drag  (r/atom nil)
        busy  (r/atom false)
        err   (r/atom nil)]
    (fn [descriptor {:keys [items label sub row-key sel action param scenario frame
                            submit-label busy-label class on-ok on-error intent]}]
      (let [keyf  (or (tbl/field-fn row-key) :id)
            labf  (or (tbl/field-fn label) (constantly ""))
            subf  (tbl/field-fn sub)
            seed  (mapv keyf items)
            by-k  (into {} (map (juxt keyf identity)) items)
            order (if (= seed (:seed @state)) (:order @state) seed)
            put!  (fn [o] (reset! state {:seed seed :order o}) (wr! sel (vec o)))
            move! (fn [i j] (put! (move-item (vec order) i j)))
            b?    @busy
            fire! (fn []
                    (reset! err nil) (reset! busy true)
                    (fr/request!
                     frame (fr/submit-request
                            descriptor {(or (->kw param) :reihenfolge) (vec order)}
                            {:action   action
                             :scenario scenario
                             :on-ok    (fn [d] (reset! busy false) (reset! state nil)
                                         (when on-ok (on-ok d)))
                             :on-error (fn [m] (reset! busy false) (reset! err m)
                                         (when on-error (on-error m)))})))]
        [:div {:class (cond-> "capp-order" class (str " " class))}
         (into [:ol.capp-order__list]
               (map-indexed
                (fn [i k]
                  (let [row (get by-k k)]
                    [:li.capp-order__item
                     {:key          (str k)
                      :draggable    true
                      :class        (when (= i @drag) "capp-order__item--drag")
                      :on-drag-start #(reset! drag i)
                      :on-drag-over  #(.preventDefault %)
                      :on-drop       #(do (.preventDefault %)
                                          (when-let [from @drag] (move! from i))
                                          (reset! drag nil))
                      :on-drag-end   #(reset! drag nil)}
                     [:span.capp-order__grip {:aria-hidden true} "⠿"]
                     [:span.capp-order__pos (inc i)]
                     [:span.capp-order__label
                      [:span.capp-order__title (str (labf row))]
                      (when subf [:span.capp-order__sub (str (subf row))])]
                     [:span.capp-order__moves
                      [ui/button
                       {:icon       :chevron-up
                        :disabled   (zero? i)
                        :aria-label "nach oben"
                        :on-click   #(move! i (dec i))}]
                      [ui/button
                       {:icon       :chevron-down
                        :disabled   (= i (dec (count order)))
                        :aria-label "nach unten"
                        :on-click   #(move! i (inc i))}]]]))
                order))
         (when action
           [:div.capp-order__bar
            [ui/button
             (merge (ci/btn-props (or intent :success))
                    {:loading  b?
                     :disabled (= order seed)
                     :on-click fire!})
             (if b? (or busy-label "…") (or submit-label (i18n/t :ui/capp-save-order)))]])
         (when-let [e @err] [:div.capp-order__error e])]))))
