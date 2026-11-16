(ns sutra-ui.apphost.widgets.filter
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui :refer [checkbox tag html-select input-group radio text-area]]
            [reagent.core :as r]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.object-filter :as of]
            [sutra.capps.variables :as vars]
            [sutra-ui.ui.filter-model :as m]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.rule-icon :as rule-icon]
            [sutra-ui.apphost.action-form :as af]
            [sutra-ui.editor.rich-text :as rich-text]
            [sutra-ui.apphost.frame :as fr]))

(defn with-label
  [label body]
  (if (str/blank? (str label))
    body
    [:div.capp-field [:div.capp-field__label label] body]))

(defn labelled
  [widget]
  (fn [{:keys [label] :as cfg} env]
    [with-label label (widget (dissoc cfg :label) env)]))

(defn- dispatch-facets!
  [st {:keys [facets!]} ask ev]
  (let [[s effects] (m/facet-step @st ev)]
    (reset! st s)
    (doseq [[_ sig] effects]
      (facets! ask
               (fr/kept #(dispatch-facets! st {:facets! facets!} ask [:answer sig (:parts %)])
                        #(dispatch-facets! st {:facets! facets!} ask [:failed sig %]))))))

(defn- facet-loader
  [_env _ask _body]
  (let [st   (r/atom {})
        sync (fn [this]
               (let [[env ask] (rest (r/argv this))
                     sig [ask ((:set-values env) (:set ask))]]
                 (dispatch-facets! st env ask [:want sig])))]
    (r/create-class
     {:component-did-mount  sync
      :component-did-update sync
      :reagent-render
      (fn [env ask body]
        ((:set-values env) (:set ask))
        (body @st))})))

(defn- problem
  [msg]
  [:div.capp-filter__error msg])

(defn- bars-view
  [{:keys [bars slot cut?]} on-pick]
  (let [top (take 50 bars)]
    [:div.capp-bars
     (doall
      (for [{:keys [value count on?]} top]
        ^{:key (str value)}
        [:button {:type "button"
                  :class (cond-> "capp-bars__row" on? (str " capp-bars__row--on"))
                  :on-click #(on-pick slot value)}
         [:span.capp-bars__label (if (nil? value) "—" (str value))]
         [:span.capp-bars__track
          [:span.capp-bars__fill {:style {:width (str (m/bar-width count bars) "%")}}]]
         [:span.capp-bars__count count]]))
     (when cut? [:div.capp-filter__hint "Mehr Werte, als hier gezählt werden."])]))

(defn- typeahead
  [{:keys [bars slot chosen]} on-toggle]
  [af/multi-pick {:class       "capp-typeahead"
                   :options     (mapv (fn [{:keys [value count]}]
                                        {:value value
                                         :label (if (nil? value) "—" (str value))
                                         :count count})
                                      bars)
                   :chosen      chosen
                   :limit       30
                   :placeholder "Werte suchen …"
                   :on-toggle   #(on-toggle slot %)}])

(defn- histogram-bins
  [bins]
  (into [:div.capp-hist]
        (map-indexed (fn [i {:keys [count] :as b}]
                       [:div.capp-hist__bin {:key i :title (str count)
                                             :style {:height (str (max 4 (m/bar-width count bins))
                                                                  "%")}
                                             :on-click (:on-click b)}]))
        bins))

(defn- range-fields
  [{:keys [min max]} input-type on-range]
  (if (= "number" input-type)
    [:div.capp-sidebar__range
     [inputs/num-field {:value min :placeholder "von"
                     :on-change #(on-range {:min % :max max})}]
     [inputs/num-field {:value max :placeholder "bis"
                     :on-change #(on-range {:min min :max %})}]]
    [:div.capp-sidebar__range
     [inputs/iso-date-field {:value min :placeholder "von"
                          :on-change #(on-range {:min (not-empty %) :max max})}]
     [inputs/iso-date-field {:value max :placeholder "bis"
                          :on-change #(on-range {:min min :max (not-empty %)})}]]))

(defn- ms->date [ms] (subs (.toISOString (js/Date. ms)) 0 10))

(defn- with-pick
  [bins f]
  (mapv (fn [b] (assoc b :on-click #(f b))) bins))

(defn- component-body
  [{:keys [component slot bars chosen keyword has-link?] :as view} {:keys [advanced?]} on-event]
  (let [on-toggle (fn [s v] (on-event [:toggle s v]))
        on-choose (fn [s v] (on-event [:choose s v]))
        on-range  (fn [x] (on-event [:range slot x]))]
    (case component
      :keyword       [input-group
                      {:type "search" :value (or keyword "")
                       :placeholder (if advanced? "a AND (b OR \"c d\")" "Suchen …")
                       :on-change #(on-event [:keyword (assoc slot :advanced? advanced?)
                                              (.. % -target -value)])}]
      :histogram     [bars-view view on-toggle]
      :multi-select  [typeahead view on-toggle]
      :single-select [html-select
                      {:value (or (first chosen) "")
                       :on-change #(let [v (.. % -target -value)]
                                     (if (str/blank? v)
                                       (on-event [:choose slot (first chosen)])
                                       (on-choose slot v)))}
                      [:option {:value ""} "—"]
                      (doall (for [{:keys [value count]} bars]
                               ^{:key (str value)}
                               [:option {:value (str value)} (str value " (" count ")")]))]
      :distribution  [:div
                      [histogram-bins
                       (with-pick (m/number-bins bars 12)
                         (fn [{:keys [lo hi]}] (on-range {:min lo :max hi})))]
                      [range-fields view "number" on-range]]
      :single-date   [inputs/iso-date-field
                      {:value (or (:min view) "")
                       :on-change #(let [d (not-empty %)]
                                     (on-range {:min d :max d}))}]
      :multi-date    [:div
                      [inputs/iso-date-field
                       {:value ""
                        :on-change #(when-let [d (not-empty %)]
                                      (on-toggle slot d))}]
                      (into [:div.capp-multi__chips]
                            (map (fn [d] ^{:key d}
                                   [tag {:minimal   true
                                         :on-remove #(on-toggle slot d)}
                                    d]))
                            chosen)]
      :timeline      [:div
                      [histogram-bins
                       (with-pick (m/date-bins bars 24)
                         (fn [{:keys [lo hi]}]
                           (on-range {:min (ms->date lo) :max (ms->date hi)})))]
                      [range-fields view "date" on-range]]
      :has-link      [checkbox {:class     "capp-check"
                                :checked   (boolean has-link?)
                                :label     [:span.capp-check__label "hat Verknüpfung"]
                                :on-change #(on-event [:has-link slot
                                                       (.. % -target -checked)])}]
      [typeahead view on-toggle])))

(defn- add-filter-menu
  [{:keys [parts on-add]}]
  (let [props (m/offered-properties parts)
        links (m/offered-links parts)]
    [html-select
     {:value ""
      :on-change #(let [v (.. % -target -value)
                        [kind nm] (str/split v #":" 2)]
                    (case kind
                      "p" (let [p (some (fn [x] (when (= nm (name (:name x))) x)) props)]
                            (on-add {:prop nm :in (when (= :single (:reach p)) (:in p))
                                     :component (case (->kw (:type p))
                                                  (:date :timestamp) :timeline
                                                  (:integer :long :short :byte :double :float
                                                            :decimal) :distribution
                                                  :multi-select)}))
                      "l" (on-add {:link nm :component :has-link :display :grouped})
                      nil))}
     [:option {:value ""} "+ Filter hinzufügen"]
     (doall (for [p props]
              ^{:key (str "p" (:name p))}
              [:option {:value (str "p:" (name (:name p)))}
               (str (or (:label p) (name (:name p)))
                    (when (= :single (:reach p)) (str " (" (:in p) ")")))]))
     (doall (for [l links]
              ^{:key (str "l" (:side l))}
              [:option {:value (str "l:" (name (:side l)))}
               (str "→ " (or (:label l) (name (:side l))))]))]))

(defn- one-filter
  [{:keys [label key summary] :as view} f {:keys [pills? open? on-open on-remove on-event]}]
  (let [body [component-body view f on-event]]
    (if pills?
      [:div.capp-filterpill
       [tag {:interactive true
             :round       true
             :minimal     (not summary)
             :intent      (when summary :primary)
             :on-click    #(on-open key)}
        label (when summary [:span.capp-pill__count summary])]
       (when open?
         [overlay/popover {:on-close #(on-open nil)}
          [:div.capp-popover body
           (when on-remove
             [ui/button {:variant :minimal :size :small
                         :on-click #(on-remove f)}
              "entfernen"])]])]
      [:div.capp-sidebar__field
       [:div.capp-sidebar__label label
        (when on-remove
          [ui/button {:variant    :minimal
                      :size       :small
                      :icon       :cross
                      :aria-label "entfernen"
                      :on-click   #(on-remove f)}])]
       body])))

(defn- grouped
  [views]
  (partition-by (fn [[v]] (:group v)) views))

(defn- filter-list-body
  [{:keys [filters layout output editable? title]} parts st]
  (let [value   (rd output)
        shown   (m/shown-filters filters @st)
        views   (mapv (fn [f] [(m/filter-view f parts value) f]) shown)
        pills?  (= :pills (->kw layout))
        on-ev   (fn [ev] (wr! output (m/value-step value ev)))
        on-rm   (when editable? #(swap! st m/list-step [:remove %]))
        one     (fn [[view f]]
                  ^{:key (:key view)}
                  [one-filter view f {:pills?    pills?
                                      :open?     (= (:key view) (:open @st))
                                      :on-open   #(swap! st m/list-step [:open %])
                                      :on-remove on-rm
                                      :on-event  on-ev}])]
    [:div {:class (if pills? "capp-filterlist capp-filterlist--pills" "capp-filterlist")}
     (when title [:div.capp-sidebar__head title])
     (doall
      (for [section (grouped views)
            :let [group (:group (ffirst section))
                  dflt  (:collapsed? (second (first section)))
                  c?    (let [t (get-in @st [:collapsed group])]
                          (if (some? t) (not= t (boolean dflt)) (boolean dflt)))]]
        (if group
          ^{:key (str "g" group)}
          [ui/section
           {:class          "capp-filtergroup"
            :compact        true
            :collapsible    true
            :title          (name group)
            :title-renderer :div
            :right-element  (when-let [total (m/total-of parts)]
                              [tag {:minimal true :round true} total])
            :collapse-props {:is-open   (not c?)
                             :on-toggle #(swap! st m/list-step [:toggle-group group])}}
           (into [:div] (map one) section)]
          ^{:key (str "s" (:key (ffirst section)))}
          (into [:div.capp-filtergroup--plain] (map one) section))))
     (when editable?
       [add-filter-menu {:parts parts :on-add #(swap! st m/list-step [:add %])}])]))

(defn filter-list
  [_cfg _env]
  (let [st (r/atom {})]
    (fn [{:keys [set filters] :as cfg} env]
      (let [shown (m/shown-filters filters @st)]
        [facet-loader env {:set set :want (m/wanted-facets shown) :describe true}
         (fn [{:keys [parts error]}]
           (if error
             [problem error]
             [filter-list-body cfg parts st]))]))))

(defn- output-of
  [{:keys [output set]} {:keys [applied-filter]}]
  (or output (applied-filter set)))

(defn- term-editor
  [{:keys [slot type parts value on-change on-close]}]
  (let [view (m/filter-view (assoc slot :component :multi-select) parts value)]
    [overlay/popover {:on-close on-close}
     [:div.capp-popover
      (case (->kw type)
        (:date :timestamp) [range-fields view "date"
                            #(on-change (m/value-step value [:range slot %]))]
        (:integer :long :short :byte :double :float :decimal)
        [range-fields view "number" #(on-change (m/value-step value [:range slot %]))]
        [:div
         [typeahead view (fn [s v] (on-change (m/value-step value [:toggle s v])))]
         [input-group
          {:placeholder "enthält …"
           :value (or (:value (of/term-for value (assoc slot :op :contains))) "")
           :on-change #(let [x (.. % -target -value)
                             t (assoc slot :op :contains :value x)]
                         (on-change (if (str/blank? x)
                                      (of/without-term value t)
                                      (of/with-term value t))))}]])
      [checkbox {:class     "capp-check"
                 :checked   (some? (of/term-for value (assoc slot :op :null)))
                 :label     [:span.capp-check__label "ist leer"]
                 :on-change #(let [t (assoc slot :op :null)]
                               (on-change (if (.. % -target -checked)
                                            (of/with-term value t)
                                            (of/without-term value t))))}]]]))

(defn- operator-switch
  [{:keys [operator on-change]}]
  [ui/tooltip {:content "Verknüpfung der Filter"}
   [tag {:interactive true
         :round       true
         :on-click    #(on-change (if (= :or operator) :and :or))}
    (if (= :or operator) "ODER" "UND")]])

(defn- pills-row
  [{:keys [pills type-pill operator operator-editable?]} {:keys [value write! st parts]}]
  (into [:div.capp-pills]
        (concat
         (when type-pill
           [^{:key "type"} [tag {:round true :intent :primary}
                            type-pill]])
         (when operator-editable?
           [^{:key "op"}
            [operator-switch {:operator  operator
                              :on-change #(write! (of/with-operator value %))}]])
         (map (fn [{:keys [term label text remove? update?]}]
                (let [k    (pr-str (select-keys term [:prop :link :op]))
                      open (= k (:open @st))]
                  ^{:key k}
                  [:span.capp-filterpill
                   [tag (cond-> {:round       true
                                 :minimal     true
                                 :interactive (boolean update?)}
                          update? (assoc :on-click #(swap! st m/list-step [:open k]))
                          remove? (assoc :on-remove #(write! (of/without-term value term))))
                    (str label ": " text)]
                   (when (and open update?)
                     [term-editor {:slot      (select-keys term [:prop :link :type])
                                   :parts     parts
                                   :value     value
                                   :on-change write!
                                   :on-close  #(swap! st assoc :open nil)}])]))
              pills))))

(defn- pill-cfg
  [{:keys [set mode prevent-operator-change? show-object-type?]} parts value env]
  {:value value
   :parts parts
   :mode mode
   :prevent-operator-change? prevent-operator-change?
   :show-object-type? show-object-type?
   :locked ((:locked-terms env) set)})

(defn- exploration-want
  [value]
  (into [] (comp (filter :prop)
                 (remove #(= :* (:prop %)))
                 (map #(select-keys % [:prop :link]))
                 (distinct))
        (:terms (of/normalize value))))

(defn- add-term-menu
  [{:keys [parts value write! st]}]
  (let [props (m/offered-properties parts)
        editing (:editing @st)]
    [:div.capp-filterpill
     [html-select
      {:value ""
       :on-change #(let [nm (.. % -target -value)
                         p  (some (fn [x] (when (= nm (name (:name x))) x)) props)]
                     (when p
                       (swap! st assoc :editing (cond-> {:prop (->kw nm) :type (:type p)}
                                                  (= :single (:reach p))
                                                  (assoc :in (:in p))))))}
      [:option {:value ""} "+ Filter"]
      (doall (for [p props]
               ^{:key (str (:name p))}
               [:option {:value (name (:name p))} (or (:label p) (name (:name p)))]))]
     (when editing
       [term-editor {:slot      (cond-> (select-keys editing [:prop :link])
                                  (:in editing) (assoc :type (:in editing)))
                     :type      (:type editing)
                     :parts     parts
                     :value     value
                     :on-change write!
                     :on-close  #(swap! st dissoc :editing)}])]))

(defn exploration-filter-pills
  [_cfg _env]
  (let [st (r/atom {})]
    (fn [{:keys [set] :as cfg} env]
      (let [out   (output-of cfg env)
            value (rd out)
            write (fn [x] (wr! out x))]
        [facet-loader env {:set set :want (exploration-want value) :describe true}
         (fn [{:keys [parts error]}]
           (if error
             [problem error]
             (let [view (m/pill-views (pill-cfg cfg parts value env))]
               [:div.capp-exploration
                [pills-row view {:value value :write! write :st st :parts parts}]
                (when (and (:add? (m/editing (:mode cfg))) (seq parts))
                  [add-term-menu {:parts parts :value value :write! write :st st}])])))]))))

(defn- suggestion-row
  [{:keys [kind label value prop count]}]
  (case kind
    :keyword  (str "Stichwort: „" value "“")
    :value    (str (name prop) " = " value " (" count ")")
    :property (str label " …")
    :link     (str "→ " label)
    ""))

(defn- take-suggestion!
  [s {:keys [value write! st on-done]}]
  (let [[v open] (m/suggestion-step value s)]
    (write! v)
    (when open
      (swap! st assoc :editing (assoc open :type (:type s))))
    (when on-done (on-done))))

(defn exploration-search-bar
  [_cfg _env]
  (let [st (r/atom {:text ""})]
    (fn [{:keys [set mode show-clear? icon fill? placeholder show-help? autocomplete?
                 keyword? available-properties available-links] :as cfg} env]
      (let [out    (output-of cfg env)
            value  (rd out)
            write  (fn [x] (wr! out x))
            rights (m/editing mode)
            want   (exploration-want value)]
        [facet-loader env {:set set :want want :describe true}
         (fn [{:keys [parts error]}]
           (if error
             [problem error]
             (let [view  (m/pill-views (pill-cfg cfg parts value env))
                   sugg  (when (:add? rights)
                           (m/suggestions {:text                 (:text @st)
                                           :parts                parts
                                           :available-properties available-properties
                                           :available-links      available-links
                                           :autocomplete?        (not (false? autocomplete?))
                                           :keyword?             (not (false? keyword?))}))
                   take! (fn [s] (take-suggestion!
                                  s {:value value :write! write :st st
                                     :on-done #(swap! st assoc :text "")}))]
               [:div {:class (cond-> "capp-searchbar" fill? (str " capp-searchbar--fill"))}
                (when icon [:span.capp-searchbar__icon icon])
                [pills-row view {:value value :write! write :st st :parts parts}]
                (when (:add? rights)
                  [ui/suggest
                   (merge overlay/combo
                          {:input-props        {:placeholder      (or placeholder "Suchen …")}
                           :query              (:text @st)
                           :on-query-change    (fn [t _] (swap! st assoc :text t))
                           :items              (vec sugg)
                           :item-renderer      (overlay/combo-option suggestion-row)
                           :on-item-select     (fn [s _] (take! s))})])
                (when (:editing @st)
                  [term-editor {:slot  (select-keys (:editing @st) [:prop :link])
                                :type  (:type (:editing @st))
                                :parts parts :value value :on-change write
                                :on-close #(swap! st dissoc :editing)}])
                (when (and show-clear? (:remove? rights) (seq (of/active value)))
                  [ui/button
                   {:minimal true :small true
                    :on-click #(write (assoc (of/normalize value) :terms []))}
                   "Alle löschen"])
                (when show-help?
                  [ui/tooltip
                   {:content (str "Text sucht in allen Texteigenschaften. Eigenschaft wählen, "
                                  "um nach Werten zu filtern; → sucht über eine Verknüpfung.")}
                   [:span.capp-help "?"]])])))]))))

(defn prominent-terms
  [{:keys [set prop terms output hide-empty?]} env]
  [facet-loader env {:set set :want [{:prop prop}]}
   (fn [{:keys [parts error]}]
     (if error
       [problem error]
       (let [value (rd output)]
         (into [:div.capp-terms]
               (map (fn [{:keys [value label icon count on?] :as _row}]
                      [:button {:key (str value) :type "button"
                                :class (cond-> "capp-term" on? (str " capp-term--on"))
                                :on-click (fn []
                                            (wr! output (of/toggle-value (rd output)
                                                                         {:prop (->kw prop)}
                                                                         value true)))}
                       (when icon [:span.capp-term__icon icon])
                       [:span.capp-term__label label]
                       [:span.capp-pill__count count]]))
               (m/prominent-rows {:terms terms :prop prop :hide-empty? hide-empty?}
                                 parts value)))))])

(defn- picker-option
  [{:keys [title lines]} _]
  [:<>
   [:div.capp-picker__title title]
   (into [:div]
         (map (fn [{:keys [prop text look]}]
                [:div.capp-picker__line
                 {:key (str prop)
                  :style (when look (cond-> {}
                                      (:color look) (assoc :color (:color look))
                                      (:background look) (assoc :background (:background look))))}
                 (rule-icon/before (:icon look))
                 text]))
         lines)])

(defn- row-found
  [rows found]
  (let [have (into #{} (map :id) rows)]
    (into (vec rows) (remove #(have (:id %))) found)))

(defn- object-picker
  [_cfg _env]
  (let [st (r/atom {:text "" :found []})]
    (fn [{:keys [rows sel multi? allow-none? placeholder] :as cfg} env]
      (let [{:keys [format-of look-of search!]} env
            all     (row-found (vars/deref* rows) (:found @st))
            sel-v   (rd sel)
            cur     (if (sequential? sel-v) (vec sel-v) (when sel-v [sel-v]))
            options (m/picker-view all cfg (format-of all) (look-of all) (:text @st))
            pick!   (fn [row]
                      (wr! sel (m/picker-step sel-v [:pick row] cfg))
                      (when-not multi? (swap! st assoc :text "")))
            on?     (fn [id] (some #(= (str (:id %)) (str id)) cur))
            single  (when-not multi?
                      (first (m/picker-view (take 1 cur) (dissoc cfg :props)
                                            (format-of all) (look-of all) "")))]
        (if multi?
          (let [opts (mapv (fn [o] {:value (str (:id o)) :label (:title o) :row (:row o)})
                           (concat (m/picker-view all cfg (format-of all) (look-of all) "")
                                   (m/picker-view cur (dissoc cfg :props)
                                                  (format-of all) (look-of all) "")))
                by-id (into {} (map (juxt :value :row)) opts)]
            [af/multi-pick {:class       "capp-picker"
                             :options     (vec (vals (into {} (map (juxt :value identity))
                                                           opts)))
                             :chosen      (mapv #(str (:id %)) cur)
                             :placeholder placeholder
                             :limit       100
                             :on-query    (fn [t]
                                            (when search!
                                              (search! t rows
                                                       #(swap! st assoc :found %))))
                             :on-toggle   #(some-> (get by-id %) pick!)}])
          [:div.capp-picker
           [ui/suggest
            (merge overlay/combo
                   {:input-props          {:placeholder      placeholder}
                    :selected-item        single
                    :input-value-renderer #(str (:title %))
                    :reset-on-close       true
                    :query                (:text @st)
                    :on-query-change      (fn [t _]
                                            (swap! st assoc :text t)
                                            (when search!
                                              (search! t rows
                                                       (fn [found]
                                                         (swap! st assoc :found found)))))
                    :items                (vec (take 100 options))
                    :items-equal          (fn [a b] (= (str (:id a)) (str (:id b))))
                    :item-renderer        (overlay/combo-option #(picker-option % false)
                                                            #(on? (:id %)))
                    :on-item-select       (fn [o _] (pick! (:row o)))})]
           (when (and (seq cur) (not (false? allow-none?)))
             [ui/button {:variant    :minimal
                         :size       :small
                         :icon       :cross
                         :aria-label "leeren"
                         :on-click   #(wr! sel (m/picker-step sel-v [:clear] cfg))}])])))))

(defn object-dropdown
  [cfg env]
  [object-picker (assoc cfg :multi? false) env])

(defn object-selector
  [cfg env]
  [object-picker (assoc cfg :multi? true) env])

(defn- options-style
  [layout columns]
  (when (= :grid (->kw layout))
    {:grid-template-columns (str "repeat(" (or columns 2) ", minmax(0, 1fr))")}))

(defn- options-class
  [layout]
  (str "capp-choice capp-choice--" (name (or (->kw layout) :vertical))))

(defn- multi-dropdown
  [{:keys [sel creatable?] :as cfg} options on-event]
  [af/multi-pick {:options     options
                   :chosen      (vec (rd sel))
                   :placeholder (m/placeholder-of cfg)
                   :on-toggle   #(on-event [:toggle %])
                   :on-create   (when creatable? #(on-event [:create %]))}])

(defn string-selector
  [_cfg]
  (let [created (r/atom [])]
    (fn [{:keys [sel items multiple? display layout columns clearable? creatable? label]
          :as cfg}]
      (let [options  (m/string-options (vars/deref* items) @created)
            display  (or (->kw display) :dropdown)
            cur      (rd sel)
            on-event (fn [ev]
                       (let [[s c] (m/string-step cur @created ev
                                                  {:multiple?  multiple?
                                                   :clearable? clearable?
                                                   :creatable? creatable?})]
                         (reset! created c)
                         (wr! sel s)))]
        [with-label label
         (cond
           (and multiple? (= :checkboxes display))
           (into [:div {:class (options-class layout) :style (options-style layout columns)}]
                 (map (fn [{:keys [value label]}]
                        ^{:key value}
                        [checkbox {:class     "capp-choice__opt"
                                   :checked   (boolean (some #{value} cur))
                                   :label     [:span label]
                                   :on-change #(on-event [:toggle value])}]))
                 options)

           multiple?
           [multi-dropdown cfg options on-event]

           (= :radio display)
           (into [:div {:class (options-class layout) :style (options-style layout columns)
                        :role "radiogroup"}]
                 (map (fn [{:keys [value label]}]
                        ^{:key value}
                        [radio {:class     "capp-choice__opt"
                                :checked   (= value cur)
                                :label     [:span label]
                                :on-change #(on-event [:set value])}]))
                 options)

           :else
           (into [html-select
                  {:value (or cur "")
                   :on-change #(on-event [:set (not-empty (.. % -target -value))])}
                  [:option {:key "__none" :value "" :disabled (false? clearable?)}
                   (m/placeholder-of cfg)]]
                 (map (fn [{:keys [value label]}]
                        [:option {:key value :value value} label]))
                 options))]))))

(defn user-select
  [_cfg _env]
  (let [st (r/atom {})]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (let [[{:keys [groups]} env] (rest (r/argv this))]
          ((:users! env) {:groups (some-> groups vars/deref*)}
                         (fr/kept #(swap! st assoc :users (:users %))
                                  #(swap! st assoc :error %)))))
      :reagent-render
      (fn [{:keys [sel multiple? allow-clear? placeholder label] :as cfg} _env]
        (let [options (mapv (fn [{:keys [id label]}] {:value id :label label}) (:users @st))
              cur     (rd sel)
              on-ev   (fn [ev]
                        (wr! sel (first (m/string-step
                                         cur nil ev
                                         {:multiple?  multiple?
                                          :clearable? (not (false? allow-clear?))}))))]
          [with-label label
           (cond
             (:error @st) [problem (:error @st)]
             multiple?    [multi-dropdown (assoc cfg :placeholder placeholder) options on-ev]
             :else
             (into [html-select
                    {:value (or cur "")
                     :on-change #(on-ev [:set (not-empty (.. % -target -value))])}
                    [:option {:key "__none" :value "" :disabled (false? allow-clear?)}
                     (or placeholder "Nutzer wählen …")]]
                   (map (fn [{:keys [value label]}]
                          [:option {:key value :value value} label]))
                   options))]))})))

(defn text-input
  [{:keys [sel placeholder class type format height on-enter auto-size? label]} env]
  (let [fmt* (or (->kw format) :single-line)]
    [with-label label
     (case fmt*
       :text-area
       [text-area {:class class
                   :style (when height {:height (if (number? height) (str height "px") height)})
                   :value (str (or (rd sel) ""))
                   :placeholder placeholder
                   :on-change #(wr! sel (.. % -target -value))}]
       :markdown
       [rich-text/view :markdown-editor {:sel sel :placeholder placeholder :class class
                                         :auto-size? auto-size? :height height}]
       (let [enter (when on-enter ((:event-fn env) on-enter))]
         [input-group {:class       class
                       :type        (or type "text")
                       :value       (str (or (rd sel) ""))
                       :placeholder placeholder
                       :on-change   #(wr! sel (.. % -target -value))
                       :on-key-down #(when (and enter (= "Enter" (.-key %)))
                                       (enter {:value (rd sel)}))}]))]))

(defn- percent-handle
  [sel]
  (r/cursor (fn
              ([_] (m/percent-in (rd sel)))
              ([_ x] (wr! sel (m/percent-out x))))
            []))

(defn number-input
  [{:keys [sel placeholder class step min max label reset? percent?] :as cfg}]
  (let [h (if percent? (percent-handle sel) sel)]
    [with-label label
     [:span.capp-numinput
      [inputs/number-input h {:placeholder placeholder :class class :step step :min min :max max
                           :format (m/number-format cfg)}]
      (when reset?
        [ui/tooltip {:content "Zurücksetzen"}
         [ui/button {:variant :minimal
                     :size    :small
                     :icon    :cross
                     :on-click #(wr! sel nil) :aria-label "Zurücksetzen"}]])]]))

(defn date-input
  [{:keys [sel sel-to range? class min max label]}]
  [with-label label
   (if range?
     [inputs/date-range-input sel sel-to {:class class :min min :max max}]
     [inputs/date-input sel {:class class :min min :max max}])])

(defn- zone-offset
  [zone]
  (if (or (nil? zone) (= "local" (str zone)) (= :local zone))
    (fn [ms] (* -60000 (.getTimezoneOffset (js/Date. ms))))
    (let [f (js/Intl.DateTimeFormat. "en-US" #js {:timeZone (name zone) :hourCycle "h23"
                                                  :year "numeric" :month "numeric"
                                                  :day "numeric" :hour "numeric"
                                                  :minute "numeric" :second "numeric"})]
      (fn [ms]
        (let [p (into {} (map (fn [x] [(.-type x) (js/parseInt (.-value x) 10)]))
                      (.formatToParts f (js/Date. ms)))
              w (js/Date.UTC (get p "year") (dec (get p "month")) (get p "day")
                             (get p "hour") (get p "minute") (get p "second"))]
          (- w (* 1000 (js/Math.floor (/ ms 1000)))))))))

(defn- date-pattern
  [date-format]
  (case (->kw date-format) :de "dd.MM.yyyy" :us "MM/dd/yyyy" "yyyy-MM-dd"))

(defn- time-pattern
  [precision]
  (case precision :second "HH:mm:ss" :millisecond "HH:mm:ss.SSS" "HH:mm"))

(defn date-time-picker
  [_cfg]
  (let [st (r/atom {:zone nil})]
    (fn [{:keys [sel date-format time-format precision timezone-editable? default-timezone
                 label class]}]
      (let [zone    (or (:zone @st) (some-> default-timezone vars/deref*) "local")
            local?  (= "local" (str zone))
            offset  (zone-offset zone)
            cur     (rd sel)
            ms      (when cur (.getTime (js/Date. cur)))
            [wd wt] (when (and ms (not (js/isNaN ms))) (m/wall-of ms offset))
            prec    (or (#{:second :millisecond} (->kw precision)) :minute)]
        [with-label label
         [:span.capp-datetime
          [ui/date-input
           {:class             class
            :value             (when wd
                                 (js/Date. (:y wd) (dec (:m wd)) (:d wd)
                                           (:h wt) (:mi wt) (or (:s wt) 0) (or (:ms wt) 0)))
            :time-precision    prec
            :time-picker-props {:use-am-pm (= :h12 (->kw time-format))}
            :date-fns-format   (str (date-pattern date-format) " " (time-pattern prec))
            :popover-props     {:portal-class-name overlay/portal-class}
            :on-change         (fn [^js d _]
                                 (wr! sel (when d
                                            (m/instant-of
                                             {:y (.getFullYear d) :m (inc (.getMonth d))
                                              :d (.getDate d)}
                                             {:h  (.getHours d) :mi (.getMinutes d)
                                              :s  (.getSeconds d) :ms (.getMilliseconds d)}
                                             offset))))}]
          (if timezone-editable?
            [ui/timezone-select
             {:value               (if local? (ui/current-timezone) (str zone))
              :show-local-timezone true
              :popover-props       {:portal-class-name overlay/portal-class}
              :on-change           #(swap! st assoc :zone %)}]
            [:span.capp-datetime__zone (if local? "Lokal" (str zone))])]]))))

(def widgets
  {:filter-list              (fn [cfg env] [filter-list cfg env])
   :exploration-filter-pills (fn [cfg env] [exploration-filter-pills cfg env])
   :exploration-search-bar   (fn [cfg env] [exploration-search-bar cfg env])
   :prominent-terms          (fn [cfg env] [prominent-terms cfg env])
   :object-dropdown          (fn [{:keys [label] :as cfg} env]
                               [with-label label [object-dropdown cfg env]])
   :object-selector          (fn [{:keys [label] :as cfg} env]
                               [with-label label [object-selector cfg env]])
   :string-selector          (fn [cfg _] [string-selector cfg])
   :user-select              (fn [cfg env] [user-select cfg env])
   :date-time-picker         (fn [cfg _] [date-time-picker cfg])
   :text-input               (fn [cfg env] [text-input cfg env])
   :date-input               (fn [cfg _] [date-input cfg])})

(defn- filter-spec?
  [spec]
  (= of/var-type (->kw (:type spec))))

(defn- following-handle
  [h spec handles]
  (let [rv      #(some-> (get handles %) vars/deref*)
        default (:default spec)]
    (r/cursor (fn
                ([_] (of/resolved (vars/deref* h) rv))
                ([_ x]
                 (reset! h x)
                 (when (:update-used-variables? spec)
                   (doseq [[nm v] (of/extraction default x)]
                     (some-> (get handles nm) (reset! v))))))
              [])))

(defn wire-filters
  [specs handles]
  (reduce-kv (fn [hs nm spec]
               (if (and (filter-spec? spec) (of/names-variables? (:default spec)) (get hs nm))
                 (assoc hs nm (following-handle (get hs nm) spec handles))
                 hs))
             handles specs))
