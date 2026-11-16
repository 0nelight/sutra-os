(ns sutra-ui.ui.filter-model
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.format :as fmt]
            [sutra.capps.object-filter :as of]))

(defn facet-step
  [state [event sig x]]
  (case event
    :want   (if (= sig (:asked state))
              [state nil]
              [(assoc state :asked sig :loading? true) [[:ask sig]]])
    :answer (if (= sig (:asked state))
              [(-> state (assoc :parts x :loading? false) (dissoc :error)) nil]
              [state nil])
    :failed (if (= sig (:asked state))
              [(assoc state :error x :loading? false) nil]
              [state nil])
    [state nil]))

(defn- same-facet?
  [prop link f]
  (and (= (->kw prop) (->kw (:prop f))) (= (->kw link) (->kw (:link f)))))

(defn facet-of
  [parts {:keys [prop link]}]
  (let [fs (mapcat (fn [p] (filter #(same-facet? prop link %) (:facets p))) parts)]
    {:cut?    (boolean (some :cut? fs))
     :buckets (->> (mapcat :buckets fs)
                   (group-by #(str (:value %)))
                   vals
                   (mapv (fn [bs] {:value (:value (first bs))
                                   :count (reduce + 0 (keep :count bs))}))
                   (sort-by (comp - :count))
                   vec)}))

(defn total-of [parts] (reduce + 0 (keep :total parts)))

(defn offered-properties
  [parts]
  (let [n     (count parts)
        props (for [{:keys [type properties]} parts, p properties] (assoc p :type* type))
        by    (group-by :name props)]
    (->> by
         vals
         (keep (fn [ps]
                 (let [p (first ps)]
                   (cond
                     (= n (count ps)) (assoc (dissoc p :type*) :reach :common)
                     (= 1 (count ps)) (assoc (dissoc p :type*) :reach :single :in (:type* p))
                     :else            nil))))
         (sort-by (fn [p] (str (or (:label p) (:name p)))))
         vec)))

(defn offered-links
  [parts]
  (into [] (mapcat :links) parts))

(defn menu-properties
  [props available]
  (let [vis #(->kw (:visibility %))]
    (case (if (sequential? available) :custom (or (->kw available) :visible))
      :all       props
      :prominent (filterv #(= :prominent (vis %)) props)
      :visible   (filterv #(not= :hidden (vis %)) props)
      :custom    (let [want (into #{} (map ->kw) available)]
                   (filterv #(want (->kw (:name %))) props)))))

(defn menu-links
  [links available]
  (cond
    (sequential? available)     (let [want (into #{} (map ->kw) available)]
                                  (filterv #(want (->kw (:side %))) links))
    (= :none (->kw available))  []
    :else                       (vec links)))

(defn property-label
  [props prop]
  (or (some #(when (= (->kw prop) (->kw (:name %))) (:label %)) props)
      (some-> prop name)))

(defn- numeric [x] (fmt/calc x))

(defn- date-ms [x] (fmt/temporal x))

(defn- bins*
  [pairs n]
  (when (seq pairs)
    (let [xs (map first pairs)
          lo (reduce min xs)
          hi (reduce max xs)
          w  (/ (- hi lo) (max n 1))]
      (if (zero? w)
        [{:lo lo :hi hi :count (reduce + 0 (map second pairs))}]
        (let [slot (fn [x] (min (dec n) (int (Math/floor (double (/ (- x lo) w))))))
              cnts (reduce (fn [m [x c]] (update m (slot x) (fnil + 0) c)) {} pairs)]
          (mapv (fn [i] {:lo (+ lo (* i w)) :hi (+ lo (* (inc i) w)) :count (get cnts i 0)})
                (range n)))))))

(defn number-bins
  [buckets n]
  (bins* (keep (fn [{:keys [value count]}]
                 (when-let [x (numeric value)] [x (or count 0)]))
               buckets)
         n))

(defn date-bins
  [buckets n]
  (bins* (keep (fn [{:keys [value count]}]
                 (when-let [x (date-ms value)] [x (or count 0)]))
               buckets)
         n))

(defn bar-width
  [cnt bars]
  (let [top (reduce max 1 (map :count bars))]
    (* 100.0 (/ (or cnt 0) top))))

(def components
  [:keyword :histogram :single-select :multi-select :distribution
   :single-date :multi-date :timeline :has-link])

(defn- slot-of
  [{:keys [prop link in]}]
  (cond-> {}
    prop (assoc :prop (->kw prop))
    link (assoc :link (->kw link))
    in   (assoc :type in)))

(defn filter-key
  [{:keys [prop link component]}]
  (str (some-> link name) "/" (some-> prop name) "/" (some-> component name)))

(defn shown-filters
  [filters {:keys [added removed]}]
  (into [] (remove #(contains? (or removed #{}) (filter-key %))) (concat filters added)))

(defn list-step
  [state [event x]]
  (case event
    :add          (update state :added (fnil conj []) x)
    :remove       (let [k (filter-key x)]
                    (-> state
                        (update :removed (fnil conj #{}) k)
                        (update :added #(filterv (fn [f] (not= k (filter-key f))) %))))
    :toggle-group (update-in state [:collapsed x] not)
    :open         (assoc state :open (when-not (= x (:open state)) x))
    state))

(defn wanted-facets
  [filters]
  (into [] (comp (filter :prop)
                 (remove #(= :keyword (->kw (:component %))))
                 (map #(select-keys % [:prop :link]))
                 (distinct))
        filters))

(defn- keyword-slot [f] (assoc (slot-of f) :op :keyword))

(defn filter-view
  [f parts value]
  (let [slot   (slot-of f)
        comp*  (or (->kw (:component f)) :multi-select)
        facet  (facet-of parts f)
        chosen (into #{} (map str) (:values (of/term-for value (assoc slot :op :is))))
        rng    (of/term-for value (assoc slot :op :range))
        bars   (mapv (fn [{:keys [value count]}]
                       {:value value :count count :on? (contains? chosen (str value))})
                     (:buckets facet))]
    {:key       (filter-key f)
     :label     (or (:label f)
                    (property-label (offered-properties parts) (:prop f))
                    (some-> (:link f) name))
     :component comp*
     :slot      slot
     :bars      bars
     :cut?      (:cut? facet)
     :chosen    chosen
     :min       (:min rng)
     :max       (:max rng)
     :keyword   (:value (of/term-for value (keyword-slot f)))
     :has-link? (some? (of/term-for value (assoc slot :op :has-link)))
     :display   (or (->kw (:display f)) :inline)
     :group     (when (= :grouped (->kw (:display f))) (:link f))
     :summary   (let [n (+ (count chosen) (if rng 1 0))] (when (pos? n) n))}))

(defn value-step
  [value [event slot x]]
  (case event
    :toggle   (of/toggle-value value slot x true)
    :choose   (of/toggle-value value slot x false)
    :range    (let [{:keys [min max]} x
                    t (assoc slot :op :range :min min :max max)]
                (if (or (some? min) (some? max))
                  (of/with-term value t)
                  (of/without-term value t)))
    :keyword  (let [t (merge (assoc slot :op :keyword :value x)
                             (select-keys slot [:advanced? :fields]))]
                (if (str/blank? (str x))
                  (of/without-term value t)
                  (of/with-term value t)))
    :has-link (let [t (assoc slot :op :has-link)]
                (if x (of/with-term value t) (of/without-term value t)))
    value))

(def modes [:read-only :remove-only :update-existing :add-update-remove])

(defn editing
  [mode]
  (case (or (->kw mode) :add-update-remove)
    :read-only         {}
    :remove-only       {:remove? true}
    :update-existing   {:remove? true :update? true}
    :add-update-remove {:remove? true :update? true :add? true}
    {}))

(defn term-text
  [{:keys [op values value min max negate? fields]}]
  (let [no (when negate? "nicht ")]
    (case op
      :is       (str no (str/join ", " (map str values)))
      :null     (str no "leer")
      :contains (str no "enthält „" value "“")
      :range    (str (when (some? min) (str "ab " min)) (when (and (some? min) (some? max)) " ")
                     (when (some? max) (str "bis " max)))
      :keyword  (str "„" value "“"
                     (when (seq fields) (str " in " (str/join ", " (map str fields)))))
      :has-link "ist verknüpft"
      (str value))))

(defn pill-views
  [{:keys [value locked parts mode prevent-operator-change? show-object-type?]}]
  (let [{:keys [operator]} (of/normalize value)
        props (offered-properties parts)
        rights (editing mode)
        label (fn [{:keys [prop link]}]
                (or (when link (str (name link) (when prop (str ": " (name prop)))))
                    (when (and prop (not= :* prop)) (property-label props prop))
                    "Suche"))
        views (fn [ts editable?]
                (mapv (fn [t] (cond-> {:term t :label (label t) :text (term-text t)}
                                editable? (merge rights)))
                      ts))]
    {:operator  operator
     :operator-editable? (and (not prevent-operator-change?) (some? (:update? rights)))
     :type-pill (when show-object-type? (str/join " + " (keep :type parts)))
     :pills     (into (views locked false) (views (of/active value) true))}))

(defn clause->term
  [[prop op _] target]
  (let [p  (->kw prop)
        op (->kw op)]
    (case op
      (:= :in)    (when (some? target)
                    {:op :is :prop p :values (if (sequential? target) (vec target) [target])})
      :not=       (when (some? target) {:op :is :negate? true :prop p :values [target]})
      :contains   (when-not (str/blank? (str target))
                    (if (= :* p)
                      {:op :keyword :value target}
                      {:op :contains :prop p :value target}))
      (:>= :>)    (when (some? target) {:op :range :prop p :min target})
      (:<= :<)    (when (some? target) {:op :range :prop p :max target})
      :truthy     {:op :null :negate? true :prop p}
      :falsy      {:op :null :prop p}
      nil)))

(defn suggestions
  [{:keys [text parts available-properties available-links autocomplete? keyword?]}]
  (let [q      (str/lower-case (str/trim (str text)))
        hit?   (fn [s] (or (str/blank? q) (str/includes? (str/lower-case (str s)) q)))
        props  (menu-properties (offered-properties parts) available-properties)
        links  (menu-links (offered-links parts) available-links)
        values (when (and autocomplete? (not (str/blank? q)))
                 (for [{:keys [facets]} parts, {:keys [prop link buckets]} facets
                       :when (nil? link)
                       {:keys [value count]} buckets
                       :when (and (some? value) (hit? value))]
                   {:kind :value :prop prop :value value :count count}))]
    (vec (concat
          (when (and keyword? (not (str/blank? q)))
            [{:kind :keyword :value (str/trim (str text))}])
          (take 20 values)
          (map (fn [p] {:kind :property :prop (->kw (:name p)) :label (or (:label p) (:name p))
                        :type (:type p) :in (when (= :single (:reach p)) (:in p))})
               (filter #(hit? (or (:label %) (:name %))) props))
          (map (fn [l] {:kind :link :link (->kw (:side l)) :label (or (:label l) (:side l))})
               (filter #(hit? (or (:label %) (:side %))) links))))))

(defn suggestion-step
  [value {:keys [kind prop link in] :as s}]
  (case kind
    :keyword  [(of/with-term value {:op :keyword :value (:value s)}) nil]
    :value    [(of/toggle-value value {:prop prop} (:value s) true) nil]
    :property [value (cond-> {:prop prop} in (assoc :type in))]
    :link     [(of/with-term value {:op :has-link :link link}) {:link link}]
    [value nil]))

(defn prominent-rows
  [{:keys [terms prop hide-empty?]} parts value]
  (let [counts (into {} (map (fn [{:keys [value count]}] [(str value) count]))
                     (:buckets (facet-of parts {:prop prop})))
        chosen (into #{} (map str) (:values (of/term-for value {:prop (->kw prop) :op :is})))
        rows   (mapv (fn [{:keys [value label icon]}]
                       {:value value
                        :label (or label (str value))
                        :icon  icon
                        :count (get counts (str value) 0)
                        :on?   (contains? chosen (str value))})
                     terms)]
    (if hide-empty? (filterv #(pos? (:count %)) rows) rows)))

(defn- comparable-value [x] (or (fmt/calc x) (some-> x str str/lower-case)))

(defn- prop-text
  [row prop format-of]
  (let [x (get (fmt/v row) (->kw prop))]
    (when (some? x) ((or (format-of prop) (fn [y] (str y))) x))))

(defn- object-rows
  [rows {:keys [sort-by-prop sort-dir]}]
  (let [dir (if (= :desc (->kw sort-dir)) -1 1)]
    (if sort-by-prop
      (vec (sort (fn [a b]
                   (let [x (comparable-value (get (fmt/v a) (->kw sort-by-prop)))
                         y (comparable-value (get (fmt/v b) (->kw sort-by-prop)))]
                     (* dir (cond (nil? x) 1 (nil? y) -1
                                  (= (number? x) (number? y)) (compare x y)
                                  :else (compare (str x) (str y))))))
                 rows))
      (vec rows))))

(defn picker-view
  [rows {:keys [title-prop props hide-null? search search-props] :as cfg} format-of look-of text]
  (let [hide?  (not (false? hide-null?))
        terms  (remove str/blank? (str/split (str/lower-case (str text)) #"\s+"))
        fields (case (if (sequential? search) :specific (or (->kw search) :on-screen))
                 :specific  (map ->kw search)
                 :on-screen (cons title-prop props)
                 :all       nil
                 (cons title-prop props))
        fields (or (seq search-props) fields)
        hay    (fn [row] (str/lower-case
                          (str/join " " (if fields
                                          (map #(get (fmt/v row) (->kw %)) fields)
                                          (vals (fmt/v row))))))
        option (fn [row]
                 {:id    (:id row)
                  :row   row
                  :title (or (prop-text row title-prop format-of) (:title row) (str (:id row)))
                  :lines (into []
                               (keep (fn [p]
                                       (let [t (prop-text row p format-of)]
                                         (when (or t (not hide?))
                                           {:prop p :text (or t "—")
                                            :look (look-of row p)}))))
                               props)})]
    (->> (object-rows rows cfg)
         (filter (fn [r] (let [h (hay r)] (every? #(str/includes? h %) terms))))
         (mapv option))))

(defn picker-step
  [selected [event row] {:keys [multi? allow-none?]}]
  (let [id  (:id row)
        cur (if (sequential? selected) (vec selected) (when selected [selected]))
        has (some #(= (str (:id %)) (str id)) cur)]
    (case event
      :pick  (if multi?
               (if has (filterv #(not= (str (:id %)) (str id)) cur) (conj cur row))
               row)
      :clear (cond multi?               []
                   (false? allow-none?) selected
                   :else                nil)
      selected)))

(defn string-options
  [items created]
  (let [base (mapv (fn [o] (if (map? o)
                             {:value (str (:value o)) :label (str (or (:label o) (:value o)))}
                             {:value (str o) :label (str o)}))
                   items)
        have (into #{} (map :value) base)]
    (into base
          (comp (remove have) (map (fn [c] {:value c :label c :created? true})))
          created)))

(defn string-step
  [selection created [event x] {:keys [multiple? clearable? creatable?]}]
  (let [cur (vec selection)]
    (case event
      :set    [(if (and (nil? x) (false? clearable?)) selection x) created]
      :toggle [(if (some #{x} cur) (filterv #(not= x %) cur) (conj cur x)) created]
      :remove [(filterv #(not= x %) cur) created]
      :create (if (and multiple? creatable? (not (str/blank? (str x))))
                (let [x (str/trim (str x))]
                  [(if (some #{x} cur) cur (conj cur x))
                   (if (some #{x} created) created (conj (vec created) x))])
                [selection created])
      [selection created])))

(defn matching-options
  [options text selected]
  (let [q (str/lower-case (str text))]
    (filterv (fn [{:keys [value label]}]
               (and (not (some #{value} selected))
                    (str/includes? (str/lower-case label) q)))
             options)))

(defn placeholder-of
  [{:keys [placeholder multiple?]}]
  (or placeholder (if multiple? "Optionen suchen …" "Option wählen …")))

(defn number-format
  [{:keys [grouping? prefix suffix percent?]}]
  (cond-> {:kind :number :grouping (true? grouping?)}
    prefix   (assoc :prefix (str prefix))
    percent? (assoc :suffix "%")
    (and suffix (not percent?)) (assoc :suffix (str suffix))))

(defn percent-in
  [x]
  (when-let [n (fmt/calc x)] (/ (Math/round (* n 1000000.0)) 10000.0)))

(defn percent-out
  [x]
  (when-let [n (fmt/calc x)] (/ (Math/round (* n 10000.0)) 1000000.0)))

(def date-formats
  {:iso {:label "JJJJ-MM-TT" :re #"^(\d{4})-(\d{1,2})-(\d{1,2})$" :order [:y :m :d]}
   :de  {:label "TT.MM.JJJJ" :re #"^(\d{1,2})\.(\d{1,2})\.(\d{4})$" :order [:d :m :y]}
   :us  {:label "MM/TT/JJJJ" :re #"^(\d{1,2})/(\d{1,2})/(\d{4})$" :order [:m :d :y]}})

(defn- pad [n w] (let [s (str n)] (str (apply str (repeat (- w (count s)) "0")) s)))

(defn- parse-int [s] #?(:clj (Long/parseLong s) :cljs (js/parseInt s 10)))

(defn parse-date
  [text date-format]
  (let [{:keys [re order]} (get date-formats (or (->kw date-format) :iso))]
    (when-let [m (re-find re (str/trim (str text)))]
      (let [parts (zipmap order (map parse-int (rest m)))]
        (when (and (<= 1 (:m parts) 12) (<= 1 (:d parts) 31)) parts)))))

(defn format-date
  [{:keys [y m d]} date-format]
  (case (or (->kw date-format) :iso)
    :de (str (pad d 2) "." (pad m 2) "." (pad y 4))
    :us (str (pad m 2) "/" (pad d 2) "/" (pad y 4))
    (str (pad y 4) "-" (pad m 2) "-" (pad d 2))))

(defn parse-time
  [text]
  (when-let [[_ h mi s ms ap]
             (re-find #"(?i)^(\d{1,2}):(\d{2})(?::(\d{2})(?:\.(\d{1,3}))?)?\s*(am|pm)?$"
                      (str/trim (str text)))]
    (let [h  (parse-int h)
          ap (some-> ap str/lower-case)
          h* (case ap "pm" (if (= 12 h) 12 (+ 12 h)) "am" (if (= 12 h) 0 h) h)
          t  {:h h* :mi (parse-int mi) :s (if s (parse-int s) 0)
              :ms (if ms (parse-int (subs (str ms "00") 0 3)) 0)}]
      (when (and (< (:h t) 24) (< (:mi t) 60) (< (:s t) 60)) t))))

(defn format-time
  [{:keys [h mi s ms]} {:keys [time-format precision]}]
  (let [h12?  (= :h12 (->kw time-format))
        shown (if h12? (let [x (mod h 12)] (if (zero? x) 12 x)) h)
        prec  (or (->kw precision) :minute)]
    (str (pad shown 2) ":" (pad mi 2)
         (when (#{:second :millisecond} prec) (str ":" (pad s 2)))
         (when (= :millisecond prec) (str "." (pad ms 3)))
         (when h12? (if (< h 12) " AM" " PM")))))

(defn- days-from-civil
  [y m d]
  (let [y   (if (<= m 2) (dec y) y)
        era (quot (if (>= y 0) y (- y 399)) 400)
        yoe (- y (* era 400))
        mp  (mod (+ m 9) 12)
        doy (quot (+ (* 153 mp) 2) 5)
        doy (+ doy (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn- civil-from-days
  [z]
  (let [z   (+ z 719468)
        era (quot (if (>= z 0) z (- z 146096)) 146097)
        doe (- z (* era 146097))
        yoe (quot (- doe (quot doe 1460) (- (quot doe 36524)) (quot doe 146096)) 365)
        doy (- doe (- (+ (* 365 yoe) (quot yoe 4)) (quot yoe 100)))
        mp  (quot (+ (* 5 doy) 2) 153)
        d   (inc (- doy (quot (+ (* 153 mp) 2) 5)))
        m   (if (< mp 10) (+ mp 3) (- mp 9))
        y   (+ yoe (* era 400) (if (<= m 2) 1 0))]
    {:y y :m m :d d}))

(defn- wall->ms
  [{:keys [y m d]} {:keys [h mi s ms]}]
  (+ (* 86400000 (days-from-civil y m d)) (* h 3600000) (* mi 60000) (* s 1000) ms))

(defn- ms->wall
  [t]
  (let [day (long (Math/floor (double (/ t 86400000))))
        r   (- t (* day 86400000))]
    [(civil-from-days day)
     {:h (quot r 3600000) :mi (quot (mod r 3600000) 60000)
      :s (quot (mod r 60000) 1000) :ms (mod r 1000)}]))

(defn- iso-of
  [t]
  (let [[date {:keys [h mi s ms]}] (ms->wall t)]
    (str (format-date date :iso) "T" (pad h 2) ":" (pad mi 2) ":" (pad s 2) "." (pad ms 3) "Z")))

(defn instant-of
  [date time offset-of]
  (let [wall (wall->ms date time)
        t    (- wall (offset-of wall))]
    (iso-of (- wall (offset-of t)))))

(defn wall-of
  [t offset-of]
  (ms->wall (+ t (offset-of t))))
