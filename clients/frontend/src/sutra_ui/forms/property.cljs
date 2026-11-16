(ns sutra-ui.forms.property
  (:require [sutra-ui.state.forms :as fs]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra.types.value-types :as vt]
            [sutra.ref.sides :as sides]
            [sutra.capps.format :as fmt]
            [sutra-ui.render-ctx :as rctx]
            [sutra.i18n.core :as i18n]
            [sutra-ui.forms.base :refer [implements-chips named via]]
            [sutra-ui.forms.external :refer [row-check]]))

(def property-base-types
  ["string" "integer" "short" "long" "byte" "boolean" "float" "double" "decimal"
   "date" "timestamp" "geopoint" "geoshape" "attachment" "media-reference"])

(def ^:private title-key-types
  #{:string :text :keyword :uuid :integer :short :long :byte :boolean :float :double
    :decimal :date :timestamp :instant :geopoint :cipher})

(defn title-key-type?
  [t]
  (cond
    (string? t)  (contains? title-key-types (keyword t))
    (keyword? t) (contains? title-key-types t)
    (and (sequential? t) (= :array (first t))) (title-key-type? (second t))
    :else false))

(def render-hint-options
  [:searchable :sortable :selectable :low-cardinality :keywords :long-text
   :identifier :disable-formatting])

(def format-kinds-by-class
  {:number   [:currency :unit :percentage]
   :temporal [:date :datetime-short :datetime-long :time :relative :iso-instant]})

(def ^:private shown-opt-keys
  #{:name :description :required :visibility :status :value-type :format :render-hints
    :derived :constraints})

(def format-kind-labels
  {:currency :ui/ot-format-currency :unit :ui/ot-format-unit :percentage :ui/ot-format-percentage
   :date :ui/ot-format-date :datetime-short :ui/ot-format-datetime-short
   :datetime-long :ui/ot-format-datetime-long :time :ui/ot-format-time
   :relative :ui/ot-format-relative :iso-instant :ui/ot-format-iso-instant})

(defn- type-kw [t] (when (or (string? t) (keyword? t)) (keyword t)))

(defn- type-label [t]
  (if (or (string? t) (keyword? t)) (clojure.core/name t) (pr-str t)))

(defn- blank-v? [v]
  (or (nil? v) (and (string? v) (str/blank? v)) (and (coll? v) (empty? v))))

(defn set-prop-opt!
  [form-atom i k v]
  (fs/edit! form-atom update-in [:fields i]
            (fn [f]
              (if-let [o (ontology/clean-prop-opts
                          (if (blank-v? v) (dissoc (:opts f) k) (assoc (:opts f) k v)))]
                (assoc f :opts o)
                (dissoc f :opts)))))

(defn set-prop-format!
  [form-atom i k v]
  (let [fmt (get-in @form-atom [:fields i :opts :format])
        fmt (cond-> (if (blank-v? v) (dissoc fmt k) (assoc fmt k v))
              (and (= k :kind) (not= v :currency)) (dissoc :currency)
              (and (= k :kind) (not= v :unit))     (dissoc :unit))]
    (set-prop-opt! form-atom i :format (not-empty fmt))))

(defn- shared-properties []
  (or (:shared-properties @catalog/canonical-schema) {}))

(defn- interface-property-rows
  [iface-kw]
  (let [ifs (:interfaces @catalog/canonical-schema)]
    (loop [q [iface-kw] seen #{} acc []]
      (if-let [k (first q)]
        (if (seen k)
          (recur (rest q) seen acc)
          (let [spec (get ifs k)]
            (recur (concat (rest q) (:extends spec)) (conj seen k)
                   (into acc (:property-list spec)))))
        acc))))

(defn missing-interface-properties
  [fields rows]
  (let [have (set (keep #(some-> (:name %) str/trim not-empty keyword) fields))]
    (->> rows
         (remove #(have (:attribute-name %)))
         (reduce (fn [acc r] (if (some #(= (:attribute-name r) (:attribute-name %)) acc)
                               acc (conj acc r)))
                 []))))

(defn- interface-closure
  [ifs iface-kw]
  (loop [q [iface-kw] seen []]
    (if-let [k (first q)]
      (if (some #{k} seen)
        (recur (rest q) seen)
        (recur (concat (rest q) (:extends (get ifs k))) (conj seen k)))
      seen)))

(defn unmet-link-constraints
  [canon ot implements]
  (let [ifs  (:interfaces canon)
        mine (distinct (mapcat #(interface-closure ifs (keyword %)) implements))
        t    (or ot ::new)
        w    (assoc-in canon [:object-types t :implements] (mapv keyword implements))]
    (vec
     (for [iface mine
           {:keys [link-name to-object-type to-interface cardinality
                   required]} (:link-constraints (get ifs iface))
           :when (true? required)
           :when (empty? (sides/constraint-sides w t iface link-name))]
       {:interface iface :link-name link-name :to (or to-object-type to-interface)
        :cardinality (sides/constraint-card cardinality)}))))

(defn- add-fields! [form-atom rows]
  (fs/edit! form-atom update :fields
            (fn [fs] (into (vec fs)
                           (map (fn [{:keys [attribute-name attribute-type]}]
                                  {:name (clojure.core/name attribute-name)
                                   :type (if (keyword? attribute-type)
                                           (clojure.core/name attribute-type)
                                           attribute-type)}))
                           rows))))

(defn format-preview [t fmt]
  (let [cls (vt/class-of (type-kw t))
        sample (case cls :number 1234.5 :temporal (.toISOString (js/Date.)) nil)
        f (when (and sample (seq fmt)) (try (fmt/by-spec fmt) (catch :default _ nil)))]
    (when f
      (try (str (f sample (rctx/current))) (catch :default _ nil)))))

(defn- chip [active? label on-click]
  [ui/tag {:on-click on-click :intent (when active? :primary) :minimal (not active?)}
   label])

(defn- property-details
  [form-atom i shared?]
  (let [{t :type opts :opts} (get-in @form-atom [:fields i])
        cls     (vt/class-of (type-kw t))
        vts     (sort-by key (or (:value-types @catalog/canonical-schema) {}))
        fmt     (:format opts)
        kinds   (get format-kinds-by-class cls)
        hints   (set (:render-hints opts))
        carried (sort (remove shown-opt-keys (keys opts)))
        preview (format-preview t fmt)
        opt     (fn [k] (via (get opts k) #(set-prop-opt! form-atom i k %)))
        kw-opt  (fn [k] (via (type-kw (get opts k)) #(set-prop-opt! form-atom i k %)))]
    [:div {:class "form-prop-details"}
     [:div {:class "form-prop-details__grid"}
      [form/field {:label (i18n/t :ui/display-name)}
       [inputs/text-input (opt :name)
        {:placeholder (:name (get-in @form-atom [:fields i])) :small true}]]
      [form/field {:label (i18n/t :ui/description)}
       [inputs/text-input (opt :description) {:small true}]]
      [form/field {:label (i18n/t :ui/ot-visibility)}
       [inputs/select-input (kw-opt :visibility)
        {:placeholder (i18n/t :ui/ot-visibility-normal)
         :strict?     true
         :class       "bp-fill"
         :items       [{:value :prominent :label (i18n/t :ui/ot-visibility-prominent)}
                       {:value :hidden :label (i18n/t :ui/ot-visibility-hidden)}]}]]
      [form/field {:label (i18n/t :ui/ot-prop-status)}
       [inputs/select-input (kw-opt :status)
        {:strict? true
         :class   "bp-fill"
         :items   (for [s [:experimental :active :deprecated :example]]
                    {:value s :label (name s)})}]]]
     [ui/checkbox
      {:checked (boolean (:required opts))
       :on-change #(set-prop-opt! form-atom i :required
                                  (when (.. % -target -checked) true))
       :label (i18n/t :ui/ot-required)}
      " " [:span {:class "form-hint"} (i18n/t :ui/ot-required-hint)]]
     (when (and (seq vts) (not shared?))
       [form/field {:label (i18n/t :ui/ot-value-type) :help (i18n/t :ui/ot-value-type-hint)}
        [inputs/select-input
         (via (type-kw (:value-type opts))
              (fn [v]
                (set-prop-opt! form-atom i :value-type v)
                (when-let [base (and v (get-in @catalog/canonical-schema
                                               [:value-types v :type]))]
                  (fs/edit! form-atom assoc-in [:fields i :type]
                            (if (keyword? base) (clojure.core/name base) base)))))
         {:strict? true
          :class   "bp-fill"
          :items   (for [[k spec] vts]
                     {:value k
                      :label (str (clojure.core/name k) " · " (type-label (:type spec)))})}]])
     (when kinds
       [form/field {:label (i18n/t :ui/ot-value-format)
                    :help  (when preview (i18n/t :ui/ot-format-preview {:value preview}))}
        [:div {:class "form-field-row"}
         [inputs/select-input (via (type-kw (:kind fmt)) #(set-prop-format! form-atom i :kind %))
          {:strict? true
           :items   (for [k kinds] {:value k :label (i18n/t (format-kind-labels k))})}]
         (when (= :currency (:kind fmt))
           [ui/tooltip {:content (i18n/t :ui/ot-format-currency-code)}
            [inputs/text-input
             (via (:currency fmt)
                  #(set-prop-format! form-atom i :currency (str/upper-case %)))
             {:placeholder "EUR" :maxLength 3 :small true :class "form-prop-details__short"}]])
         (when (= :unit (:kind fmt))
           [ui/tooltip {:content (i18n/t :ui/ot-format-unit-name)}
            [inputs/text-input (via (:unit fmt) #(set-prop-format! form-atom i :unit %))
             {:placeholder "kg" :small true :class "form-prop-details__short"}]])
         (when (= :number cls)
           [ui/numeric-input
            {:min 0 :max 10 :value (str (or (:decimals fmt) ""))
             :title (i18n/t :ui/ot-format-decimals)
             :placeholder (i18n/t :ui/ot-format-decimals)
             :on-value-change (fn [_ s]
                                (set-prop-format! form-atom i :decimals
                                                  (when (seq s) (js/parseInt s 10))))
             :small true
             :class "form-prop-details__short"}])
         (when (= :number cls)
           [row-check {:checked (boolean (:grouping fmt))
                       :on-change #(set-prop-format! form-atom i :grouping
                                                     (when (.. % -target -checked) true))
                       :label (i18n/t :ui/ot-format-grouping)}])]])
     [form/field {:label (i18n/t :ui/ot-render-hints)}
      [:div {:class "form-chips"}
       (for [h render-hint-options]
         ^{:key h}
         [chip (hints h) (clojure.core/name h)
          (fn [] (set-prop-opt! form-atom i :render-hints
                                (let [s (if (hints h) (disj hints h) (conj hints h))]
                                  (vec (filter s render-hint-options)))))])]]
     (when (:derived opts)
       [:div {:class "form-prop-details__note"}
        [:b "ƒ " (i18n/t :ui/ot-derived)] " — " (i18n/t :ui/ot-derived-note)])
     (when (:constraints opts)
       [:div {:class "form-prop-details__note"}
        [:b (i18n/t :ui/ot-constraints)] " " [:code (pr-str (:constraints opts))]
        " — " (i18n/t :ui/ot-constraints-note)])
     (when (seq carried)
       [:div {:class "form-prop-details__note"}
        (i18n/t :ui/ot-carried {:keys (str/join ", " (map clojure.core/name carried))})])]))

(defn- property-row
  [form-atom i {:keys [names-locked? selected? shared? n title]}]
  (let [{fname :name ftype :type opts :opts} (get-in @form-atom [:fields i])
        complex?  (not (or (string? ftype) (keyword? ftype)))
        type-opts (cond-> property-base-types
                    (and (not complex?) (not (some #{(type-label ftype)} property-base-types)))
                    (conj (type-label ftype)))
        update!   (fn [k v] (fs/edit! form-atom update-in [:fields i] assoc k v))
        rename!   (fn [v]
                    (let [old (:name (get-in @form-atom [:fields i]))]
                      (fs/edit! form-atom
                                (fn [m] (cond-> (assoc-in m [:fields i :name] v)
                                          (and (seq old) (= old (:title m))) (assoc :title v))))))
        move!     (fn [delta]
                    (fs/edit! form-atom
                              (fn [m]
                                (let [fs (:fields m) j (+ i delta)]
                                  (if (and (<= 0 j) (< j (count fs)))
                                    (assoc m :fields (assoc fs i (fs j) j (fs i)) :selected-prop j)
                                    m)))))
        remove!   (fn []
                    (fs/edit! form-atom
                              (fn [m]
                                (-> m
                                    (update :fields
                                            #(vec (concat (subvec % 0 i) (subvec % (inc i)))))
                                    (dissoc :selected-prop)))))]
    [:div {:class ["form-prop" (when selected? "form-prop--selected")]}
     [:div {:class "form-field-row"}
      [inputs/text-input (via fname rename!)
       {:placeholder (i18n/t :ui/om-property-api-name)
        :disabled    names-locked?
        :small       true
        :class       "u-flex-1"}]
      (if complex?
        [:span {:class "form-prop__type--fixed"} (type-label ftype)]
        [ui/tooltip {:content (when shared? (i18n/t :ui/ot-shared-type-locked))}
         [inputs/select-input
          (via (type-label ftype)
               (fn [v]
                 (let [vt-base (some->> (:value-type opts)
                                        (get (:value-types @catalog/canonical-schema))
                                        :type)]
                   (update! :type v)
                   (when (and vt-base (not= (keyword v) vt-base))
                     (set-prop-opt! form-atom i :value-type nil)))))
          {:strict?    true
           :clearable? false
           :disabled?  (or names-locked? shared?)
           :items      (named type-opts)}]])
      [:span {:class "form-prop__badges"}
       (when (and (seq fname) (= fname title))
         [ui/tooltip {:content (i18n/t :ui/ot-title-key-hint)}
          [:span {:class "form-prop__badge"}
           (i18n/t :ui/ot-title-badge)]])
       (when shared?
         [ui/tooltip {:content (i18n/t :ui/ot-shared-badge-hint)}
          [:span {:class "form-prop__badge"} "◈"]])
       (when (:derived opts) [ui/tooltip {:content (i18n/t :ui/ot-derived)}
                              [:span {:class "form-prop__badge"} "ƒ"]])
       (when (:required opts)
         [ui/tooltip {:content (i18n/t :ui/ot-required)}
          [:span {:class "form-prop__badge form-required"} "*"]])]
      [ui/tooltip {:content (i18n/t :ui/ot-property-details)}
       [ui/button {:small    true
                   :minimal  true
                   :active   selected?
                   :on-click #(fs/edit! form-atom
                                        (fn [m]
                                          (if selected? (dissoc m :selected-prop)
                                              (assoc m :selected-prop i))))}
        (i18n/t :ui/ot-property-details)]]
      (when-not names-locked?
        [:<>
         [ui/tooltip {:content (i18n/t :ui/move-up)}
          [ui/button {:small true :minimal true :icon :chevron-up
                      :on-click #(move! -1) :disabled (zero? i)
                      :aria-label (i18n/t :ui/move-up)}]]
         [ui/tooltip {:content (i18n/t :ui/move-down)}
          [ui/button {:small true :minimal true :icon :chevron-down
                      :on-click #(move! 1) :disabled (= i (dec n))
                      :aria-label (i18n/t :ui/move-down)}]]
         [ui/tooltip {:content (i18n/t :ui/delete)}
          [ui/button
           {:small true :minimal true :icon :cross :on-click remove!
            :aria-label (i18n/t :ui/delete)}]]])]
     (when selected?
       [property-details form-atom i shared?])]))

(defn properties-section
  [form-atom {:keys [names-locked?]}]
  (let [{:keys [fields title selected-prop]} @form-atom
        sps       (shared-properties)
        present   (set (keep #(some-> (:name %) str/trim not-empty keyword) fields))
        n         (count fields)
        title-opt (filter #(and (seq (str/trim (str (:name %)))) (title-key-type? (:type %)))
                          fields)]
    [:div {:class "form-ot__section"}
     [:div {:class "form-ot__section-title"} (i18n/t :ui/ot-properties)]
     (doall
      (for [i (range n)
            :let [fname (:name (nth fields i))]]
        ^{:key i}
        [property-row form-atom i {:names-locked? names-locked?
                                   :selected? (= i selected-prop)
                                   :shared? (contains? sps (keyword (str/trim (str fname))))
                                   :n n :title title}]))
     (when-not names-locked?
       [:div {:class "form-field-row"}
        [:button
         {:type "button"
          :on-click #(fs/edit! form-atom
                               (fn [m] (-> m
                                           (update :fields (fnil conj []) {:name "" :type "string"})
                                           (assoc :selected-prop (count (:fields m))))))
          :class "form-add-row__btn u-flex-1"}
         (i18n/t :ui/om-add-property)]
        (when (seq sps)
          [inputs/select-input nil
           {:value       nil
            :placeholder (i18n/t :ui/ot-add-shared-property)
            :strict?     true
            :class       "u-flex-1"
            :items       (for [[k sp] (sort-by key sps) :when (not (present k))]
                           {:value k
                            :label (str (clojure.core/name k) " · " (type-label (:type sp)))})
            :on-change   (fn [k _]
                           (when-let [sp (get sps k)]
                             (add-fields! form-atom
                                          [{:attribute-name k :attribute-type (:type sp)}])))}])])
     [form/field {:label     (i18n/t :ui/ot-title-key)
                  :required? true
                  :help      (i18n/t :ui/ot-title-key-hint)
                  :class     "form-ot__keys"}
      [inputs/select-input (fs/at form-atom :title)
       {:strict?    true
        :clearable? false
        :items      (named (cond->> (distinct (map :name title-opt))
                             (and (not (str/blank? title))
                                  (not (some #(= title (:name %)) title-opt)))
                             (cons title)))}]]]))

(defn- form-object-type
  [form]
  (or (:loaded-as form) (some-> (:name form) str/trim not-empty keyword)))

(defn form-unmet-links [form]
  (unmet-link-constraints @catalog/canonical-schema (form-object-type form) (:implements form)))

(defn interfaces-section [form-atom]
  (let [{:keys [fields implements] :as form} @form-atom
        unmet (group-by :interface (form-unmet-links form))]
    [:div {:class "form-ot__section"}
     [implements-chips {:form-atom form-atom}]
     (doall
      (for [[iface links] unmet]
        ^{:key (str "l" iface)}
        [:div {:class "form-ot__iface-missing"}
         (i18n/t :ui/ot-interface-links-missing
                 {:interface (clojure.core/name iface)
                  :links (str/join ", " (for [{:keys [link-name to cardinality]} links]
                                          (str (clojure.core/name link-name) " → "
                                               (some-> to clojure.core/name) " ("
                                               (some-> cardinality clojure.core/name) ")")))})]))
     (doall
      (for [iface (map keyword implements)
            :let [missing (missing-interface-properties fields (interface-property-rows iface))]
            :when (seq missing)]
        ^{:key iface}
        [:div {:class "form-ot__iface-missing"}
         (i18n/t :ui/ot-interface-missing
                 {:interface (clojure.core/name iface)
                  :props (str/join ", " (map (comp clojure.core/name :attribute-name) missing))})
         [ui/button {:small true :on-click #(add-fields! form-atom missing)}
          (i18n/t :ui/ot-interface-add-missing)]]))]))

(defn fed-by-section [transforms]
  [:div {:class "form-backing" :style {:border "1px solid var(--c-primary)"}}
   [:div {:class "form-backing__title" :style {:color "var(--c-primary)"}}
    (i18n/t :ui/backing-question)]
   [:div {:class "dataconn-runs__caption"}
    (i18n/t :ui/ot-fed-by {:transforms (str/join ", " (map clojure.core/name transforms))})]])
