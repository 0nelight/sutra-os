(ns sutra.types.submission
  (:require [sutra.types.refs :as refs]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.ref.params :as rp]
            [sutra.ref.term :as term]
            [sutra.types.value-types :as vt]))

(def supplied? rp/supplied?)
(def with-section-overrides rp/with-section-overrides)

(defn client-gated? [spec]
  (and (map? spec)
       (true? (:required spec))
       (not (contains? spec :prefill))
       (not (contains? spec :default))
       (empty? (:overrides spec))))

(defn param-label [spec p]
  (or (not-empty (str (:name spec))) (name p)))

(defn hidden?
  [ps]
  (= :hidden (keyword (some-> (:exposure ps) name))))

(defn param-descriptor
  [ps]
  (cond-> {}
    (client-gated? ps)      (assoc :required true)
    (true? (:required ps))  (assoc :declared-required true)
    (true? (:multiple ps))  (assoc :multiple true)
    (:name ps)              (assoc :name (:name ps))
    (:type ps)              (assoc :type (:type ps))
    (:constraints ps)       (assoc :constraints (:constraints ps))
    (:exposure ps)          (assoc :exposure (:exposure ps))
    (:prefill ps)           (assoc :prefill (:prefill ps))
    (:description ps)       (assoc :description (:description ps))
    (seq (:render-hints ps)) (assoc :render-hints (vec (:render-hints ps)))
    (false? (:editable ps))  (assoc :editable false)
    (:options-from ps)       (assoc :options-from (:options-from ps))
    (seq (:overrides ps))    (assoc :overrides true)
    (some #(contains? % :default) (:overrides ps)) (assoc :override-default true)
    (:compute ps)            (assoc :compute true)
    (contains? ps :default)  (assoc :default (:default ps))))

(defn evaluated?
  [action specs]
  (boolean (or (seq (:submission-criteria action))
               (some :overrides (vals specs))
               (some :compute (vals specs))
               (some #(map? (:default %)) (vals specs))
               (some #(seq (:args (:options-from %))) (vals specs)))))

(defn option-row
  [label r]
  (let [vals (if (map? (:values r)) (:values r) (dissoc r :id))]
    {:id     (:id r)
     :values (cond-> vals
               (and label (nil? (get vals :name))) (assoc :name (get vals label)))}))

(def option-caption-fallbacks
  [:name :bezeichnung])

(def option-visible-max 20)

(defn option-row-show
  [{:keys [label property match-keys text]}]
  (not-empty
   (into []
         (comp cat (filter some?) (distinct))
         (if property
           [[property label]]
           [[label] option-caption-fallbacks match-keys text]))))

(defn with-on-prefill
  ([action] (with-on-prefill action #(some-> % name keyword)))
  ([action type-id]
   (let [on (some-> (:on action) type-id)
         ps (when on (for [[p spec] (:params action)
                           :when (and (= on (some-> (:type spec) type-id))
                                      (not (:multiple spec))
                                      (nil? (:prefill spec)))]
                       p))]
     (if (= 1 (count ps))
       (assoc-in action [:params (first ps) :prefill] :current-object)
       action))))

(defn action-param-descriptors
  ([declared] (action-param-descriptors declared nil))
  ([declared allowed]
   (into {}
         (keep (fn [p] (when-let [ps (get declared p)] [p (param-descriptor ps)])))
         (or (seq allowed) (keys declared)))))

(defn form-sections
  [form specs]
  (let [named    (into #{} (mapcat :params) (:sections form))
        leftover (->> (keys specs)
                      (remove named)
                      (sort-by (fn [p] [(if (:required (get specs p)) 0 1)
                                        (str/lower-case (param-label (get specs p) p))]))
                      vec)]
    (cond-> (into []
                  (keep (fn [sec]
                          (let [ps (vec (filter #(contains? specs %) (:params sec)))]
                            (when (seq ps) (assoc sec :params ps)))))
                  (:sections form))
      (seq leftover) (conj {:params leftover}))))

(def param-widening
  {:byte   :integer
   :short  :integer
   :float  :double
   :cipher :string})

(def no-param-type
  #{:mandatory-control})

(defn param-type-of
  [prop-type]
  (let [head (if (sequential? prop-type) (first prop-type) prop-type)]
    (when-not (contains? no-param-type head)
      (get param-widening head head))))

(defn- labels [param-specs ps]
  (str/join ", " (map #(param-label (get param-specs %) %) ps)))

(defn- listing [param-specs ps]
  (str (if (next ps) "Pflichtfelder fehlen: " "Pflichtfeld fehlt: ")
       (labels param-specs ps)))

(defn implicit-create-id [i]
  (keyword (str "regel-" i "-id")))

(defn implicit-create-id? [p]
  (boolean (and (keyword? p) (re-matches #"regel-\d+-id" (name p)))))

(def implicit-context-params
  {:regel-jetzt      {:source :now
                      :spec {:type :timestamp :prefill :now :exposure :hidden}}
   :regel-heute      {:source :now          :spec {:type :date :prefill :now :exposure :hidden}}
   :regel-jetzt-text {:source :now          :spec {:type :string :prefill :now :exposure :hidden}}
   :regel-benutzer   {:source :current-user
                      :spec {:type :string :prefill :current-user :exposure :hidden}}})

(defn context-param
  [source base]
  (if (= :current-user source)
    :regel-benutzer
    (case (when (or (keyword? base) (string? base)) (name base))
      "date" :regel-heute "timestamp" :regel-jetzt :regel-jetzt-text)))

(defn implicit-context-param? [p]
  (contains? implicit-context-params (keyword p)))

(defn implicit-param? [p]
  (or (implicit-create-id? p) (implicit-context-param? p)))

(defn param-bound?
  [spec v]
  (if (true? (:multiple spec)) (some? v) (supplied? v)))

(defn missing-required
  ([param-specs params] (missing-required param-specs params client-gated?))
  ([param-specs params gate?]
   (->> param-specs
        (filter (fn [[p spec]] (and (gate? spec) (not (param-bound? spec (get params p))))))
        (map key)
        (sort-by name)
        vec)))

(defn submit-gate [param-specs params]
  (let [missing (missing-required param-specs params)]
    {:missing  missing
     :blocked? (boolean (seq missing))
     :reason   (when (seq missing) (listing param-specs missing))}))

(defn- refused
  [action-name]
  (str "„" (name action-name) "“ nicht ausgeführt — "))

(defn required-refusal [action-name param-specs missing]
  (str (refused action-name) (listing param-specs missing)))

(defn constraint-refusal
  [action-name param-specs problems]
  (str (refused action-name)
       (str/join "; "
                 (for [{:keys [parameter constraint value allowed message]} problems]
                   (str (param-label (get param-specs parameter) parameter) ": "
                        (if (= :one-of constraint)
                          (str (pr-str value) " ist nicht erlaubt (erlaubt: "
                               (str/join ", " (map #(if (map? %) (:value %) %) allowed)) ")")
                          message))))))

(defn criterion-refusal [action-name criterion]
  (or (not-empty (str (:message criterion)))
      (str (refused action-name) "eine Vorbedingung ist nicht erfüllt")))

(defn unresolved-param-refusal
  [action-name param spec id {:keys [reason key]}]
  (str (refused action-name) (param-label spec param)
       " verweist auf " (some-> (:type spec) name) " " (pr-str (str id))
       (case reason
         :ambiguous (str ", und das ist " (name key) " mehrerer Objekte — "
                         (name key) " ist in den Daten nicht eindeutig; die Objekt-ID angeben.")
         (str ", und dazu gibt es keine Zeile"
              (if key
                (str " — weder als " (name key) " (sein Schlüssel) noch als Objekt-ID.")
                " — der Typ hat keine einzelne Schlüsseleigenschaft, nur die Objekt-ID benennt eines.")))))

(def unreadable rp/unreadable)

(defn type-refusal [action-name param-specs problems]
  (str (refused action-name)
       (if (next problems)
         "Parameterwerte passen nicht zu ihrem Typ: "
         "Parameterwert passt nicht zu seinem Typ: ")
       (str/join ", "
                 (for [{:keys [parameter expected value]} problems]
                   (str (param-label (get param-specs parameter) parameter)
                        " erwartet " (some-> expected name)
                        ", bekam " (pr-str value))))))

(defn- tok [x] (when (or (keyword? x) (string? x)) (name x)))

(defn- schema-entry
  [schema kind name-key t]
  (let [m (get schema kind) s (tok t)]
    (when (and (map? m) s)
      (or (get m t) (get m (keyword s)) (get m s)
          (some (fn [[k v]] (when (and (map? v)
                                       (or (= s (tok k)) (= s (tok (get v name-key)))
                                           (= s (tok (:id v))) (= s (tok (:rid v)))
                                           (= s (tok (:api-name v)))))
                              v))
                m)))))

(defn property-format
  [schema t prop]
  (let [ot (schema-entry schema :object-types :object-type-name t)
        p  (tok prop)
        a  (some (fn [a] (when (= p (tok (:attribute-name a))) a))
                 (:attribute-list ot))
        o  (:opts a)
        vt (some->> (:value-type o) (schema-entry schema :value-types :value-type-name))]
    (not-empty (merge (:format vt) (:format o)))))

(defn- written-property
  [rules p]
  (let [s (tok p)]
    (some (fn [{:keys [object-type set values]}]
            (when object-type
              (or (when (some #(= s (tok %)) set) [object-type p])
                  (some (fn [[k v]]
                          (when (and (vector? v) (= 2 (count v))
                                     (= "get" (tok (first v))) (= s (tok (second v))))
                            [object-type k]))
                        values))))
          (refs/steps rules))))

(defn- read-property
  [action params spec]
  (let [d (:default spec)]
    (when (and (map? d) (:property d))
      (let [from (:from d)
            src  (if from
                   (:type (get params (keyword (tok from))))
                   (or (some (fn [[_ s]] (when (= "current-object" (tok (:prefill s))) (:type s)))
                             params)
                       (:on action)))]
        (when src [src (:property d)])))))

(defn param-formats
  [schema action]
  (let [params (:params action)]
    (into {}
          (keep (fn [[p spec]]
                  (when (contains? vt/numeric-types (keyword (or (tok (:type spec)) "")))
                    (when-let [f (some (fn [[t prop]] (property-format schema t prop))
                                       (remove nil? [(written-property (:rules action) p)
                                                     (read-property action params spec)]))]
                      [p f]))))
          params)))

(defn with-param-formats
  [descriptors schema action]
  (let [fs (param-formats schema action)]
    (reduce-kv (fn [m p d] (if-let [f (get fs p)] (assoc m p (assoc d :format f)) m))
               descriptors descriptors)))

(defn decode-param-text
  [spec s]
  (rp/decode-text (:type spec) s))

(defn coerce-param-value
  [spec v]
  (:value (term/parse (term/of spec) v)))

(defn decode-params-text
  [param-specs params]
  (reduce-kv
   (fn [m p v]
     (assoc m p
            (if-let [spec (get param-specs p)]
              (let [{:keys [value problems]} (term/parse (term/of spec) v)]
                (if (seq problems) v value))
              v)))
   {} params))

(defn decode-object-set-text
  ([s] (decode-object-set-text s nil))
  ([s read-json]
   (let [t (str/trim (str s))]
     (if-not (some #(str/starts-with? t %) ["[" "{" "("])
       (into [] (comp (map str/trim) (remove str/blank?)) (str/split t #","))
       (let [v (or (try (edn/read-string t) (catch #?(:clj Exception :cljs :default) _ nil))
                   (when read-json
                     (try (read-json t) (catch #?(:clj Exception :cljs :default) _ nil))))]
         (cond
           (sequential? v) (mapv #(if (keyword? %) (name %) (str %)) v)
           (map? v)        (walk/keywordize-keys v)
           :else           unreadable))))))

(defn template-term
  [pspec]
  (case (some-> (:type pspec) name keyword)
    :object     [:ref [(:object-type pspec)]]
    :object-set [:object-set]
    :graph      [:named :graph]
    (term/base (:type pspec))))

(defn decode-template-value
  ([pspec v] (decode-template-value pspec v nil))
  ([pspec v read-json]
   (let [t (template-term pspec)]
     (cond
       (not (string? v))           v
       (= [:object-set] t)         (let [d (decode-object-set-text v read-json)]
                                     (if (= unreadable d) v d))
       :else                       (let [{:keys [value problems]} (term/parse t v)]
                                     (if (seq problems) v value))))))

(defn decode-template-values
  ([param-specs values] (decode-template-values param-specs values nil))
  ([param-specs values read-json]
   (reduce-kv
    (fn [m p v] (assoc m p (decode-template-value (get param-specs p) v read-json)))
    {} (or values {}))))
