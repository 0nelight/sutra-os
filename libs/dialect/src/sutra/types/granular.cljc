(ns sutra.types.granular
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [sutra.types.table :as tbl]))

(def user-builtins
  {:id            {:collection? false}
   :username      {:collection? false}
   :groups        {:collection? true :membership? true}
   :markings      {:collection? true :membership? true}
   :organizations {:collection? true :membership? true}})

(def membership-attributes (tbl/where user-builtins :membership?))

(def control-kinds
  {:markings      {:pass (fn [_ denied? xs] (not-any? denied? xs))}
   :organizations {:pass (fn [subj _ xs]
                           (or (empty? xs) (boolean (some (set (:organizations subj)) xs))))}})

(defn- as-set [v] (if (set? v) v (set v)))

(defn- ordered [f] (fn [a b] (f (compare a b) 0)))

(def comparisons
  {:=           {:shape :scalars :test =}
   :intersects  {:shape :sets    :test #(boolean (seq (set/intersection %1 %2)))}
   :subset-of   {:shape :sets    :test set/subset?}
   :superset-of {:shape :sets    :test set/superset?}
   :<           {:shape :scalars :test (ordered <)  :ordered? true}
   :<=          {:shape :scalars :test (ordered <=) :ordered? true}
   :>           {:shape :scalars :test (ordered >)  :ordered? true}
   :>=          {:shape :scalars :test (ordered >=) :ordered? true}})

(def connectives #{:and :or :not})

(def limits
  {:comparisons 10
   :weight      10000
   :weights     {:constant 1 :collection 1000 :marking 3000}})

(def security-kinds
  {:object   {:ordered? false :field :property}
   :property {:ordered? false :field :property}
   :view     {:ordered? true  :field :column}})

(defn- term-kind
  [t]
  (cond
    (and (vector? t) (= :user (first t)))                    :user
    (and (vector? t) (#{:property :column} (first t)))       :field
    (or (string? t) (number? t) (boolean? t) (keyword? t))   :literal
    (and (set? t) (every? #(not (coll? %)) t))               :literal
    :else                                                    nil))

(defn- collection-term?
  [t]
  (case (term-kind t)
    :user    (get-in user-builtins [(second t) :collection?] false)
    :literal (set? t)
    false))

(defn- scalar
  [v]
  (cond (keyword? v)             (name v)
        (uuid? v)                (str v)
        (int? v)                 (long v)
        (number? v)              (double v)
        :else                    v))

(defn- value
  [v]
  (cond (nil? v)                       nil
        (or (set? v) (sequential? v))  (into #{} (comp (remove nil?) (map scalar)) v)
        :else                          (scalar v)))

(defn- same-kind?
  [a b]
  (or (and (number? a) (number? b)) (and (string? a) (string? b))
      (and (boolean? a) (boolean? b))))

(defn- holds?
  [{:keys [shape test]} a b]
  (and (some? a) (some? b)
       (case shape
         :scalars (and (not (set? a)) (not (set? b)) (same-kind? a b) (boolean (test a b)))
         :sets    (boolean (test (as-set (if (set? a) a [a])) (as-set (if (set? b) b [b])))))))

(defn exempt?
  [authority]
  (or (nil? authority) (true? (:system? authority)) (= :all (:clearances authority))))

(defn subject
  [{:keys [id username groups markings organizations attributes]}]
  {:id            (some-> id str)
   :username      (some-> username str)
   :groups        (into #{} (map str) groups)
   :markings      (into #{} (map name) markings)
   :organizations (into #{} (map str) organizations)
   :attributes    (or attributes {})})

(defn- user-value
  [subj k]
  (if (contains? user-builtins k) (get subj k) (get-in subj [:attributes k])))

(defn- resolve-term
  [subj t]
  (case (term-kind t)
    :user    (let [v (value (user-value subj (second t)))] (constantly v))
    :field   (let [k (second t)] (fn [vs] (value (get vs k))))
    :literal (let [v (value t)] (constantly v))
    (constantly nil)))

(defn predicate
  [policy subj]
  (cond
    (true? policy)  (constantly true)
    (not (vector? policy)) (constantly false)
    :else
    (let [[op & args] policy]
      (case op
        :and (let [ps (mapv #(predicate % subj) args)] (fn [vs] (every? #(% vs) ps)))
        :or  (let [ps (mapv #(predicate % subj) args)] (fn [vs] (boolean (some #(% vs) ps))))
        :not (if (= 1 (count args))
               (complement (predicate (first args) subj))
               (constantly false))
        (if-let [c (and (= 2 (count args)) (get comparisons op))]
          (let [[l r] (map #(resolve-term subj %) args)]
            (fn [vs] (holds? c (l vs) (r vs))))
          (constantly false))))))

(defn admits?
  [policy subj values]
  ((predicate policy subj) values))

(defn- control-values
  [v]
  (cond
    (nil? v)        #{}
    (string? v)     (into #{} (comp (map str/trim) (remove str/blank?)) (str/split v #","))
    (coll? v)       (into #{} (map #(if (keyword? %) (name %) (str %))) v)
    (keyword? v)    #{(name v)}
    :else           #{(str v)}))

(defn control-spec
  [d]
  (if (map? d)
    (cond-> {:kind (:kind d)} (seq (:properties d)) (assoc :properties (vec (:properties d))))
    {:kind d}))

(defn- control-pass
  [kind subj denied?]
  (let [pass (get-in control-kinds [kind :pass])]
    (fn [v] (boolean (and pass (pass subj denied? (control-values v)))))))

(defn mandatory-control
  [controls subj denied?]
  (let [checks (vec (for [[p d] controls]
                      [p (control-pass (:kind (control-spec d)) subj denied?)]))]
    (fn [vs] (every? (fn [[p pass?]] (pass? (get vs p))) checks))))

(defn datasource-scoped
  [controls supplements]
  (into {}
        (map (fn [[p d]]
               (let [spec (control-spec d)
                     ds   (when-not (:properties spec)
                            (first (filter #(contains? % p) supplements)))]
                 [p (if ds (assoc spec :properties (vec (sort ds))) d)])))
        controls))

(defn with-org-names
  [subj name-of]
  (update subj :organizations
          (fn [os] (into (set os) (keep #(some-> (name-of %) str)) os))))

(defn object-guard
  [security subj denied?]
  (when (seq security)
    (let [policy (predicate (get security :policy true) subj)
          scoped (group-by (comp some? :properties control-spec val)
                           (:mandatory-controls security))
          ctl    (mandatory-control (get scoped false) subj denied?)
          admit  (fn [vs] (and (ctl vs) (policy vs)))
          groups (vec (concat
                       (for [[_ {:keys [properties policy markings]}]
                             (:property-policies security)]
                         {:properties (vec properties)
                          :pass?      (if (some denied? markings)
                                        (constantly false)
                                        (predicate (if (some? policy) policy true) subj))})
                       (for [[p d] (get scoped true)
                             :let [{:keys [kind properties]} (control-spec d)
                                   pass? (control-pass kind subj denied?)]]
                         {:properties properties :pass? #(pass? (get % p))})))]
      {:admit? admit
       :redact (if (empty? groups)
                 identity
                 (fn [vs]
                   (reduce (fn [out {:keys [properties pass?]}]
                             (if (pass? vs) out (apply dissoc out properties)))
                           vs groups)))})))

(defn restricted-rows
  [policy authority rows]
  (if (exempt? authority)
    (vec rows)
    (filterv (comp (predicate policy (or (:subject authority) (subject {}))) :values) rows)))

(defn- comparison-weight
  [[_ l r]]
  (let [{w :weights} limits
        ts [l r]]
    (cond (some #(= [:user :markings] %) ts)     (:marking w)
          (some collection-term? ts)              (:collection w)
          :else                                   (:constant w))))

(defn comparisons-of
  ([policy] (comparisons-of policy false))
  ([policy negated?]
   (if (and (vector? policy) (connectives (first policy)))
     (mapcat #(comparisons-of % (or negated? (= :not (first policy)))) (rest policy))
     (when (vector? policy) [{:comparison policy :negated? negated?}]))))

(defn weight
  [policy]
  (transduce (map (comp comparison-weight :comparison)) + 0 (comparisons-of policy)))

(defn- shape-problems
  [policy fields kind]
  (letfn [(walk [p]
            (cond
              (true? p) nil
              (not (vector? p)) [{:problem :malformed :at p}]
              :else
              (let [[op & args] p]
                (cond
                  (#{:and :or} op)
                  (if (empty? args) [{:problem :malformed :at p}] (mapcat walk args))

                  (= :not op)
                  (if (= 1 (count args)) (walk (first args)) [{:problem :malformed :at p}])

                  (not (contains? comparisons op))
                  [{:problem :unknown-op :at p :op op}]

                  (not= 2 (count args))
                  [{:problem :malformed :at p}]

                  :else
                  (concat
                   (for [t args :when (nil? (term-kind t))] {:problem :bad-term :at p :term t})
                   (for [t args
                         :when (and (= :field (term-kind t)) fields
                                    (not (contains? fields (second t))))]
                     {:problem :unknown-field :at p :field (second t)})
                   (when (and (:ordered? (comparisons op))
                              (not (get-in security-kinds [kind :ordered?])))
                     [{:problem :ordering-not-supported :at p :op op}])
                   (when (every? #(= :literal (term-kind %)) args)
                     [{:problem :constant-comparison :at p}]))))))]
    (walk policy)))

(defn policy-problems
  [policy fields kind]
  (let [cs (comparisons-of policy)
        w  (weight policy)]
    (vec (concat
          (shape-problems policy fields kind)
          (for [{[_ & ts] :comparison :keys [negated?]} cs
                :when (and negated?
                           (some #(and (= :user (term-kind %))
                                       (membership-attributes (second %)))
                                 ts))]
            {:problem :negated-membership})
          (when (> (count cs) (:comparisons limits))
            [{:problem :too-many-comparisons :count (count cs) :max (:comparisons limits)}])
          (when (>= w (:weight limits))
            [{:problem :too-heavy :weight w :max (:weight limits)}])))))

(defn security-problems
  [{:keys [policy property-policies] :as security} fields pk]
  (let [object-policy (if (contains? security :policy) policy true)
        guarded       (for [[pp {:keys [properties]}] property-policies, p properties] [pp p])]
    (vec
     (concat
      (map #(assoc % :policy :object) (policy-problems object-policy fields :object))
      (when (and (seq property-policies) (not (contains? security :policy)))
        [{:problem :property-policy-without-object-policy}])
      (for [[pp {:keys [policy markings properties]}] property-policies
            problem (concat
                     (when (and (nil? policy) (empty? markings))
                       [{:problem :property-policy-empty}])
                     (when (empty? properties)
                       [{:problem :property-policy-guards-nothing}])
                     (some-> policy (policy-problems fields :property))
                     (let [w (+ (weight object-policy) (weight (or policy true)))]
                       (when (>= w (:weight limits))
                         [{:problem :too-heavy :weight w :max (:weight limits)}])))]
        (assoc problem :policy pp))
      (for [[pp p] guarded :when (= p pk)]
        {:problem :primary-key-guarded :policy pp :property p})
      (for [[pp p] guarded :when (and fields (not (contains? fields p)))]
        {:problem :unknown-property :policy pp :property p})
      (for [[p d] (:mandatory-controls security)
            :let [{:keys [kind properties]} (control-spec d)]
            :when (or (and fields (not (contains? fields p))) (= p pk)
                      (not (contains? control-kinds kind))
                      (some #(= pk %) properties)
                      (and fields (some #(not (contains? fields %)) properties)))]
        {:problem :mandatory-control-invalid :policy :mandatory-control :property p})
      (for [[p pps] (group-by second guarded) :when (> (count pps) 1)]
        {:problem :property-guarded-twice :property p :policies (mapv first pps)})))))

(def Policy
  [:or :boolean [:vector :any]])

(def Security
  [:map {:closed true}
   [:policy {:optional true} Policy]
   [:property-policies {:optional true}
    [:map-of :keyword [:map {:closed true}
                       [:properties [:vector :keyword]]
                       [:policy {:optional true} Policy]
                       [:markings {:optional true} [:vector :keyword]]]]]
   [:mandatory-controls {:optional true}
    [:map-of :keyword [:or [:enum :markings :organizations]
                       [:map {:closed true}
                        [:kind [:enum :markings :organizations]]
                        [:properties {:optional true} [:vector :keyword]]]]]]])

(def RestrictedView
  [:map {:closed true}
   [:dataset :keyword]
   [:policy Policy]])
