(ns sutra-ui.forms.link-type
  (:require [sutra.types.link-side :as ls]
            [sutra.types.scaffold :as scaffold]
            [sutra.types.transform-io :as tio]))

(defn- kw [x] (when (some? x) (if (keyword? x) x (keyword (str x)))))

(defn backing-of
  [canon lname]
  (let [lt (get-in canon [:link-types lname])]
    (some (fn [[tk t]]
            (when-let [out (some #(when (and (= :link-type (kw (:kind %)))
                                             (= lname (kw (:link-type %))))
                                    %)
                                 (tio/outputs-of t))]
              (let [in-type (when (= :object-type (kw (get-in t [:input :kind])))
                              (kw (get-in t [:input :object-type])))
                    from    (kw (:from-object-type lt))
                    to      (kw (:to-object-type lt))
                    side    (cond
                              (and (= in-type from) (= in-type to))
                              (or (scaffold/inferred-fk-side (:cardinality lt)) :from)
                              (= in-type from) :from
                              (= in-type to)   :to)]
                (if (and side (:fk-attribute out))
                  (merge {:kind :fk :producer (kw (or (:transform-name t) tk))
                          :fk-side side :fk-attribute (kw (:fk-attribute out))}
                         (select-keys out [:on-null :on-unresolved :on-duplicate]))
                  {:kind :producer :producer (kw (or (:transform-name t) tk))}))))
          (:transforms canon))))

(defn primary-key-of
  [canon ot]
  (let [ot (kw ot)]
    (or (get-in canon [:object-types ot :backing :primary-key])
        (get-in canon [:object-types ot :external-source :primary-key])
        (some (fn [[_ t]]
                (some #(when (and (= :object-type (kw (:kind %)))
                                  (= ot (kw (:object-type %)))
                                  (not= :supplement (kw (:role %))))
                         (kw (:primary-key %)))
                      (tio/outputs-of t)))
              (:transforms canon)))))

(defn- fields-of [sch ot]
  (get-in sch [:object-types (kw ot) :fields]))

(defn fk-candidates
  [sch ot]
  (let [cmap (get-in sch [:object-types (kw ot) :external-source :column-map])
        mapped? (if (map? cmap) #(contains? cmap %) (constantly true))]
    (vec (for [{:keys [name type]} (fields-of sch ot)
               :when (mapped? name)]
           {:name name :type type}))))

(defn suggest-fk
  [sch canon fk-type pk-type]
  (when-not (= (kw fk-type) (kw pk-type))
    (let [cands (fk-candidates sch fk-type)
          pk    (primary-key-of canon pk-type)
          pk-t  (some #(when (= pk (:name %)) (:type %)) (fields-of sch pk-type))
          want  (or pk (keyword (str (name pk-type) "-id")))]
      (some #(when (and (= want (:name %))
                        (or (nil? pk-t) (= pk-t (:type %))))
               (:name %))
            cands))))

(defn effective-implements
  [sch ot]
  (let [ifs (:interfaces sch)]
    (loop [todo (vec (get-in sch [:object-types (kw ot) :implements]))
           seen #{}]
      (if-let [i (first todo)]
        (if (seen i)
          (recur (subvec todo 1) seen)
          (recur (into (subvec todo 1) (get-in ifs [i :extends])) (conj seen i)))
        seen))))

(defn constraint-options
  [sch from to card]
  (let [to-impl (effective-implements sch to)]
    (vec (for [i (sort (effective-implements sch from))
               {:keys [link-name to-object-type to-interface cardinality required]}
               (get-in sch [:interfaces i :link-constraints])
               :let [target (or to-object-type to-interface)
                     reach? (or (= target (kw to))
                                (and to-interface (contains? to-impl to-interface)))
                     want   (ls/constraint-cardinality cardinality)
                     card?  (or (nil? want) (= want (ls/far-cardinality (kw card) :to)))]]
           {:interface i :constraint link-name :target target
            :cardinality want :required (boolean required)
            :fits? (and reach? card?)
            :why (cond (not reach?) :target (not card?) :cardinality)}))))

(def side-keys [:forward-api-name :reverse-api-name :forward-name :reverse-name])

(defn- satisfies-set [s]
  (set (for [{:keys [interface constraint]} (cond (map? s) [s] (sequential? s) s)]
         [(kw interface) (kw constraint)])))

(defn load-form
  [sch canon lname]
  (let [row  (some #(when (= lname (:name %)) %) (:links sch))
        spec (merge (get-in canon [:link-types lname]) row)
        {:keys [from to cardinality]} row
        b    (backing-of canon lname)
        es   (:external-source row)
        mode (cond
               (= :fk (:kind b))   :synced
               b                   :producer
               es                  :synced
               :else               :in-system)]
    (merge
     {:name        (name lname)
      :from        (some-> from name)
      :to          (some-> to name)
      :cardinality (some-> cardinality name)
      :backing-mode mode
      :external    (when (= :fk (:kind b))
                     (select-keys b [:fk-side :fk-attribute :on-null
                                     :on-unresolved :on-duplicate]))
      :satisfies   (satisfies-set (:satisfies spec))
      :loaded      {:name lname :backing b :from from :to to :cardinality cardinality
                    :mode mode
                    :satisfies (satisfies-set (:satisfies spec))}}
     (into {} (for [k side-keys
                    :let [v (get spec k)]
                    :when (some? v)]
                [k (if (keyword? v) (name v) (str v))])))))

(defn api-name-conflicts
  [canon {:keys [name from to forward-api-name reverse-api-name loaded]}]
  (let [lk (keyword name)
        ct (cond-> {:link-type-name lk :from-object-type (kw from) :to-object-type (kw to)}
             (seq forward-api-name) (assoc :forward-api-name (keyword forward-api-name))
             (seq reverse-api-name) (assoc :reverse-api-name (keyword reverse-api-name)))
        sch (-> canon
                (update :link-types dissoc (:name loaded))
                (assoc-in [:link-types lk] ct))]
    (vec (filter (fn [c] (some #(= lk (:link %)) (:sides c)))
                 (ls/name-conflicts sch)))))

(defn fk-external-source
  [{:keys [fk-side fk-attribute on-null on-unresolved on-duplicate]}]
  (when (and (#{:from :to} fk-side) fk-attribute)
    (cond-> {:strategy :endpoint-fk :fk-side fk-side :fk-attribute (kw fk-attribute)}
      (= :skip on-null)       (assoc :on-null :skip)
      (= :skip on-unresolved) (assoc :on-unresolved :skip)
      (= :skip on-duplicate)  (assoc :on-duplicate :skip))))

(defn- fk-type [from to side] (case side :from from :to to nil))

(defn fk-unchanged?
  [{:keys [from to external loaded]}]
  (let [b (:backing loaded)]
    (and (= :fk (:kind b))
         (= (fk-type (kw (:from loaded)) (kw (:to loaded)) (:fk-side b))
            (fk-type (kw from) (kw to) (:fk-side external)))
         (= (:fk-attribute b) (kw (:fk-attribute external))))))

(defn form->spec
  [{:keys [name from to cardinality backing-mode external satisfies loaded] :as form}
   canon join-es]
  (let [card   (kw cardinality)
        m-m?   (= :many-to-many card)
        mode   (or backing-mode :synced)
        legacy (and (= :in-system (:mode loaded)) (not m-m?))
        es     (case mode
                 :synced    (if m-m? (join-es external cardinality) (fk-external-source external))
                 nil)
        keep-fk? (and (= :synced mode) (not m-m?) (fk-unchanged? form))
        missing  (scaffold/link-type-problem (assoc form :external-source es) (nil? loaded))]
    (cond
      missing
      {:error (first missing)}

      (nil? card)
      {:error :ui/pick-a-cardinality}

      (and (= :synced mode) (nil? es) (not keep-fk?))
      {:error (if m-m? :ui/link-needs-join-table :ui/link-fk-required)}

      (seq (api-name-conflicts canon form))
      (let [{t :type n :api-name} (first (api-name-conflicts canon form))]
        {:error :ui/link-api-name-taken
         :args {:type (clojure.core/name t) :api-name (clojure.core/name n)}})

      (and (= :in-system mode) (not m-m?) (not legacy))
      {:error :ui/link-fk-required}

      :else
      {:spec (cond-> (merge {:name name :from from :to to :cardinality cardinality}
                            (into {} (filter (comp seq val)) (select-keys form side-keys)))
               (and es (not keep-fk?)) (assoc :external-source es)
               (not= (set satisfies) (:satisfies loaded #{}))
               (assoc :satisfies (vec (for [[i c] (sort (set satisfies))]
                                        {:interface i :constraint c}))))})))
