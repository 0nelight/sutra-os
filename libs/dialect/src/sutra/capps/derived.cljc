(ns sutra.capps.derived
  (:require [sutra.kit.coerce :refer [->kw]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.types.query :as q]
            [sutra.types.link-side :as ls]))

(def kinds
  [{:id :aggregation :label "Verknüpfte Eigenschaft / Aggregation"
    :hint "Über einen Link aggregieren — oder bei einer 1:1-Verknüpfung einen Wert übernehmen."}
   {:id :column-math :label "Spaltenrechnung"
    :hint "Werte mehrerer Eigenschaften EINES Objekttyps verrechnen."}])

(def aggs q/aggs)

(def math-ops
  #{:+ :- :* :div :mod :abs})

(defn kind-of
  [spec]
  (when (map? spec)
    (let [k (->kw (:kind spec))]
      (cond
        (contains? #{:aggregation :column-math} k) k
        (contains? spec :expression)               :column-math
        (contains? spec :over)                     :aggregation
        :else                                      nil))))

(def ^:private rollup-slots [:of :by :pick :where])

(defn- slot-list [ks] (str/join ", " (map str (sort ks))))

(defn rollup-arity-error
  [spec]
  (if-let [op (q/op-of-agg (->kw (:agg spec)))]
    (let [nm       (name (->kw (:agg spec)))
          required (set (rest (q/link-slots op)))
          allowed  (cond-> required (q/link-takes-where? op) (conj :where))
          present  (into #{} (filter #(some? (get spec %))) rollup-slots)
          missing  (remove present required)
          extra    (remove allowed present)]
      (cond
        (seq missing) (str nm " braucht " (slot-list missing)
                           (when-let [forbidden (seq (remove allowed rollup-slots))]
                             (str " — und nicht " (slot-list forbidden))))
        (seq extra)   (str nm " nimmt kein " (slot-list extra)
                           (if (seq allowed)
                             (str " — nur " (slot-list allowed))
                             " — nur :over"))))
    (str ":agg fehlt oder ist unbekannt — eines von " (str/join ", " (map name aggs)))))

(defn expr-ops
  [expr]
  (if-not (vector? expr)
    #{}
    (let [[op & args] expr]
      (into (if (keyword? op) #{op} #{}) (mapcat expr-ops) args))))

(defn expr-refs
  [expr]
  (cond
    (keyword? expr)      #{expr}
    (not (vector? expr)) #{}
    :else (let [[op & args] expr]
            (if (= :get op)
              (into #{} (comp (take 1) (filter keyword?)) args)
              (into #{} (mapcat expr-refs) args)))))

(defn types
  [module]
  (vec (sort (keys (or module {})))))

(defn properties
  [module type]
  (->> (get module (->kw type))
       (sort-by (fn [[nm spec]] [(if (= :aggregation (kind-of spec)) 0 1) (name nm)]))
       vec))

(defn all-properties
  [module]
  (vec (for [t (types module), [nm spec] (properties module t)] [t nm spec])))

(defn names-of-kind [module type k]
  (vec (for [[nm spec] (properties module type) :when (= k (kind-of spec))] nm)))

(defn referenceable
  [module type native]
  {:native      (vec (sort (map ->kw (or native []))))
   :aggregation (names-of-kind module type :aggregation)
   :column-math (names-of-kind module type :column-math)})

(defn put
  [module type nm spec]
  (assoc-in (or module {}) [(->kw type) (->kw nm)] spec))

(defn drop-property
  [module type nm]
  (let [t (->kw type)
        m (update (or module {}) t dissoc (->kw nm))]
    (if (empty? (get m t)) (dissoc m t) m)))

(defn rename
  [module type old new]
  (let [t (->kw type) old (->kw old) new (->kw new)]
    (if-let [spec (get-in module [t old])]
      (update module t
              (fn [props]
                (into {}
                      (for [[nm s] (-> props (dissoc old) (assoc new spec))]
                        [nm (if (= :column-math (kind-of s))
                              (update s :expression
                                      #(walk/postwalk (fn [x] (if (= x old) new x)) %))
                              s)]))))
      module)))

(defn- problem [type nm error field]
  (cond-> {:type type :name nm :error error} field (assoc :field field)))

(defn far-type
  [links type link]
  (:far (ls/table-side links type link)))

(defn- explain-aggregation
  [_module type nm spec {:keys [props links]}]
  (let [far    (far-type links type (:over spec))
        native (when far (get props (->kw far)))]
    (or (when-not (:over spec)
          (problem type nm ":over fehlt — eine Aggregation nennt den Link, über den sie geht"
                   :over))
        (when (and (seq links) (:over spec) (nil? far))
          (problem type nm (str (name type) " hat keine Link-Seite " (name (:over spec))
                                " (Seiten: "
                                (str/join ", " (sort (map (comp name :api-name)
                                                          (ls/table-sides links type))))
                                ")")
                   :over))
        (when-let [m (rollup-arity-error spec)] (problem type nm m :agg))
        (some (fn [k]
                (let [v (->kw (get spec k))]
                  (when (and v native (not (contains? native v)))
                    (problem type nm (str (name k) ": " (name v) " ist keine Eigenschaft von "
                                          (name far))
                             k))))
              [:of :by :pick]))))

(defn- explain-column-math
  [module type nm spec {:keys [props]}]
  (let [expr   (:expression spec)
        native (get props (->kw type))
        maths  (set (names-of-kind module type :column-math))
        aggr   (set (names-of-kind module type :aggregation))
        refs   (expr-refs expr)]
    (or (when-not (vector? expr)
          (problem type nm
                   ":expression fehlt — eine Spaltenrechnung ist ein Ausdruck wie [:div :a :b]"
                   :expression))
        (when-let [bad (seq (sort (remove #(or (math-ops %) (= :get %)) (expr-ops expr))))]
          (problem type nm
                   (if (some q/link-op-names bad)
                     (str "eine Spaltenrechnung verrechnet Werte EINES Objekts — "
                          (str/join ", " (map name bad))
                          " geht über einen Link; dafür ist die Art „Aggregation“ da")
                     (str "unerlaubte Rechenart: " (str/join ", " (map name bad))
                          " — erlaubt sind " (str/join ", " (sort (map name math-ops)))))
                   :expression))
        (when (empty? refs)
          (problem type nm
                   "die Spaltenrechnung liest keine Eigenschaft — sie wäre eine Konstante"
                   :expression))
        (when (contains? refs (->kw nm))
          (problem type nm "eine Spaltenrechnung kann sich nicht selbst lesen" :expression))
        (when-let [bad (seq (sort (filter maths refs)))]
          (problem type nm
                   (str "eine Spaltenrechnung darf keine andere Spaltenrechnung lesen ("
                        (str/join ", " (map name bad))
                        ") — erlaubt sind die eigenen Eigenschaften des Objekttyps"
                        " und Aggregationen dieses Moduls")
                   :expression))
        (when native
          (when-let [bad (seq (sort (remove #(or (contains? native %) (aggr %)) refs)))]
            (problem type nm
                     (str "unbekannte Eigenschaft: " (str/join ", " (map name bad))
                          " — " (name type) " hat sie nicht, und dieses Modul leitet sie nicht ab")
                     :expression))))))

(defn explain
  ([module] (explain module nil))
  ([module {:keys [props object-types] :as ctx}]
   (vec
    (for [[type nm spec] (all-properties module)
          :let [p (or (when (and (seq object-types)
                                 (not (contains? (set object-types) (->kw type))))
                        (problem type nm
                                 (str (name type) " ist keiner der Objekttypen dieser App"
                                      " — die abgeleitete Eigenschaft würde nie gelesen")
                                 nil))
                      (when-let [native (get props (->kw type))]
                        (when (contains? native (->kw nm))
                          (problem type nm
                                   (str (name type) " hat bereits eine Eigenschaft " (name nm)
                                        " — eine abgeleitete Eigenschaft überschreibt keine echte")
                                   nil)))
                      (case (kind-of spec)
                        :aggregation (explain-aggregation module type nm spec ctx)
                        :column-math (explain-column-math module type nm spec ctx)
                        (problem type nm
                                 (str "unbekannte Art — :kind ist :aggregation (dann :over/:agg)"
                                      " oder :column-math (dann :expression)")
                                 :kind)))]
          :when p]
      p))))

