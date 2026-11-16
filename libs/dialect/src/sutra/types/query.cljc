(ns sutra.types.query
  (:require [clojure.set :as set]
            [sutra.ref.geo :as geo]
            [sutra.types.table :as tbl]))

(def predicate-ops
  {:and {:shape :junction :identity true
         :doc  "Conjunction. [:and & nodes]"}
   :or  {:shape :junction :identity false
         :doc  "Disjunction. [:or & nodes] — the natural shape for 'match this
                text in ANY of a type's columns', i.e. free-text search."}

   :id-in {:shape :id-set
           :doc "[:id-in <coll-of-ids>]. Distinct from a comparison
                 because it addresses the object's identity, not a property.
                 An EMPTY coll is the empty set and matches NOTHING — never
                 'no filter', which is how an unscoped read hides inside a
                 scoped-looking call."}

   :having-all {:shape :having-all :index-only? true
                :doc "[:having-all <as-name> <coll>] — objects linked to ALL of
                      `coll` via the :include entry named `as-name`: set(coll) ⊆
                      linked ids, so an empty coll holds for every object.
                      Needs a link index, which MemSubstrate has not."}

   :=    {:shape :comparison :class :equality :test :eq  :sql "="  :surface? true}
   :not= {:shape :comparison :class :equality :test :ne  :sql "<>" :surface? true}
   :<    {:shape :comparison :class :ordering :test :lt  :sql "<"  :surface? true}
   :>    {:shape :comparison :class :ordering :test :gt  :sql ">"  :surface? true}
   :<=   {:shape :comparison :class :ordering :test :lte :sql "<=" :surface? true}
   :>=   {:shape :comparison :class :ordering :test :gte :sql ">=" :surface? true}

   :nil? {:shape :comparison :class :nullity :test :nil :surface? true
          :doc "[:nil? <field> <boolean>].
                The VALUE is the answer wanted, not a value to compare against:
                `true` selects rows where the property is unset, `false` rows
                where it is set. It is its own op because `[:= field nil]`
                lowers to `col = NULL`, which SQL never satisfies — so an unset
                property was unaskable, and the read came back empty rather
                than refused."}

   :starts-with {:shape :comparison :class :text :test :prefix
                 :match :prefix :surface? true}
   :contains    {:shape :comparison :class :text :test :substring
                 :match :substring :surface? true}
   :contains-ci {:shape :comparison :class :text :test :substring
                 :match :substring :fold-case? true :surface? true}

   :has {:shape :comparison :class :membership :test :has :surface? true
         :doc "[:has <array-field> <value>] — the array holds value as one of its members.
               Never a substring: \"ab\" is not in [\"abc\"]."}

   :matches-tokens-in {:shape :tokens
                       :doc "[:matches-tokens-in [<attr> …] <text>] — :matches-tokens over exactly
                             these attrs (App Studio \"Search items by: Specific properties\"),
                             where :matches-tokens may read the type's whole document."}

   :matches-tokens {:shape :tokens
                    :doc "[:matches-tokens [<attr> …] <text>] — text search
                          (object-finder/understanding-text-search.md): the attrs read as
                          one document of tokens, cut at whitespace and punctuation, case
                          folded; every word of text is a token or the start of one, and
                          quoted words must stand next to each other in order. `rown` does
                          not match `brown`: that is :contains-ci."}

   :within-distance {:shape :geo :surface? true
                     :doc "[:within-distance <field> {:center point :distance d :unit u}] —
                           great-circle distance ≤ d. <field> is
                           a geopoint attribute (\"lat,lon\") or [lat-attr lon-attr]; point
                           [lat lon], {:lat :lon} or \"lat,lon\"; :unit :km (default) :m
                           :mi :nmi :ft :yd. See sutra.ref.geo."}
   :within-bounding-box {:shape :geo :surface? true
                         :doc "[:within-bounding-box <field> {:top-left point :bottom-right
                               point}]; a top-left east of the
                               bottom-right crosses the antimeridian."}})

(def comparison-ops
  (tbl/where predicate-ops #(= :comparison (:shape %))))

(def text-ops
  (tbl/where predicate-ops #(= :text (:class %))))

(def surface-ops
  (tbl/where predicate-ops :surface?))

(def link-ops
  {:connected?        {:slots [:over :pred]        :expr-slots #{:pred}}
   :connected-all?    {:slots [:over :of :values]  :expr-slots #{:values}}
   :connected-count   {:slots [:over]              :agg :count   :where? true}
   :connected-sum     {:slots [:over :of]          :agg :sum     :where? true}
   :connected-max     {:slots [:over :of]          :agg :max     :where? true}
   :connected-min     {:slots [:over :of]          :agg :min     :where? true}
   :connected-avg     {:slots [:over :of]          :agg :avg     :where? true}
   :connected-arg-max {:slots [:over :by :pick]    :agg :arg-max :where? true}
   :connected-one     {:slots [:over :pick]        :agg :one}})

(def link-op-names (tbl/known link-ops))

(def ^:private agg-of-op (tbl/column link-ops :agg))

(def op-of-agg (set/map-invert agg-of-op))

(def aggs (vec (keep :agg (map link-ops (sort link-op-names)))))

(def expr-arg-positions
  (into {} (for [[op {:keys [slots expr-slots]}] link-ops
                 :when (seq expr-slots)]
             [op (into #{} (keep-indexed (fn [i s] (when (expr-slots s) i))) slots)])))

(defn link-slots
  [op]
  (:slots (link-ops op)))

(defn link-takes-where?
  [op]
  (boolean (:where? (link-ops op))))

(defn link-args
  [op args]
  (let [{:keys [slots where?]} (link-ops op)
        named (zipmap slots args)]
    (cond-> named
      (and where? (> (count args) (count slots)))
      (assoc :where (nth args (count slots))))))

(defn connected-nodes
  [expr]
  (cond
    (and (vector? expr) (link-ops (first expr)))
    (cons expr (mapcat connected-nodes (rest expr)))
    (vector? expr) (mapcat connected-nodes expr)
    :else nil))

(defn like-pattern
  [op escaped]
  (case (:match (predicate-ops op))
    :prefix    (str escaped "%")
    :substring (str "%" escaped "%")))

(defn fold-case?
  [op]
  (boolean (:fold-case? (predicate-ops op))))

(defn explain-predicate
  [pred]
  (cond
    (nil? pred) nil

    (not (vector? pred))
    {:problem :not-a-node :node pred
     :message "a predicate-IR node is a vector [op & args]"}

    :else
    (let [[op & args]      pred
          {:keys [shape]}  (predicate-ops op)
          arity            (count args)
          bad              (fn [msg] {:problem :bad-arity :op op :node pred
                                      :message msg})]
      (case shape
        nil (assoc (tbl/unknown :unknown-op "predicate op" predicate-ops op) :op op :node pred)

        :junction   (some explain-predicate args)

        :comparison (when-not (= 2 arity)
                      (bad "a comparison is [op field value]"))

        :tokens     (when-not (and (= 2 arity) (sequential? (first args)))
                      (bad "[:matches-tokens [<attr> …] <text>]"))

        :id-set     (when-not (and (= 1 arity) (coll? (first args)))
                      (bad "[:id-in <coll-of-ids>]"))

        :having-all (when-not (and (= 2 arity) (coll? (second args)))
                      (bad "[:having-all <as-name> <coll>]"))

        :geo        (if (= 2 arity)
                      (when-let [m (geo/explain op (first args) (second args))]
                        {:problem :bad-geo :op op :node pred :message m})
                      (bad (str "[" op " <field> <map>]")))))))

(defn valid-predicate? [pred]
  (nil? (explain-predicate pred)))

(defn geo? [op] (= :geo (:shape (predicate-ops op))))

(def relevance :_relevance)

(def title :_title)

(defn tokens? [op] (= :tokens (:shape (predicate-ops op))))

(defn nullity?
  [op]
  (= :nullity (:class (predicate-ops op))))

