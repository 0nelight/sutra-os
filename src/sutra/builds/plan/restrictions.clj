(ns sutra.builds.plan.restrictions
  (:require [clojure.string :as str]
            [sutra.types.value-types :as vt]
            [sutra.foundation.encoding :as enc]
            [sutra.catalog.constraints :as cstr]
            [sutra.catalog.values :as values]
            [sutra.catalog.lookup :as lookup]))

(set! *warn-on-reflection* true)

(def max-string-bytes
  (* 12 1024 1024))

(def max-array-elements
  100000)

(def ^:private report-cap
  20)

(def ^:private nondeterministic-pk-types
  #{:double :float :decimal :media-reference :attachment})

(defn- collection-type? [t]
  (and (vector? t) (contains? #{:array :struct :vector} (first t))))

(defn pk-type-problem
  [schema obj-type primary-key]
  (let [t (get (lookup/attr-type-map schema obj-type) primary-key)]
    (cond
      (nil? t) nil

      (or (contains? nondeterministic-pk-types t)
          (and (vector? t) (= :decimal (first t)))
          (collection-type? t))
      {:violation   :nondeterministic-primary-key
       :primary-key primary-key
       :type        t
       :why (str "primary key " (pr-str primary-key) " is declared " (pr-str t)
                 " — a floating-point, collection or reference type is not stable across"
                 " a rebuild. Sutra-OS derives the object id from the PK, so a PK that"
                 " drifts mints a NEW object and orphans every link edge pointing at"
                 " the old one. Use a string/integer/date key.")})))

(defn- pk-value-violation
  [primary-key pk]
  (when (or (instance? Double pk) (instance? Float pk) (decimal? pk))
    {:violation :nondeterministic-primary-key
     :primary-key primary-key
     :pk pk
     :why (str "primary-key VALUE " (pr-str pk) " is floating-point — it does not"
               " round-trip identically across a rebuild, so the derived object id"
               " changes and every link edge pointing at the old id is orphaned."
               " Key on a string or integer column instead.")}))

(defn- non-finite? [v]
  (and (number? v)
       (or (and (instance? Double v) (or (Double/isNaN v) (Double/isInfinite v)))
           (and (instance? Float v) (or (Float/isNaN v) (Float/isInfinite v))))))

(defn- string-bytes-over?
  [^String s]
  (and (> (.length s) (quot max-string-bytes 4))
       (> (alength (.getBytes s "UTF-8")) max-string-bytes)))

(defn- value-violation
  [k t v]
  (cond
    (nil? v) nil

    (non-finite? v)
    {:violation :non-finite-number :attribute k :value (str v)}

    (and (string? v) (= "" v))
    {:violation :empty-string :attribute k}

    (and (string? v) (string-bytes-over? v))
    {:violation :string-too-large :attribute k
     :bytes (alength (.getBytes ^String v "UTF-8")) :limit max-string-bytes}

    (and (sequential? v) (> (count v) max-array-elements))
    {:violation :array-too-large :attribute k
     :elements (count v) :limit max-array-elements}

    (and (some? t) (not (values/value-ok? t v)))
    {:violation :type-mismatch :attribute k :expected t
     :value (enc/elide 120 (pr-str v))}))

(defn duplicate-pks
  [rows]
  (let [freqs (frequencies (map :pk rows))]
    (into [] (comp (map :pk) (distinct) (filter #(> (long (get freqs % 0)) 1))) rows)))

(defn violations
  [{:keys [schema object-type primary-key]} rows]
  (let [types (lookup/attr-type-map schema object-type)
        pk-t  (pk-type-problem schema object-type primary-key)
        cons  (vt/resolve-value-types (:attribute-list (lookup/get-type schema :object object-type))
                                               (:value-types schema))
        per-row (for [[i {:keys [pk values tombstone?]}] (map-indexed vector rows)
                      v (cond-> (into []
                                      (keep (fn [[k val]] (value-violation k (get types k) val)))
                                      values)
                          (not tombstone?)
                          (into (map (fn [{:keys [attribute message]}]
                                       {:violation :constraint :attribute attribute
                                        :message message}))
                                (cstr/violations cons values {:require-complete? false}))
                          (nil? pk) (conj {:violation :missing-primary-key
                                           :primary-key primary-key})
                          (and (some? pk) (nil? pk-t))
                          (into (keep identity [(pk-value-violation primary-key pk)])))]
                  (assoc v :row i))
        dups  (for [pk (duplicate-pks rows)]
                {:violation :duplicate-primary-key :primary-key primary-key :pk pk})]
    (vec (concat (when pk-t [pk-t]) per-row dups))))

(def violation-kinds
  {:nondeterministic-primary-key
   {:detail (fn [{:keys [pk why]}] (str (when pk (str "pk=" (pr-str pk) " ")) why))}
   :missing-primary-key
   {:detail (fn [{:keys [primary-key]}] (str "no value for " (pr-str primary-key)))}
   :duplicate-primary-key
   {:detail (fn [{:keys [pk]}] (str "pk=" (pr-str pk)))}
   :non-finite-number
   {:detail (fn [{:keys [value]}] (str "value " value))}
   :empty-string
   {:detail (constantly nil)}
   :string-too-large
   {:detail (fn [{:keys [bytes limit]}] (str bytes " bytes, limit " limit))}
   :array-too-large
   {:detail (fn [{:keys [elements limit]}] (str elements " elements, limit " limit))}
   :type-mismatch
   {:detail (fn [{:keys [expected value]}] (str "expected " (pr-str expected) ", got " value))}
   :constraint
   {:detail :message}})

(defn describe
  [{:keys [violation attribute row] :as v}]
  (let [kind (or (violation-kinds violation)
                 (throw (ex-info (str "unknown restriction violation " (pr-str violation))
                                 {:violation violation
                                  :known (vec (sort (keys violation-kinds)))})))
        d    ((:detail kind) v)]
    (str "· " (name violation)
         (when attribute (str " on " (pr-str attribute)))
         (when row (str " (row " row ")"))
         (when (seq d) (str " — " d)))))

(defn check-rows!
  [{:keys [source object-type] :as ctx} rows]
  (let [vs (violations ctx rows)]
    (when (seq vs)
      (throw (ex-info
              (str "Data restrictions rejected " (count vs) " violation(s) importing "
                   (pr-str object-type)
                   (when source (str " from " (pr-str source))) ":\n"
                   (str/join "\n" (map describe (take report-cap vs)))
                   (when (> (count vs) report-cap)
                     (str "\n… +" (- (count vs) report-cap) " more"))
                   "\nFix the source data or widen the declared type — a source is"
                   " never exempted from the gate.")
              {:object-type object-type
               :source      source
               :violations  (vec (take 200 vs))
               :violation-count (count vs)}))))
  rows)

(defn check-duplicate-pks!
  [rows {:keys [what hint]}]
  (let [dups (duplicate-pks rows)]
    (when (seq dups)
      (throw (ex-info
              (str (or what "Input") " produced " (count dups)
                   " duplicate primary key(s) — each key must appear at most once."
                   (when hint (str " " hint))
                   " Offending PK(s): " (pr-str (vec (take 10 dups)))
                   (when (> (count dups) 10)
                     (str " … +" (- (count dups) 10) " more")))
              {:duplicate-pks (vec (take 100 dups))
               :duplicate-count (count dups)}))))
  rows)
