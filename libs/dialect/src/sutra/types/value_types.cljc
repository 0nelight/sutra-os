(ns sutra.types.value-types
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.types.coltype :as ctype]
            [sutra.types.table :as tbl]))

(def value-types
  {:string  {:class :text :coltype (ctype/col-type :text)
             :coerce :string :auto-cast? false :authorable? true}
   :text    {:class :text :coltype (ctype/col-type :text)
             :coerce :string :auto-cast? false :authorable? true}

   :number  {:class :number :coltype (ctype/col-type :decimal {:precision 38 :scale 10})
             :coerce :exact :auto-cast? true :authorable? false}
   :decimal {:class :number :coltype (ctype/col-type :decimal {:precision 38 :scale 10})
             :coerce :exact :auto-cast? true :authorable? true}
   :byte    {:class :number :coltype (ctype/col-type :tinyint)
             :coerce :number :auto-cast? true :authorable? true}
   :short   {:class :number :coltype (ctype/col-type :smallint)
             :coerce :number :auto-cast? true :authorable? true}
   :integer {:class :number :coltype (ctype/col-type :int)
             :coerce :number :auto-cast? true :authorable? true}
   :long    {:class :number :coltype (ctype/col-type :bigint)
             :coerce :number :auto-cast? true :authorable? true}
   :float   {:class :number :coltype (ctype/col-type :float)
             :coerce :number :auto-cast? true :authorable? true}
   :double  {:class :number :coltype (ctype/col-type :double)
             :coerce :number :auto-cast? true :authorable? true}

   :boolean {:class :boolean :coltype (ctype/col-type :boolean)
             :coerce :boolean :auto-cast? true :authorable? true}

   :keyword {:class :keyword :coltype (ctype/col-type :varchar {:length 255})
             :authorable? true}
   :uuid    {:class :uuid :coltype (ctype/col-type :varchar {:length 36})
             :authorable? true}

   :instant   {:class :temporal :coltype (ctype/col-type :timestamp {:precision 6})
               :coerce :instant :auto-cast? true :authorable? true}
   :timestamp {:class :temporal :coltype (ctype/col-type :timestamp {:precision 6})
               :coerce :instant :auto-cast? true :authorable? true}
   :date      {:class :temporal :coltype (ctype/col-type :date)
               :coerce :date :auto-cast? true :authorable? true}

   :geoshape {:class :geo :coltype (ctype/col-type :text)          :authorable? true}
   :geopoint {:class :geo :coltype (ctype/col-type :varchar {:length 255}) :authorable? true}
   :geohash  {:class :geo :coltype (ctype/col-type :varchar {:length 32})  :authorable? true}

   :media-reference {:class :media :coltype (ctype/col-type :varchar {:length 64})
                     :authorable? true}
   :attachment      {:class :media :coltype (ctype/col-type :varchar {:length 64})
                     :authorable? true}
   :timeseries      {:class :series :coltype (ctype/col-type :varchar {:length 255})
                     :authorable? true}

   :vector  {:authorable? true}
   :marking {:authorable? true}
   :cipher  {:authorable? true}})

(def storable-types
  (tbl/where value-types :coltype))

(def ^:private of-kind
  {:boolean :boolean :tinyint :byte :smallint :short :int :integer :bigint :long
   :float :double :double :double :decimal :decimal
   :varchar :string :text :string :json :string
   :date :date :timestamp :timestamp})

(defn of-column-kind
  [kind]
  (get of-kind kind :string))

(def authorable-types
  (tbl/where value-types :authorable?))

(def numeric-types
  (tbl/where value-types #(= :number (:class %))))

(def auto-cast-types
  (tbl/where value-types :auto-cast?))

(defn base
  [t]
  (if (and (vector? t) (= :decimal (first t))) :decimal t))

(defn numeric?
  [t]
  (contains? numeric-types (if (and (sequential? t) (seq t)) (first t) t)))

(def media-types
  (tbl/where value-types #(= :media (:class %))))

(defn media?
  [t]
  (contains? media-types
             (if (and (sequential? t) (seq t) (= :array (first t))) (second t) t)))

(defn class-of
  [t]
  (get-in value-types [(base t) :class]))

(defn coltype-of
  [t]
  (get-in value-types [t :coltype]))

(def ^:private json-value-type-heads
  #{:array :struct :vector})

(defn json-attribute?
  [value-type]
  (boolean (and (vector? value-type) (json-value-type-heads (first value-type)))))

(defn col-type-of
  [value-type]
  (or (when (and (vector? value-type) (= :decimal (first value-type)))
        (let [[_ p s] value-type] (ctype/col-type :decimal {:precision p :scale s})))
      (when (json-attribute? value-type) (ctype/col-type :json))
      (coltype-of value-type)
      (anomaly/coded! :refusal.dialect/no-column-type :incorrect
        "No column-type mapping for value-type {value-type:pr}"
        {:value-type value-type
         :known storable-types})))

(defn storable?
  [value-type]
  (try (some? (col-type-of value-type))
       (catch #?(:clj Throwable :cljs :default) _ false)))

(defn same-storage?
  [a b]
  (try (= (col-type-of a) (col-type-of b))
       (catch #?(:clj Throwable :cljs :default) _ false)))

(defn coerce-of
  [t]
  (get-in value-types [(base t) :coerce]))

(defn auto-cast?
  [t]
  (contains? auto-cast-types (base t)))

(defn- declared-by-name
  [value-types]
  (persistent!
   (reduce-kv (fn [m k v] (assoc! m (or (:value-type-name v) k) v))
              (transient {}) value-types)))

(defn resolve-value-types
  [attribute-list value-types]
  (if (empty? value-types)
    attribute-list
    (let [by-name (declared-by-name value-types)]
      (mapv (fn [{:keys [opts] :as attr}]
              (if-let [vt (some-> (:value-type opts) by-name)]
                (update attr :opts
                        (fn [o]
                          (cond-> o
                            (not-empty (:constraints vt))
                            (assoc :constraints (merge (:constraints vt) (:constraints o)))
                            (not-empty (:format vt))
                            (assoc :format (merge (:format vt) (:format o)))
                            (and (seq (:description vt)) (not (seq (:description o))))
                            (assoc :description (:description vt))
                            (and (seq (:render-hints vt)) (not (seq (:render-hints o))))
                            (assoc :render-hints (vec (:render-hints vt))))))
                attr))
            attribute-list))))
