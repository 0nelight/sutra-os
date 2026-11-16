(ns sutra.ref.term
  (:require [clojure.string :as str]
            [sutra.ref.value :as v]))

(def scalar-classes
  {:string :text :text :text :keyword :text
   :byte :integral :short :integral :integer :integral :long :integral
   :number :fractional :decimal :fractional :float :fractional :double :fractional
   :boolean :boolean
   :uuid :uuid
   :date :date
   :instant :instant :timestamp :instant
   :geoshape :opaque :geopoint :opaque :geohash :opaque
   :media-reference :opaque :attachment :opaque :timeseries :opaque
   :marking :opaque :cipher :opaque})

(defn types-of
  [& classes]
  (let [cs (set classes)]
    (into #{} (keep (fn [[t c]] (when (cs c) t))) scalar-classes)))

(defn- kw [x] (cond (keyword? x) x (string? x) (keyword x)))

(defn base
  ([t] (base t nil))
  ([t ref-types]
   (if (sequential? t)
     (let [[h a b] t]
       (case (kw h)
         :decimal [:decimal a b]
         :vector  [:vector a]
         :array   [:array (base a ref-types)]
         :struct  [:struct (into {} (map (fn [[k ft]] [(kw k) (base ft ref-types)])) a)]
         [:named t]))
     (let [k (kw t)]
       (cond
         (nil? k)                     [:named t]
         (contains? scalar-classes k) [:scalar (scalar-classes k) k]
         (= :vector k)                [:vector nil]
         (= :object-set k)            [:object-set]
         :else (if-let [ts (some-> ref-types (apply [t]) seq vec)]
                 [:ref ts]
                 [:named t]))))))

(defn of
  ([spec] (of spec nil))
  ([spec ref-types]
   (let [b (base (:type spec) ref-types)]
     (if (true? (:multiple spec)) [:list b] b))))

(defn- ok [x] {:value x :problems []})

(defn- kind-name [x]
  #?(:clj (.getName (class x)) :cljs (goog/typeOf x)))

(defn- no
  [t x kept]
  {:value kept :problems [{:expected t :got (kind-name x)}]})

(defn- all [rs]
  {:value (mapv :value rs) :problems (into [] (mapcat :problems) rs)})

(defn- many? [x] (or (sequential? x) (set? x)))

(defn- temporal? [x]
  #?(:clj  (or (instance? java.time.temporal.TemporalAccessor x) (instance? java.util.Date x))
     :cljs (instance? js/Date x)))

(defn- temporal-text [x]
  #?(:clj  (condp instance? x
             java.sql.Date      (str (.toLocalDate ^java.sql.Date x))
             java.sql.Timestamp (str (.toInstant ^java.sql.Timestamp x))
             java.util.Date     (str (.toInstant ^java.util.Date x))
             (str x))
     :cljs (.toISOString x)))

(defn- data
  [x]
  (cond
    (nil? x)                                  (ok nil)
    (or (string? x) (number? x) (boolean? x)) (ok x)
    (keyword? x)                              (ok (name x))
    (uuid? x)                                 (ok (str x))
    (temporal? x)                             (ok (temporal-text x))
    (v/handle? x)                             (ok (str (:id x)))
    (map? x)                                  (let [r (all (map data (vals x)))]
                                                (assoc r :value (zipmap (keys x) (:value r))))
    (many? x)                                 (all (map data x))
    :else                                     (no [:data] x nil)))

(defn- row? [x] (and (map? x) (some? (:id x))))

(defn- render-text
  [x]
  (cond
    (string? x)                               (ok x)
    (row? x)                                  (ok (str (:id x)))
    (keyword? x)                              (ok (name x))
    (or (number? x) (boolean? x) (uuid? x))   (ok (str x))
    :else                                     (data x)))

(defn render
  [t x]
  (if (nil? x)
    (ok nil)
    (case (first t)
      :scalar        (if (= :text (second t)) (render-text x) (data x))
      (:decimal :vector :named) (data x)
      (:list :array) (all (map #(render (second t) %) (if (many? x) x [x])))
      :struct        (if (and (map? x) (not (v/handle? x)))
                       (let [fs (second t)
                             r  (all (map (fn [[k y]] (render (get fs k [:named nil]) y)) x))]
                         (assoc r :value (zipmap (keys x) (:value r))))
                       (data x))
      :ref           (cond
                       (map? x)        (if (some? (:id x)) (ok (str (:id x))) (no t x nil))
                       (or (string? x) (uuid? x) (number? x)) (ok (str x))
                       :else           (no t x nil))
      :object-set    (if (many? x) (all (map #(render [:ref []] %) x)) (data x)))))

(defn references
  [t x]
  (case (first t)
    :ref                    (if (row? x) [x] [])
    (:list :array)          (into [] (mapcat #(references (second t) %)) (if (many? x) x [x]))
    :object-set             (if (many? x) (filterv row? x) [])
    (:scalar :decimal :vector :struct :named) []))

(defn- read-text [t class s]
  (let [s' (str/trim s)]
    (case class
      :integral   (if-some [n (parse-long s')] (ok n) (no t s s))
      :fractional (if-some [n (parse-double s')] (ok n) (no t s s))
      :boolean    (case (str/lower-case s')
                    ("true" "wahr" "ja" "1")      (ok true)
                    ("false" "falsch" "nein" "0") (ok false)
                    (no t s s))
      (:text :uuid :date :instant :opaque) (ok s))))

(defn- parse-scalar [t class w]
  (cond
    (string? w)                        (read-text t class w)
    (and (= :text class) (not (coll? w))) (ok (str w))
    :else                              (ok w)))

(defn parse
  [t w]
  (if (nil? w)
    (ok nil)
    (case (first t)
      :scalar        (parse-scalar t (second t) w)
      :decimal       (parse-scalar t :fractional w)
      (:list :array) (if (sequential? w) (all (map #(parse (second t) %) w)) (parse (second t) w))
      (:vector :struct :ref :object-set :named) (ok w))))

(defn update-at
  [t classes f x]
  (if (nil? x)
    x
    (case (first t)
      :scalar        (if (contains? classes (second t)) (f x) x)
      :decimal       (if (contains? classes :fractional) (f x) x)
      (:list :array) (if (sequential? x)
                       (mapv #(update-at (second t) classes f %) x)
                       (update-at (second t) classes f x))
      :struct        (if (map? x)
                       (reduce-kv (fn [m k y]
                                    (assoc m k (update-at (get (second t) k [:named nil])
                                                          classes f y)))
                                  x x)
                       x)
      (:vector :ref :object-set :named) x)))
