(ns sutra.types.shape
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.types.entities :as ents]
            [sutra.types.versions :as vers]))

(def entity-fields ents/fields)

(defn object-view? [spec]
  (some? (:form-factor spec)))

(defn present? [rule v]
  (case rule
    :seq    (boolean (seq v))
    :truthy (boolean v)
    :map    (map? v)
    :number (number? v)
    (some? v)))

(defn- carry [rule v]
  (case rule
    :kw          (keyword v)
    :vec         (vec v)
    :kws         (mapv keyword v)
    :kw-map      (when (map? v) (into {} (map (fn [[k val]] [(->kw k) (->kw val)])) v))
    :bool        (boolean v)
    :str         (str v)
    :long        (long v)
    :sorted-strs (vec (sort (map str v)))
    v))

(defn project [section spec base {:keys [from to coerce?] :or {coerce? true}}]
  (reduce-kv
   (fn [acc author {:keys [as present coerce in custom author-only default]}]
     (let [canonical (or as author)
           src-key   (if (= :author from) author canonical)
           dst-key   (if (= :author to)   author canonical)
           inbound?  (= :canonical to)
           raw       (get spec src-key)
           v         (if (some? raw) raw default)]
       (if (and (not custom) (not author-only) (present? present v))
         (assoc acc dst-key (cond-> v
                              coerce?                   (->> (carry coerce))
                              (and coerce? in inbound?) (->> (carry in))))
         acc)))
   base
   (get entity-fields section)))

(defn ->canonical [section spec base]
  (project section spec base {:from :author :to :canonical}))

(defn ->author [section spec base]
  (project section spec base {:from :canonical :to :author}))

(defn authored-entry
  [section spec]
  (vers/authored section (->author section spec {})))

(def identity-axis (ents/axis :object-types :identity))

(def presentation-axis (ents/axis :object-types :presentation))

(def binding-axis (ents/axis :object-types :binding))

(defn reading-order
  [m]
  (let [placed (zipmap (into presentation-axis (sort ents/placement-fields)) (range))
        rank   (fn [k] (cond (placed k)        [0 (placed k)]
                             (coll? (get m k)) [2 0]
                             :else             [1 0]))]
    (into (sorted-map-by #(compare [(rank %1) (str %1)] [(rank %2) (str %2)])) m)))

(defn- fold-empty [v]
  (if (or (coll? v) (string? v)) (not-empty v) v))

(def ^:private compare-rules
  (into {}
        (for [[section fs] entity-fields]
          [section (into {}
                         (mapcat (fn [[k {:keys [as compare]}]]
                                   (cond-> [[k compare]] as (conj [as compare]))))
                         fs)])))

(defn fold [section field v]
  (case (get-in compare-rules [section field])
    :bool                    (when v true)
    (:empty-folded :lenient) (fold-empty v)
    v))

(defn- field-satisfied? [rule want have]
  (case rule
    :lenient      (or (nil? want) (= want have))
    :bool         (= (boolean want) (boolean have))
    :empty-folded (= (fold-empty want) (fold-empty have))
    (= want have)))

(defn scalars-satisfied? [section args canon]
  (let [author-canon (->author section canon {})
        strict       (when (:whole-declaration? args)
                       (set (concat identity-axis presentation-axis)))]
    (every? (fn [[author-key {:keys [custom author-only compare]}]]
              (or custom author-only
                  (field-satisfied? (if (and strict (strict author-key) (= :lenient compare))
                                      :empty-folded
                                      compare)
                                    (get args author-key)
                                    (get author-canon author-key))))
            (get entity-fields section))))
