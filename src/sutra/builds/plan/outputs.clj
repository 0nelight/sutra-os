(ns sutra.builds.plan.outputs
  (:require [clojure.string :as str]
            [sutra.types.attribute :as attribute]
            [sutra.types.kinds :as kinds]))

(set! *warn-on-reflection* true)

(defn output-ref
  [o]
  (when-let [{:keys [at names]} (get-in kinds/kinds [(:kind o) :output-ref])]
    [names (get o at)]))

(defn external-key [k] (str "external/" k))

(defn external-key? [k] (str/starts-with? (str k) "external/"))

(defn attr-names
  [obj-spec]
  (:names (attribute/of obj-spec)))

(defn identity-keys-for
  [schema object-type]
  (let [spec  (get-in schema [:object-types object-type])
        attrs (set (attr-names spec))]
    (vec (filter attrs (:match-keys spec)))))

(defn resolved-object-type?
  [schema object-type]
  (boolean (and (:resolve-entity (get-in schema [:object-types object-type]))
                (seq (identity-keys-for schema object-type)))))
