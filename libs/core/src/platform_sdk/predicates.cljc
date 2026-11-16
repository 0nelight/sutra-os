(ns platform-sdk.predicates
  (:refer-clojure :exclude [= not= < > <= >= nil?])
  (:require [platform-sdk.errors :as err]
            [sutra.types.query :as q]))

(def ops q/surface-ops)

(defn deep-merge
  [a b]
  (reduce-kv (fn [acc k v]
               (let [cur (get acc k)]
                 (assoc acc k (if (and (map? cur) (map? v)) (deep-merge cur v) v))))
             (or a {})
             (or b {})))

(defn all [& preds] (reduce deep-merge {} preds))

(defn- op
  [o attr v]
  (when-not (contains? ops o)
    (err/raise! :usage (str (pr-str o) " is not a surface operator — known: "
                            (pr-str (vec (sort ops))))
                {:op o :known ops}))
  (when-not (keyword? attr)
    (err/raise! :usage (str "a predicate names an attribute (a keyword), got " (pr-str attr))
                {:attr attr}))
  {attr {o v}})

(defn =           [attr v] (op := attr v))
(defn not=        [attr v] (op :not= attr v))
(defn <           [attr v] (op :< attr v))
(defn >           [attr v] (op :> attr v))
(defn <=          [attr v] (op :<= attr v))
(defn >=          [attr v] (op :>= attr v))
(defn starts-with [attr v] (op :starts-with attr v))
(defn contains    [attr v] (op :contains attr v))

(defn nil?
  ([attr] (nil? attr true))
  ([attr unset?] (op :nil? attr unset?)))

(defn between
  [attr lo hi]
  (all (>= attr lo) (<= attr hi)))

(defn linked
  [side & preds]
  (when-not (keyword? side)
    (err/raise! :usage (str "a link sub-filter names a link side by its API name (a keyword), got "
                            (pr-str side))
                {:link side}))
  {side (apply all preds)})
