(ns platform-sdk.aggregate
  (:refer-clojure :exclude [count min max])
  (:require [sutra.types.aggregate :as agg]))

(def fns agg/agg-fns)

(def units agg/bucket-units)

(defn count
  ([] {:fn :count})
  ([attr] {:fn :count :attr attr}))

(defn sum         [attr] {:fn :sum :attr attr})
(defn avg         [attr] {:fn :avg :attr attr})
(defn min         [attr] {:fn :min :attr attr})
(defn max         [attr] {:fn :max :attr attr})
(defn cardinality [attr] {:fn :cardinality :attr attr})

(defn as [spec result-key] (assoc spec :name result-key))

(defn by     [attr] attr)
(defn bucket [attr unit] [:bucket attr unit])
(defn width  [attr n] [:width attr n])
(defn origin [] [:origin])

(defn link
  ([side] [:link side])
  ([side title-attr] [:link side title-attr]))

(defn normalize [aggs group-by] (agg/normalize {:aggs aggs :group-by group-by}))

(defn result-keys [aggs group-by] (agg/result-keys (normalize aggs group-by)))

(defn result-key-groups [aggs group-by] (agg/result-key-groups (normalize aggs group-by)))
