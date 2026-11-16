(ns sutra.objects.api
  (:require [sutra.objects.explorer :as explorer]
            [sutra.objects.history :as history]
            [sutra.objects.knn :as knn]
            [sutra.objects.reads :as reads]
            [sutra.objects.views :as views]))

(set! *warn-on-reflection* true)

(defn edit-history-page [store T opts] (history/page store T opts))

(defn edit-timeline [store schema T opts] (history/timeline store schema T opts))

(defn edit-history-row [e] (history/row e))

(defn read-layout [store object-type] (explorer/read-layout store object-type))

(defn readable-type! [store schema ot] (explorer/readable-type! store schema ot))

(defn upsert-layout! [store opts] (explorer/upsert-layout! store opts))

(defn nearest-paged [query-vec fetch opts] (knn/nearest-paged query-vec fetch opts))

(defn aggregate-matching
  [store schema obj-type query agg] (reads/aggregate-matching store schema obj-type query agg))

(defn app-snapshot [store schema] (reads/app-snapshot store schema))

(defn count-matching
  [store schema obj-type query] (reads/count-matching store schema obj-type query))

(defn links-of-type
  ([store schema conn-type] (reads/links-of-type store schema conn-type))
  ([store schema conn-type filters] (reads/links-of-type store schema conn-type filters)))

(defn object-counts [store schema] (reads/object-counts store schema))

(defn objects-by-ids [query schema obj-type ids] (reads/objects-by-ids query schema obj-type ids))

(defn objects-matching
  [store schema obj-type query opts] (reads/objects-matching store schema obj-type query opts))

(defn objects-of-type
  ([store schema obj-type] (reads/objects-of-type store schema obj-type))
  ([store schema obj-type opts] (reads/objects-of-type store schema obj-type opts)))

(defn page-matching
  [store schema obj-type query opts] (reads/page-matching store schema obj-type query opts))

(defn tableless [schema opts] (reads/tableless schema opts))

(defn attach-derived
  ([reads schema rid rows] (views/attach-derived reads schema rid rows))
  ([reads schema rid rows want] (views/attach-derived reads schema rid rows want))
  ([reads schema rid rows want extra] (views/attach-derived reads schema rid rows want extra)))

(defn connected [reads schema rid side root-ids] (views/connected reads schema rid side root-ids))

(defn derive-all [schema] (views/derive-all schema))

(defn fold-gaps [store schema rid a e] (views/fold-gaps store schema rid a e))

(defn folded-properties [schema] (views/folded-properties schema))

(defn object-query [store schema] (views/object-query store schema))

(defn run-object-query [store schema spec] (views/run-object-query store schema spec))

(defn run-series-query [store schema spec] (views/run-series-query store schema spec))
