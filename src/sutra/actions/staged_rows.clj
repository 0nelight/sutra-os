(ns sutra.actions.staged-rows
  (:require [sutra.foundation.ops :as op]
            [sutra.store.control :as ctl]
            [sutra.store.records :as r]))

(set! *warn-on-reflection* true)

(defn- staged-row
  ([branch action args tx-vector provenance]
   (staged-row branch action args tx-vector provenance nil))
  ([branch action args tx-vector {:keys [author origin origin-ref]} delta]
   (cond-> {:branch branch :action action :args args :tx-vector tx-vector
            :author author :origin (some-> origin name) :origin-ref origin-ref
            :created-at ctl/now}
     delta (assoc :delta delta))))

(defn stage!
  ([store branch action args] (stage! store branch action args nil nil))
  ([store branch action args tx-vector] (stage! store branch action args tx-vector nil))
  ([store branch action args tx-vector provenance]
   (:seq (ctl/insert! store :staged-actions (staged-row branch action args tx-vector provenance)))))

(defn staged-entry
  ([store branch action args tx-vector provenance]
   (staged-entry store branch action args tx-vector provenance nil))
  ([store branch action args tx-vector provenance delta]
   (ctl/entry store :staged-actions
              (staged-row branch action args tx-vector provenance delta))))

(defn list-staged
  ([store] (list-staged store (:branch store)))
  ([store branch] (ctl/list store :staged-actions {:branch branch})))

(op/defop :actions/staged [{:keys [store]} {:keys [branch]}]
  (vec (list-staged store branch)))

(defn open-batches [store]
  (->> (r/list store (ctl/decl :staged-actions) {} {:order [[:seq :asc]] :all-branches? true})
       (group-by :branch)
       (map (fn [[b rows]]
              (let [f (first rows)]
                {:branch b :n (clojure.core/count rows)
                 :opened-at (:created-at f) :latest-at (:created-at (last rows))
                 :author (:author f) :origin (:origin f) :origin-ref (:origin-ref f)})))
       (sort-by #(some-> ^java.util.Date (:opened-at %) .getTime))
       vec))

(defn clear-staged! [store branch] (ctl/delete! store :staged-actions {:branch branch}))
