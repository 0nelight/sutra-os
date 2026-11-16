(ns sutra.store.access.counts)

(set! *warn-on-reflection* true)

(def kinds
  {:object-type {:section :object-types :names :object-type-name :total :objects :by :by-type}
   :link-type   {:section :link-types   :names :link-type-name   :total :links}})

(defn plan
  ([schema missing] (plan schema missing nil))
  ([schema missing only]
   (let [asked (into (sorted-set) (filter #(or (nil? only) (contains? only %))) (keys kinds))]
     {:asked asked
      :ask   (vec (for [kind       asked
                        :let       [{:keys [section names]} (kinds kind)
                                    skip (set (get missing section))]
                        [ref spec] (get schema section)
                        :when      (not (skip ref))]
                    {:kind kind :ref ref :as (keyword (name (or (get spec names) ref)))}))})))

(defn tally
  [{:keys [asked ask]} ns]
  (reduce (fn [acc [{:keys [kind as]} n]]
            (let [{:keys [total by]} (kinds kind)]
              (cond-> (update acc total + n)
                by (assoc-in [by as] n))))
          (into {} (mapcat (fn [kind]
                             (let [{:keys [total by]} (kinds kind)]
                               (cond-> [[total 0]] by (conj [by {}]))))
                           asked))
          (map vector ask ns)))
