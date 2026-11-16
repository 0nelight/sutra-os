(ns support.in-project
  (:require [sutra.parser.sugar :as sugar]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]))

(defn in-project
  [p schema]
  (let [schema   (sugar/derive-projects schema)
        lens     (forms/author schema)
        authored (reduce (fn [s tag]
                           (reduce (fn [s [nm spec]]
                                     (if (and (map? spec) (not (:project spec)))
                                       (forms/assoc-project s tag nm p) s))
                                   s
                                   ((:entries lens) tag)))
                         (update schema :projects #(merge {p {}} %))
                         member/member-tags)]
    (reduce (fn [s section]
              (if (map? (get s section))
                (update s section update-vals
                        #(if (and (map? %) (not (:project %))) (assoc % :project p) %))
                s))
            authored
            (remove (set (vals member/author-sections)) (vals member/installed-sections)))))

(defn install-in-project
  ([body] (install-in-project :flotte body))
  ([p body]
   ((requiring-resolve 'sutra.wire.api/post!) :install (update body :schema #(in-project p %)))))
