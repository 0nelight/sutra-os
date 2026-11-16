(ns sutra.projects.map
  (:require [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(def ^:private nm rid/->name)

(defn- app-rows
  [apps want]
  (->> (for [[app-name spec] apps
             :let [pj (nm (:project spec))]
             :when (or (nil? want) (= want pj))]
         (let [lay    (:layout spec)
               binds  (->> (or (:object-types lay) (some-> (:object-type spec) vector))
                           (keep nm) distinct vec)
               writes (->> (concat (map nm (get-in lay [:upload :build]))
                                   (when (:action lay) [(nm (:object-type spec))]))
                           (keep identity) set)]
           {:app (nm app-name) :project pj :kind (nm (:kind lay))
            :title (:title lay) :binds binds :writes writes}))
       (sort-by :app)
       vec))

(defn- owned-types
  [schema want prid resolve-name]
  (when want
    (into (sorted-set)
          (keep (fn [[type-rid spec]]
                  (when (= prid (:project spec)) (resolve-name type-rid))))
          (:object-types schema))))

(defn project-map
  [schema applications counts project]
  (let [want   (nm project)
        cnt    (fn [t] (or (get counts (keyword t)) (get counts t)))
        known  (into (sorted-set)
                     (keep #(some-> (:project-name (val %)) nm))
                     (:projects schema))]
    (if (and want (not= "unassigned" want) (not (known want)))
      {:scope want :error (str "unknown project: " want) :known (vec known)}
      (let [rows     (app-rows applications want)
            bound    (into (sorted-set) (mapcat :binds rows))
            by-type  (reduce (fn [m {:keys [app binds]}]
                               (reduce #(update %1 %2 (fnil conj []) app) m binds))
                             {} rows)
            shared   (->> by-type (filter (fn [[_ as]] (> (count as) 1)))
                          (sort-by (comp - count val)))
            idx      (sch/rid-name-index schema)
            resolve-name (fn [x] (or (some-> (rid/resolve-rid->name idx x) nm) (nm x)))
            prid     (when (and want (not= "unassigned" want))
                       (sch/rid-of (sch/index schema) :projects want))
            owned    (owned-types schema want prid resolve-name)
            edges    (for [[_ l] (:link-types schema)
                           :let [from (resolve-name (:from-object-type l))
                                 to   (resolve-name (:to-object-type l))]
                           :when (and from to (or (bound from) (bound to)) (not= from to))]
                       {:from from :to to :card (nm (:cardinality l))
                        :internal? (boolean (and (bound from) (bound to)))})
            internal (sort-by (juxt :from :to) (filter :internal? edges))
            bridges  (sort-by (juxt :from :to) (remove :internal? edges))]
        (cond-> {:scope        (or want "*")
                 :apps         (mapv #(update % :writes vec) rows)
                 :object-types (into {} (for [t bound] [t (cnt t)]))
                 :shared       (into {} (for [[t as] shared] [t (vec (sort as))]))
                 :connections  {:internal (mapv #(dissoc % :internal?) internal)
                                :bridges  (mapv #(dissoc % :internal?) bridges)}}
          want (assoc :owns (into {} (for [t owned] [t (cnt t)]))))))))
