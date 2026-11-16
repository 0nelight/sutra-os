(ns sutra.world.gate.marking
  (:require [sutra.world.db :as db]
            [sutra.world.delta :as d]
            [sutra.world.gate.base :as b]
            [sutra.world.policy :as policy]
            [sutra.world.view :as view]))

(def ^:private marked-sections
  [:object-types :datasets :transforms :sources])

(defn- project-members
  [db0 db1 p]
  (into #{} (for [n (into (b/names-of db0 :projects p) (b/names-of db1 :projects p))
                  d [db0 db1]
                  node (db/nodes d :members [:project n])]
              node)))

(defn- violations [db [sec r]]
  (when-let [e (db/entry db [sec r])]
    (when-let [p (policy/resolve-in db :projects (:project e))]
      (let [{:keys [allowed-markings forbidden-markings]} (db/entry db [:projects p])
            allowed   (some-> allowed-markings seq set)
            forbidden (set forbidden-markings)]
        (when (or allowed (seq forbidden))
          (for [mk (sort (policy/marked db r))
                :let [reason (cond (contains? forbidden mk)                   :forbidden
                                   (and allowed (not (contains? allowed mk))) :not-allowed)]
                :when reason]
            {:resource r :project p :marking mk :reason reason}))))))

(def ^:private marking-constraints-met
  {:id :marking-constraints-met? :rule :project/marking-constraint :scope :local
   :keys (fn [db0 db1 changes]
           (let [resources (set marked-sections)]
             (-> (into #{} (filter (comp resources first)) (d/nodes changes))
                 (into (for [x (policy/moved changes db0 db1) sec marked-sections] [sec x]))
                 (into (comp (mapcat #(project-members db0 db1 %))
                             (filter (comp resources first)))
                       (view/touched changes :projects)))))
   :all (fn [db] (for [sec marked-sections [k _] (get (db/world db) sec)] [sec k]))
   :check violations})

(def fact-rows
  [{:id :required-categories :sections #{:markings}
    :facts (fn [_ {:keys [category required-on-projects]}]
             (when (and required-on-projects category) [[:required (str category)]]))}])

(defn- category-of [db m]
  (some-> (or (db/entry db [:markings (keyword (name m))])
              (db/entry db [:markings (name m)]))
          :category str))

(defn- required [db] (sort (map second (keys (get (db/view db :gate-facts)
                                                   :required-categories)))))

(defn- unclassified [db p]
  (when (db/entry db [:projects p])
    (let [cats (into #{} (keep #(category-of db %)) (policy/marked db p))]
      (for [c (required db) :when (not (contains? cats c))]
        {:project p :category c}))))

(defn- required-set [db] (set (keys (get (db/view db :gate-facts) :required-categories))))

(defn- category-moved? [db0 db1 changes]
  (some #(not= (get (db/entry db0 [:markings %]) :category)
               (get (db/entry db1 [:markings %]) :category))
        (view/touched changes :markings)))

(def ^:private project-classification-met
  {:id :project-classification-met? :rule :project/classification-required :scope :global
   :why (str "a change to the categories every project must carry changes the finding of"
             " every project, so then it re-checks them all. Only a change to the markings"
             " section does that, never a create path of a resource; any other step"
             " re-checks the projects whose effective markings move")
   :keys (fn [db0 db1 changes]
           (let [projects (into #{} (filter #(or (db/entry db0 [:projects %])
                                                 (db/entry db1 [:projects %])))
                                (policy/moved changes db0 db1))
                 r1       (required-set db1)]
             (if (or (not= (required-set db0) r1)
                     (and (seq r1) (category-moved? db0 db1 changes)))
               (into projects (concat (keys (get (db/world db0) :projects))
                                      (keys (get (db/world db1) :projects))))
               projects)))
   :all (b/section-keys :projects)
   :check unclassified})

(def rows [marking-constraints-met project-classification-met])
