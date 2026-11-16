(ns sutra.types.directory)

(defn- edges
  [{:keys [users groups]}]
  (into (set (for [[u {gs :groups}] users g gs] [:user u g]))
        (for [[g {ps :groups}] groups p ps] [:group g p])))

(defn- diff-set [want have] [(remove have want) (remove want have)])

(defn reconcile
  [snapshot current]
  (if (empty? (:users snapshot))
    {:refused
       "the directory lists no user — refusing to disable everyone; check the source's filter"}
    (let [facts      #(select-keys % [:attributes :disabled?])
          want-users (:users snapshot)
          have-users (:users current)
          gone       (into #{} (comp (remove #(contains? want-users %))
                                     (remove #(:disabled? (get have-users %))))
                           (keys have-users))
          [add-g retire-g] (diff-set (set (keys (:groups snapshot))) (set (keys (:groups current))))
          retired    (set retire-g)
          [add-e rm-e] (diff-set (edges snapshot) (edges current))]
      {:users  {:add     (into (sorted-map)
                               (for [[u x] want-users :when (not (contains? have-users u))]
                                 [u (facts x)]))
                :change  (into (sorted-map) (for [[u x] want-users
                                                  :let [h (get have-users u)]
                                                  :when (and h (not= (facts x) (facts h)))]
                                              [u {:from (facts h) :to (facts x)}]))
                :disable (into (sorted-set) gone)}
       :groups {:add (into (sorted-set) add-g) :retire (into (sorted-set) retire-g)}
       :edges  {:add    (into (sorted-set) add-e)
                :remove (into (sorted-set)
                              (concat rm-e
                                      (for [[_ _ g :as e] (edges current) :when (retired g)]
                                        e)))}})))

(defn sync-steps
  [users projects gone]
  (let [named   (into (sorted-set) (comp (mapcat :grants) (map (comp name :project))) users)
        missing (remove (set (map name projects)) named)]
    (vec (concat
          (for [p missing] {:step :create-project :project (keyword p)})
          (for [{:keys [subject sam display action]} users
                :when (= :create action)]
            {:step :create-user :subject subject :username sam :display (or display sam)})
          (for [{:keys [subject user-id action grants]} users
                {:keys [project role]} grants]
            {:step :grant :user (if (= :create action) [:subject subject] user-id)
             :project (keyword (name project)) :role role})
          (for [[subject id] (sort-by key gone)]
            {:step :disable :user id :subject subject})))))

(defn membership-steps
  [snapshot groups users members nested]
  (let [owned  (set (keys (:groups snapshot)))
        want-m (set (for [[s {gs :groups}] (:users snapshot) g gs :when (owned g) :let [u (users s)]
                          :when u] [u g]))
        want-n (set (for [[g {ps :groups}] (:groups snapshot) p ps :when (owned p)] [g p]))
        have-m (set (filter (comp owned second) members))
        have-n (set (filter (fn [[g p]] (and (owned g) (owned p))) nested))]
    (vec (concat
          (for [g (sort owned) :when (not (contains? groups g))] {:step :create-group :group g})
          (for [[u g] (sort-by str (remove have-m want-m))] {:step :join :user u :group g})
          (for [[g p] (sort (remove have-n want-n))] {:step :nest :group g :in p})
          (for [[u g] (sort-by str (remove want-m have-m))] {:step :leave :user u :group g})
          (for [[g p] (sort (remove want-n have-n))] {:step :unnest :group g :in p})))))
