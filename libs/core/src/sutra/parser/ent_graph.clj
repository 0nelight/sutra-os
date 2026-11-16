(ns sutra.parser.ent-graph
  (:require [sutra.kit.refusal :as refusal]
            [sutra.types.attribute :as attribute]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn- keyed? [section-val] (map? section-val))

(defn- tuples? [section-val]
  (and (vector? section-val) (every? #(and (vector? %) (some? (first %))) section-val)))

(defn entry-name
  [section-val x]
  (rid/->name (if (keyed? section-val) (key x) (first x))))

(defn node-key
  [section section-val x]
  [section (entry-name section-val x)])

(defn attr-name [a] (rid/->name (attribute/attr-name a)))

(defn extension-entry?
  [v]
  (and (map? v) (seq (:attributes v)) (every? #{:attributes :project} (keys v))))

(defn- conflict! [section nm field p q]
  (refusal/refuse! :refusal.parser/declared-twice
                   {:kind ::conflict :section section :key nm :field field :paths [p q]}
                   {:p p :q q :field-note (if field (str " (" field ")") "")}))

(defn- add-attributes
  [have attrs path {:keys [section nm owner-path extension? contributor]} placed]
  (reduce (fn [[as placed] a]
            (let [n   (attr-name a)
                  cur (some #(when (= n (attr-name %)) %) as)]
              (cond
                (nil? cur)
                [(conj as (if-let [c (and extension? contributor)]
                            (let [[an t o] (attribute/->tuple a)]
                              [an t (assoc (or o {}) :project c)])
                            a))
                 (cond-> placed (not= path owner-path) (assoc [[section nm] n] path))]

                (and (not extension?) (= cur a))
                [as placed]

                extension?
                (refusal/refuse! :refusal.parser/extension-redeclares
                                 {:kind ::conflict :section section :key nm :attribute n
                                  :paths [path]}
                                 {:path path})

                :else
                (conflict! section nm (str "attribute " n) (get placed [[section nm] n] owner-path)
                           path))))
          [(vec have) placed]
          attrs))

(defn project-imports
  [entries]
  (reduce (fn [m [nm spec]]
            (update m (rid/->name nm) (fnil into #{}) (map rid/->name (:imports spec))))
          {}
          entries))

(defn extensions
  [contributions imports]
  (let [owner    (fn [[_ v]] (some-> (:project v) rid/->name))
        defines? (fn [c] (not (extension-entry? (second c))))]
    (into #{}
          (filter (fn [c]
                    (and (not (defines? c))
                         (let [p (owner c)]
                           (some (fn [d] (let [q (owner d)]
                                           (and (not= p q)
                                                (or (defines? d) (contains? (get imports p) q)))))
                                 contributions)))))
          contributions)))

(defn- merge-entry
  [section nm contributions imports]
  (let [owner     (fn [[_ v]] (some-> (:project v) rid/->name))
        extends?  (extensions contributions imports)
        ext       (filter extends? contributions)
        declaring (remove extends? contributions)
        [p0
         v0]   (first (sort-by (fn [[p v]] [(- (+ (count v) (if (contains? v :attributes) 1 0))) p])
                                  declaring))
        _         (let [owned (sort-by (fn [[p v]] [(- (count v)) p]) (filter owner declaring))
                        [p :as a] (first owned)]
                    (when-let [[q :as b] (first (filter #(not= (owner %) (owner a)) owned))]
                      (refusal/refuse! :refusal.parser/two-owners
                                       {:kind ::conflict :section section :key nm :field :project
                                        :paths [p q]}
                                       {:owner-a (owner a) :owner-b (owner b) :p p :q q})))
        ctx       {:section section :nm nm :owner-path p0}
        [v fields attrs]
        (reduce (fn [[v fields attrs] [p w]]
                  (if (= p p0)
                    [v fields attrs]
                    (reduce-kv
                     (fn [[v fields attrs] k x]
                       (cond
                         (= :attributes k)
                         (let [[as attrs] (add-attributes (:attributes v) x p ctx attrs)]
                           [(assoc v :attributes as) fields attrs])

                         (not (contains? v
                                         k)) [(assoc v k x) (assoc fields [[section nm] k] p) attrs]
                         (= (get v k) x)       [v fields attrs]
                         :else (conflict! section nm k (get fields [[section nm] k] p0) p)))
                     [v fields attrs] w)))
                [(or v0 {}) {} {}]
                declaring)
        [as attrs] (reduce (fn [[as attrs] [p w]]
                             (add-attributes as (:attributes w) p
                                             (assoc ctx :extension? true :contributor (:project w))
                                             attrs))
                           [(:attributes v) attrs] ext)]
    {:value  (cond-> v (seq as) (assoc :attributes as))
     :path   p0
     :fields fields
     :attrs  attrs}))

(defn- shape
  [section-val]
  (cond (keyed? section-val) :keyed (tuples? section-val) :tuples :else :scalar))

(defn- entries
  [sv]
  (case (shape sv)
    :keyed  (map (fn [x] [(entry-name sv x) (key x) (val x) (val x)]) sv)
    :tuples (map (fn [x] [(entry-name sv x) nil x (get x 4)]) sv)
    :scalar [[nil nil sv nil]]))

(defn- put
  [s kind cur k v]
  (case s
    :keyed  (if-let [have (get cur k)]
              (assoc cur k (cond-> (merge have (dissoc v :attributes))
                             (seq (:attributes v))
                               (update :attributes (fnil into []) (:attributes v))))
              (assoc (or cur kind) k v))
    :tuples (conj (or cur []) v)
    :scalar v))

(defn- collect
  [files]
  (reduce (fn [acc [path body]]
            (reduce-kv
             (fn [acc section sv]
               (let [s (shape sv)]
                 (reduce (fn [acc [nm k v]]
                           (cond-> (update-in acc [:parts [section nm]] (fnil conj []) [path v])
                             (not= :scalar s) (update-in [:order section] (fnil conj []) nm)
                             (= :keyed s)     (-> (update-in [:keys [section nm]] #(or % k))
                                                  (update-in [:kind section] #(or % (empty sv))))
                             (= :tuples s)    (assoc-in [:kind section] [])))
                         acc (entries sv))))
             acc body))
          {} files))

(defn- agreed!
  [section nm [[p x] & more]]
  (when-let [[q _] (some (fn [[q y]] (when (not= x y) [q y])) more)]
    (conflict! section nm nil p q))
  [p x])

(defn merge-bodies
  [files]
  (let [contribs (collect files)
        sections (distinct (for [[_ body] files s (keys body)] s))
        imports  (project-imports (for [[[section nm] parts] (:parts contribs)
                                        :when (= :projects section)
                                        [_ v] parts]
                                    [nm v]))]
    (reduce
     (fn [out section]
       (let [kind (get-in contribs [:kind section])
             s    (shape kind)]
         (reduce (fn [out nm]
                   (let [parts (get-in contribs [:parts [section nm]])
                         {:keys [value path fields attrs]}
                         (if (= :keyed s)
                           (merge-entry section nm parts imports)
                           (let [[p x] (agreed! section nm parts)] {:value x :path p}))]
                     (-> out
                         (update-in [:schema section]
                                    #(put s kind % (get-in contribs [:keys [section nm]]) value))
                         (assoc-in [:placement [section nm]] path)
                         (update :fields merge fields)
                         (update :attributes merge attrs))))
                 out
                 (if (= :scalar s) [nil] (distinct (get-in contribs [:order section]))))))
     {:schema {} :placement {} :fields {} :attributes {}}
     sections)))

(defn- split
  [node v p {:keys [fields attributes]}]
  (if-not (map? v)
    [[p v]]
    (let [moved  (for [[f fv] v
                       :when (not= :attributes f)
                       :let [q (get fields [node f])]
                       :when (and q (not= q p))]
                   [q f fv])
          as     (for [a (:attributes v)
                       :let [q (get attributes [node (attr-name a)])]
                       :when (and q (not= q p))]
                   [q a])
          gone-f (into #{} (map second) moved)
          gone-a (into #{} (map second) as)
          own    (cond-> (apply dissoc v gone-f)
                   (seq gone-a) (update :attributes #(vec (remove gone-a %)))
                   (and (seq gone-a)
                        (empty? (remove gone-a (:attributes v))))
                     (dissoc :attributes))]
      (concat [[p own]]
              (for [[q f fv] moved] [q {f fv}])
              (for [[q a] as] [q {:attributes [a]}])))))

(defn distribute
  [schema home memory]
  (reduce-kv
   (fn [bodies section sv]
     (let [s (shape sv)]
       (reduce (fn [bodies [nm k v hv]]
                 (let [node [section nm]]
                   (reduce (fn [b [q part]]
                             (update-in b [q section] #(put s (empty sv) % k part)))
                           bodies
                           (split node v (home node hv) memory))))
               bodies (entries sv))))
   {}
   schema))
