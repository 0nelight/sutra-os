(ns sutra.types.authority
  (:require [sutra.types.entities :as ents]
            [clojure.set :as set]
            [sutra.kit.datalog :as dl]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.transform-io :as tio]))

(def default-space sch/default-space)
(def default-org :org/default)

(def schema-rules
  [[{:id :role-op/def      :head '[:role-op ?r ?o]   :body '[[:role-def ?r ?o]]}
    {:id :role-op/includes :head '[:role-op ?r ?o]   :body '[[:includes ?r ?s] [:role-op ?s ?o]]}
    {:id :within/self      :head '[:within ?c ?c]    :body '[[:container ?c]]}
    {:id :within/parent    :head '[:within ?c ?a]    :body '[[:parent ?c ?p] [:within ?p ?a]]}
    {:id :holds/grant      :head '[:holds ?p ?o ?c]
     :body '[[:grant ?p ?r ?a] [:within ?c ?a] [:role-op ?r ?o]]}
    {:id :marked/own       :head '[:marked ?x ?m]    :body '[[:mark ?x ?m]]}
    {:id :marked/container :head '[:marked ?c ?m]    :body '[[:marked ?a ?m] [:parent ?c ?a]]}
    {:id :marked/resource  :head '[:marked ?r ?m]    :body '[[:marked ?c ?m] [:resource-of ?r ?c]]}
    {:id :marked/lineage   :head '[:marked ?x ?m]
     :body '[[:marked ?y ?m] [:derived-from ?x ?y]
             [:not [:stop-propagating ?x ?y ?m]]]}
    {:id :homed/org        :head '[:homed ?c ?o]     :body '[[:organization ?o] [:within ?c ?o]]}
    {:id :walled/home      :head '[:walled ?c ?o]    :body '[[:homed ?c ?o]]}
    {:id :walled/shared    :head '[:walled ?c ?o]
     :body '[[:shared-into ?a ?o] [:within ?c ?a] [:homed ?c ?h]]}
    {:id :walled/resource  :head '[:walled ?r ?o]
     :body '[[:walled ?c ?o] [:resource-of ?r ?c]]}
    {:id :requires/home    :head '[:requires ?c ?c]  :body '[[:homed ?c ?o]]}
    {:id :requires/resource :head '[:requires ?r ?g]
     :body '[[:requires ?c ?g] [:resource-of ?r ?c]]}
    {:id :kept/wall        :head '[:kept ?x ?y ?g]
     :body '[[:derived-from ?x ?y] [:walled ?g ?o] [:not [:stop-requiring ?x ?y ?o]]]}
    {:id :requires/lineage :head '[:requires ?x ?g]
     :body '[[:requires ?y ?g] [:derived-from ?x ?y] [:kept ?x ?y ?g]]}]])

(def subject-rules
  [[{:id :member/self      :head '[:member ?u ?u]    :body '[[:subject ?u]]}
    {:id :member/group     :head '[:member ?u ?g]    :body '[[:member ?u ?h] [:member-of ?h ?g]]}
    {:id :covers/cleared   :head '[:covers ?u ?m]    :body '[[:cleared ?u ?m]]}
    {:id :covers/dominates :head '[:covers ?u ?m]    :body '[[:cleared ?u ?n] [:dominates ?n ?m]]}
    {:id :covers/group     :head '[:covers ?u ?m]    :body '[[:member ?u ?g] [:clears ?g ?m]]}
    {:id :covers/group-dominates :head '[:covers ?u ?m]
     :body '[[:member ?u ?g] [:clears ?g ?n] [:dominates ?n ?m]]}
    {:id :inside/member    :head '[:inside ?u ?o]    :body '[[:member ?u ?p] [:org-member ?p ?o]]}
    {:id :admitted/org     :head '[:admitted ?u ?g]  :body '[[:walled ?g ?o] [:inside ?u ?o]]}
    {:id :covers-any/held  :head '[:covers-any ?u ?x ?k]
     :body '[[:marked ?x ?n] [:disjunctive ?n ?k] [:covers ?u ?n]]}
    {:id :granted/grant    :head '[:granted ?u ?o ?c]
     :body '[[:member ?u ?p] [:grant ?p ?r ?a] [:within ?c ?a] [:role-op ?r ?o]]}]
   [{:id :blocked/marking  :head '[:blocked ?u ?x]
     :body '[[:subject ?u] [:not [:unrestricted ?u]] [:marked ?x ?m] [:not [:disjunctive ?m ?k]]
             [:not [:covers ?u ?m]]]}
    {:id :blocked/disjunctive :head '[:blocked ?u ?x]
     :body '[[:subject ?u] [:not [:unrestricted ?u]] [:marked ?x ?m] [:disjunctive ?m ?k]
             [:not [:covers-any ?u ?x ?k]]]}
    {:id :blocked/organization :head '[:blocked ?u ?x]
     :body '[[:subject ?u] [:not [:unrestricted ?u]] [:requires ?x ?g] [:not [:admitted ?u ?g]]]}
    {:id :blocked/disabled :head '[:blocked ?u ?x]   :body '[[:disabled ?u] [:container ?x]]}
    {:id :blocked/disabled-resource :head '[:blocked ?u ?r]
     :body '[[:disabled ?u] [:resource-of ?r ?c]]}]
   [{:id :permitted/grant  :head '[:permitted ?u ?o ?c]
     :body '[[:granted ?u ?o ?c] [:not [:blocked ?u ?c]]]}
    {:id :may/granted      :head '[:may ?u ?o ?c]    :body '[[:permitted ?u ?o ?c]]}
    {:id :may/global-role  :head '[:may ?u ?o ?c]
     :body '[[:global-role ?u ?r] [:role-op ?r ?o] [:container ?c] [:not [:blocked ?u ?c]]]}
    {:id :may/unrestricted :head '[:may ?u ?o ?c]
     :body '[[:unrestricted ?u] [:not [:disabled ?u]] [:op ?o] [:container ?c]]}]])

(def rules
  (into schema-rules subject-rules))

(defn- add [facts rel tuple] (update facts rel (fnil conj #{}) (vec tuple)))

(defn role-facts
  [roles]
  (reduce-kv (fn [f role {:keys [operations includes]}]
               (cond-> (reduce #(-> %1 (add :role-def [role %2]) (add :op [%2])) f operations)
                 includes (add :includes [role includes])))
             {}
             roles))

(def marked-sections
  [:object-types :datasets :transforms :sources])

(def container-sections
  [{:section :organizations :name-field :organization-name}
   {:section :spaces        :name-field :space-name        :parent :organization}
   {:section :projects      :name-field :project-name      :parent :space}])

(defn- rid-in
  [m name-field x]
  (when x
    (if (contains? m x)
      x
      (some (fn [[rid spec]] (when (= (name x) (some-> (get spec name-field) name)) rid)) m))))

(defn- project-rid
  [projects p]
  (rid-in projects :project-name p))

(defn organization-rid
  [schema x]
  (rid-in (:organizations schema) :organization-name x))

(defn space-rid
  [schema x]
  (rid-in (:spaces schema) :space-name x))

(defn organization-of
  [schema s]
  (let [spec (get-in schema [:spaces (space-rid schema s)])]
    (or (organization-rid schema (:organization spec))
        (some #(organization-rid schema %) (:organizations spec))
        default-org)))

(defn space-organizations
  [schema s]
  (let [spec (get-in schema [:spaces (space-rid schema s)])
        home (organization-of schema s)]
    (into (if (= default-org home) #{} #{home})
          (keep #(organization-rid schema %))
          (concat (:organizations spec) (:shared-with spec)))))

(defn container-chain
  [schema p]
  (when-let [rid (project-rid (:projects schema) p)]
    (let [s (sch/project-space schema rid)]
      [rid s (organization-of schema s)])))

(defn- principals-of
  [xs]
  (keep #(some-> (if (map? %) (:principal %) %) str) xs))

(defn- container-facts
  [f schema]
  (let [orgs   (:organizations schema)
        org    #(rid-in orgs :organization-name %)
        own    (fn [f c spec]
                 (as-> (add f :container [c]) f
                   (reduce (fn [f {:keys [principal role]}]
                             (cond-> f (and principal role) (add :grant [(str principal) role c])))
                           f (:grants spec))
                   (reduce #(add %1 :mark [c %2]) f (:markings spec))
                   (reduce #(if-let [o (org %2)] (add %1 :shared-into [c o]) %1) f
                           (concat (:shared-with spec) (:organizations spec)))))]
    (as-> f f
      (-> f (add :container [default-org]) (add :container [default-space])
          (add :parent [default-space default-org]))
      (reduce-kv (fn [f o spec]
                   (reduce #(add %1 :org-member [%2 o])
                           (-> (own f o spec) (add :organization [o]))
                           (principals-of (concat (:members spec) (:guests spec)))))
                 f orgs)
      (reduce-kv (fn [f s spec] (-> (own f s spec) (add :parent [s (organization-of schema s)])))
                 f (:spaces schema))
      (reduce-kv (fn [f p spec]
                   (-> (own f p spec) (add :parent [p (sch/project-space schema p)])))
                 f (:projects schema)))))

(defn- stop-facts
  [f schema t y input]
  (let [org #(rid-in (:organizations schema) :organization-name %)]
    (as-> f f
      (reduce #(add %1 :stop-propagating [t y (keyword (name %2))]) f (:stop-propagating input))
      (reduce #(if-let [o (org %2)] (add %1 :stop-requiring [t y o]) %1) f
              (:stop-requiring input)))))

(defn- lineage-facts
  [f schema]
  (let [src  #(rid-in (:sources schema) :source-name %)
        ref  (fn [x] (or (:dataset x) (:object-type x) (:transform x) (:link-type x)
                         (some-> (:datasource x) src)))
        from (fn [f x spec]
               (if-let [s (some-> (or (get-in spec [:external-source :datasource])
                                      (get-in spec [:virtual :source]))
                                  src)]
                 (add f :derived-from [x s])
                 f))
        f    (reduce-kv (fn [f t spec]
                          (as-> f f
                            (reduce #(if-let [y (ref %2)]
                                       (-> (add %1 :derived-from [t y]) (stop-facts schema t y %2))
                                       %1)
                                    f (tio/inputs-of spec))
                            (reduce #(if-let [y (src %2)] (add %1 :derived-from [t y]) %1) f
                                    (:external-systems spec))
                            (reduce #(if-let [x (ref %2)] (add %1 :derived-from [x t]) %1) f
                                    (tio/outputs-of spec))))
                        f (:transforms schema))
        f    (reduce-kv (fn [f ot spec]
                          (cond-> (from f ot spec)
                            (or (:dataset spec) (get-in spec [:backing :dataset]))
                            (as-> f (let [ds (or (:dataset spec) (get-in spec [:backing :dataset]))]
                                      (-> f (add :derived-from [ot ds])
                                          (add :derived-from [ds ot]))))))
                        f (:object-types schema))]
    (reduce-kv (fn [f ds spec]
                 (cond-> (from f ds spec)
                   (get-in spec [:restricted-view :dataset])
                   (as-> f (if-let [y (rid-in (:datasets schema) :dataset-name
                                              (get-in spec [:restricted-view :dataset]))]
                             (add f :derived-from [ds y])
                             f))
                   (get-in spec [:stream :derive :from])
                   (as-> f (let [d (get-in spec [:stream :derive])]
                             (if-let [y (rid-in (:datasets schema) :dataset-name (:from d))]
                               (-> (add f :derived-from [ds y]) (stop-facts schema ds y d))
                               f)))
                   (:object-type spec)
                   (as-> f
                         (-> f (add :derived-from [(:object-type spec) ds])
                             (add :derived-from [ds (:object-type spec)])))))
               f (:datasets schema))))

(declare dominance)

(defn classification-of
  [schema]
  (into {} (for [[m {:keys [category rank]}] (:markings schema) :when (and category (some? rank))]
             [(keyword (name m)) {:category (str category) :rank (long rank)}])))

(defn- marking-facts
  [f schema]
  (as-> f f
    (reduce-kv (fn [f m {:keys [cleared]}]
                 (reduce #(add %1 :clears [(str %2) (keyword (name m))]) f cleared))
               f (:markings schema))
    (reduce #(add %1 :dominates %2) f (dominance (classification-of schema)))
    (reduce-kv (fn [f m {:keys [category disjunctive]}]
                 (cond-> f (and disjunctive category)
                         (add :disjunctive [(keyword (name m)) (str category)])))
               f (:markings schema))))

(defn schema-facts
  [schema]
  (let [schema   (sch/indexed schema)
        projects (:projects schema)
        f        (reduce (fn [f section]
                           (reduce-kv (fn [f rid spec]
                                        (let [p (project-rid projects (:project spec))]
                                          (cond-> (reduce #(add %1 :mark [rid %2]) f
                                                          (:markings spec))
                                            p (add :resource-of [rid p]))))
                                      f (get schema section)))
                         (marking-facts (container-facts {} schema) schema)
                         marked-sections)]
    (lineage-facts f schema)))

(defn dominance
  [classification]
  (for [[n {c :category k :rank}] classification
        [m {c' :category j :rank}] classification
        :when (and c k j (= c c') (not= n m) (>= (long k) (long j)))]
    [n m]))

(defn subject-facts
  [{:keys [who memberships organizations global-role unrestricted? disabled? clears
           classification attributes]}]
  (let [u (str who)]
    (cond-> (reduce #(add %1 :member-of %2) (add {} :subject [u]) memberships)
      global-role   (add :global-role [u global-role])
      unrestricted? (add :unrestricted [u])
      disabled?     (add :disabled [u])
      true          (as-> f (reduce (fn [f [p o]] (add f :org-member [(str p) o])) f organizations))
      true          (as-> f (reduce #(add %1 :cleared [u %2]) f clears))
      true          (as-> f (reduce #(add %1 :dominates %2) f (dominance classification)))
      true          (as-> f (reduce-kv #(add %1 :attribute [u %2 %3]) f attributes)))))

(defn schema-database
  [& fact-sets]
  (dl/run (apply merge-with into fact-sets) schema-rules))

(defn with-subject
  [base subject]
  (dl/run (merge-with into base subject) subject-rules))

(defn database
  [& fact-sets]
  (dl/run (apply merge-with into fact-sets) rules))

(defn blocked
  [db who]
  (into #{} (map '?x) (dl/query db [:blocked (str who) '?x])))

(defn decide
  [db {:keys [who on] op :do}]
  (let [t [(str who) op on]
        b [(str who) on]]
    (if (dl/holds? db :may t)
      {:allow? true :because (dl/explain db [:may t])}
      {:allow? false :because (when (dl/holds? db :blocked b) (dl/explain db [:blocked b]))})))

(defn holders
  [db op c]
  (into (sorted-set) (map '?p) (dl/query db [:holds '?p op c])))

(defn granted-operations
  ([db who] (granted-operations db who nil))
  ([db who among]
   (reduce (fn [m {o '?o c '?c}]
             (if (or (nil? among) (contains? among c)) (update m c (fnil conj #{}) o) m))
           {}
           (dl/query db [:permitted (str who) '?o '?c]))))

(defn- walls
  [db]
  (reduce (fn [m [x o]] (update m x (fnil conj #{}) o)) {} (get db :walled)))

(defn access-delta
  [before after]
  (let [diff   (fn [rel]
                 (let [b (set (get before rel)) a (set (get after rel))]
                   [(set/difference a b) (set/difference b a)]))
        f      (fn [rel change xs]
                 (map (fn [t] {:relation rel :fact t :change change}) (sort-by str xs)))
        [ha hr] (diff :holds)
        [ma mr] (diff :marked)
        wb     (walls before)
        wa     (walls after)
        wall   (for [x (sort-by str (set/union (set (keys wb)) (set (keys wa))))
                     :let [b (get wb x #{}) a (get wa x #{})]
                     :when (not= a b)
                     :let [opened (or (empty? a) (seq (set/difference a b)))
                           closed (or (empty? b) (seq (set/difference b a)))]]
                 {:relation :walled :fact [x (vec (sort-by str a))] :from (vec (sort-by str b))
                  :change (cond (empty? a) :removed (empty? b) :added :else :changed)
                  :expands? (boolean (and opened (seq b)))
                  :restricts? (boolean (and closed (seq a)))})
        [oa orm] (diff :org-member)
        [ra rr]  (diff :requires)]
    {:expands   (vec (concat (f :holds :added ha) (f :marked :removed mr)
                             (filter :expands? wall) (f :org-member :added oa)
                             (f :requires :removed rr)))
     :restricts (vec (concat (f :holds :removed hr) (f :marked :added ma)
                             (filter :restricts? wall) (f :org-member :removed orm)
                             (f :requires :added ra)))}))

(defn lifted
  [delta before]
  (let [wb (walls before)
        at (fn [m x k vs] (update-in m [x k] (fnil into #{}) vs))]
    (reduce (fn [m {:keys [relation fact from]}]
              (case relation
                :marked   (at m (first fact) :markings [(second fact)])
                :requires (at m (first fact) :organizations (get wb (second fact)))
                :walled   (at m (first fact) :organizations from)
                m))
            {}
            (:expands delta))))

(defn applied
  [before after]
  (let [wb (walls before)
        cs (set (map first (get after :container)))]
    (into {}
          (for [[x a] (walls after)
                :when (contains? cs x)
                :let [new (disj (set/difference a (get wb x #{})) x)]
                :when (seq new)]
            [x new]))))

(def access-sections
  (ents/sections-in :widens-access))

(defn expanding
  [delta]
  (let [subject (fn [{:keys [relation fact]}]
                  (case relation
                    (:marked :walled :requires) (first fact)
                    nil))
        es      (remove #(= :holds (:relation %)) (:expands delta))]
    {:subjects      (into #{} (keep subject) es)
     :unattributed? (boolean (some #(= :org-member (:relation %)) es))}))

(defn constraint-violations
  [schema]
  (let [projects (:projects schema)
        db       (schema-database (schema-facts schema))
        marked   (reduce (fn [m [x mk]] (update m x (fnil conj #{}) mk)) {} (get db :marked))]
    (for [section marked-sections
          [rid spec] (get schema section)
          :let [p (project-rid projects (:project spec))
                {:keys [allowed-markings forbidden-markings]} (get projects p)
                allowed   (some-> allowed-markings seq set)
                forbidden (set forbidden-markings)]
          :when (and p (or allowed (seq forbidden)))
          mk (sort (get marked rid))
          :let [reason (cond (contains? forbidden mk)              :forbidden
                             (and allowed (not (contains? allowed mk))) :not-allowed)]
          :when reason]
      {:resource rid :project p :marking mk :reason reason})))

(defn unreferenced-inputs
  [schema]
  (let [schema   (sch/indexed schema)
        projects (:projects schema)
        section  {:dataset :datasets :object-type :object-types :transform :transforms}
        home     (fn [k x]
                   (let [m (get schema (section k))]
                     (when-let [r (rid-in m (rid/name-field-of (section k)) x)]
                       [r (project-rid projects (get-in m [r :project]))])))]
    (for [[t spec] (:transforms schema)
          :let [p (project-rid projects (:project spec))]
          :when p
          :let [{:keys [references imports]} (get projects p)
                refs (into #{} (map str) references)
                imps (into #{} (keep #(project-rid projects %)) imports)]
          in (tio/inputs-of spec)
          :let [k (some #(when (get in %) %) (keys section))
                [r q] (when k (home k (get in k)))]
          :when (and q (not= q p) (not (imps q))
                     (not (refs (str r))) (not (refs (str (get in k)))))]
      {:transform t :project p :input r :from q})))

(defn unclassified-projects
  [schema]
  (let [schema   (sch/indexed schema)
        cats     (into #{} (keep (fn [[_ {:keys [category required-on-projects]}]]
                                   (when (and required-on-projects category) (str category))))
                       (:markings schema))
        cat-of   (into {} (for [[m {:keys [category]}] (:markings schema) :when category]
                            [(keyword (name m)) (str category)]))]
    (when (seq cats)
      (let [db     (schema-database (schema-facts schema))
            marked (reduce (fn [m [x mk]] (update m x (fnil conj #{}) mk)) {} (get db :marked))]
        (for [p (sort (keys (:projects schema)))
              c (sort cats)
              :when (not-any? #(= c (cat-of (keyword (name %)))) (get marked p))]
          {:project p :category c})))))
