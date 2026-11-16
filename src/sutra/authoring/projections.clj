(ns sutra.authoring.projections
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as scope]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.parser.project-export :as pe]
            [sutra.store.kept :as kept]
            [sutra.parser.checkout :as co]
            [sutra.parser.projection :as proj]
            [sutra.repos.api :as repos]
            [sutra.authoring.placements :as placements]
            [sutra.actions.api :as actions]
            [sutra.authoring.instance-change :as instance]
            [sutra.store.world :as world]
            [sutra.store.seat :as seat]
            [sutra.authoring.stage :as stage]
            [sutra.authoring.ports :as ports]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- declared! [schema nm]
  (or (proj/declared schema nm)
      (refusal/refuse! :refusal.authoring/no-projection {:name nm})))

(defn memory
  [{:keys [store]} nm]
  (placements/memory store nm))

(defn- source
  [{:keys [store]} nm]
  (placements/source store nm))

(defn seed-placements-from-tree!
  [store schema]
  (let [nm (pe/git-projections :ontology)]
    (placements/seed! store nm
                      (proj/memory-of
                       (pe/placement-of (:formats (proj/declared schema nm))
                                        (repos/ontology-texts store (scope/repos)
                                                              pe/trunk-branch))))))

(defn decorator-statements
  [schema absorbed]
  (let [declared (into #{}
                       (keep (fn [[_ t]] (when (= :code (:spec-source t))
                                           (rid/->name (:transform-name t)))))
                       (:transforms schema))]
    (vec (for [[k body] (:transforms absorbed)
               :when (declared (rid/->name k))
               f ents/decorator-fields
               :when (if (= :output f)
                       (seq (apply dissoc (:output body) ents/output-ontology-fields))
                       (contains? body f))]
           (str (rid/->name k) " " f)))))

(defn- refuse-decorator-statements!
  [schema absorbed]
  (when-let [stated (seq (decorator-statements schema absorbed))]
    (refusal/refuse! :refusal.authoring/declared-by-decorator
                     {:kind ::declared-by-decorator :stated (vec stated)}
                     {:names (str/join ", " stated)})))

(defn- trunk-schema
  [{:keys [store]}]
  (seat/branch-schema (world/at store pe/trunk-branch)))

(defn- on-trunk?
  [{:keys [branch]}]
  (= (str (or branch pe/trunk-branch)) pe/trunk-branch))

(defn inputs-around
  [{:keys [schema] :as ctx} base]
  (ports/inputs-around schema
                       (when-not (on-trunk? ctx) (trunk-schema ctx))
                       (mapcat (comp keys val) base)))

(defn inputs-changed
  [{:keys [schema] :as ctx} around]
  (:inputs-changed (ports/push-report schema
                                      (when (seq (:main around)) (trunk-schema ctx))
                                      around
                                      {})))

(defn- held-schema
  [{:keys [schema store branch]}]
  (pe/holding schema (kept/export store branch)))

(defn pull
  [ctx nm pick rehome?]
  (declared! (:schema ctx) nm)
  (let [src  (cond rehome?                                   nil
                   (or (seq (:texts pick)) (:project pick)) (source ctx nm)
                   :else                                     (memory ctx nm))
        view (proj/view (held-schema ctx) nm src pick)]
    (assoc view :name nm :around (inputs-around ctx (:base view)))))

(defn- not-stored
  [expected before actual]
  (let [element (fn [m k] (when (map? m) (get m k)))]
    (vec (for [[path nodes] expected
               [node els] nodes
               :let [was  (get-in before [path node])
                     have (get-in actual [path node])
                     lost (if (map? els)
                            (vec (sort-by str (for [[k t] els
                                                    :when (and (not= t (element was k))
                                                               (= (element was k)
                                                                  (element have k)))]
                                                k)))
                            (when (and (not= els was) (= was have)) :all))]
               :when (seq (if (keyword? lost) [lost] lost))]
           {:text path :node node :elements lost}))))

(defn- texts-holding
  [memory nodes]
  (let [node (fn [k] (if (and (vector? k) (vector? (first k))) (first k) k))]
    (into #{} (for [part [:placement :fields :attributes]
                    [k path] (get memory part)
                    :when (contains? nodes (node k))]
                path))))

(defn- read-back
  [{:keys [store branch] :as ctx} nm expected sent moved]
  (let [sent    (update-keys (or sent {}) name)
        judged  (into (set (mapcat (fn [[_ _ from to]] [from to]) moved))
                      (keep (fn [[t b]] (when (not= b (get sent t)) t)))
                      expected)
        expect  (select-keys expected judged)
        nodes   (set (mapcat (comp keys val) expect))
        texts   (vec (sort (keep (fn [[t b]] (when (some nodes (keys b)) t)) expected)))]
    (if (empty? expect)
      []
      (let [b     (or branch pe/trunk-branch)
            read  (:read (actions/staging-view (world/at store b)))
            after (assoc ctx :schema (seat/branch-schema (world/at store read)))
            src   (source after nm)
            read? (vec (sort (-> (set texts) (into (keys expect))
                                 (into (texts-holding (co/memory-for src nodes) nodes)))))
            base  (:base (proj/view (:schema after) nm src {:texts read?}))]
        (not-stored expect (:base (proj/view (:schema ctx) nm src {:texts read?})) base)))))

(defn- with-sent-code
  [body sent]
  (let [texts (into {} (for [[repo files] sent, [path {:keys [text]}] files]
                         [[(rid/->name repo) (rid/->name path)] text]))
        text  (fn [{:keys [repository code-file]}]
                (when code-file
                  (if repository
                    (get texts [(rid/->name repository) code-file])
                    (some (fn [[[_ p] t]] (when (= p code-file) t)) texts))))
        read  (fn [node]
                (if-let [t (when (and (map? node) (not (:code node))) (text node))]
                  (-> node (assoc :code t) (dissoc :code-file))
                  node))]
    (reduce (fn [b section]
              (cond-> b (map? (get b section)) (update section update-vals read)))
            body [:transforms :functions])))

(defn- refuse-unread-code!
  [schema branch body]
  (when-let [gone (seq (for [section [:transforms :functions]
                             [k node] (get body section)
                             :when (and (map? node) (:code-file node) (not (:code node))
                                        (not (rid/resolve-name->rid (sch/name-index schema)
                                                                    #{section} k)))]
                         (str (name section) " " (rid/->name k) ": " (:code-file node)
                              (when-let [r (:repository node)]
                                (str " in repository " (rid/->name r))))))]
    (refusal/refuse! :refusal.authoring/code-file-missing
                     {:kind ::code-file-missing}
                     {:branch branch :gone (str/join "; " gone)})))

(defn push-plan
  [{:keys [schema branch] :as ctx} nm files {:keys [base shape resolve sent-code]}]
  (let [{:keys [moved] :as r}
        (proj/absorb (held-schema ctx) nm files
                     {:memory  (placements/source (:store ctx) nm) :base base :shape shape
                      :resolve (some-> resolve keyword #{:mine :theirs})})
        r          (update r :schema #(repos/resolve-code-files
                                       schema (scope/repos) (with-sent-code % sent-code) branch
                                       (when (= 1 (count files))
                                         (some-> (key (first files)) name
                                                 (str/replace #"\.ent$" "")))))
        _          (refuse-unread-code! schema branch (:schema r))
        [inst own] (instance/split (instance/instance-pred schema) r)
        {absorbed :schema opts :opts} own
        _          (refuse-decorator-statements! schema absorbed)]
    {:absorbed    r
     :inst        inst
     :schema      absorbed
     :opts        opts
     :moves       (mapv (fn [[[s n] from to]] [(name s) n from to]) moved)
     :only-moves? (and (empty? absorbed) (empty? @(:strict-projected opts)))}))

(defn push!
  [{:keys [store schema branch] :as ctx} nm
   {:keys [files preview? land? justification around] :as body}]
  (declared! schema nm)
  (when-not (map? files)
    (refusal/refuse! :refusal.authoring/projection-write-files {:name nm}))
  (let [b       (or branch pe/trunk-branch)
        {r :absorbed :keys [inst opts moves only-moves?] absorbed :schema}
        (push-plan (assoc ctx :branch b) nm (update-keys files name) body)
        arrived (instance/arrive! store (:edge ctx) nm b inst preview? justification)
        ctx     (cond-> ctx (and arrived (not preview?))
                  (assoc :schema (seat/branch-schema (world/at store b))))
        staged  (if only-moves?
                  {:created 0 :updated 0 :deleted 0 :skipped 0 :preview? (boolean preview?)
                   :staged []}
                  (stage/stage! ctx {:schema absorbed :preview? (boolean preview?)} opts))]
    (when-not preview? (placements/remember! store nm (:said r)))
    (let [answer (cond-> (merge (assoc staged :base (:base r) :moved moves) arrived
                                (ports/push-report
                                 schema
                                 (when (seq (:main around)) (trunk-schema ctx))
                                 around
                                 {:sent (:base body) :next (:base r)
                                  :before (:theirs r) :after (:result r)}))
                   (and (not preview?) (not only-moves?)
                        (actions/land-asked? (:edge ctx) b land?))
                   (merge (actions/land store (:edge ctx) b)))
          lost   (when-not (or preview? only-moves?)
                   (read-back ctx nm (:base r) (:base body) moves))]
      (cond-> answer (seq lost) (assoc :not-stored lost)))))
