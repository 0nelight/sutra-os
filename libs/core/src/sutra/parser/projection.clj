(ns sutra.parser.projection
  (:require [sutra.types.plan :as cp]
            [sutra.kit.refusal :as refusal]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.checkout :as co]
            [sutra.parser.ent :as ent]
            [sutra.parser.ent-errors :refer [ent-errors-params]]
            [sutra.parser.ent-graph :as graph]
            [sutra.parser.project-export :as pe]
            [sutra.parser.sugar :as sugar]
            [clojure.string :as str]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.types.shape :as tshape]
            [sutra.types.tree :as tree]))

(set! *warn-on-reflection* true)

(defn- kw [x] (if (keyword? x) x (keyword (str x))))

(def ^:private all-edits #{:create :update :delete})

(defn declared
  [schema nm]
  (when-let [p (registry/projection schema nm)]
    (let [{:keys [sections project repository fields]} (:select p)]
      {:name     (kw nm)
       :formats  (registry/formats (:filename p))
       :sections (if (seq sections) (into #{} (map kw) sections) pe/ontology-sections)
       :fields   (into {} (for [[s fs] fields] [(kw s) (mapv kw fs)]))
       :edits    (if (seq (:edits p)) (into #{} (map kw) (:edits p)) all-edits)
       :elements (into {}
                       (for [[sec fs] (:elements p)]
                         [(kw sec) (into {} (for [[f sp] fs] [(kw f) (kw sp)]))]))
       :scope    (cond project    {:project (kw project)}
                       repository {:repository (kw repository)}
                       :else      {:all-projects? true})})))

(defn- known!
  [schema nm]
  (or (declared schema nm)
      (refusal/refuse! :refusal.parser/unknown-projection
                       {:kind ::unknown-projection :projection nm})))

(defn render
  ([schema nm] (render schema nm nil))
  ([schema nm memory]
   (let [{:keys [formats sections fields scope]} (known! schema nm)]
     (into (sorted-map)
           (map (fn [[path {:keys [content]}]] [path content]))
           (pe/lay-out schema
                       {:scope scope :sections sections :fields fields
                        :formats formats :memory memory})))))

(defn- read-back
  [{:keys [formats]} texts]
  (pe/read-texts formats (keys texts) texts))

(defn- read-text
  [formats path text]
  (try {:read (authoring/read-file formats path text)}
       (catch clojure.lang.ExceptionInfo e {:refused e})))

(defn- unreadable
  [{:keys [formats]} texts]
  (into #{}
        (filter #(:refused (read-text formats % (get texts %))))
        (filter #(authoring/file? formats %) (keys texts))))

(defn- refusal [formats path text]
  (try (authoring/read-file formats path text) nil
       (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defn- read-around
  [p rendered]
  (try [(read-back p rendered) #{}]
       (catch clojure.lang.ExceptionInfo e
         (let [bad (unreadable p rendered)]
           (when (empty? bad) (throw e))
           [(read-back p (apply dissoc rendered bad)) bad]))))

(def ^:dynamic *rendering-cache*
  nil)

(defn- rendering
  [schema p memory]
  (let [nm  (:name p)
        cache *rendering-cache*
        hit (some (fn [[[s n m] v]] (when (and (identical? s schema) (= n nm) (= m memory)) v))
                  (some-> cache deref))]
    (or hit
        (let [rendered (render schema nm memory)
              [now bad] (read-around p rendered)
              v        [rendered now bad]]
          (when cache (swap! cache #(vec (take 4 (cons [[schema nm memory] v] %)))))
          v))))

(defn shape-of
  [p]
  (co/fingerprint (:elements p)))

(defn- touched
  [read paths]
  (let [in?  (set paths)
        node (fn [k] (if (and (vector? k) (vector? (first k))) (first k) k))]
    (into #{}
          (for [part [:placement :fields :attributes]
                [k path] (get read part)
                :when (in? path)]
            (node k)))))

(defn- unit-of-project?
  [now project path]
  (and (= :unit (get-in now [:files path :rule]))
       (some (fn [[section nm]]
               (let [entries (get (:schema now) section)
                     entry   (or (get entries nm) (get entries (kw nm)))]
                 (= (name project) (some-> (:project entry) name))))
             (touched now [path]))))

(defn- texts-of
  [_formats now rendered {:keys [texts project]}]
  (cond
    (seq texts) (vec (sort (distinct texts)))
    project     (vec (sort (for [[path {:keys [owner]}] (:files now)
                                 :when (and (or (= (name project) (some-> owner name))
                                                (unit-of-project? now project path))
                                            (contains? rendered path))]
                             path)))
    :else       (vec (sort (keys rendered)))))

(defn- fragments-of
  [schema {:keys [formats sections] :as p} memory {:keys [texts project]}]
  (let [ex     (pe/exports schema (:scope p))
        picked (if (seq texts)
                 (set texts)
                 (pe/project-paths ex formats memory (name project)))
        nodes  (into #{} (mapcat #(pe/pulled-nodes ex formats memory sections %)) picked)
        rendered (into (sorted-map)
                       (map (fn [[path {:keys [content]}]] [path content]))
                       (pe/lay-out schema
                                   {:scope (:scope p) :sections sections :fields (:fields p)
                                    :formats formats :memory (co/memory-for memory nodes)
                                    :nodes nodes
                                    :paths (pe/related-paths ex formats memory nodes)}))
        [now bad] (read-around p rendered)]
    [rendered now bad
     (if (seq texts)
       (vec (sort (distinct texts)))
       (vec (sort (filter #(contains? rendered %) picked))))]))

(defn view
  ([schema nm] (view schema nm nil nil))
  ([schema nm memory] (view schema nm memory nil))
  ([schema nm memory pick]
   (let [{:keys [formats] :as p} (known! schema nm)
         local?   (and (co/source? memory) (or (seq (:texts pick)) (:project pick)))
         [rendered now bad paths]
         (if local?
           (fragments-of schema p memory pick)
           (let [[rendered now bad] (rendering schema p memory)]
             [rendered now bad (texts-of formats now rendered pick)]))
         unread   (pe/unread-files formats paths)
         _        (when (seq unread)
                    (refusal/refuse! :refusal.parser/projection-unread
                                     {:kind ::unread :files (vec (sort unread))}
                                     {:name (name (:name p))}))
         base     (co/base-of (:schema now) (:elements p))]
     {:shape (shape-of p)
      :unreadable (into (sorted-map)
                        (for [path paths :when (contains? bad path)]
                          [path (refusal formats path (get rendered path))]))
      :files (into (sorted-map)
                   (for [path paths]
                     [path
                      (or (get rendered path)
                          (authoring/print-file formats (authoring/default-format formats path) {}
                                                {}))]))
      :base  (into (sorted-map)
                   (for [path paths]
                     [path (select-keys base (touched now [path]))]))})))

(defn- by-section [nodes]
  (reduce (fn [m [section nm]] (update m section (fnil conj #{}) nm)) {} nodes))

(defn- names
  [body]
  (by-section (keys (tree/index (sugar/lower-schema body)))))

(defn shown
  [schema nm]
  (names (:schema (read-back (known! schema nm) (render schema nm)))))

(defn- changes
  [before after]
  (let [{:keys [added changed removed]} (tree/changes before after)]
    {:create (sort (keys added)) :update (sort (keys changed)) :delete (sort (keys removed))}))

(defn- live-types
  [ex]
  (let [one (fn [k]
              (when-let [[r o] ((:type ex) k)]
                (let [pk ((:primary-key ex) r)]
                  (cond-> {:attributes (mapv (juxt :attribute-name :attribute-type)
                                             (:attribute-list o))
                           :title      (:title o)
                           :match-keys (vec (:match-keys o))}
                    (and pk (not (sequential? pk))) (assoc :primary-key (keyword (name pk)))
                    (and (sequential? pk) (= 1 (count pk)))
                    (assoc :primary-key (keyword (name (first pk))))))))]
    (reify
      clojure.lang.ILookup
      (valAt [_ k] (one k))
      (valAt [_ k nf] (if-some [v (one k)] v nf))
      clojure.lang.Associative
      (containsKey [_ k] (some? (one k)))
      (entryAt [_ k] (when-some [v (one k)] (clojure.lang.MapEntry/create k v))))))

(defn- lowered
  [ex body]
  (sugar/lower-schema-against body (live-types ex)))

(defn- gone
  [ex before after]
  (by-section (keys (:removed (tree/changes (lowered ex before) (lowered ex after))))))

(defn- allowed!
  [{:keys [edits] :as p} before after]
  (when-let [bad (seq (for [[op nodes] (changes before after)
                            :when (and (seq nodes) (not (edits op)))]
                        [op (vec nodes)]))]
    (refusal/refuse! :refusal.parser/projection-not-allowed
                     {:kind ::not-allowed :edits (into {} bad)}
                     {:name  (name (:name p))
                      :what  (apply str (interpose "; " (for [[op nodes] bad]
                                                          (str (name op) " " (pr-str nodes)))))})))

(defn- complete
  [body live fields]
  (reduce-kv (fn [b section fs]
               (let [stored (into {} (for [[k v] (get live section)] [(rid/->name k) v]))]
                 (cond-> b
                   (map? (get b section))
                   (update section
                           #(into (empty %)
                                  (for [[k v] %]
                                    [k
                                     (merge (apply dissoc (get stored (rid/->name k)) fs) v)]))))))
             body
             fields))

(def ^:private sibling-bodies
  {:code-file :code})

(defn- by-name [body section] (into {} (for [[k v] (get body section)] [(rid/->name k) v])))

(defn- inlined
  [body live bodies]
  (reduce-kv
   (fn [b section m]
     (if-not (map? m)
       b
       (let [stored (by-name live section)
             full   (delay (by-name @bodies section))
             carry  (fn [v n]
                      (reduce-kv (fn [v file-k body-k]
                                   (if (and (get v file-k) (not (contains? v body-k))
                                            (= (get v file-k) (get-in stored [n file-k])))
                                     (-> v (assoc body-k (get-in @full [n body-k])) (dissoc file-k))
                                     v))
                                 v sibling-bodies))]
         (assoc b section (into (empty m) (for [[k v] m] [k (carry v (rid/->name k))]))))))
   body
   body))

(defn memory-of
  [read]
  (-> (dissoc read :schema :formats :files)
      (assoc :files (update-vals (:files read) #(select-keys % [:format :owner :rule])))))

(defn- said-of
  [memory paths]
  (into {} (for [[part m] memory
                 :let [m (into {} (filter (fn [[k v]] (if (= :files part) (paths k) (paths v)))) m)]
                 :when (seq m)]
             [part m])))

(defn- element-label [{[section nm] :node f :field}]
  (str (name section) " " nm
       (cond (vector? f) (str " " (pr-str (first f)) " " (second f))
             (and f (not= f co/whole)) (str " " (pr-str f)))))

(def ^:private refusal-codes
  {::unread       :refusal.projection/unread
   ::no-base      :refusal.projection/no-base
   ::stale-shape  :refusal.projection/stale-shape
   ::outside      :refusal.projection/outside
   ::out-of-scope :refusal.projection/out-of-scope
   ::conflict     :refusal.projection/conflict})

(defn- refuse!
  ([push kind data] (refuse! push kind data nil))
  ([{p :projection} kind data params]
   (refusal/refuse! (refusal-codes kind) (assoc data :kind kind)
                    (assoc params :projection (name (:name p))))))

(defn- raised
  [mine theirs]
  (let [changed (keep (fn [[k v]] (when (not= v (get-in theirs [:actions k])) k))
                      (:actions mine))
        cut     (update mine :actions select-keys changed)
        r       (when (seq changed)
                  (try (sugar/raise-actions cut) (catch clojure.lang.ExceptionInfo _ nil)))]
    (if (and r (= (dissoc r :actions) (dissoc cut :actions)))
      (update mine :actions merge (:actions r))
      mine)))

(defn- read-alone
  [{:keys [formats]} given]
  (try (let [r (pe/read-texts formats (keys given) given)]
         {:whole r :index (tree/index (:schema r))})
       (catch clojure.lang.ExceptionInfo _
         {:index (into {} (for [[path text] given
                                :let [r (try (pe/read-texts formats [path] {path text})
                                             (catch clojure.lang.ExceptionInfo _ nil))]
                                :when r
                                e (tree/index (:schema r))]
                            e))})))

(defn- project-of [{:keys [value]}]
  (cond (map? value)        (:project value)
        (sequential? value) (some #(when (map? %) (:project %)) value)))

(defn- in-scope
  [ex p memory paths base alone]
  (let [mine     (:index alone)
        in-text  (co/texts memory)
        held     (-> (set (keys base))
                     (into (keys mine))
                     (into (for [path paths
                                 [part el] (in-text path)]
                             (if (= :placement part) el (first el)))))
        live     (tree/index (pe/exported-nodes ex (:sections p) held {}))
        projects (into (set (keep (fn [[s n]] (when (= :projects s) n)) held))
                       (for [idx [live mine]
                             [_ e] idx
                             :let [pr (project-of e)]
                             :when (some? pr)]
                         (rid/->name pr)))]
    (into held
          (concat (for [pn projects] [:projects pn])
                  (keep #(pe/witness ex (:formats p) memory paths %)
                        projects)))))

(defn- text-schema
  [formats path text]
  (let [r (:read (read-text formats path text))]
    (when (and r (nil? (:kind (get formats (:format r)))))
      (:schema r))))

(defn- as-held
  [ex body]
  (into {}
        (for [[sec m] body]
          [sec (if (map? m)
                 (into (empty m)
                       (for [[k v] m
                             :let [now (:value (get @(:authored ex) [sec (rid/->name k)]))]]
                         [k (if (and (map? now) (map? v)) (merge now v) v)]))
                 m)])))

(def ^:private push-steps
  [[::unread
    (fn [{{:keys [formats]} :projection :keys [paths] :as push}]
      (when-let [unread (seq (pe/unread-files formats paths))]
        (refuse! push ::unread {:files (vec (sort unread))}))
      push)]

   [::grammar
    (fn [{{:keys [formats scope]} :projection :keys [schema texts] :as push}]
      (doseq [[path text] (sort-by key texts)
              :let [body (some->> text (text-schema formats path))
                    errs (when body
                           (ent/explain-schema (as-held (pe/exports schema scope) body)))]
              :when errs]
        (refusal/refuse! :refusal.parser/invalid-text {:source path :errors errs}
                         (ent-errors-params (str "Invalid " path) errs)))
      push)]

   [::no-base
    (fn [{:keys [base shape] p :projection :as push}]
      (when-not (map? base)
        (refuse! push ::no-base {}))
      (when (not= shape (shape-of p))
        (refuse! push ::stale-shape {}))
      (assoc push :base (if (every? string? (keys base)) (apply merge (vals base)) base)))]

   [::now
    (fn [{:keys [schema memory texts base paths] p :projection :as push}]
      (let [given (into {} (remove (comp nil? val)) texts)
            alone (read-alone p given)
            ex    (pe/exports schema (:scope p))
            nodes (in-scope ex p memory (set paths) base alone)
            mem   (co/memory-for memory nodes)
            rendered (into (sorted-map)
                           (map (fn [[path {:keys [content]}]] [path content]))
                           (pe/lay-out schema {:scope (:scope p) :sections (:sections p)
                                               :fields (:fields p) :formats (:formats p)
                                               :memory mem :nodes nodes :paths paths}))
            [now bad] (read-around p rendered)]
        (assoc push :ex ex :mem mem :given given :alone alone :nodes nodes :rendered rendered
               :rendered-now now :theirs (:schema now) :unreadable bad)))]

   [::outside
    (fn [{{:keys [formats]} :projection :keys [given paths base rendered-now] :as push}]
      (let [held  (set (keys base))
            alone-body (:schema (:whole (:alone push)))
            alone (tree/index alone-body)
            now-idx (tree/index (:schema rendered-now))
            extends? (fn [node]
                       (let [mine-part [:given (:value (get alone node))]
                             defined   [:now (:value (get now-idx node))]]
                         (and (map? (second mine-part)) (map? (second defined))
                              (contains?
                               (graph/extensions [mine-part defined]
                                                 (graph/project-imports
                                                  (concat (:projects (:schema rendered-now))
                                                          (:projects alone-body))))
                               mine-part))))
            taken (sort (for [[node _] (:placement (pe/placement-of formats given))
                              :let [from (get-in rendered-now [:placement node])]
                              :when (and from (not ((set paths) from)) (not (extends? node)))]
                          (str (name (first node)) " " (second node)
                               (if (held node) " moved to " " lives in ") from)))]
        (when (seq taken)
          (refuse! push ::outside {:nodes (vec taken)} {:taken (str/join "; " taken)}))
        push))]

   [::read
    (fn [{:keys [rendered paths given unreadable theirs] p :projection :as push}]
      (let [others (apply dissoc rendered (concat paths unreadable))
            read   (read-back p (merge others given))]
        (assoc push :read read :mine (raised (:schema read) theirs))))]

   [::out-of-scope
    (fn [{:keys [mine theirs] p :projection :as push}]
      (let [shown (into (:sections p) (keys theirs))]
        (when-let [stray (seq (remove shown (keys mine)))]
          (refuse! push ::out-of-scope {:sections (vec (sort stray))}))
        (assoc push :shown shown)))]

   [::conflict
    (fn [{:keys [base mine theirs resolve] p :projection :as push}]
      (let [{:keys [body
                    conflicts]} (co/merge3 base mine theirs
                                           {:resolve resolve :elements (:elements p)})]
        (when (seq conflicts)
          (refuse! push ::conflict {:conflicts (vec conflicts)}
                   {:labels (str/join "; " (map element-label conflicts))}))
        (assoc push :body body)))]

   [::not-allowed
    (fn [{:keys [theirs body] p :projection :as push}]
      (allowed! p theirs body)
      push)]

   [::unsound
    (fn [{:keys [body] p :projection :as push}]
      (cp/check! (cp/of-findings
                  (cp/problems ::unsound
                               (pe/kinds-problems
                                (or (some-> (registry/projection body (:name p))
                                            :filename registry/formats)
                                    (:formats p)))))
                 {:code   :refusal.projection/unsound
                  :data   {:kind ::unsound}
                  :params (fn [bs] {:detail     (str/join "; " (map :message bs))
                                    :projection (name (:name p))})})
      push)]])

(defn- stale-generators
  [ex body]
  (let [lowered (try (lowered ex body) (catch clojure.lang.ExceptionInfo _ nil))
        live    (fn [tn] (second ((:type ex) tn)))]
    (for [[an a] (:actions body)
          :let [cfg (:action-log a)
                tn  (when cfg (or (when (map? cfg) (:object-type cfg))
                                  (keyword (str (rid/->name an) "-log"))))
                gen (when tn (get-in lowered [:objects tn]))
                canon (when tn (live tn))]
          :when (and gen canon (not (contains? (:objects body) tn))
                     (not (tshape/scalars-satisfied?
                           :object-types (apply dissoc gen ents/placement-fields)
                           (apply dissoc canon ents/placement-fields))))]
      [:actions (rid/->name an)])))

(defn- answer
  [{:keys [ex mem body theirs read rendered-now given shown nodes] p :projection}]
  (let [said  (said-of (memory-of read) (set (keys given)))
        after {:placement (merge (select-keys (:placement mem) nodes) (:placement said))}
        live  (pe/exported-nodes ex (:sections p) nodes {})
        next  (co/base-of (tree/select body (touched read (keys given))) (:elements p))
        {:keys [create update]} (changes theirs body)]
    {:schema (-> (tree/select body (set (concat create update (stale-generators ex body))))
                 (complete live (:fields p))
                 (inlined live
                          (delay (pe/exported-nodes ex (:sections p) nodes
                                                    {:inline? true}))))
     :result body
     :theirs theirs
     :opts   {:strict?          true
              :strict-sections  shown
              :strict-projected (delay (gone ex theirs body))}
     :said   said
     :base   (into (sorted-map)
                   (for [path (keys given)] [path (select-keys next (touched read [path]))]))
     :moved  (vec (co/moved (memory-of rendered-now) after))}))

(defn absorb
  [schema nm texts {:keys [memory base shape resolve]}]
  (answer (reduce (fn [push [_ step]] (step push))
                  {:projection (known! schema nm) :schema schema :texts texts
                   :paths (vec (sort (keys texts))) :memory memory :base base :shape shape
                   :resolve resolve}
                  push-steps)))
