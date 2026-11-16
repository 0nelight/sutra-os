(ns support.projection-orakel
  (:require [sutra.types.plan :as cp]
            [sutra.kit.refusal :as refusal]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.checkout :as co]
            [sutra.parser.ent-graph :as graph]
            [sutra.parser.project-export :as pe]
            [sutra.parser.projection :as-alias p]
            [sutra.parser.sugar :as sugar]
            [clojure.string :as str]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.types.shape :as tshape]
            [sutra.types.tree :as tree]))

(set! *warn-on-reflection* true)

(defn- kw [x] (if (keyword? x) x (keyword (str x))))

(defn- known!
  [schema nm]
  (let [pr (or (registry/projection schema nm)
               (refusal/refuse! :refusal.parser/unknown-projection
                                {:kind ::p/unknown-projection :projection nm}))
        {:keys [sections project repository fields]} (:select pr)]
    {:name     (kw nm)
     :formats  (registry/formats (:filename pr))
     :sections (if (seq sections) (into #{} (map kw) sections) pe/ontology-sections)
     :fields   (into {} (for [[s fs] fields] [(kw s) (mapv kw fs)]))
     :edits    (if (seq (:edits pr)) (into #{} (map kw) (:edits pr)) #{:create :update :delete})
     :elements (into {}
                     (for [[sec fs] (:elements pr)]
                       [(kw sec) (into {} (for [[f sp] fs] [(kw f) (kw sp)]))]))
     :scope    (cond project    {:project (kw project)}
                     repository {:repository (kw repository)}
                     :else      {:all-projects? true})}))

(defn- read-back
  [{:keys [formats]} texts]
  (pe/read-texts formats (keys texts) texts))

(defn- unreadable
  [{:keys [formats]} texts]
  (into #{}
        (filter #(try (authoring/read-file formats % (get texts %)) false
                      (catch clojure.lang.ExceptionInfo _ true)))
        (filter #(authoring/file? formats %) (keys texts))))

(defn- rendering
  [schema {:keys [formats sections fields scope] :as p} memory]
  (let [rendered (into (sorted-map)
                       (map (fn [[path {:keys [content]}]] [path content]))
                       (pe/lay-out schema {:scope scope :sections sections :fields fields
                                           :formats formats :memory memory}))
        [now bad] (try [(read-back p rendered) #{}]
                       (catch clojure.lang.ExceptionInfo e
                         (let [bad (unreadable p rendered)]
                           (when (empty? bad) (throw e))
                           [(read-back p (apply dissoc rendered bad)) bad])))]
    [rendered now bad]))

(defn- shape-of [p] (co/fingerprint (:elements p)))

(defn- touched
  [read paths]
  (let [in?  (set paths)
        node (fn [k] (if (and (vector? k) (vector? (first k))) (first k) k))]
    (into #{}
          (for [part [:placement :fields :attributes]
                [k path] (get read part)
                :when (in? path)]
            (node k)))))

(defn- by-section [nodes]
  (reduce (fn [m [section nm]] (update m section (fnil conj #{}) nm)) {} nodes))

(defn- changes
  [before after]
  (let [{:keys [added changed removed]} (tree/changes before after)]
    {:create (sort (keys added)) :update (sort (keys changed)) :delete (sort (keys removed))}))

(defn- allowed!
  [{:keys [edits] :as p} before after]
  (when-let [bad (seq (for [[op nodes] (changes before after)
                            :when (and (seq nodes) (not (edits op)))]
                        [op (vec nodes)]))]
    (refusal/refuse! :refusal.parser/projection-not-allowed
                     {:kind ::p/not-allowed :edits (into {} bad)}
                     {:name (name (:name p))
                      :what (str/join "; " (for [[op nodes] bad]
                                             (str (name op) " " (pr-str nodes))))})))

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
                      (if (and (:code-file v) (not (contains? v :code))
                               (= (:code-file v) (get-in stored [n :code-file])))
                        (-> v (assoc :code (get-in @full [n :code])) (dissoc :code-file))
                        v))]
         (assoc b section (into (empty m) (for [[k v] m] [k (carry v (rid/->name k))]))))))
   body
   body))

(defn- memory-of
  [read]
  (-> (dissoc read :schema :formats :files)
      (assoc :files (update-vals (:files read) #(select-keys % [:format :owner :rule])))))

(defn- said-of
  [memory paths]
  (into {} (for [[part m] memory
                 :let [m (into {} (filter (fn [[k v]] (if (= :files part) (paths k) (paths v))))
                               m)]
                 :when (seq m)]
             [part m])))

(defn- element-label [{[section nm] :node f :field}]
  (str (name section) " " nm
       (cond (vector? f) (str " " (pr-str (first f)) " " (second f))
             (and f (not= f co/whole)) (str " " (pr-str f)))))

(defn- refuse!
  ([push kind data] (refuse! push kind data nil))
  ([{p :projection} kind data params]
   (refusal/refuse! (keyword "refusal.projection" (name kind)) (assoc data :kind kind)
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

(defn- gone
  [before after]
  (by-section (keys (:removed (tree/changes (sugar/lower-schema before)
                                            (sugar/lower-schema after))))))

(def ^:private push-steps
  [[::p/unread
    (fn [{{:keys [formats]} :projection :keys [paths] :as push}]
      (when-let [unread (seq (pe/unread-files formats paths))]
        (refuse! push ::p/unread {:files (vec (sort unread))}))
      push)]

   [::p/no-base
    (fn [{:keys [base shape] p :projection :as push}]
      (when-not (map? base)
        (refuse! push ::p/no-base {}))
      (when (not= shape (shape-of p))
        (refuse! push ::p/stale-shape {}))
      (assoc push :base (if (every? string? (keys base)) (apply merge (vals base)) base)))]

   [::p/now
    (fn [{:keys [schema memory] p :projection :as push}]
      (let [[rendered now bad] (rendering schema p memory)]
        (assoc push :rendered rendered :rendered-now now :theirs (:schema now)
               :unreadable bad)))]

   [::p/outside
    (fn [{{:keys [formats]} :projection :keys [texts paths base rendered-now] :as push}]
      (let [given (into {} (remove (comp nil? val)) texts)
            held  (set (keys base))
            alone-body (try (:schema (pe/read-texts formats (keys given) given))
                            (catch clojure.lang.ExceptionInfo _ nil))
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
          (refuse! push ::p/outside {:nodes (vec taken)} {:taken (str/join "; " taken)}))
        (assoc push :given given)))]

   [::p/read
    (fn [{:keys [rendered paths given unreadable theirs] p :projection :as push}]
      (let [others (apply dissoc rendered (concat paths unreadable))
            read   (read-back p (merge others given))]
        (assoc push :read read :mine (raised (:schema read) theirs))))]

   [::p/out-of-scope
    (fn [{:keys [mine theirs] p :projection :as push}]
      (let [shown (into (:sections p) (keys theirs))]
        (when-let [stray (seq (remove shown (keys mine)))]
          (refuse! push ::p/out-of-scope {:sections (vec (sort stray))}))
        (assoc push :shown shown)))]

   [::p/conflict
    (fn [{:keys [base mine theirs resolve] p :projection :as push}]
      (let [{:keys [body
                    conflicts]} (co/merge3 base mine theirs
                                           {:resolve resolve :elements (:elements p)})]
        (when (seq conflicts)
          (refuse! push ::p/conflict {:conflicts (vec conflicts)}
                   {:labels (str/join "; " (map element-label conflicts))}))
        (assoc push :body body)))]

   [::p/not-allowed
    (fn [{:keys [theirs body] p :projection :as push}]
      (allowed! p theirs body)
      push)]

   [::p/unsound
    (fn [{:keys [body] p :projection :as push}]
      (cp/check! (cp/of-findings
                  (cp/problems ::p/unsound
                               (pe/kinds-problems
                                (or (some-> (registry/projection body (:name p))
                                            :filename registry/formats)
                                    (:formats p)))))
                 {:code   :refusal.projection/unsound
                  :data   {:kind ::p/unsound}
                  :params (fn [bs] {:detail     (str/join "; " (map :message bs))
                                    :projection (name (:name p))})})
      push)]])

(defn- stale-generators
  [schema body]
  (let [lowered (try (sugar/lower-schema body) (catch clojure.lang.ExceptionInfo _ nil))
        live    (into {} (for [[_ o] (:object-types schema)]
                           [(some-> (:object-type-name o) rid/->name) o]))]
    (for [[an a] (:actions body)
          :let [cfg (:action-log a)
                tn  (when cfg (or (when (map? cfg) (:object-type cfg))
                                  (keyword (str (rid/->name an) "-log"))))
                gen (when tn (get-in lowered [:objects tn]))
                canon (when tn (get live (rid/->name tn)))]
          :when (and gen canon (not (contains? (:objects body) tn))
                     (not (tshape/scalars-satisfied?
                           :object-types (apply dissoc gen ents/placement-fields)
                           (apply dissoc canon ents/placement-fields))))]
      [:actions (rid/->name an)])))

(defn- answer
  [{:keys [schema memory body theirs read rendered-now given shown] p :projection}]
  (let [said  (said-of (memory-of read) (set (keys given)))
        after (co/memory-merge memory said)
        live  (pe/selected-body schema (:scope p) (:sections p))
        next  (co/base-of (tree/select body (touched read (keys given))) (:elements p))
        {:keys [create update]} (changes theirs body)]
    {:schema (-> (tree/select body (set (concat create update (stale-generators schema body))))
                 (complete live (:fields p))
                 (inlined live
                          (delay
                            (pe/selected-body schema (:scope p) (:sections p) {:inline? true}))))
     :result body
     :theirs theirs
     :opts   {:strict?          true
              :strict-sections  shown
              :strict-projected (delay (gone theirs body))}
     :memory after
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
