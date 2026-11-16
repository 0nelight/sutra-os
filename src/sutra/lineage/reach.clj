(ns sutra.lineage.reach
  (:require [clojure.string :as str]
            [sutra.lineage.facts :as facts]
            [sutra.parser.checkout :as ck]
            [sutra.parser.impact :as impact]
            [sutra.types.rid :as rid]
            [sutra.types.tree :as tree]))

(set! *warn-on-reflection* true)

(def ^:private text-section
  {:objects :object-types :links :link-types})

(defn node-id-of
  [schema]
  (let [known (facts/named-nodes schema)]
    (fn [[section n]]
      (let [s (keyword (rid/->name section))]
        (facts/known-node known (get text-section s s) n)))))

(defn node-ids
  [schema nodes]
  (into #{} (keep (node-id-of schema)) nodes))

(defn- typed
  [xs]
  (when (sequential? xs)
    (mapv (fn [x]
            (cond
              (map? x)        [(:attribute-name x) (:attribute-type x)]
              (sequential? x) (vec (take 2 x))
              :else           x))
          xs)))

(defn- function-contract
  [spec]
  {:inputs (:inputs spec)
   :params (impact/function-params (:source spec))
   :edits  (:edits-types (impact/function-references (:source spec)))})

(def ^:private contract-by-section
  {:object-types (fn [spec] (-> (select-keys spec [:primary-key :implements])
                                (assoc :attributes (typed (:attribute-list spec)))))
   :objects      (fn [spec] (-> (select-keys spec [:primary-key :implements])
                                (assoc :attributes (typed (:attributes spec)))))
   :datasets     (fn [spec] (-> (select-keys spec [:primary-key])
                                (assoc :columns (typed (:columns spec)))))
   :link-types   #(select-keys % [:from-object-type :to-object-type :cardinality])
   :links        identity
   :interfaces   #(select-keys % [:property-list :link-constraints])
   :actions      #(select-keys % [:params])
   :functions    function-contract
   :transforms   #(select-keys % [:inputs :output :output-columns])})

(defn- contract-of
  [section spec]
  (when-let [f (contract-by-section section)] (f spec)))

(defn contracts
  [schema]
  (let [known (facts/named-nodes schema)
        specs (concat
               (for [section [:object-types :datasets :link-types :actions :functions]
                     [k spec] (get schema section)]
                 [section k spec])
               (for [[_ t] (:transforms schema) :when (:function-name t)]
                 [:functions (:function-name t) t]))]
    (into {}
          (for [[section k spec] specs
                :let [id (facts/known-node known section (or (:action-name spec) k))]
                :when id]
            [id (ck/fingerprint (contract-of section spec))]))))

(defn- direct-inputs
  [es ids]
  (distinct (for [{:keys [from to]} es :when (and (ids to) (not (ids from)))] from)))

(defn around
  [schema ids]
  (let [have (contracts schema)]
    (vec (sort (for [id   (direct-inputs (facts/edges schema nil) (set ids))
                     :let [fp (get have id)]
                     :when fp]
                 [id fp])))))

(defn- logic-of
  [schema es]
  (let [known  (facts/named-nodes schema)
        code   (into {} (for [[k t] (:transforms schema)]
                          [(facts/known-node known :transforms (or (:transform-name t) k))
                           (select-keys t [:code :query])]))
        writes (group-by :to (filter #(= :output (:rel %)) es))]
    (into {} (for [[id ws] writes]
               [id (ck/fingerprint (sort-by pr-str (map #(get code (:from %)) ws)))]))))

(defn logic-around
  [schema ids]
  (let [es   (facts/edges schema nil)
        have (logic-of schema es)]
    (vec (sort (for [id   (direct-inputs es (set ids))
                     :let [fp (get have id)]
                     :when fp]
                 [id fp])))))

(defn logic-drifted
  [schema pulled]
  (let [now (logic-of schema (facts/edges schema nil))]
    (vec (sort (for [[id fp] pulled :when (not= fp (get now id))] id)))))

(defn drifted
  [schema pulled]
  (let [now (contracts schema)]
    (vec (sort (for [[id fp] pulled :when (not= fp (get now id))] id)))))

(defn- node-changes
  [sent next]
  (let [sent     (update-keys (or sent {}) rid/->name)
        elements (fn [base paths]
                   (apply merge-with merge {} (map #(get base %) paths)))
        paths    (map rid/->name (keys next))
        before   (elements sent paths)
        after    (elements (update-keys next rid/->name) paths)]
    (vec (for [node (distinct (concat (keys before) (keys after)))
               :when (not= (get before node) (get after node))]
           [node (get before node) (get after node)]))))

(defn changed-nodes
  [sent next]
  (mapv first (node-changes sent next)))

(defn contract-nodes
  [before after nodes]
  (let [old (tree/index before)
        new (tree/index after)
        sig (fn [idx [section :as node]]
              (when-let [e (get idx node)]
                [(contract-of (keyword (rid/->name section)) (:value e))]))]
    (vec (remove (fn [node] (= (sig old node) (sig new node))) nodes))))

(defn downstream
  [schema ids]
  (let [ids   (set ids)
        out   (reduce (fn [m {:keys [from to]}] (update m from (fnil conj #{}) to))
                      {} (facts/edges schema nil))
        step  (fn [from] (into #{} (comp (mapcat out) (remove ids)) from))
        one   (step ids)
        two   (remove one (step one))]
    {:direct (vec (sort one)) :further (count two)}))

(defn inputs-around
  [schema trunk nodes]
  (let [of    (fn [s] (around s (node-ids s nodes)))
        logic (fn [s] (logic-around s (node-ids s nodes)))]
    (cond-> {:branch (of schema) :logic {:branch (logic schema)}}
      trunk (-> (assoc :main (of trunk))
                (assoc-in [:logic :main] (logic trunk))))))

(defn- moved
  [what drift where s pulled]
  (for [id (drift s pulled)] {:node id :where where :what what}))

(defn inputs-changed
  [schema trunk {:keys [branch main logic]}]
  (vec (concat
        (moved "contract" drifted "branch" schema branch)
        (moved "logic" logic-drifted "branch" schema (:branch logic))
        (when trunk
          (concat
           (moved "contract" drifted "main" trunk main)
           (moved "logic" logic-drifted "main" trunk (:main logic)))))))

(defn report
  [schema trunk around {:keys [sent next before after]}]
  (let [changed (changed-nodes sent next)
        ids     (node-ids schema changed)
        hit     (node-ids schema (contract-nodes before after changed))
        calm    (set (remove hit ids))]
    (cond-> {}
      (seq hit)  (assoc :downstream (downstream schema hit))
      (seq calm) (assoc :readers-unaffected (count (:direct (downstream schema calm))))
      around     (assoc :inputs-changed (inputs-changed schema trunk around)))))

(defn feeds-line
  [schema ids]
  (let [{:keys [direct further]} (downstream schema (set ids))]
    (when (seq direct)
      (str "sutra: feeds " (count direct) " node(s) directly"
           (when (pos? further) (str ", " further " more one step on")) ": "
           (str/join ", " (take 8 direct))
           (when (< 8 (count direct)) " …")))))
