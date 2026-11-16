(ns sutra.viz.lineage-projection
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.types.languages :as langs]))

(def transform-language-aliases
  (disj (set (keys langs/compute-languages)) :clojure))

(def node-kinds
  [{:kind :datasource  :inventory :datasources  :scope :datasources
    :label "Data source" :color "#238551" :icon "⛁"}
   {:kind :dataset     :inventory :datasets     :scope :datasets
    :label "Dataset" :color "#f0b726" :icon "▤"}
   {:kind :object-type :inventory :object-types :scope :object-types :bare? true
    :label "Object type" :color "#2d72d2"}
   {:kind :transform   :aliases transform-language-aliases :scope :transforms
    :label "Transform" :color "#c87619" :icon "ƒ"}
   {:kind :application :inventory :applications :scope :applications
    :label "Application" :color "#7961db" :icon "▣"}
   {:kind :action      :inventory :actions :scope :actions
    :label "Action type" :color "#1c2127" :icon "⚒"}
   {:kind :writeback   :inventory :writebacks :scope :writebacks
    :label "Writeback dataset" :color "#d33d17" :icon "✎"}
   {:kind :function    :scope :functions
    :label "Function" :color "#00a396" :icon "▶"}
   {:kind :automation  :scope :automations
    :label "Automation" :color "#9d3f9d" :icon "⟳"}
   {:kind :agent       :scope :agents
    :label "AI Platform agent" :color "#147eb3" :icon "◈"}
   {:kind :model       :scope :models
    :label "Language model" :color "#db2c6f" :icon "◇"}])

(def ^:private by-kind
  (into {} (for [{:keys [kind aliases] :as row} node-kinds
                 k (cons kind aliases)]
             [k row])))

(defn row [kind] (by-kind kind))

(def rendered-kinds
  (filterv :inventory node-kinds))

(def ^:private bare-kind
  (:kind (some #(when (:bare? %) %) node-kinds)))

(def scope-of
  (into {} (keep (fn [{:keys [kind scope]}] (when scope [kind scope]))) node-kinds))

(defn node-id
  [kind k]
  (let [{:keys [bare?] :as row} (by-kind kind)]
    (if bare? (name k) (str (name (:kind row kind)) "/" (name k)))))

(defn status-key
  [kind k]
  (str (name (:kind (by-kind kind) kind)) "/" (name k)))

(defn parse-node-id
  [id]
  (let [s (str id)
        i (str/index-of s "/")]
    (if (nil? i)
      (when bare-kind [bare-kind (keyword s)])
      (when-let [kind (:kind (by-kind (keyword (subs s 0 i))))]
        [kind (keyword (subs s (inc i)))]))))

(defn of-kind
  [edge k kinds]
  (into [] (comp (filter #(kinds (:kind %))) (keep :name)) (get edge k)))

(declare port-nodes)

(def ^:private port-kinds
  {:dataset     {:node :dataset :sides #{:in :out} :lands? true}
   :media-set   {:node :dataset :sides #{:in :out}}
   :object-type {:node :object-type :sides #{:in :out}}
   :datasource  {:node :datasource :sides #{:in} :source? true}
   :link-type   {:sides #{:out}
                 :nodes (fn [{:keys [links]} {:keys [name]}]
                          (let [c (get links name)]
                            (for [ot [(:from c) (:to c)]] [:object-type ot])))}
   :transform   {:sides #{:in}
                 :nodes (fn [{:keys [edges] :as ctx} {:keys [name]}]
                          (mapcat #(port-nodes ctx :out %) (:outputs (get edges name))))}})

(defn- port-ctx
  [{:keys [links all-edges]}]
  {:links (coll/index-by :name links) :edges (into {} all-edges)})

(defn port-nodes
  [ctx side port]
  (let [{:keys [node nodes sides]} (port-kinds (:kind port))]
    (when (contains? sides side)
      (remove (comp nil? second)
              (if nodes (nodes ctx port) [[node (:name port)]])))))

(defn- port-is? [facet port] (get-in port-kinds [(:kind port) facet]))

(defn- named [kind pairs] (for [[k n] pairs :when (= kind k)] n))

(defn output-object-types
  [links edge]
  (let [ctx (port-ctx {:links links})]
    (set (mapcat #(named :object-type (port-nodes ctx :out %)) (:outputs edge)))))

(defn transform-dataset-kws
  [trs]
  (into #{} (concat (mapcat (fn [[_ e]] (of-kind e :inputs #{:dataset})) trs)
                    (mapcat (fn [[_ e]] (of-kind e :outputs #{:dataset})) trs))))

(defn- source-landing-producers
  [all-edges dataset-kws]
  (let [want          (set dataset-kws)
        lands-source? (fn [e] (or (seq (:external-systems e))
                                  (seq (of-kind e :inputs #{:datasource}))))
        lands-wanted? (fn [e] (some want (of-kind e :outputs #{:dataset})))]
    (filter (fn [[_ e]] (and (lands-source? e) (lands-wanted? e))) all-edges)))

(defn- scoped? [scope kind kw]
  (contains? (get scope kind) kw))

(defn transforms-in-scope
  [all-edges scope]
  (let [seeded (filter (fn [[k _]] (scoped? scope :transforms k)) all-edges)
        extra  (source-landing-producers all-edges (transform-dataset-kws seeded))]
    (distinct (concat seeded extra))))

(defn inventory
  [{:keys [links datasources]} trs scope]
  (let [ctx (port-ctx {:links links})]
    {:actions      (set (get scope :actions))
     :applications (set (get scope :applications))
     :writebacks   (set (get scope :writebacks))
     :datasets
     (into (set (get scope :datasets)) (transform-dataset-kws trs))
     :object-types
     (into (set (get scope :object-types))
           (for [[_ e] trs [side ports] [[:in (:inputs e)] [:out (:outputs e)]]
                 p ports
                 o (named :object-type (port-nodes ctx side p))
                 :when (or (= :out side) (= :object-type (:kind p)))]
             o))
     :datasources
     (into (set (filter #(scoped? scope :datasources %) datasources))
           (for [[_ e] trs
                 :when (some #(and (port-is? :lands? %) (:name %)) (:outputs e))
                 src (concat (:external-systems e)
                             (keep :name (filter #(port-is? :source? %) (:inputs e))))]
             src))}))

(defn visible-inventory
  [inv hidden]
  (let [hidden (set hidden)]
    (reduce (fn [inv {:keys [kind inventory]}]
              (cond-> inv
                (contains? inv inventory)
                (update inventory #(into #{} (remove (fn [k] (hidden (node-id kind k)))) %))))
            inv rendered-kinds)))

(defn rendered-node-ids
  [inv]
  (into #{}
        (mapcat (fn [{:keys [kind inventory]}]
                  (map #(node-id kind %) (get inv inventory))))
        rendered-kinds))

(defn- edge-id [t-kw s t] (str "lineage/" s "=>" t "@" (name t-kw)))

(defn edges
  [ctx trs node-ids]
  (let [ctx     (port-ctx ctx)
        ids     (fn [side port] (map (fn [[k n]] (node-id k n)) (port-nodes ctx side port)))
        out-ids (partial ids :out)
        in-ids  (partial ids :in)]
    (->> trs
         (mapcat
          (fn [[t-kw edge]]
            (let [outs         (:outputs edge)
                  all-tgts     (distinct (mapcat out-ids outs))
                  dataset-tgts (distinct (mapcat out-ids (filter #(port-is? :lands? %) outs)))
                  data-srcs    (distinct (mapcat in-ids
                                                 (remove #(port-is? :source? %) (:inputs edge))))
                  source-srcs  (distinct (concat
                                          (map #(node-id :datasource %) (:external-systems edge))
                                          (mapcat in-ids
                                                  (filter #(port-is? :source? %)
                                                          (:inputs edge)))))]
              (concat
               (for [s data-srcs   t all-tgts     :when (and s t (not= s t))]
                 {:id (edge-id t-kw s t) :source s :target t :transform (name t-kw)})
               (for [s source-srcs t dataset-tgts :when (and s t (not= s t))]
                 {:id (edge-id t-kw s t) :source s :target t :transform (name t-kw)})))))
         (filter (fn [e] (and (contains? node-ids (:source e))
                              (contains? node-ids (:target e)))))
         (group-by :id) vals (mapv first))))

(defn action-edges
  [action-edits node-ids]
  (vec (for [[a ots] action-edits
             :let [aid (node-id :action a)]
             :when (contains? node-ids aid)
             ot (sort ots)
             :let [oid (node-id :object-type ot)]
             :when (contains? node-ids oid)]
         {:id (str "lineage/" oid "=>" aid "@edits") :source oid :target aid :edits true})))

(defn app-edges
  [app-uses node-ids]
  (vec (for [[a ots] app-uses
             :let [aid (node-id :application a)]
             :when (contains? node-ids aid)
             ot (sort ots)
             :let [oid (node-id :object-type ot)]
             :when (contains? node-ids oid)]
         {:id (str "lineage/" oid "=>" aid "@surfaces") :source oid :target aid})))

(defn writeback-edges
  [node-ids]
  (vec (for [id node-ids
             :let [[kind ot] (parse-node-id id)]
             :when (= :writeback kind)
             :let [oid (node-id :object-type ot)]
             :when (contains? node-ids oid)]
         {:id (str "lineage/" id "=>" oid "@writeback") :source id :target oid})))

(defn media-set-datasets
  [all-edges]
  (into #{} (for [[_ e] all-edges
                  side [:inputs :outputs]
                  io (get e side)
                  :when (= :media-set (:kind io))
                  :let [n (:name io)]
                  :when n]
              n)))

(defn project
  [{:keys [all-edges scope hidden action-edits app-uses] :as ctx}]
  (let [trs   (transforms-in-scope all-edges scope)
        inv   (visible-inventory (inventory ctx trs scope) hidden)
        ids   (rendered-node-ids inv)
        nodes (vec (for [{:keys [kind inventory]} rendered-kinds
                         k (sort (get inv inventory))]
                     {:id (node-id kind k) :kind kind :key k :label (name k)}))]
    {:nodes nodes
     :edges (vec (sort-by :id (concat (edges (assoc ctx :all-edges all-edges) trs ids)
                                      (action-edges action-edits ids)
                                      (app-edges app-uses ids)
                                      (writeback-edges ids))))}))
