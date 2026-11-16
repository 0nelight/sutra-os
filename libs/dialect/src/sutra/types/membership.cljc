(ns sutra.types.membership
  (:require [sutra.types.transform-io :as tio]
            [sutra.types.kinds :as kinds]
            [sutra.types.entities :as ents]))

(def member-tags
  (into #{} (for [e ents/entities :when (contains? (:fields e) :project)] (:tag e))))

(def author-sections
  (into {}
        (for [e ents/entities :when (member-tags (:tag e))]
          [(:tag e) (or (:author e) (:section e))])))

(def installed-sections
  (into {} (for [e ents/entities :when (member-tags (:tag e))] [(:tag e) (:section e)])))

(def ^:private in-repository
  (into #{} (for [e ents/entities :when (contains? (:fields e) :repository)] (:tag e))))

(def platform-datasets
  #{:dataset_consumption :dataset_transactions :jobs :link_edits
    :llm_call_log :llm_model_rates :object_edits})

(def derivations
  (merge
   (into {} (for [t in-repository] [t [:repository]]))
   {:transform   [:repository :output-target :input-dataset]
    :dataset     [:producer :backed-type]
    :action      [:rule-object-type :rule-link :param-object-type :rule-function]
    :object-type [:log-of-action]
    :link-type   [:link-source-type]
    :object-set  [:root-object-type]
    :application [:viewed-object-type :repository]}))

(def order
  (let [first-ones [:action :object-type :link-type :object-set :application]]
    (into first-ones (sort (remove (set first-ones) (keys derivations))))))

(declare project-of)

(def sources
  {:rule-object-type   (fn [_ spec {:keys [rules type-project]}]
                         (some #(some-> (:object-type %) type-project) (rules spec)))
   :rule-link          (fn [_ spec {:keys [rules link-project]}]
                         (some #(some-> (or (:link %) (:link-type %)) link-project) (rules spec)))
   :param-object-type  (fn [_ spec {:keys [params type-project]}]
                         (some #(some-> (:type %) type-project) (params spec)))
   :rule-function      (fn [_ spec {:keys [rules function-project]}]
                         (some #(some-> (:function %) function-project) (rules spec)))
   :log-of-action      (fn [nm _ {:keys [log-project]}] (log-project nm))
   :link-source-type   (fn [_ spec {:keys [link-from type-project]}]
                         (some-> (link-from spec) type-project))
   :viewed-object-type (fn [_ spec {:keys [viewed-type type-project]}]
                         (some-> (viewed-type spec) type-project))
   :root-object-type   (fn [_ spec {:keys [root-type type-project]}]
                         (some-> (root-type spec) type-project))
   :repository         (fn [_ spec {:keys [repository-project]}]
                         (some-> (:repository spec) repository-project))
   :output-target      (fn [_ spec {:keys [type-project link-project dataset-project]}]
                         (let [of {:object-types type-project :link-types link-project
                                   :datasets dataset-project}]
                           (some (fn [o]
                                   (let [[sec v] (kinds/output-target o)]
                                     (some-> v ((of sec (constantly nil))))))
                                 (tio/outputs-of spec))))
   :input-dataset      (fn [_ spec {:keys [dataset-project]}]
                         (some #(some-> (:dataset %) dataset-project) (tio/inputs-of spec)))
   :producer           (fn [nm _ lens]
                         (some #(project-of :transform % lens) ((:producers lens) nm)))
   :backed-type        (fn [nm spec lens]
                         (when (= :object-backing (keyword (:kind spec)))
                           (when-let [t ((:backed-type lens) nm)]
                             (project-of :object-type t lens))))})

(def place-optional
  #{:graph})

(defn derived
  [tag [nm spec] lens]
  (some #((sources %) nm spec lens) (derivations tag)))

(defn project-of
  [tag [_ spec :as entry] lens]
  (or (:project spec) (derived tag entry lens)))

(declare unplaced? without-project)

(defn- project-rid
  [after project]
  (let [nm (if (keyword? project) (name project) (str project))]
    (some (fn [[rid spec]] (when (or (= (str project) (str rid))
                                     (= nm (some-> (:project-name spec) name)))
                             rid))
          (:projects after))))

(defn place-new
  [lens-of before after project]
  (let [p      (project-rid after project)
        added? (fn [tag [k _]] (not (contains? (get before (installed-sections tag)) k)))]
    (if-not p
      after
      (reduce (fn [w [tag [k _ :as entry]]]
                (if (added? tag entry) (assoc-in w [(installed-sections tag) k :project] p) w))
              after
              (without-project (lens-of after))))))

(def ^:private section-tags
  (into {} (map (fn [[t s]] [s t])) installed-sections))

(defn place-added
  [lens-of after project nodes]
  (let [p (project-rid after project)]
    (if-not p
      after
      (let [lens (lens-of after)]
        (reduce (fn [w [sec k :as node]]
                  (let [tag (section-tags sec)]
                    (if (and tag (unplaced? lens tag [k (get-in after node)]))
                      (assoc-in w [sec k :project] p)
                      w)))
                after (sort-by str nodes))))))

(defn- unplaced?
  [lens tag entry]
  (let [nm ((:name-of lens) tag entry)]
    (and (map? (second entry))
         (not (place-optional tag))
         (nil? (project-of tag entry lens))
         (not (and (= :dataset tag) (platform-datasets nm))))))

(defn without-project
  [lens]
  (for [tag   member-tags
        entry ((:entries lens) tag)
        :when (unplaced? lens tag entry)]
    [tag entry]))
