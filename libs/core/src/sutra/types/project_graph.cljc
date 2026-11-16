(ns sutra.types.project-graph
  (:require [sutra.types.refs :as refs]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(def ^:private skip-refs
  #{[:project] [:repository]})

(defn owners
  [schema]
  (let [repo-project (fn [r] (get-in schema [:repositories r :project]))]
    (into {}
          (for [k rid/project-owned-kinds
                [id spec] (get schema k)
                :let [p (or (:project spec) (repo-project (:repository spec)))]
                :when p]
            [id p]))))

(defn- nm [x] (when (some? x) (if (keyword? x) (name x) (str x))))

(defn- name-of [schema _kinds-key id]
  (nm (or (sch/name-of (sch/index schema) id) id)))

(defn references
  [schema]
  (for [[kinds-key refs] rid/ref-fields
        [id spec]        (get schema kinds-key)
        rspec            refs
        :when            (not (skip-refs (:at rspec)))
        [targets target] (refs/ref-values [rspec] spec)
        :when            (some? target)
        :let             [to-kind (some (fn [k] (when (get-in schema [k target]) k)) targets)]
        :when            to-kind]
    {:from-kind kinds-key :from id :to-kind to-kind :to target :slot (:at rspec)}))

(defn consumers
  [schema]
  (let [own (owners schema)]
    (reduce (fn [acc {:keys [from to]}]
              (let [pf (own from)]
                (if (and pf (not= pf (own to)))
                  (update acc (str to) (fnil conj (sorted-set)) (name-of schema :projects pf))
                  acc)))
            {}
            (references schema))))

(defn downstream
  [schema ids]
  (let [users (reduce (fn [m {:keys [from to]}] (update m (str to) (fnil conj #{}) (str from)))
                      {} (references schema))]
    (loop [seen (set (map str ids)) todo (seq seen)]
      (if-let [[x & more] todo]
        (let [new (remove seen (get users x))]
          (recur (into seen new) (seq (concat more new))))
        seen))))

(defn- action-edits
  [schema action]
  (let [steps (refs/steps (:rules action))
        obj   (into #{} (keep :object-type) steps)
        lnk   (into #{} (keep :link-type) steps)]
    (into obj
          (mapcat (fn [l]
                    (when-let [spec (get-in schema [:link-types l])]
                      (keep spec [:from-object-type :to-object-type]))))
          lnk)))

(defn graph
  [schema]
  (let [own      (owners schema)
        declared (into {} (for [[rid p] (:projects schema) :when (seq (:imports p))]
                            [rid (set (:imports p))]))
        pname    (fn [rid] (when rid (name-of schema :projects rid)))
        refs     (references schema)
        crossing (fn [{:keys [from to]}]
                   (let [pf (own from) pt (own to)]
                     (when (and pt (not= pf pt)) [pf pt])))
        by-pair  (reduce (fn [acc {:keys [from-kind from to-kind to slot] :as r}]
                           (if-let [pair (crossing r)]
                             (update acc pair (fnil conj (sorted-set))
                                     (str (name from-kind) " " (name-of schema from-kind from)
                                          " " (pr-str slot) " → " (name-of schema to-kind to)))
                             acc))
                         {} refs)
        demand   (reduce (fn [acc {:keys [from to to-kind] :as r}]
                           (if (crossing r)
                             (update acc [to to-kind] (fnil conj []) (own from))
                             acc))
                         {} refs)
        edge     (fn [[pf pt] reasons]
                   {:from       (pname pf)
                    :to         (pname pt)
                    :declared   (when (and pf (declared pf))
                                  (contains? (declared pf) pt))
                    :references (count reasons)
                    :reasons    (vec reasons)})]
    {:edges      (vec (for [[pair reasons] (sort-by (comp str key) by-pair)] (edge pair reasons)))
     :declared   (into (sorted-map)
                       (for [[rid imps] declared]
                         [(pname rid) (vec (sort (map pname imps)))]))
     :undeclared (vec (for [[[pf pt] reasons] (sort-by (comp str key) by-pair)
                            :when (and pf (not (contains? (get declared pf #{}) pt)))]
                        {:project (pname pf) :imports (pname pt) :reasons (vec reasons)}))
     :without-project (vec (for [[[pf pt] reasons] (sort-by (comp str key) by-pair)
                                 :when (nil? pf)]
                             {:to (pname pt) :references (count reasons) :reasons (vec reasons)}))
     :stale      (vec (for [[pf imps] (sort-by (comp str key) declared)
                            imp (sort imps)
                            :when (not (contains? by-pair [pf imp]))]
                        {:project (pname pf) :import (pname imp)}))
     :shared     (vec (for [[[rid kind] bys] (sort-by (comp - count val) demand)]
                        {:resource   (name-of schema kind rid)
                         :kind       (name kind)
                         :owner      (pname (own rid))
                         :wanted-by  (vec (sort (map #(or (pname %) "unassigned") (distinct bys))))
                         :references (count bys)}))
     :actions    (vec (for [[an action] (sort-by (comp str key) (:actions schema))
                            :when (seq (:rules action))
                            :let  [edits (action-edits schema action)
                                   ps    (into (sorted-set) (keep #(pname (own %))) edits)]]
                        {:action   (nm (or (:action-name action) an))
                         :owner    (pname (:project action))
                         :edits    (vec (sort (map #(name-of schema :object-types %) edits)))
                         :projects (vec ps)}))}))
