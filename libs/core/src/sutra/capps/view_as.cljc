(ns sutra.capps.view-as
  (:require [sutra.kit.tree :as tree]
            [clojure.set]
            [sutra.capps.walk :as walk]
            [sutra.capps.visibility :as vis]))

(def pane-slots
  [:tabs :pages])

(defn- id-label [id] (if (keyword? id) (name id) (str id)))

(defn- named? [x] (or (keyword? x) (string? x)))

(defn audiences
  [tree]
  (let [ms       (tree/nodes tree)
        roles    (into #{} (comp (mapcat #(get-in % [:visibility :roles])) (filter named?)) ms)
        profiles (into #{} (comp (mapcat #(get-in % [:visibility :profiles])) (filter named?)) ms)
        entries  (into [] (comp (mapcat :view-as)
                                (filter map?)
                                (filter (comp named? :id)))
                       ms)
        labels   (into {} (comp (filter (comp string? :label)) (map (juxt :id :label))) entries)
        ids      (sort-by id-label (distinct (concat roles profiles (map :id entries))))]
    (mapv (fn [id]
            {:id       id
             :label    (get labels id (id-label id))
             :role?    (contains? roles id)
             :profile? (contains? profiles id)
             :view-as? (some? (some #(= id (:id %)) entries))})
          ids)))

(defn viewer
  [id]
  (when (some? id) {:role id :profile id}))

(defn- audience-facets
  [pane]
  (if-let [v (:visibility pane)]
    (assoc pane :visibility (dissoc v :when :link))
    pane))

(defn pane-visible-for?
  [pane id]
  (vis/pane-visible? (audience-facets pane) (viewer id)))

(defn prune
  [tree id]
  (if (nil? id)
    tree
    (walk/postwalk
     (fn [x]
       (if (map? x)
         (reduce (fn [n k]
                   (if (walk/children? (get n k))
                     (assoc n k (filterv #(or (not (map? %)) (pane-visible-for? % id))
                                         (get n k)))
                     n))
                 x pane-slots)
         x))
     tree)))

(defn hidden-panes
  [tree id]
  (if (nil? id)
    []
    (vec (for [m    (tree/nodes tree)
               k    pane-slots
               pane (when (sequential? (get m k)) (get m k))
               :when (and (map? pane) (not (pane-visible-for? pane id)))]
           {:slot  k
            :id    (:id pane)
            :label (or (:label pane)
                       (when (named? (:id pane)) (id-label (:id pane)))
                       "—")}))))

(defn audience-split
  [tree]
  (let [ids (mapv :id (audiences tree))]
    (when (> (count ids) 1)
      (vec
       (for [m    (tree/nodes tree)
             slot pane-slots
             :let [panes (get m slot)]
             :when (and (sequential? panes) (> (count panes) 1))
             :let [seen  (for [p panes
                               :when (and (map? p) (seq (:visibility p)))]
                           {:id  (:id p)
                            :for (into #{} (filter #(pane-visible-for? p %)) ids)})
                   named (filterv (comp seq :for) seen)]
             :when (and (> (count named) 1)
                        (some (fn [a]
                                (some #(empty? (clojure.set/intersection (:for a) (:for %)))
                                      named))
                              named))]
         {:slot   slot
          :groups (vec (distinct (map :for named)))
          :panes  (mapv (fn [p] {:id (:id p) :for (vec (sort-by id-label (:for p)))})
                        named)})))))

(def max-profiles
  10)

(defn- empty-body?
  [b]
  (or (nil? b)
      (and (map? b) (empty? b))
      (and (map? b) (contains? b :kind)
           (every? #(empty? (get b %)) [:items :body :tabs :pages :variants]))))

(defn- pane-label
  [pane]
  (or (:label pane) (when (named? (:id pane)) (id-label (:id pane))) "—"))

(defn blind-audiences
  [tree]
  (vec (for [{:keys [id]} (audiences tree)
             m    (tree/nodes (prune tree id))
             slot pane-slots
             :let [panes (get m slot)]
             :when (and (sequential? panes) (empty? panes))]
         {:audience id :slot slot :select (:select m)})))

(defn empty-panes
  [tree]
  (vec (for [m    (tree/nodes tree)
             slot pane-slots
             pane (when (sequential? (get m slot)) (get m slot))
             :when (and (map? pane) (empty-body? (:body pane)))]
         {:slot slot :id (:id pane) :label (pane-label pane)})))
