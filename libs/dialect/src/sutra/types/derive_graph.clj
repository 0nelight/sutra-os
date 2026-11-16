(ns sutra.types.derive-graph
  (:require [clojure.string :as str]
            [sutra.types.expr :as expr]
            [sutra.types.query :as q]))

(set! *warn-on-reflection* true)

(def ^:private far-slots
  #{:of :by :pick :where :pred})

(def ^:private far-self-slots
  #{:pred})

(def ^:private near-slots
  #{:values})

(defn- slot-attrs
  [v]
  (cond
    (keyword? v) #{v}
    (vector? v)  (expr/field-refs v)
    :else        #{}))

(defn far-reads
  [far-of T e]
  (letfn [(walk [T e]
            (when (vector? e)
              (let [[op & args] e]
                (if (q/link-slots op)
                  (let [slots (q/link-args op args)
                        over  (:over slots)
                        far   (and over (far-of T over))]
                    (concat
                     (when far
                       (for [[slot v] slots
                             :when    (far-slots slot)
                             attr     (slot-attrs v)]
                         {:near T :handle over :far far :attr attr :slot slot}))
                     (when far
                       (for [[slot v] slots
                             :when    (far-self-slots slot)
                             r        (walk far v)]
                         r))
                     (for [[slot v] slots
                           :when    (near-slots slot)
                           r        (if (and (vector? v) (not (keyword? (first v))))
                                      (mapcat #(walk T %) v)
                                      (walk T v))]
                       r)))
                  (mapcat #(walk T %) args)))))]
    (distinct (walk T e))))

(defn handles
  [e]
  (letfn [(walk [e]
            (when (vector? e)
              (let [[op & args] e]
                (if (q/link-slots op)
                  (let [slots (q/link-args op args)]
                    (cons (:over slots)
                          (mapcat (fn [[_ v]]
                                    (when (vector? v)
                                      (if (keyword? (first v)) (walk v) (mapcat walk v))))
                                  slots)))
                  (mapcat walk args)))))]
    (into #{} (remove nil?) (walk e))))

(defn- derived? [derived-of T a]
  (boolean (and T (some #(= a (first %)) (derived-of T)))))

(defn deps-of
  [derived-of far-of T e]
  (into (into #{} (comp (filter #(derived? derived-of T %)) (map #(vector T %)))
              (expr/field-refs e))
        (comp (filter (fn [{:keys [far attr]}] (derived? derived-of far attr)))
              (map (juxt :far :attr)))
        (far-reads far-of T e)))

(defn graph
  [types derived-of far-of]
  (into {}
        (for [T types
              [a e] (derived-of T)]
          [[T a] (deps-of derived-of far-of T e)])))

(defn cycle-from
  [path node]
  (vec (drop-while #(not= % node) path)))

(defn cycles
  [graph]
  (let [state (volatile! {})
        found (volatile! [])]
    (letfn [(dfs [node path]
              (case (get @state node)
                :done nil
                :open (vswap! found conj (cycle-from path node))
                (do (vswap! state assoc node :open)
                    (doseq [d (get graph node)] (dfs d (conj path node)))
                    (vswap! state assoc node :done))))]
      (doseq [n (sort-by str (keys graph))] (when-not (get @state n) (dfs n []))))
    (distinct @found)))

(defn cycle-index
  [graph]
  (into {} (for [chain (cycles graph), node chain] [node chain])))

(defn far-needs
  [derived-of far-of T derived]
  (->> (mapcat (fn [[_ e]] (far-reads far-of T e)) derived)
       (filter (fn [{:keys [far attr]}] (derived? derived-of far attr)))
       (group-by (juxt :near :handle))
       (map (fn [[[near handle] sites]]
              {:near  near
               :handle handle
               :far    (:far (first sites))
               :attrs  (into (sorted-set) (map :attr) sites)}))
       (sort-by (juxt (comp str :near) (comp str :handle)))
       vec))

(defn chain-plan
  [derived-of far-of T derived]
  (let [
        level-of (fn [S attrs] (expr/eval-order (derived-of S) attrs))]
    (loop [frontier (far-needs derived-of far-of T derived)
           seen     #{}
           levels   0
           sides    #{}
           nodes    #{}]
      (let [frontier (remove (fn [{:keys [far attrs]}]
                               (every? #(seen [far %]) attrs))
                             frontier)]
        (if (empty? frontier)
          {:levels levels :sides sides :nodes nodes}
          (let [computed (for [{:keys [far attrs]} frontier]
                           [far (level-of far attrs)])
                reached  (into #{} (mapcat (fn [[far pairs]] (map #(vector far (first %)) pairs)))
                               computed)]
            (recur (mapcat (fn [[far pairs]] (far-needs derived-of far-of far pairs)) computed)
                   (into seen reached)
                   (inc levels)
                   (into sides (map (juxt :near :handle)) frontier)
                   (into nodes reached))))))))

(defn cycle-message
  [label-of chain]
  (str "derived cycle "
       (str/join " → " (map label-of chain)) " → " (label-of (first chain))
       " — a derived property is computed where it is read, so a chain that comes back to"
       " where it started can never start: no member of it can be computed first. That"
       " holds inside a type and across a link alike, and a link that reaches the type"
       " itself (a hierarchy, a parent of the same type) closes the chain on its first"
       " hop. Break the chain, or have a transform compute one member and store it"
       " (a supplement, :role :supp), so the read finds a value there instead of a"
       " computation."))
