(ns sutra.calx.graph
  (:require [clojure.string :as str]
            [sutra.calx.ast :as ast]))

(set! *warn-on-reflection* true)

(def empty-graph {:precedents {} :cells {} :cols {} :wide {} :names {} :volatile #{}})

(def ^:private wide-threshold 64)

(defn- formula-deps
  [ast host name-ast]
  (loop [todo [ast] seen-names #{} deps #{}]
    (if-let [node (first todo)]
      (let [refs (ast/refs node)
            deps (into deps
                       (map (fn [r]
                              (let [{:keys [sheet r1 c1 r2 c2]} (ast/ref->area r host)]
                                (if (and (= r1 r2) (= c1 c2))
                                  [:cell sheet r1 c1]
                                  [:area sheet r1 c1 r2 c2]))))
                       (remove :sheet-name refs))
            used (->> (concat (map (comp str/upper-case :name)
                                   (remove :sheet-name (ast/names node)))
                              (ast/calls node))
                      (remove seen-names)
                      distinct)
            name-asts (keep (fn [n] (when-let [a (name-ast n host)] [n a])) used)]
        (recur (into (vec (rest todo)) (map second name-asts))
               (into seen-names used)
               (into deps (map (fn [[n _]] [:name n])) name-asts)))
      deps)))

(defn- update-index [g dep fkey f]
  (case (first dep)
    :cell (let [[_ s r c] dep] (update-in g [:cells [s r c]] f fkey))
    :area (let [[_ s _ c1 _ c2] dep]
            (if (> (- c2 c1) wide-threshold)
              (update-in g [:wide s] f fkey)
              (reduce (fn [g col] (update-in g [:cols [s col]] f fkey)) g (range c1 (inc c2)))))
    :name (update-in g [:names (second dep)] f fkey)))

(defn- conj-set [s x] (conj (or s #{}) x))
(defn- disj-set [s x] (let [s (disj (or s #{}) x)] (when (seq s) s)))

(defn remove-formula [g fkey]
  (let [deps (get-in g [:precedents fkey])
        g (reduce (fn [g dep] (update-index g dep fkey disj-set)) g deps)]
    (-> g
        (update :precedents dissoc fkey)
        (update :volatile disj fkey))))

(defn add-formula
  [g fkey ast name-ast]
  (let [g (remove-formula g fkey)
        deps (formula-deps ast (first fkey) name-ast)
        g (reduce (fn [g dep] (update-index g dep fkey conj-set)) g deps)
        vol? (or (ast/volatile? ast)
                 (some (fn [[t n]]
                         (and (= t :name) (some-> (name-ast n (first fkey)) ast/volatile?))) deps))]
    (cond-> (assoc-in g [:precedents fkey] deps)
      vol? (update :volatile conj fkey))))

(defn- contains-cell? [dep s r c]
  (case (first dep)
    :cell (let [[_ s' r' c'] dep] (and (= s s') (= r r') (= c c')))
    :area (let [[_ s' r1 c1 r2 c2] dep] (and (= s s') (<= r1 r r2) (<= c1 c c2)))
    false))

(defn dependents
  [g [s r c :as ckey]]
  (let [direct (get-in g [:cells ckey])
        candidates (concat (get-in g [:cols [s c]]) (get-in g [:wide s]))
        via-area (filter (fn [f] (some #(contains-cell? % s r c) (get-in g [:precedents f])))
                         candidates)]
    (into (or direct #{}) via-area)))

(defn name-dependents [g name] (get-in g [:names (str/upper-case name)] #{}))

(defn precedents [g fkey] (get-in g [:precedents fkey] #{}))
