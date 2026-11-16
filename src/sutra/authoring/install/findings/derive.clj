(ns sutra.authoring.install.findings.derive
  (:require [sutra.types.attribute :as attribute]
            [sutra.types.derive-graph :as dg]
            [sutra.types.link-side :as ls]
            [sutra.types.rid :as rid]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.findings :as fnd]))

(set! *warn-on-reflection* true)

(defn- derived-of
  [objs]
  (fn [T]
    (into {}
          (keep (fn [a] (when-let [e (attribute/derived-expression a)]
                          [(:attribute-name a) e])))
          (attribute/rows (live/lookup objs T)))))

(defn- far-of
  [links-near objs]
  (fn [T handle]
    (:far (or (ls/table-side (links-near T) T handle)
              (some #(let [I (rid/->name %)] (ls/table-side (links-near I) I handle))
                    (:implements (live/lookup objs T)))))))

(defn- normal
  [chain]
  (let [start (first (sort-by str chain))]
    (vec (take (count chain) (drop-while #(not= % start) (cycle chain))))))

(defn- strong
  [deps u]
  (let [fwd (loop [seen #{u} todo [u]]
              (if-let [[w & more] (seq todo)]
                (let [n (remove seen (deps w))] (recur (into seen n) (into (vec more) n)))
                seen))]
    (loop [r #{u}]
      (let [r' (into r (filter #(some r (deps %))) fwd)]
        (if (= r r') r (recur r'))))))

(defn- chain-of
  [deps s u v]
  (if (= u v)
    [u]
    (loop [q (conj clojure.lang.PersistentQueue/EMPTY v) parent {v nil}]
      (let [w (peek q)]
        (if (= w u)
          (vec (cons u (butlast (reverse (take-while some? (iterate parent u))))))
          (let [ns (sort-by str (remove #(contains? parent %) (filter s (deps w))))]
            (recur (into (pop q) ns) (reduce #(assoc %1 %2 w) parent ns))))))))

(defn added-chains
  [deps1 added]
  (let [deps   (memoize deps1)
        part   (memoize #(strong deps %))
        inside (for [[u v :as e] added :let [s (part u)] :when (contains? s v)] [s e])]
    (sort-by str
             (for [[s es] (group-by first inside)
                   :let [[u v] (first (sort-by (fn [[a b]] [(str a) (str b)]) (map second es)))]]
               (normal (chain-of deps s u v))))))

(defn- handles-of
  [table]
  (into #{} (for [[l {:keys [forward-api-name reverse-api-name]}] table
                  h [(or forward-api-name l) (or reverse-api-name l)]]
              (keyword (name h)))))

(defn- local
  [d]
  (let [{:keys [derive-handle]} (diff/index d)
        cur     (:current d)
        ob      (get-in d [:before :object-types])
        oa      (get-in d [:after :object-types])
        near    (diff/links-near d)
        named   (fn [k] (or (get-in cur [:object-types k (rid/name-field-of :object-types)]) k))
        changed (diff/changed d :object-type)
        readers (into #{} (for [h  (into #{} (mapcat (comp handles-of near)) changed)
                                k  (derive-handle h)]
                            (named k)))
        dob     (derived-of ob)
        doa     (derived-of oa)
        fb      (far-of near ob)
        fa      (far-of near oa)
        deps1   (fn [[T a]] (if-let [e (get (doa T) a)] (dg/deps-of doa fa T e) #{}))
        added   (for [T (into changed readers)
                      a (into (set (keys (dob T))) (keys (doa T)))
                      :let [e1 (get (doa T) a)] :when e1
                      :let [e0 (get (dob T) a)
                            d0 (if e0 (dg/deps-of dob fb T e0) #{})]
                      v (dg/deps-of doa fa T e1) :when (not (contains? d0 v))]
                  [[T a] v])]
    (added-chains deps1 added)))

(def derived-cycles
  {:find (fn [d]
           (when (seq (diff/changed d :object-type))
             (for [chain (local d)]
               {:rule :derived-cycle :entity-class :object-type
                :entity (ffirst chain) :property (second (first chain))
                ::fnd/ctx {:chain chain}})))
   :says {:derived-cycle
          (fn [{{:keys [chain]} ::fnd/ctx}]
            (dg/cycle-message (fn [[T a]] (str (name T) "." (name a))) chain))}})
