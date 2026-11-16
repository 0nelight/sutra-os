(ns sutra.calx.engine
  (:require [clojure.string :as str]
            [sutra.calx.value :as v]
            [sutra.calx.address :as addr]
            [sutra.calx.eval :as ev]
            [sutra.calx.graph :as graph]
            [sutra.calx.functions :as functions]
            [sutra.calx.i18n :as i18n]
            [sutra.calx.parser :as parser]))

(set! *warn-on-reflection* true)

(defn cell-at [wb [s r c]] (get-in wb [:sheets s :cells [r c]]))

(defn occupied?
  [cell]
  (boolean (or (:ast cell) (:invalid cell) (some? (:value cell)))))

(defn name-ast
  [wb name sheet]
  (let [up (str/upper-case name)]
    (:ast (or (get-in wb [:names [sheet up]]) (get-in wb [:names [nil up]])))))

(defn registry [wb] (merge functions/registry (:functions wb)))

(defn- region-contains? [[ar ac] [nr nc] r c]
  (and (<= ar r (+ ar nr -1)) (<= ac c (+ ac nc -1))))

(defn claimants
  [spills blocked [s r c :as k]]
  (into #{}
        (comp (filter (fn [[[as ar ac :as a] size]]
                        (and (= as s) (not= a k) (region-contains? [ar ac] size r c))))
              (map first))
        (concat spills blocked)))

(defn- region-keys [[s r c] [nr nc]]
  (for [i (range nr) j (range nc) :when (or (pos? i) (pos? j))]
    [s (+ r i) (+ c j)]))

(defn base-ctx
  [wb value-at spill-at]
  (let [cfg (:config wb)
        rng (java.util.Random. (long (or (:seed cfg) (System/nanoTime))))
        names-by-id (into {} (map (fn [[id sh]] [id (:name sh)])) (:sheets wb))
        ids-by-name (into {} (map (fn [[id sh]] [(str/upper-case (:name sh)) id])) (:sheets wb))]
    {:env {}
     :fns (registry wb)
     :names (fn [n sheet] (name-ast wb n sheet))
     :value-at value-at
     :spill-at spill-at
     :extent (fn [s] (get-in wb [:sheets s :extent] [0 0]))
     :sheet-id (fn [n] (ids-by-name (str/upper-case n)))
     :sheet-name (fn [id] (names-by-id id))
     :sheet-names (fn [] (mapv names-by-id (:order wb)))
     :sheet-index (fn [id]
                    (some (fn [[i x]] (when (= x id) (inc i))) (map-indexed vector (:order wb))))
     :now (or (:now cfg)
              (let [t (java.time.LocalDateTime/now)]
                (+ (v/date->serial (.getYear t) (.getMonthValue t) (.getDayOfMonth t))
                   (v/time->fraction (.getHour t) (.getMinute t) (.getSecond t)))))
     :rand (fn [] (.nextDouble rng))
     :locale (i18n/locale (:locale cfg))
     :formula-at (fn [s r c]
                   (when-let [ast (:ast (get-in wb [:sheets s :cells [r c]]))]
                     (i18n/localize (str "=" (parser/unparse ast {:sheet-name names-by-id}))
                                    (:locale cfg))))
     :wb wb}))

(defn- discover
  [wb g changed volatile]
  (let [spills (:spills wb) blocked (:blocked wb)
        formula? (fn [k] (some? (:ast (cell-at wb k))))]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY (concat changed volatile))
           seen (set (concat changed volatile))
           edges {}]
      (if-let [k (peek queue)]
        (let [ds (into (graph/dependents g k) (claimants spills blocked k))
              new (remove seen ds)]
          (recur (into (pop queue) new)
                 (into seen new)
                 (if (seq ds) (assoc edges k ds) edges)))
        {:dirty (into #{} (filter formula?) seen)
         :edges edges}))))

(defn- topo-order
  [dirty edges]
  (let [indeg (reduce (fn [m [k ds]]
                        (if (dirty k)
                          (reduce (fn [m d] (if (dirty d) (update m d (fnil inc 0)) m)) m ds)
                          m))
                      {} edges)
        start (filter #(zero? (get indeg % 0)) dirty)]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY (sort start))
           indeg indeg
           out (transient [])]
      (if-let [k (peek queue)]
        (let [ds (filter dirty (get edges k))
              [queue indeg] (reduce (fn [[q m] d]
                                      (let [n (dec (get m d 0))]
                                        [(if (zero? n) (conj q d) q) (assoc m d n)]))
                                    [(pop queue) indeg] ds)]
          (recur queue indeg (conj! out k)))
        (let [out (persistent! out)
              done (set out)]
          (into out (sort (remove done dirty))))))))

(defn- run-pass
  [wb g changed volatile]
  (let [{:keys [dirty edges]} (discover wb g changed volatile)
        order (topo-order dirty edges)
        vals (volatile! {})
        spills (volatile! (:spills wb {}))
        owner (volatile! (:owner wb {}))
        blocked (volatile! (:blocked wb {}))
        pending (volatile! (set dirty))
        stack (volatile! [])
        onstack (volatile! #{})
        cycles (volatile! #{})
        spill-changed (volatile! #{})
        anchor-array (fn [a] (let [x (if (contains? @vals a) (@vals a) (:value (cell-at wb a)))]
                               (when (v/array? x) x)))
        raw (fn [[s r c :as k]]
              (if-let [cell (cell-at wb k)]
                (let [x (if (contains? @vals k) (@vals k) (:value cell))]
                  (if (v/array? x) (ffirst x) x))
                (when-let [[_ ar ac :as a] (@owner k)]
                  (get-in (anchor-array a) [(- r ar) (- c ac)]))))
        eval-cell! (volatile! nil)
        value-at (fn [s r c]
                   (let [k [s r c]]
                     (cond
                       (@onstack k) (let [st @stack
                                          i (.indexOf ^java.util.List st k)]
                                      (vswap! cycles into (subvec st i))
                                      v/ERR-CYCLE)
                       (@pending k) (do (@eval-cell! k) (if (@cycles k) v/ERR-CYCLE (raw k)))
                       :else (let [a (@owner k)]
                               (when (and a (@pending a)) (@eval-cell! a))
                               (raw k)))))
        spill-at (fn [s r c]
                   (let [k [s r c]]
                     (when (@pending k) (@eval-cell! k))
                     (@spills k)))
        base (base-ctx wb value-at spill-at)
        free-region! (fn [a]
                       (when-let [size (@spills a)]
                         (let [ks (region-keys a size)]
                           (vswap! owner #(apply dissoc % ks))
                           (vswap! spill-changed into ks)))
                       (vswap! spills dissoc a)
                       (vswap! blocked dissoc a))
        store! (fn [[s r c :as k] result]
                 (if (and (v/array? result) (or (> (v/rows result) 1) (> (v/cols result) 1)))
                   (let [size [(v/rows result) (v/cols result)]
                         old-size (@spills k)
                         ks (region-keys k size)
                         fits? (and (<= (+ r (first size)) addr/max-rows)
                                    (<= (+ c (second size)) addr/max-cols))
                         blocker? (or (not fits?)
                                      (some (fn [ck]
                                              (or (occupied? (cell-at wb ck))
                                                  (let [o (@owner ck)] (and o (not= o k)))))
                                            ks))]
                     (if blocker?
                       (do (free-region! k)
                           (vswap! blocked assoc k size)
                           (vswap! vals assoc k v/ERR-SPILL))
                       (let [old-arr (anchor-array k)]
                         (when old-size
                           (let [old-ks (set (region-keys k old-size))]
                             (vswap! owner #(apply dissoc % old-ks))
                             (vswap! spill-changed into (remove (set ks) old-ks))))
                         (vswap! blocked dissoc k)
                         (vswap! spills assoc k size)
                         (vswap! owner into (map (fn [ck] [ck k]) ks))
                         (vswap! vals assoc k result)
                         (vswap! spill-changed into
                                (filter (fn [[_ cr cc]]
                                          (not= (get-in old-arr [(- cr r) (- cc c)] ::none)
                                                (get-in result [(- cr r) (- cc c)])))
                                        ks)))))
                   (do (free-region! k)
                       (vswap! vals assoc k result))))]
    (vreset! eval-cell!
            (fn [[s r c :as k]]
              (vswap! pending disj k)
              (vswap! stack conj k)
              (vswap! onstack conj k)
              (let [cell (cell-at wb k)
                    result (try
                             (ev/evaluate-value (assoc base :sheet s :row r :col c) (:ast cell))
                             (catch StackOverflowError _ v/ERR-CYCLE))]
                (vswap! stack pop)
                (vswap! onstack disj k)
                (store! k (if (@cycles k) v/ERR-CYCLE result)))))
    (doseq [k order]
      (when (@pending k) (@eval-cell! k)))
    (doseq [k @cycles] (vswap! vals assoc k v/ERR-CYCLE) (vswap! spills dissoc k))
    (let [vals @vals
          wb' (reduce (fn [wb [[s r c] x]] (assoc-in wb [:sheets s :cells [r c] :value] x)) wb vals)
          wb' (reduce (fn [wb [[s r c] [nr nc]]]
                        (update-in wb [:sheets s :extent]
                                   (fn [[er ec]]
                                     [(max (or er 0) (+ r nr)) (max (or ec 0) (+ c nc))])))
                      wb' @spills)
          changed-values (into (set (keep (fn [[k x]] (when (not= x (:value (cell-at wb k))) k))
                                          vals))
                               @spill-changed)]
      [(assoc wb' :spills @spills :owner @owner :blocked @blocked)
       changed-values
       @spill-changed])))

(defn recalculate
  [wb changed]
  (let [g (:graph wb)]
    (loop [wb wb changed (set changed) all-changes #{} n 0]
      (let [[wb' changes spill-changed] (run-pass wb g changed (when (zero? n) (:volatile g)))
            all-changes (into all-changes changes)]
        (if (and (seq spill-changed) (< n 16))
          (recur wb' spill-changed all-changes (inc n))
          (vary-meta wb' assoc ::changes all-changes))))))

(defn formula-keys [wb]
  (for [[s sheet] (:sheets wb) [[r c] cell] (:cells sheet) :when (:ast cell)] [s r c]))

(defn recalculate-all [wb]
  (recalculate (assoc wb :spills {} :owner {} :blocked {}) (formula-keys wb)))
