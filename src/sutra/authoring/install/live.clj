(ns sutra.authoring.install.live
  (:require [sutra.catalog.world-install :as world-install]
            [sutra.catalog.publishing :as publishing]
            [sutra.world.db :as kdb]
            [sutra.world.index :as ix]
            [sutra.types.rid :as rid])
  (:import (clojure.lang ILookup Associative IPersistentMap IDeref MapEntry)))

(set! *warn-on-reflection* true)

(defn- rank [k] [(str k) (str (type k))])

(defn- last-by
  [f xs]
  (last (sort-by (comp rank f) xs)))

(defn by-name
  [triples]
  (into {} (for [[k ts] (group-by first triples) :when (some? k)]
             [k (peek (last-by second ts))])))

(defn- walked [how]
  (throw (ex-info (str "A section read through the index of names is read as a map ("
                       (name how) "): use live/lookup or live/holds?, or say live/whole")
                  {:kind ::walked :how how})))

(defprotocol Section
  (entry-of [s k]))

(deftype Looked [one full extra]
  ILookup
  (valAt [_ _k] (walked :get))
  (valAt [_ _k _nf] (walked :get))
  IDeref
  (deref [_] @full)
  Section
  (entry-of [_ k] (one k))
  Object
  (hashCode [_] (walked :hash))
  (equals [_ _o] (walked :equiv)))

(extend-protocol Section
  nil
  (entry-of [_ _] nil)
  IPersistentMap
  (entry-of [m k] (when-let [e (find m k)] [(val e)])))

(defn lookup
  ([s k] (lookup s k nil))
  ([s k nf] (if-let [[v] (entry-of s k)] v nf)))

(defn holds?
  [s k]
  (some? (entry-of s k)))

(defn select
  [s ks]
  (into {} (keep (fn [k] (when-let [[v] (entry-of s k)] [k v]))) ks))

(defn looked
  ([one full] (Looked. one full #{}))
  ([one full extra] (Looked. one full extra)))

(defn looked? [x] (instance? Looked x))

(defn whole
  [m]
  (if (looked? m) @m m))

(defn as-lookup
  [s]
  (reify
    ILookup
    (valAt [_ k] (lookup s k))
    (valAt [_ k nf] (lookup s k nf))
    Associative
    (containsKey [_ k] (holds? s k))
    (entryAt [_ k] (when-let [[v] (entry-of s k)] (MapEntry/create k v)))
    IDeref
    (deref [_] (whole s))))

(defn- shown [current kk] (publishing/ref-names current kk))

(defn- keyed
  [current names kk nk n]
  (let [es (get current kk)]
    (distinct
     (concat (for [r (names kk (ix/nm n))
                   :let [v (get (get es r) nk)]
                   :when v]
               [v r])
             (for [x (distinct [n (ix/nm n) (ix/kw n)])
                   :let [e (get es x)]
                   :when (and (some? e) (not (get e nk)))]
               [x x])))))

(defn section
  ([current names kk nk full] (section current names kk nk identity full))
  ([current names kk nk keyf full]
   (let [sh  (shown current kk)
         one (fn [k]
               (when (some? k)
                 (when-let [r (last-by identity
                                       (for [[K r] (keyed current names kk nk k)
                                             :when (= k (keyf K))]
                                         r))]
                   [(sh (get-in current [kk r]))])))]
     (cond
       (nil? (get current kk))                                 {}
       (and (map? (get current kk)) (= nk (ix/name-field kk))) (looked one (delay (full)))
       :else                                                   (full)))))

(defn patched
  [m ops full]
  (if-not (looked? m)
    @full
    (let [^Looked m m
          base (.-one m)]
      (looked (fn [k] (if-let [[op v] (get ops k)] (when (= :put op) [v]) (base k)))
              full
              (into (.-extra m) (keep (fn [[k [op]]] (when (= :put op) k))) ops)))))

(defn named
  [current names kk nk m n]
  (let [ks (if (looked? m)
             (concat (map first (keyed current names kk nk n)) (.-extra ^Looked m))
             (distinct [n (keyword n) (symbol n)]))]
    (sort-by (comp str first)
             (for [k (distinct ks)
                   :when (= n (rid/->name k))
                   :let [e (entry-of m k)]
                   :when e]
               [k (first e)]))))

(defn keys-named
  [names n]
  (distinct (concat [n (ix/nm n) (ix/kw n)]
                    (for [kk (keys rid/name-field-of) r (names kk (ix/nm n))] r))))

(defn normalized
  ([current names kk nk m full]
   (normalized current names kk nk #(keyword (rid/->name %)) m full))
  ([current names kk nk keyf m full]
   (if-not (looked? m)
     @full
     (looked (fn [k]
               (when (keyword? k)
                 (when-let [[_ v] (last-by first
                                           (filter #(= k (keyf (first %)))
                                                   (named current names kk nk m (name k))))]
                   [v])))
             full))))

(defn merged
  [m1 m2]
  (let [full (delay (merge (whole m1) m2))]
    (if-not (looked? m1)
      @full
      (looked (fn [k] (or (entry-of m2 k) (entry-of m1 k))) full))))

(defn whole-section
  [current kk nk]
  (update-vals (by-name (for [[r v] (get current kk)] [(or (get v nk) r) r v]))
               (shown current kk)))

(defn index-of
  [d]
  {:names     (fn [sec n] (map second (kdb/nodes d :by-name-str [sec n])))
   :producing (fn [kind v] (map second (kdb/nodes d :edges [:output kind v])))
   :reading   (fn [kind v] (map second (kdb/nodes d :edges [:input kind v])))
   :fk-col    (fn [c] (map second (world-install/nodes d [:fk-col c])))
   :dataset-out (fn [v] (map second (world-install/nodes d [:dataset-out v])))
   :fk-link   (fn [v] (map second (world-install/nodes d [:fk-link v])))
   :backed-by (fn [v] (map second (world-install/nodes d [:backed-by v])))
   :calling   (fn [f] (world-install/nodes d [:calls f]))
   :link-end  (fn [t] (map second (world-install/nodes d [:link-end t])))
   :writeback-of (fn [t] (map second (world-install/nodes d [:writeback-of t])))
   :fn-named  (fn [f] (world-install/nodes d [:fn-named f]))
   :derive-handle (fn [h] (map second (world-install/nodes d [:derive-handle h])))
   :loops     (fn [] (world-install/loop-state d))
   :referring (fn [v] (kdb/nodes d :refs-in v))})

(defn world-index
  [W]
  (index-of (kdb/db W world-install/views)))

(defn by-field
  ([current names kk nk f full] (by-field current names kk nk f full nil))
  ([current names kk nk f full {:keys [own-key?]}]
   (looked (fn [k]
             (when (some? k)
               (let [es (get current kk)
                     r  (last-by identity
                                 (concat (for [r (names kk (ix/nm k))
                                               :let [e (get es r)]
                                               :when (and (map? e) (= k (get e nk)))]
                                           r)
                                         (when own-key?
                                           (when-let [e (get es k)]
                                             (when (nil? (get e nk)) [k])))))]
                 (when r [(f r (get es r))]))))
           full)))

(defn mapped
  [m f full]
  (if-not (looked? m)
    @full
    (let [^Looked m m
          base (.-one m)]
      (looked (fn [k] (when-let [[v] (base k)] [(f k v)]))
              full
              (.-extra m)))))
