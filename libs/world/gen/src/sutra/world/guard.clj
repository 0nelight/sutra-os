(ns sutra.world.guard)

(defn- walked! [] (throw (ex-info "a section was walked" {::walked true})))

(defn walked? [e] (boolean (::walked (ex-data e))))

(deftype Guard [^clojure.lang.IPersistentMap m]
  clojure.lang.IPersistentMap
  (assoc [_ k v] (Guard. (.assoc m k v)))
  (assocEx [_ k v] (Guard. (.assocEx m k v)))
  (without [_ k] (Guard. (.without m k)))
  (containsKey [_ k] (.containsKey m k))
  (entryAt [_ k] (.entryAt m k))
  (count [_] (.count m))
  (cons [_ o] (Guard. (.cons m o)))
  (empty [_] (Guard. {}))
  (equiv [_ _] (walked!))
  (seq [_] (walked!))
  (valAt [_ k] (.valAt m k))
  (valAt [_ k nf] (.valAt m k nf))
  (iterator [_] (walked!))
  clojure.lang.IKVReduce
  (kvreduce [_ _ _] (walked!))
  clojure.lang.IFn
  (invoke [_ k] (.valAt m k))
  (invoke [_ k nf] (.valAt m k nf)))

(defn guarded
  [d]
  (update d :ents update-vals #(if (map? %) (Guard. %) %)))

(defn unguarded
  [W]
  (update-vals W #(if (instance? Guard %) (.-m ^Guard %) %)))
