(ns sutra.capps.walk
  (:require [sutra.capps.bind :as bind]
            [sutra.kit.coerce :refer [->kw]]))

(defn kind
  [x]
  (cond (or (bind/variable-ref? x) (bind/fill-ref? x)) :reference
        (map? x)                                     :map
        (or (vector? x) (seq? x) (set? x))           :seq
        :else                                        :value))

(defn leaf-ref?
  [x]
  (= :reference (kind x)))

(defn children?
  [x]
  (and (sequential? x) (not (leaf-ref? x))))

(def fill-slots
  {:loop    {:body :item :group-head :item}
   :overlay {:body :arg}})

(defn fills-below
  [node k]
  (when (map? node)
    (get-in fill-slots [(some-> (:kind node) ->kw) k])))

(defn- rebuild
  [go raw? x]
  (case (kind x)
    :map  (reduce-kv (fn [m k v] (assoc m k (if (raw? x k) v (go v)))) (empty x) x)
    :seq  (cond (vector? x) (with-meta (mapv go x) (meta x))
                (set? x)    (into (empty x) (map go) x)
                :else       (with-meta (apply list (map go x)) (meta x)))
    x))

(defn walk-tree
  ([hit x] (walk-tree hit (fn [_ _] false) x))
  ([hit raw? x]
   (letfn [(go [x] (if-let [[r] (hit x)] r (rebuild go raw? x)))]
     (go x))))

(defn postwalk
  [f x]
  (letfn [(go [x] (f (rebuild go (fn [_ _] false) x)))]
    (go x)))

(defn with-vars
  [scope node]
  (if (map? (:vars node))
    (into scope (map (fn [[k v]] [(->kw k) v])) (:vars node))
    scope))

(defn- step
  [ctx node k down]
  (cond-> (update ctx :path conj k)
    (fills-below node k) (update :fills conj (fills-below node k))
    (= :vars k)          (assoc :decl? true)
    down                 (down node k)))

(defn visit
  ([f x] (visit f x {}))
  ([f x {:keys [scope fills references? down ctx]}]
   (letfn [(go [x ctx decls?]
             (case (if decls? :map (kind x))
               :reference (when references? (f x ctx))
               :map (let [ctx (update ctx :scope with-vars x)]
                      (concat (f x ctx)
                              (mapcat (fn [[k v]]
                                        (go v (step ctx x k down)
                                            (and (= :vars k) (map? v))))
                                      x)))
               :seq (mapcat (fn [i v] (go v (update ctx :path conj i) false)) (range) x)
               nil))]
     (vec (go x (merge ctx {:path [] :fills (set fills) :scope (or scope {}) :decl? false})
             false)))))
